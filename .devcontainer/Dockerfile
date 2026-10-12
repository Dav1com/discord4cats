FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

ARG USERNAME=vscode
ARG JDK_VERSION=25

ENV DEBIAN_FRONTEND=noninteractive

# Developer utilities, fish (4.x PPA) and the Temurin JDK repo
RUN apt-get update \
    && apt-get install -y --no-install-recommends software-properties-common gnupg \
    && add-apt-repository -y ppa:fish-shell/release-4 \
    && curl -fsSL https://packages.adoptium.net/artifactory/api/gpg/key/public \
        | gpg --dearmor -o /usr/share/keyrings/adoptium.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/adoptium.gpg] https://packages.adoptium.net/artifactory/deb $(. /etc/os-release && echo $VERSION_CODENAME) main" \
        > /etc/apt/sources.list.d/adoptium.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
        temurin-${JDK_VERSION}-jdk \
        fish \
        build-essential git curl wget ca-certificates unzip zip xz-utils \
        jq ripgrep fd-find fzf bat tree htop less man-db openssh-client \
        procps lsof iproute2 dnsutils netcat-openbsd \
    && ln -sf /usr/bin/fdfind /usr/local/bin/fd \
    && ln -sf /usr/bin/batcat /usr/local/bin/bat \
    && ln -s /usr/lib/jvm/temurin-${JDK_VERSION}-jdk-* /usr/lib/jvm/default-jdk \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

ENV JAVA_HOME=/usr/lib/jvm/default-jdk

# Cross-platform builds: Node.js runs the Scala.js tests, clang/lld link Scala Native.
# s2n-tls is needed to link Ember (and any fs2 networking) on Scala Native; Ubuntu does not package it.
ARG S2N_VERSION=v1.7.11
RUN apt-get update \
    && apt-get install -y --no-install-recommends nodejs clang lld cmake libssl-dev \
    && git clone --depth 1 --branch ${S2N_VERSION} https://github.com/aws/s2n-tls.git /tmp/s2n-tls \
    && cmake -S /tmp/s2n-tls -B /tmp/s2n-tls/build -DBUILD_TESTING=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_INSTALL_PREFIX=/usr/local \
    && cmake --build /tmp/s2n-tls/build -j"$(nproc)" \
    && cmake --install /tmp/s2n-tls/build \
    && ldconfig \
    && rm -rf /tmp/s2n-tls \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Discord's libdave (DAVE end-to-end encryption for voice), which the voice module binds on the JVM and
# Scala Native, built with its MLS dependency (cisco/mlspp) against the system OpenSSL. Same steps and
# pinned commits as scripts/install-libdave.sh. npm installs the Node.js side (@snazzah/davey).
ARG LIBDAVE_REF=8de72b1f8a2ac3c5a5270755bb8091a62e3c6169
ARG MLSPP_REF=1cc50a124a3bc4e143a787ec934280dc70c1034d
RUN apt-get update \
    && apt-get install -y --no-install-recommends nlohmann-json3-dev npm \
    && git init -q /tmp/mlspp && git -C /tmp/mlspp fetch -q --depth 1 https://github.com/cisco/mlspp.git ${MLSPP_REF} \
    && git -C /tmp/mlspp checkout -q FETCH_HEAD \
    && cmake -S /tmp/mlspp -B /tmp/mlspp/build -DCMAKE_BUILD_TYPE=Release -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
        -DBUILD_SHARED_LIBS=OFF -DDISABLE_GREASE=ON -DMLS_CXX_NAMESPACE=mlspp -DTESTING=OFF -DCMAKE_INSTALL_PREFIX=/tmp/mls-deps \
    && cmake --build /tmp/mlspp/build -j"$(nproc)" \
    && cmake --install /tmp/mlspp/build \
    && git init -q /tmp/libdave && git -C /tmp/libdave fetch -q --depth 1 https://github.com/discord/libdave.git ${LIBDAVE_REF} \
    && git -C /tmp/libdave checkout -q FETCH_HEAD \
    && cmake -S /tmp/libdave/cpp -B /tmp/libdave/build -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DTESTING=OFF \
        -DCMAKE_PREFIX_PATH=/tmp/mls-deps -DCMAKE_INSTALL_PREFIX=/usr/local \
    && cmake --build /tmp/libdave/build --target libdave -j"$(nproc)" \
    && cmake --install /tmp/libdave/build \
    && ldconfig \
    && rm -rf /tmp/mlspp /tmp/mls-deps /tmp/libdave \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Rootless podman inside the (rootless) devcontainer.
# The outer container only has ~65k uids mapped, so the user's subordinate range
# must fit inside it, skipping uid 1000 itself. newuidmap/newgidmap need file
# capabilities instead of setuid to write the nested uid/gid maps.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        podman crun fuse-overlayfs uidmap slirp4netns passt containernetworking-plugins catatonit \
    && apt-get clean && rm -rf /var/lib/apt/lists/* \
    && echo "${USERNAME}:1:999" > /etc/subuid \
    && echo "${USERNAME}:1001:64535" >> /etc/subuid \
    && cp /etc/subuid /etc/subgid \
    && chmod 0755 /usr/bin/newuidmap /usr/bin/newgidmap \
    && setcap cap_setuid=ep /usr/bin/newuidmap \
    && setcap cap_setgid=ep /usr/bin/newgidmap
COPY --chown=${USERNAME}:${USERNAME} containers/ /home/${USERNAME}/.config/containers/

# Helix (latest GitHub release)
RUN ARCH="$(uname -m)" \
    && TAG="$(curl -fsSL https://api.github.com/repos/helix-editor/helix/releases/latest | jq -r .tag_name)" \
    && curl -fsSL "https://github.com/helix-editor/helix/releases/download/${TAG}/helix-${TAG}-${ARCH}-linux.tar.xz" \
        | tar -xJ -C /opt \
    && mv "/opt/helix-${TAG}-${ARCH}-linux" /opt/helix \
    && ln -s /opt/helix/hx /usr/local/bin/hx
ENV HELIX_RUNTIME=/opt/helix/runtime \
    EDITOR=hx \
    VISUAL=hx

# Coursier + latest sbt, Scala, scala-cli, Metals (LSP for Helix) and scalafmt
RUN ARCH="$(uname -m)" \
    && curl -fsSL "https://github.com/coursier/coursier/releases/latest/download/cs-${ARCH}-pc-linux.gz" \
        | gzip -d > /usr/local/bin/cs \
    && chmod +x /usr/local/bin/cs \
    && COURSIER_CACHE=/opt/coursier/cache COURSIER_ARCHIVE_CACHE=/opt/coursier/arc \
        cs install --install-dir /usr/local/bin sbt scala scala-cli metals scalafmt \
    && chmod -R a+rX /opt/coursier

# Fish as the default shell; pre-create cache dirs so named volumes inherit ownership
RUN chsh -s /usr/bin/fish ${USERNAME} \
    && mkdir -p /home/${USERNAME}/.claude /home/${USERNAME}/.cache /home/${USERNAME}/.sbt \
        /home/${USERNAME}/.ivy2 /home/${USERNAME}/.config/fish/conf.d \
        /home/${USERNAME}/.local/share/containers \
    && chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}

USER ${USERNAME}

# Claude Code (native installer -> ~/.local/bin/claude)
RUN curl -fsSL https://claude.ai/install.sh | bash
ENV PATH=/home/${USERNAME}/.local/bin:${PATH}

RUN echo 'fish_add_path -g ~/.local/bin' > /home/${USERNAME}/.config/fish/conf.d/path.fish \
    && echo 'set -g fish_greeting' >> /home/${USERNAME}/.config/fish/conf.d/path.fish
