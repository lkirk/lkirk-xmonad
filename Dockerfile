FROM debian:buster-slim

RUN \
    set -ex; \
    uname -a; \
    ; \
    export DEBIAN_FRONTEND=noninteractive; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        libffi6 \
        libffi-dev \
        libgmp10 \
        libgmp-dev \
        libncurses5 \
        libncurses-dev \
        libtinfo5 \
    ; \
    export BOOTSTRAP_HASKELL_NONINTERACTIVE=1; \
    export BOOTSTRAP_HASKELL_INSTALL_STACK=1; \
    curl --proto '=https' --tlsv1.2 -sSf --output get-ghcup https://get-ghcup.haskell.org; \
    chmod +x get-ghcup; \
    set +e; \
    ./get-ghcup; \
    if [ $? != 0 ]; then \
        for f in /root/.ghcup/logs/*; \
            do echo $f; \
            cat $f;
        done; \
    fi; \
    set -e; \
