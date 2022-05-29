FROM debian:bookworm-slim
RUN \
    set -ex; \
    uname -a; \
    apt-get install -y --no-install-recommends \
        curl \
    ; \
    export BOOTSTRAP_HASKELL_NONINTERACTIVE=1; \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
