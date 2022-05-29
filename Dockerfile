FROM debian:bookworm-slim

RUN \
    set -ex; \
    uname -a; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
    ; \
    export BOOTSTRAP_HASKELL_NONINTERACTIVE=1; \
    curl --proto '=https' --tlsv1.2 -sSf --output get-ghcup https://get-ghcup.haskell.org; \
    chmod +x get-ghcup; \
    ./get-ghcup || true; \
    for f in /root/.ghcup/logs; do echo $f; cat $f; done; \
    exit 1
