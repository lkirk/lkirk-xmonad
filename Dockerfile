FROM quay.io/lkirk/haskell-stack as builder
RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
      libx11-dev \
      libxft-dev \
      libxinerama-dev \
      libxrandr-dev \
      libxss-dev

WORKDIR /build
ADD . .
RUN stack build


FROM scratch
COPY --from=builder /root/.stack/snapshots/x86_64-linux-tinfo6/bf68e38f1cb3b9a0f169e9800fc40a8bb0c950626f03cfecd077e31f845e75de/8.10.7/bin/xmonad /
COPY --from=builder /root/.stack/snapshots/x86_64-linux-tinfo6/bf68e38f1cb3b9a0f169e9800fc40a8bb0c950626f03cfecd077e31f845e75de/8.10.7/bin/xmobar /
