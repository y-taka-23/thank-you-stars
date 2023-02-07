FROM haskell:8.10.7-slim AS builder

COPY . /thank-you-stars
WORKDIR /thank-you-stars

RUN cabal update
RUN cabal install


FROM gcr.io/distroless/base:nonroot

COPY --from=builder --chown=nonroot:nonroot /root/.cabal/bin/thank-you-stars /usr/local/bin/

WORKDIR /home/nonroot/project
ENTRYPOINT [ "thank-you-stars" ]
