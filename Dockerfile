FROM haskell:9.4.5 as build

WORKDIR /events

RUN apt-get update && apt-get install -y libgmp10 postgresql postgresql-contrib libpq-dev

COPY events.cabal /events/

RUN cabal update
RUN cabal build --only-dependencies

COPY src /events/src

RUN cabal install --installdir=. --enable-executable-static

# FROM ubuntu:latest
FROM alpine:latest
WORKDIR /events

# RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y libpq-dev
# libc6-compat is needed to be able to run the binary on alpine
RUN apk update && apk add --no-cache libpq libc6-compat so:libgmp.so.10

COPY --from=build /events/events /events/events
COPY frontend/index.html /events/frontend/index.html
COPY frontend/static/ /events/frontend/static/

# ENTRYPOINT ["cabal", "run"]
CMD ["/events/events"]

