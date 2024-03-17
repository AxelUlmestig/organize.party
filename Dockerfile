# prepare third party libraries needed in runtime
FROM alpine:latest as run
WORKDIR /events

RUN apk update && apk add --no-cache libpq libc6-compat so:libgmp.so.10

# build the server
FROM haskell:9.4.5 as build-server
WORKDIR /events

RUN apt-get update && apt-get install -y libgmp10 postgresql postgresql-contrib libpq-dev

COPY events.cabal /events/

RUN cabal update
RUN cabal build --only-dependencies

COPY src /events/src

RUN cabal install --installdir=/events/ --enable-executable-static

# build the frontend
FROM node:21-alpine as build-frontend
WORKDIR /events

RUN apk update && apk add --no-cache curl make

RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
  && gunzip elm.gz \
  && chmod +x elm \
  && mv elm /usr/local/bin/

RUN npm install uglify-js -g

COPY frontend/ /events/frontend/
COPY scripts /events/scripts

RUN rm frontend/index.html || true
RUN ./scripts/build-frontend.sh --optimize

# create image with executables
FROM run

COPY --from=build-server /events/events /events/events
COPY --from=build-frontend /events/frontend/index.html /events/frontend/index.html
COPY --from=build-frontend /events/frontend/static/ /events/frontend/static/

# ENTRYPOINT ["cabal", "run"]
CMD ["/events/events"]

