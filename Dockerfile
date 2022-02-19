FROM haskell:8.10

WORKDIR /events

RUN apt-get update && apt-get install -y libgmp10 postgresql postgresql-contrib libpq-dev

COPY events.cabal /events/

RUN cabal update
RUN cabal build --only-dependencies

COPY reset-database.sh /events/
COPY src /events/src

COPY frontend/index.html /events/frontend/index.html
COPY frontend/static/ /events/frontend/static/

RUN cabal update

RUN cabal build --only-dependencies
RUN cabal build

# FROM haskell:8 as build
# RUN cabal install --installdir=. --enable-executable-static

ENTRYPOINT ["cabal", "run"]



