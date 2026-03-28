# build the server
FROM haskell:9.8.4 AS build-server
WORKDIR /

# Install dependencies for adding the PostgreSQL repository
RUN apt-get update && apt-get install -y curl ca-certificates gnupg lsb-release

# Add PostgreSQL repository
RUN curl https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor | tee /etc/apt/trusted.gpg.d/apt.postgresql.org.gpg >/dev/null
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list

RUN apt-get update && apt-get install -y \
    libgmp10 \
    postgresql-18 \
    postgresql-client-18 \
    libpq5 \
    libpq-dev \
    zlib1g-dev \
    libzstd-dev \
    pkg-config

COPY op-webapi/op-webapi.cabal /op-webapi/
COPY op-cache/op-cache.cabal /op-cache/
COPY op-db/op-db.cabal /op-db/
COPY op-worker/op-worker.cabal /op-worker/
COPY cabal.project /
COPY cabal.project.freeze /

RUN cabal update

# Remove zlib constraints if needed
RUN if [ -f cabal.project.freeze ]; then \
    grep -v "zlib" cabal.project.freeze > cabal.project.freeze.tmp && \
    mv cabal.project.freeze.tmp cabal.project.freeze || true; \
    fi

RUN cabal build op-webapi op-db op-cache --only-dependencies

COPY op-webapi/src /op-webapi/src
COPY op-cache/src /op-cache/src
COPY op-db/src /op-db/src
COPY op-worker/src /op-worker/src

# Build the executable
RUN cabal build op-webapi

# Copy the actual binary from the dist directory, not the symlink
RUN cp $(cabal list-bin op-webapi) /op-webapi/webapi-real

# Create a minimal runtime image for the server
FROM debian:bullseye-slim AS server-runtime
WORKDIR /op-webapi

# Install only runtime dependencies (much smaller than build dependencies)
RUN apt-get update && apt-get install -y \
    libpq5 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    && rm -rf /var/lib/apt/lists/*

# Copy the actual binary, not the symlink
COPY --from=build-server /op-webapi/webapi-real /op-webapi/webapi

# build the frontend
FROM node:24-alpine AS build-frontend
WORKDIR /

RUN apk update && apk add --no-cache curl make

# RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
#   && gunzip elm.gz \
#   && chmod +x elm \
#   && mv elm /usr/local/bin/

RUN ARCH=$(uname -m) && \
    if [ "$ARCH" = "x86_64" ]; then \
        ELM_URL="https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz"; \
    elif [ "$ARCH" = "aarch64" ]; then \
        ELM_URL="https://github.com/lydell/compiler/releases/download/0.19.1/binary-for-linux-arm-64-bit.gz"; \
    else \
        echo "Unsupported architecture: $ARCH"; \
        exit 1; \
    fi && \
    curl -L -o elm.gz "$ELM_URL" && \
    gunzip elm.gz && \
    chmod +x elm && \
    mv elm /usr/local/bin/
# RUN npm install -g elm@0.19.1-5 uglify-js
RUN npm install -g uglify-js

COPY frontend/ /frontend/
COPY scripts /scripts

RUN rm frontend/index.html || true
RUN ./scripts/build-frontend.sh --optimize

# Create final minimal image
FROM debian:trixie-slim AS final
WORKDIR /

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libpq5 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    && rm -rf /var/lib/apt/lists/*

# Copy the server binary
COPY --from=build-server /op-webapi/webapi-real /webapi

# Copy the frontend files
COPY --from=build-frontend /frontend/index.html /frontend/index.html
COPY --from=build-frontend /frontend/static/ /frontend/static/

CMD ["/webapi"]
