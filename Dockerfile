# build the server
FROM haskell:9.8.4 AS build-server
WORKDIR /events

# Install dependencies for adding the PostgreSQL repository
RUN apt-get update && apt-get install -y curl ca-certificates gnupg lsb-release

# Add PostgreSQL repository
RUN curl https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor | tee /etc/apt/trusted.gpg.d/apt.postgresql.org.gpg >/dev/null
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list

RUN apt-get update && apt-get install -y \
    libgmp10 \
    postgresql-17 \
    postgresql-client-17 \
    libpq5 \
    libpq-dev \
    zlib1g-dev \
    libzstd-dev \
    pkg-config

COPY events.cabal /events/
COPY cabal.project.freeze /events/

RUN cabal update

# Remove zlib constraints if needed
RUN if [ -f cabal.project.freeze ]; then \
    grep -v "zlib" cabal.project.freeze > cabal.project.freeze.tmp && \
    mv cabal.project.freeze.tmp cabal.project.freeze || true; \
    fi

RUN cabal build --only-dependencies

COPY src /events/src

# Build the executable
RUN cabal build

# Copy the actual binary from the dist directory, not the symlink
RUN cp $(cabal list-bin events) /events/events-real

# Create a minimal runtime image for the server
FROM debian:bullseye-slim AS server-runtime
WORKDIR /events

# Install only runtime dependencies (much smaller than build dependencies)
RUN apt-get update && apt-get install -y \
    libpq5 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    && rm -rf /var/lib/apt/lists/*

# Copy the actual binary, not the symlink
COPY --from=build-server /events/events-real /events/events

# build the frontend
FROM node:24-alpine AS build-frontend
WORKDIR /events

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

COPY frontend/ /events/frontend/
COPY scripts /events/scripts

RUN rm frontend/index.html || true
RUN ./scripts/build-frontend.sh --optimize

# Create final minimal image
FROM debian:bullseye-slim AS final
WORKDIR /events

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libpq5 \
    libgmp10 \
    zlib1g \
    libzstd1 \
    && rm -rf /var/lib/apt/lists/*

# Copy the server binary
COPY --from=build-server /events/events-real /events/events

# Copy the frontend files
COPY --from=build-frontend /events/frontend/index.html /events/frontend/index.html
COPY --from=build-frontend /events/frontend/static/ /events/frontend/static/

CMD ["/events/events"]
