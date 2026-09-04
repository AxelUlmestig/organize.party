FROM postgres:18

RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install \
  sqitch \
  jq \
  # needed by the semver extension
  make gcc postgresql-server-dev-18 \
  pgxnclient \
  curl ca-certificates

RUN pgxn install semver

RUN \
  set -ex; \
  for EXTENSION_NAME in pg-statecharts pg-statecharts-dev; do \
    PG_VERSION=$(pg_config --version | sed -n 's/^PostgreSQL \([0-9]*\).*/\1/p'); \
    RELEASE_NAME_PATTERN="${EXTENSION_NAME}-${PG_VERSION}"; \
    JQ_QUERY='.assets[] | select(.name | startswith($PATTERN)) | .browser_download_url';  \
    DOWNLOAD_URL=$(curl -s https://api.github.com/repos/kronor-io/statecharts/releases/latest | jq -r --arg PATTERN "$RELEASE_NAME_PATTERN" "$JQ_QUERY"); \
    EXTENSION_PATH=/tmp/extension.deb; \
    curl -L -o $EXTENSION_PATH "$DOWNLOAD_URL"; \
    apt install $EXTENSION_PATH; \
    rm $EXTENSION_PATH; \
  done

