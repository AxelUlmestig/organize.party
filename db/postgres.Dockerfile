FROM postgres:18

RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install \
  sqitch \
  # needed by the semver plugin
  make gcc postgresql-server-dev-18 \
  pgxnclient

RUN pgxn install \
  # needed by statecharts library
  semver

