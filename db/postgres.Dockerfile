FROM postgres:16

RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install \
  sqitch
