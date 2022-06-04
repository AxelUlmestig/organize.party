FROM postgres:14

RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install \
  sqitch
