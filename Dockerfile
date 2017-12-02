FROM debian:buster as build

WORKDIR /workdir
COPY . /workdir

RUN apt-get update && \
    apt-get install -y \
        curl \
        git \
        libmecab-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack setup
RUN stack build --ghc-options=-O2 --only-dependencies
RUN stack build --ghc-options=-O2
RUN stack --local-bin-path=/usr/bin install

FROM debian:buster-slim
RUN apt-get update -qq && \
    apt-get install -y \
        ca-certificates \
        libmecab-dev \
        libnss-lwres \
        libnss3 \
        mecab \
        mecab-ipadic-utf8 \
        && \
    rm -rf /var/lib/apt/cache/*
COPY --from=build /usr/bin/markov-bot /usr/bin
ENTRYPOINT ["markov-bot"]
