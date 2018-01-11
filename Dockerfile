FROM alpine as build

ENV MECAB_VERSION 0.996
ENV MECAB_URL https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE

WORKDIR /workdir
COPY . /workdir

RUN apk upgrade --update-cache --available
RUN apk add --update --no-cache \
    bash \
    build-base \
    curl \
    ghc \
    git \
    zlib-dev
RUN curl -SL -o mecab-${MECAB_VERSION}.tar.gz ${MECAB_URL}
RUN tar zxf mecab-${MECAB_VERSION}.tar.gz
RUN cd mecab-${MECAB_VERSION} && \
    ./configure --enable-utf8-only --with-charset=utf8 && \
    make && \
    make install
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN stack config set system-ghc true
RUN stack setup
RUN stack build --ghc-options=-O2 --only-dependencies
RUN stack build --ghc-options=-O2
RUN stack --local-bin-path=/usr/bin install


FROM alpine

ENV MECAB_VERSION 0.996
ENV IPADIC_VERSION 2.7.0-20070801
ENV MECAB_URL https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE
ENV IPADIC_URL https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM

RUN apk add --update --no-cache --virtual build-deps \
        bash \
        build-base \
        curl \
        git \
        && \
    cd && \
    curl -SL -o mecab-${MECAB_VERSION}.tar.gz ${MECAB_URL} && \
    tar zxf mecab-${MECAB_VERSION}.tar.gz && \
    cd mecab-${MECAB_VERSION} && \
    ./configure --enable-utf8-only --with-charset=utf8 && \
    make && \
    make install && \
    curl -SL -o mecab-ipadic-${IPADIC_VERSION}.tar.gz ${IPADIC_URL} && \
    tar zxf mecab-ipadic-${IPADIC_VERSION}.tar.gz && \
    cd mecab-ipadic-${IPADIC_VERSION} && \
    ./configure --with-charset=utf8 && \
    make && \
    make install && \
    cd && \
    rm -rf ./* && \
    apk --no-cache add \
        ca-certificates \
        gmp-dev \
        libffi \
        libgcc \
        libstdc++ \
        && \
    apk del --purge build-deps && \
    rm -rf /var/cache/apk/*

COPY --from=build /usr/bin/markov-bot /usr/bin
ENTRYPOINT ["markov-bot"]
