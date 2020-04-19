FROM nixos/nix:2.3

ENV LANG C.UTF-8

WORKDIR /workdir
COPY . /workdir

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable && \
    nix-channel --update && \
    nix-env -iA nixpkgs.stack nixpkgs.upx && \
    stack build --system-ghc --no-nix-add-gc-roots --ghc-options=-O2 && \
    stack --local-bin-path=/usr/bin install && \
    upx -q -9 --brute /usr/bin/markov-bot && \
    ln -s $(find /nix/store -maxdepth 1 \
      -type d -name '*-gmp-*' \
      -or \
      -type d -name '*-libffi-*' \
      -or \
      -type d -name '*-libmecab-*' \
      -or \
      -type d -name '*-mecab-*' \
      -or \
      -type d -name '*-mecab-ipadic-*' \
      -or \
      -type d -name '*-zlib-*' \
    ) /nix/var/nix/gcroots/ && \
    nix-env -e stack upx && \
    nix-collect-garbage && \
    find /nix/store -maxdepth 1 -type f | xargs rm && \
    rm -rf \
      /nix/store/*-nixpkgs-* \
      /nix/var/ \
      /root/.stack \
      /workdir

WORKDIR /
ENTRYPOINT ["markov-bot"]
