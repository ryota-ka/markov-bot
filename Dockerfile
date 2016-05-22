FROM samdoshi/haskell-stack:latest

RUN apt-get update && \
    apt-get install -y mecab libmecab-dev mecab-ipadic-utf8

COPY . /markov-bot
WORKDIR /markov-bot

RUN stack setup

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN stack build --only-dependencies

RUN stack build
ENTRYPOINT ["stack", "exec", "markov-bot-exe"]
