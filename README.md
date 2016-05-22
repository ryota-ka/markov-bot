# markov-bot

Generates random sentences from your tweet history using the Markov chain, and post them into Twitter.

# Requirements

## [MeCab](http://taku910.github.io/mecab/)

### Mac OS X

```sh
$ brew install mecab mecab-ipadic
```

### Debian

```sh
$ apt-get install mecab libmecab-dev mecab-ipadic-utf8
```

## [Stack](http://haskellstack.org/)

### Mac OS X

```sh
$ brew install haskell-stack
```

### Debian

Follow [the installation guide on the official document](http://docs.haskellstack.org/en/stable/install_and_upgrade/#debian)

# Usage

### 1. Obtain tweets.csv

Download your Twitter archive from [Twitter](https://twitter.com/), which is available on `Settings > Account > "Request your archive"`

### 2. Clone and build

```sh
$ git clone git@github.com:ryota-ka/markov-bot.git
$ cd markov-bot
$ stack build
```

### 3. Set credentials

Register your application on [Twitter Application Management](https://apps.twitter.com/) with "Read and Write" permissions at least, then set your credentials to the following environment variables.

- `MARKOV_BOT_CONSUMER_KEY`
- `MARKOV_BOT_CONSUMER_SECRET`
- `MARKOV_BOT_ACCESS_TOKEN`
- `MARKOV_BOT_ACCESS_TOKEN_SECRET`

### 4. Run

```sh
$ stack exec -- markov-bot-exe --tweets-csv /path/to/tweets.csv
```

# Runtime options

Option | Description | Example values
--- | --- | ---
`--interval` | The time between status updates (in seconds) | `1800` `86400`
`--tweets-csv` | File path or URL for tweets.csv (required) | `./tweets.csv` `https://example.com/path/to/tweets.csv`

# Using Docker

### 1. Build

```sh
$ docker build -t markov-bot .
```

### 2. Run

```sh
# (put tweets.csv in current directory)
$ docker run -d -e "MARKOV_BOT_CONSUMER_KEY=<YOUR_CONSUMER_KEY>" \
-e "MARKOV_BOT_CONSUMER_SECRET=<YOUR_CONSUMER_SECRET>" \
-e "MARKOV_BOT_ACCESS_TOKEN=<YOUR_ACCESS_TOKEN>" \
-e "MARKOV_BOT_ACCESS_TOKEN_SECRET=<YOUR_TOKEN_SECRET>" \
-v $(pwd)/tweets.csv:/markov-bot/tweets.csv \
markov-bot -- --tweets-csv ./tweets.csv --interval 1800
```

# Related links

- [Markov chain - Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/Markov_chain)
- [morishin/poem-generator](https://github.com/morishin/poem-generator)

# Lisence

This software is distributed under BSD3 lisence.
