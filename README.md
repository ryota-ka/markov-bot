# markov-bot

Generates random sentences from your tweet history using the Markov chain, and post them into Twitter.

# Usage

### 1. Obtain tweets.csv

Download your Twitter archive from [Twitter](https://twitter.com/), which is available on `Settings > Account > "Request your archive"`

### 2. Obtain credentials

Register your application on [Twitter Application Management](https://apps.twitter.com/) with "Read and Write" permissions at least, then set your credentials to the following environment variables.

- `MARKOV_BOT_CONSUMER_KEY`
- `MARKOV_BOT_CONSUMER_SECRET`
- `MARKOV_BOT_ACCESS_TOKEN`
- `MARKOV_BOT_ACCESS_TOKEN_SECRET`

### 3. Run

```sh
$ docker run \
  -e MARKOV_BOT_CONSUMER_KEY=... \ # or --env-file=...
  -e MARKOV_BOT_CONSUMER_SECRET=... \
  -e MARKOV_BOT_ACCESS_TOKEN=... \
  -e MARKOV_BOT_ACCESS_TOKEN_SECRET=... \
  ryotakameoka/markov-bot \
  --tweets-csv=...
```

# Runtime options

Option | Description | Example values
--- | --- | ---
`--tweets-csv` | File path or URL for tweets.csv (required) | `./tweets.csv` `https://example.com/path/to/tweets.csv`

# Related links

- [Markov chain - Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/Markov_chain)
- [morishin/poem-generator](https://github.com/morishin/poem-generator)

# Lisence

This software is distributed under BSD3 lisence.
