# markov-bot

Generates random sentences from your tweet history using the Markov chain, and post them into Twitter.

# Usage

## 1. Obtain tweet.js

Download your Twitter archive from [Twitter](https://twitter.com/), which is available on `Settings > Account > "Request your archive"`

## 2. Obtain credentials

Register your application on [Twitter Application Management](https://apps.twitter.com/) with "Read and Write" permissions at least, then set your credentials to the following environment variables.

- `MARKOV_BOT_CONSUMER_KEY`
- `MARKOV_BOT_CONSUMER_SECRET`
- `MARKOV_BOT_ACCESS_TOKEN`
- `MARKOV_BOT_ACCESS_TOKEN_SECRET`

## 3. Run

```sh
$ docker run \
  -e MARKOV_BOT_CONSUMER_KEY=... \ # or --env-file=...
  -e MARKOV_BOT_CONSUMER_SECRET=... \
  -e MARKOV_BOT_ACCESS_TOKEN=... \
  -e MARKOV_BOT_ACCESS_TOKEN_SECRET=... \
  ryotakameoka/markov-bot \
  --tweet-js=...
```

# Scheduled execution on [Heroku](https://heroku.com/)

Note that `tweet.js` must be hosted somewhere (with a public URL).
Personally I have a repository for the file and deploy it to [Netlify](https://www.netlify.com/).

Instead, you can also build your own image extending [`docker.io/ryotakameoka/markov-bot`](https://hub.docker.com/r/ryotakameoka/markov-bot/) which contains your `tweet.js` inside and push it to Heroku Container Registry.

## Install the Heroku CLI

https://devcenter.heroku.com/articles/heroku-cli#download-and-install

## Login to Heroku

```sh
$ heroku login
```

## Create an app

Replace `<APP_NAME>` with an arbitrary name.

```sh
$ heroku apps:create <APP_NAME>
```

## Configure environment variables

```sh
$ heroku config:set --app=<APP_NAME> MARKOV_BOT_CONSUMER_KEY=...
$ heroku config:set --app=<APP_NAME> MARKOV_BOT_CONSUMER_SECRET=...
$ heroku config:set --app=<APP_NAME> MARKOV_BOT_ACCESS_TOKEN=...
$ heroku config:set --app=<APP_NAME> MARKOV_BOT_ACCESS_TOKEN_SECRET=...
```

## Add [Heroku Scheduler](https://devcenter.heroku.com/articles/scheduler) to your app

```sh
$ heroku addons:create --app <APP_NAME> scheduler:standard
```

## Pull the pre-built Docker image from [Docker Hub](https://hub.docker.com/r/ryotakameoka/markov-bot/)

```sh
$ docker pull docker.io/ryotakameoka/markov-bot
```

## Push the Docker image to Heroku

Note that Heroku Scheduler only supports "web" dyno.

```sh
$ heroku container:login
$ docker tag docker.io/ryotakameoka/markov-bot registry.heroku.com/<APP_NAME>/web
$ docker push registry.heroku.com/<APP_NAME>/web
$ heroku container:release --app <APP_NAME> web
```

## Open the Heroku Scheduler in the browser

```sh
$ heroku addons:open --app <APP_NAME> scheduler
```

## Press "Add Job" button and fill "Run Command" section as below

```sh
$ --tweet-js <URL_OF_TWEET_JS>
```

# Runtime options

Option | Description | Example values
--- | --- | ---
`--tweet-js` | File path or URL for tweet.js (required) | `./tweet.js` `https://example.com/path/to/tweet.js`
`--order` | Order of Markov chain, in positive integer (optional) | `2` `3`

# Related links

- [Markov chain - Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/Markov_chain)
- [morishin/poem-generator](https://github.com/morishin/poem-generator)

# Lisence

This software is distributed under BSD3 lisence.
