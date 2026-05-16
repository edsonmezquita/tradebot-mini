-- All tradebot-mini state. Idempotent — safe to re-run.

-- One row per pipeline cycle. The human-readable history.
CREATE TABLE IF NOT EXISTS memos (
  id         BIGSERIAL PRIMARY KEY,
  cycle_id   UUID        NOT NULL UNIQUE,
  ts         TIMESTAMPTZ NOT NULL DEFAULT now(),
  picks      TEXT        NOT NULL,
  buys       TEXT        NOT NULL DEFAULT '',
  sells      TEXT        NOT NULL DEFAULT '',
  holds      TEXT        NOT NULL DEFAULT '',
  memo       TEXT        NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_memos_ts ON memos (ts DESC);

-- Singleton row holding twitter bookkeeping (last seen mention, last own tweet).
CREATE TABLE IF NOT EXISTS twitter_state (
  id              INT         PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  user_id         TEXT,
  username        TEXT,
  last_mention_id TEXT,
  last_tweet_id   TEXT,
  last_tweet_at   TIMESTAMPTZ,
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- Alpaca tradable-symbol universe. Refreshed monthly (or on demand).
-- The whole table is wiped + reinserted on refresh; `fetched_at` is the
-- single batch timestamp.
CREATE TABLE IF NOT EXISTS alpaca_assets (
  symbol       TEXT        PRIMARY KEY,
  name         TEXT,
  exchange     TEXT,
  asset_class  TEXT,
  tradable     BOOLEAN,
  fractionable BOOLEAN,
  status       TEXT,
  fetched_at   TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_alpaca_assets_fetched_at ON alpaca_assets (fetched_at);

-- Scraped page cache. Avoids re-hitting the same URL across cycles
-- (anti-block + cost saver).
CREATE TABLE IF NOT EXISTS scraped_pages (
  url           TEXT        PRIMARY KEY,
  domain        TEXT,
  content       TEXT,
  grade_usable  BOOLEAN,
  grade_reason  TEXT,
  scraped_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS idx_scraped_pages_domain     ON scraped_pages (domain);
CREATE INDEX IF NOT EXISTS idx_scraped_pages_scraped_at ON scraped_pages (scraped_at DESC);

-- Log every pipeline cycle execution (success or failure).
CREATE TABLE IF NOT EXISTS cycle_runs (
  cycle_id          UUID        PRIMARY KEY,
  started_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
  finished_at       TIMESTAMPTZ,
  iteration         INT,
  success           BOOLEAN,
  error_message     TEXT,
  picks             TEXT,
  decisions_summary TEXT
);
CREATE INDEX IF NOT EXISTS idx_cycle_runs_started_at ON cycle_runs (started_at DESC);

-- Log every tweet the bot actually posts.
CREATE TABLE IF NOT EXISTS tweets_sent (
  tweet_id    TEXT        PRIMARY KEY,
  posted_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
  text        TEXT        NOT NULL,
  in_reply_to TEXT,
  reason      TEXT
);
CREATE INDEX IF NOT EXISTS idx_tweets_sent_posted_at ON tweets_sent (posted_at DESC);
