# SearXNG (local search API)

Self-hosted [SearXNG](https://docs.searxng.org/) used by tradebot-mini as a
unified search backend (Google, Bing, DDG, Yandex, Baidu, news sources, ...).

## First-time setup

1. Generate a secret key and paste it into `settings.yml` under
   `server.secret_key`:

   ```bash
   openssl rand -hex 32
   ```

2. Start the container:

   ```bash
   cd searxng
   docker compose up -d
   ```

3. Verify:

   ```bash
   curl 'http://localhost:8080/search?q=test&format=json' | jq '.results[0]'
   ```

   The web UI is at <http://localhost:8080>.

## Usage from R

```r
box::use(./src/searxng[ search ])
res <- search("EUR USD forecast", categories = "news")
```

## Stopping

```bash
docker compose down
```
