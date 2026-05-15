#!/usr/bin/env bash
# Restart SearXNG (picks up settings.yml changes).
set -euo pipefail
cd "$(dirname "$0")/../searxng"
docker compose restart
