#!/usr/bin/env bash
# Tail SearXNG logs.
set -euo pipefail
cd "$(dirname "$0")/../searxng"
docker compose logs -f --tail=100
