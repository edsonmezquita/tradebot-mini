#!/usr/bin/env bash
# Stop the local SearXNG container.
set -euo pipefail
cd "$(dirname "$0")/../searxng"
docker compose down
