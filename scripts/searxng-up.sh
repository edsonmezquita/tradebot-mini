#!/usr/bin/env bash
# Start the local SearXNG container in the background.
set -euo pipefail
cd "$(dirname "$0")/../searxng"
docker compose up -d
echo "SearXNG running at http://localhost:8080"
