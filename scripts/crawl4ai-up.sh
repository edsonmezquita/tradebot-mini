#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/../crawl4ai"
docker compose up -d
echo "crawl4ai running at http://localhost:11235"
