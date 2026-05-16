# tradebot-mini — R script container fired by host cron.
# Multi-stage so we cache renv restore separately from source changes.

ARG R_VERSION=4.5.1

# ---------------------------------------------------------------------------
# Stage 1: renv restore
# ---------------------------------------------------------------------------
FROM rocker/r-ver:${R_VERSION} AS deps

# System deps required by RPostgres, httr2, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libpq-dev libssl-dev libcurl4-openssl-dev libxml2-dev \
    libgit2-dev libsodium-dev libfontconfig1-dev libfreetype6-dev \
    libharfbuzz-dev libfribidi-dev libpng-dev libtiff5-dev libjpeg-dev \
    git ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
RUN R -e "install.packages('renv', repos='https://packagemanager.posit.co/cran/latest')"

COPY renv.lock ./
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .Rprofile ./
# hpfi is fetched from a private GitHub repo; mount the build-time token
# secret so renv::restore can authenticate. CI passes it via
# --secret id=github_token,env=GITHUB_TOKEN in the docker build action.
RUN --mount=type=secret,id=github_token \
    GITHUB_PAT="$(cat /run/secrets/github_token 2>/dev/null || true)" \
    R -e "if (nzchar(Sys.getenv('GITHUB_PAT'))) Sys.setenv(GITHUB_PAT = Sys.getenv('GITHUB_PAT')); renv::restore(prompt = FALSE)"

# ---------------------------------------------------------------------------
# Stage 2: runtime
# ---------------------------------------------------------------------------
FROM deps AS runtime

# Source — copied after renv so a source-only change doesn't bust the deps layer.
COPY src ./src
COPY db ./db
COPY main.R ./

ENTRYPOINT ["Rscript", "main.R"]
