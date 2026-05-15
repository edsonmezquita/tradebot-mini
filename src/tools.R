box::use(
  ./searxng[search],
  ./alpaca[market],
  ./features[compute_features, FEATURE_REGISTRY],
  lubridate
)

# ---- bars cache ------------------------------------------------------------
# Tools that produce large data (e.g. get_bars_multi) stash the result here
# under a short id and return only the id + a small summary to the model.
# A subsequent tool call (e.g. compute_features) references the data by id.
# This keeps each step explicit AND avoids round-tripping huge OHLCV tables
# through the model's JSON arguments.
#
# Inspect / mutate from R:
#   ls(tools_state$bars)
#   tools_state$bars$bars_abc123
#' @export
tools_state <- list(bars = new.env(parent = emptyenv()))

.new_bars_id <- function() {
  paste0("bars_", format(Sys.time(), "%H%M%S"), "_", sample.int(9999, 1))
}

# ---- TOOL: search ----------------------------------------------------------
#' @export
TOOL_SEARCH <- list(
  type = "function",
  `function` = list(
    name = "search",
    description = paste(
      "Search the web via a self-hosted SearXNG instance.",
      "Returns up to 10 results (title, url, content snippet, engine).",
      "Use to gather news, prices, sentiment, or any external info."
    ),
    parameters = list(
      type = "object",
      properties = list(
        query = list(
          type = "string",
          description = "Search query, e.g. 'AAPL earnings May 2026'"
        ),
        engines = list(
          type = "string",
          description = paste(
            "Optional comma-separated engines to restrict to.",
            "Available: google, bing, duckduckgo, brave, mojeek, qwant,",
            "startpage, yandex, baidu. Omit to use all enabled engines."
          )
        ),
        language = list(
          type = "string",
          description = "Language code, e.g. 'en' or 'all'. Default 'all'."
        )
      ),
      required = list("query")
    )
  )
)

# ---- TOOL: get_bars_multi --------------------------------------------------
#' @export
TOOL_GET_BARS <- list(
  type = "function",
  `function` = list(
    name = "get_bars_multi",
    description = paste(
      "Fetch historical OHLCV bars from Alpaca for one or more US tickers.",
      "Stores the bars in a server-side cache and returns a SUMMARY plus a",
      "`bars_id` you can pass to compute_features in a follow-up call.",
      "Pass multiple symbols in one call rather than calling repeatedly."
    ),
    parameters = list(
      type = "object",
      properties = list(
        symbols = list(
          type = "array",
          description = "Ticker symbols, e.g. ['AAPL', 'MSFT', 'NVDA']",
          items = list(type = "string")
        ),
        days = list(
          type = "integer",
          description = "How many days of history to pull. Default 90."
        ),
        timeframe = list(
          type = "string",
          description = "Bar size, e.g. '1Day', '1Hour', '15Min'. Default '1Day'."
        )
      ),
      required = list("symbols")
    )
  )
)

# ---- TOOL: compute_features -----------------------------------------------
.feature_menu <- paste(
  vapply(
    names(FEATURE_REGISTRY),
    function(n) {
      entry <- FEATURE_REGISTRY[[n]]
      params <- entry$params
      if (length(params) == 0L) {
        param_str <- "(no params)"
      } else {
        param_str <- paste(
          vapply(
            names(params),
            function(p) sprintf("%s=%s", p, paste(params[[p]], collapse = ",")),
            character(1)
          ),
          collapse = ", "
        )
      }
      sprintf("- %s [defaults: %s]: %s", n, param_str, entry$description)
    },
    character(1)
  ),
  collapse = "\n"
)

#' @export
TOOL_COMPUTE_FEATURES <- list(
  type = "function",
  `function` = list(
    name = "compute_features",
    description = paste0(
      "Compute selected technical indicators from previously-fetched bars.\n",
      "REQUIRES a `bars_id` returned by an earlier get_bars_multi call.\n",
      "Returns one row per symbol with the latest indicator values; output ",
      "column names encode the parameters you chose ",
      "(e.g. rsi_14, macd_hist_12_26_9, bb_pct_b_20_2, ema_cross_9_21).\n\n",
      "You may pass the SAME indicator multiple times with different ",
      "parameters to compare (e.g. rsi period=7 AND period=21).\n\n",
      "Each feature is an OBJECT: { \"name\": \"<feature>\", ...params }.\n\n",
      "Available features (defaults shown — override any of them):\n",
      .feature_menu
    ),
    parameters = list(
      type = "object",
      properties = list(
        bars_id = list(
          type = "string",
          description = "The bars_id returned by a prior get_bars_multi call."
        ),
        features = list(
          type = "array",
          description = paste(
            "List of feature spec objects. Example:",
            '[{"name":"rsi","period":14},',
            '{"name":"rsi","period":21},',
            '{"name":"macd","fast":12,"slow":26,"signal":9},',
            '{"name":"ema","periods":[9,21,50]}]'
          ),
          items = list(
            type = "object",
            properties = list(
              name = list(
                type = "string",
                description = "Indicator name from the menu.",
                enum = as.list(names(FEATURE_REGISTRY))
              ),
              period = list(type = "integer", description = "Lookback period (rsi/atr/bbands/supertrend)"),
              sd = list(type = "number", description = "Std-dev multiplier (bbands)"),
              fast = list(type = "integer", description = "Fast period (macd)"),
              slow = list(type = "integer", description = "Slow period (macd)"),
              signal = list(type = "integer", description = "Signal smoothing period (macd)"),
              multiplier = list(type = "number", description = "ATR multiplier (supertrend)"),
              periods = list(
                type = "array",
                items = list(type = "integer"),
                description = "List of EMA periods (ema). Multiple periods also yield ema_cross_<short>_<long> signals."
              ),
              window = list(type = "integer", description = "Slope window (obv_slope)")
            ),
            required = list("name")
          )
        )
      ),
      required = list("bars_id", "features")
    )
  )
)

#' All tool schemas bundled for ask_with_tools().
#' @export
TOOLS <- list(TOOL_SEARCH, TOOL_GET_BARS, TOOL_COMPUTE_FEATURES)

# ---- handlers --------------------------------------------------------------
handle_search <- function(args) {
  language <- args$language
  if (is.null(language) || !nzchar(language)) {
    language <- "all"
  }
  res <- search(
    query = args$query,
    engines = args$engines,
    language = language,
    pageno = 1
  )
  if (nrow(res) > 10) {
    res <- res[seq_len(10)]
  }
  return(res)
}

handle_get_bars_multi <- function(args) {
  symbols <- unlist(args$symbols, use.names = FALSE)
  days <- if (is.null(args$days)) 90L else as.integer(args$days)
  timeframe <- if (is.null(args$timeframe) || !nzchar(args$timeframe)) {
    "1Day"
  } else {
    args$timeframe
  }
  now <- lubridate$now(tzone = "UTC")
  then <- now - lubridate$ddays(days)
  fmt <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  bars <- market$get_bars_multi(
    symbols = symbols,
    timeframe = timeframe,
    start = fmt(then),
    end = fmt(now),
    feed = "iex"
  )

  bars_id <- .new_bars_id()
  assign(bars_id, bars, envir = tools_state$bars)

  # Compact summary returned to the model — not the full bars.
  summary_per_symbol <- bars[,
    list(
      n_bars = .N,
      first_date = as.character(min(timestamp)),
      last_date = as.character(max(timestamp)),
      last_close = close[.N]
    ),
    by = symbol
  ]

  return(list(
    bars_id = bars_id,
    timeframe = timeframe,
    summary = summary_per_symbol
  ))
}

handle_compute_features <- function(args) {
  bars_id <- args$bars_id
  if (is.null(bars_id) || !exists(bars_id, envir = tools_state$bars)) {
    stop(
      "Unknown bars_id '",
      bars_id,
      "'. ",
      "Call get_bars_multi first and pass its returned bars_id."
    )
  }
  bars <- get(bars_id, envir = tools_state$bars)
  # `features` arrives as a list of named lists (one per feature spec),
  # already parsed from JSON by ask_with_tools. Pass straight through.
  return(compute_features(bars = bars, features = args$features))
}

#' Handlers bundled for ask_with_tools(). Names must match `function$name`.
#' @export
TOOL_HANDLERS <- list(
  search = handle_search,
  get_bars_multi = handle_get_bars_multi,
  compute_features = handle_compute_features
)
