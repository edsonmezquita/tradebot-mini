box::use(
  ./searxng[ search ],
  ./features[ FEATURE_REGISTRY, format_feature_menu ],
  ./universe[ validate_picks ]
)

# ============================================================================
# TOOL: search
# ============================================================================
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

# ============================================================================
# TOOL: validate_symbols
# ============================================================================
#' @export
TOOL_VALIDATE_SYMBOLS <- list(
  type = "function",
  `function` = list(
    name = "validate_symbols",
    description = paste(
      "Check whether a list of ticker symbols is tradable on Alpaca",
      "(NYSE / NASDAQ / ARCA / AMEX, common stock).",
      "Returns { valid: [...], invalid: [...] }.",
      "MANDATORY: call this before finalising your picks. Failure to validate",
      "your final list will cause the trader to skip this iteration entirely",
      "and your rationale will be discarded. Validate, correct any invalid",
      "tickers (aliases, OTC names, foreign listings), and re-validate until",
      "all picks are valid before writing your JSON reply."
    ),
    parameters = list(
      type = "object",
      properties = list(
        symbols = list(
          type = "array",
          items = list(type = "string"),
          description = "Ticker symbols to validate, e.g. ['AAPL','BRK.B']"
        )
      ),
      required = list("symbols")
    )
  )
)

# ============================================================================
# TOOL: get_bars_multi
# Used as an `ask_for_args` schema — the model picks symbols/timeframe/days,
# R does the actual fetching. There is no handler for this tool in the
# agentic loop.
# ============================================================================
#' @export
TOOL_GET_BARS_MULTI <- list(
  type = "function",
  `function` = list(
    name = "get_bars_multi",
    description = paste(
      "Pick the right historical bar parameters for a set of tickers given",
      "the swing-trade horizon (1 day to 1 month) and each name's character.",
      "R will execute the actual fetch with the parameters you choose."
    ),
    parameters = list(
      type = "object",
      properties = list(
        symbols = list(
          type = "array",
          items = list(type = "string"),
          description = "Tickers to fetch, e.g. ['AAPL','MSFT','NVDA']"
        ),
        timeframe = list(
          type = "string",
          description = "Bar size: '1Day', '1Hour', '15Min', etc."
        ),
        days = list(
          type = "integer",
          description = "Days of history to pull, ending today."
        ),
        rationale = list(
          type = "string",
          description = "One short sentence on why you chose this timeframe and window for these names."
        )
      ),
      required = list("symbols", "timeframe", "days", "rationale")
    )
  )
)

# ============================================================================
# TOOL: compute_features
# Also `ask_for_args` style — model picks indicator specs, R runs them
# against the bars fetched in the previous stage. No handler.
# ============================================================================
#' @export
TOOL_COMPUTE_FEATURES <- list(
  type = "function",
  `function` = list(
    name = "compute_features",
    description = paste0(
      "Pick a tailored set of technical indicators (and parameters per indicator) ",
      "to compute on the bars previously fetched. Multi-instance same indicator ",
      "with different params is encouraged (e.g. rsi period=7 AND period=21 for ",
      "divergence). R will execute the computation.\n\n",
      "Available features (defaults shown — override any of them):\n",
      format_feature_menu()
    ),
    parameters = list(
      type = "object",
      properties = list(
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
                description = "List of EMA periods. Multiple periods also yield ema_cross_<short>_<long> signals."
              ),
              window = list(type = "integer", description = "Slope window (obv_slope)")
            ),
            required = list("name")
          )
        ),
        rationale = list(
          type = "string",
          description = "One short paragraph: why these indicators and these parameters for these names and this horizon."
        )
      ),
      required = list("features", "rationale")
    )
  )
)

# ============================================================================
# Handlers — only for the agentic stage 1 (search loop with validation).
# ============================================================================
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

handle_validate_symbols <- function(args) {
  symbols <- unlist(args$symbols, use.names = FALSE)
  validate_picks(symbols)
}

#' @export
TOOL_HANDLERS <- list(
  search = handle_search,
  validate_symbols = handle_validate_symbols
)
