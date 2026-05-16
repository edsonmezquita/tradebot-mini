box::use(
  ../universe[validate_picks]
)

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

#' @export
handle_validate_symbols <- function(args) {
  symbols <- unlist(args$symbols, use.names = FALSE)
  return(validate_picks(symbols))
}
