#' Tools module — re-exports every TOOL_* schema and TOOL_HANDLERS bundle.
#' Add a new tool: drop a file in this folder defining TOOL_<name> (and
#' handle_<name> if it's used in an agentic loop), then add it to the
#' box::use line and the TOOL_HANDLERS list below.

box::use(
  ./search          [ TOOL_SEARCH,            handle_search           ],
  ./validate_symbols[ TOOL_VALIDATE_SYMBOLS,  handle_validate_symbols ],
  ./get_bars_multi  [ TOOL_GET_BARS_MULTI                             ],
  ./compute_features[ TOOL_COMPUTE_FEATURES                           ],
  ./submit_trades   [ TOOL_SUBMIT_TRADES                              ]
)

#' @export
TOOL_SEARCH <- TOOL_SEARCH

#' @export
TOOL_VALIDATE_SYMBOLS <- TOOL_VALIDATE_SYMBOLS

#' @export
TOOL_GET_BARS_MULTI <- TOOL_GET_BARS_MULTI

#' @export
TOOL_COMPUTE_FEATURES <- TOOL_COMPUTE_FEATURES

#' @export
TOOL_SUBMIT_TRADES <- TOOL_SUBMIT_TRADES

#' Handlers for tools that participate in agentic loops (search + validate).
#' @export
TOOL_HANDLERS <- list(
  search           = handle_search,
  validate_symbols = handle_validate_symbols
)
