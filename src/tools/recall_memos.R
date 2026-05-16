box::use(
  ../memos[read_memos]
)

#' @export
TOOL_RECALL_MEMOS <- list(
  type = "function",
  `function` = list(
    name = "recall_memos",
    description = paste(
      "Read prior cycle memos from the bot's append-only history.",
      "Each memo records WHAT WAS DONE on a past cycle (picks, trades, factual",
      "rationale). Use this to maintain continuity — see what positions are",
      "still on your mind, avoid re-pitching old trades, look up historical",
      "activity on a specific symbol, or check what you were doing N days ago.",
      "Returns matching rows. Empty result on first run (no prior memos)."
    ),
    parameters = list(
      type = "object",
      properties = list(
        limit = list(
          type = "integer",
          description = "Max rows to return (default 10, cap 100)."
        ),
        order = list(
          type = "string",
          enum = list("newest", "oldest"),
          description = "Sort order. Default 'newest'."
        ),
        ticker = list(
          type = "string",
          description = "Optional: only memos that mentioned this symbol (case-insensitive)."
        ),
        since = list(
          type = "string",
          description = "Optional ISO date floor, e.g. '2026-04-01' or '2026-04-01T00:00:00Z'."
        ),
        until = list(
          type = "string",
          description = "Optional ISO date ceiling."
        )
      ),
      required = list()
    )
  )
)

#' @export
handle_recall_memos <- function(args) {
  limit <- if (is.null(args$limit)) 10L else min(as.integer(args$limit), 100L)
  order <- if (is.null(args$order) || !nzchar(args$order)) "newest" else args$order
  return(read_memos(
    limit = limit,
    order = order,
    ticker = args$ticker,
    since = args$since,
    until = args$until
  ))
}
