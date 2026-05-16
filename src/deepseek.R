box::use(
  httr2,
  jsonlite[ toJSON ]
)

# ---- internal -------------------------------------------------------------
.post <- function(body, api_key, timeout) {
  resp <- httr2$request("https://api.deepseek.com") |>
    httr2$req_url_path("/chat/completions") |>
    httr2$req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2$req_body_raw(
      toJSON(body, auto_unbox = TRUE, null = "null"),
      type = "application/json"
    ) |>
    httr2$req_timeout(timeout) |>
    httr2$req_user_agent("tradebot-mini/0.1") |>
    httr2$req_perform()
  return(httr2$resp_body_json(resp, simplifyVector = FALSE))
}

.build_messages <- function(prompt, system) {
  msgs <- list()
  if (!is.null(system) && nzchar(system)) {
    msgs <- c(msgs, list(list(role = "system", content = system)))
  }
  if (!is.null(prompt) && nzchar(prompt)) {
    msgs <- c(msgs, list(list(role = "user", content = prompt)))
  }
  return(msgs)
}

# ---- public ---------------------------------------------------------------

#' Extract the textual content of every `role: "tool"` message from a
#' conversation transcript (the `messages` field returned by ask_with_tools).
#' Useful when re-prompting after validation failure so you don't re-run
#' expensive tool calls (e.g. searches).
#' @export
extract_tool_results <- function(messages) {
  tool_msgs <- Filter(
    function(m) !is.null(m$role) && m$role == "tool",
    messages
  )
  if (length(tool_msgs) == 0L) {
    return("")
  }
  paste(
    vapply(
      tool_msgs,
      function(m) paste0("[tool result]\n", as.character(m$content)),
      character(1)
    ),
    collapse = "\n\n"
  )
}

#' One-shot structured-output via tool calling.
#'
#' Forces the model to emit a single tool call matching `tool`'s schema, then
#' returns the parsed arguments. There is NO agentic loop and the model
#' NEVER sees a tool result — we're using the tool schema purely as a strict
#' JSON shape constraint on the output. R takes the args and runs whatever
#' it needs to.
#'
#' @param prompt User prompt.
#' @param tool A single tool schema object (not a list of them).
#' @param system Optional system prompt.
#' @param model,temperature,max_tokens,api_key,timeout See [ask()].
#' @return Named list of parsed arguments.
#' @export
ask_for_args <- function(
  prompt,
  tool,
  system = NULL,
  model = "deepseek-v4-pro",
  temperature = 0.7,
  max_tokens = NULL,
  api_key = Sys.getenv("DEEPSEEK_KEY"),
  timeout = 60
) {
  stopifnot(nzchar(prompt), nzchar(api_key), is.list(tool))

  body <- list(
    model = model,
    messages = .build_messages(prompt, system),
    tools = list(tool),
    tool_choice = "required",
    temperature = temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )
  body <- body[!vapply(body, is.null, logical(1))]

  parsed <- .post(body, api_key, timeout)
  msg <- parsed$choices[[1]]$message
  calls <- msg$tool_calls

  if (is.null(calls) || length(calls) == 0L) {
    stop("Model did not emit a tool call. content: ", substr(as.character(msg$content), 1, 200))
  }
  raw_args <- calls[[1]]$`function`$arguments
  if (!nzchar(raw_args)) {
    return(list())
  }
  return(jsonlite::fromJSON(raw_args, simplifyVector = FALSE))
}

#' Call the DeepSeek chat completions API.
#'
#' @param prompt User message (string).
#' @param system Optional system prompt to steer the model.
#' @param model DeepSeek model ID. Default "deepseek-v4-pro" — 1M context,
#'   higher-quality reasoning. "deepseek-v4-flash" is the cheaper 1M-context
#'   option ($0.14/$0.28 vs $1.74/$3.48 per 1M tokens in/out).
#' @param temperature Sampling temperature (0-2).
#' @param max_tokens Cap on response length. V4 models are "thinking" models —
#'   internal reasoning tokens count against this cap, so set it high enough
#'   (>= a few hundred) or `content` may come back empty.
#' @param json If TRUE, sets `response_format = {type: "json_object"}` so the
#'   model is forced to return a single valid JSON document in `content`. Per
#'   DeepSeek's docs you must also include the word "json" in your prompt and
#'   ideally show an example of the desired shape, otherwise the call may
#'   return an empty string.
#' @param api_key DeepSeek API key. Defaults to env var DEEPSEEK_KEY.
#' @param timeout Request timeout in seconds.
#' @return The assistant's reply as a single string.
#' @export
ask <- function(
  prompt,
  system = NULL,
  model = "deepseek-v4-pro",
  temperature = 0.7,
  max_tokens = NULL,
  json = FALSE,
  api_key = Sys.getenv("DEEPSEEK_KEY"),
  timeout = 60
) {
  stopifnot(nzchar(prompt), nzchar(api_key))

  body <- list(
    model = model,
    messages = .build_messages(prompt, system),
    temperature = temperature,
    max_tokens = max_tokens,
    stream = FALSE
  )
  if (isTRUE(json)) {
    body$response_format <- list(type = "json_object")
  }
  body <- body[!vapply(body, is.null, logical(1))]

  parsed <- .post(body, api_key, timeout)
  return(parsed$choices[[1]]$message$content)
}

#' Call DeepSeek with tool/function calling.
#'
#' Runs an OpenAI-style tool-call loop: sends the prompt + tool schemas; if
#' the model asks to call a tool, the matching R handler runs locally and the
#' result is fed back. Repeats until the model returns final text or `max_iter`
#' is hit.
#'
#' @param prompt User message.
#' @param tools List of tool schemas in OpenAI format. See example below.
#' @param handlers Named list of R functions, names matching `tools[[i]]$function$name`.
#'   Each handler receives one argument: a named list of the parsed JSON
#'   arguments the model passed.
#' @param system Optional system prompt.
#' @param model,temperature,max_tokens,api_key,timeout See [ask()].
#' @param json If TRUE, the FINAL assistant reply (after all tool calls finish)
#'   must be valid JSON — sets `response_format = {type: "json_object"}`. As
#'   with [ask()], include the word "json" and a shape example in your prompt.
#'   Tool-call turns themselves are unaffected.
#' @param max_iter Hard cap on tool-call round-trips. Local safety net — the
#'   DeepSeek API itself imposes no limit (their sample uses `while True`).
#'   Prevents runaway loops if a handler keeps erroring or the model keeps
#'   re-calling the same tool. Raise if you need deeper tool chains.
#' @param verbose If TRUE, prints each tool call and result.
#' @return List with `content` (final assistant text) and `messages` (full
#'   conversation transcript including tool calls/results).
#'
#' @examples
#' \dontrun{
#'   tools <- list(list(
#'     type = "function",
#'     `function` = list(
#'       name = "get_price",
#'       description = "Get the latest stock price for a ticker.",
#'       parameters = list(
#'         type = "object",
#'         properties = list(
#'           ticker = list(type = "string", description = "Ticker symbol")
#'         ),
#'         required = list("ticker")
#'       )
#'     )
#'   ))
#'   handlers <- list(get_price = function(args) {
#'     list(ticker = args$ticker, price = 187.42)
#'   })
#'   ask_with_tools("What's AAPL trading at?", tools, handlers)
#' }
#' @export
ask_with_tools <- function(
  prompt,
  tools,
  handlers,
  system = NULL,
  model = "deepseek-v4-pro",
  temperature = 0.7,
  max_tokens = NULL,
  json = FALSE,
  api_key = Sys.getenv("DEEPSEEK_KEY"),
  timeout = 60,
  max_iter = 5,
  verbose = FALSE
) {
  stopifnot(
    nzchar(prompt),
    nzchar(api_key),
    is.list(tools),
    length(tools) > 0,
    is.list(handlers),
    !is.null(names(handlers))
  )

  messages <- .build_messages(prompt, system)

  for (iter in seq_len(max_iter)) {
    body <- list(
      model = model,
      messages = messages,
      tools = tools,
      tool_choice = "auto",
      temperature = temperature,
      max_tokens = max_tokens,
      stream = FALSE
    )
    if (isTRUE(json)) {
      body$response_format <- list(type = "json_object")
    }
    body <- body[!vapply(body, is.null, logical(1))]

    parsed <- .post(body, api_key, timeout)
    msg <- parsed$choices[[1]]$message
    messages <- c(messages, list(msg))

    calls <- msg$tool_calls
    if (is.null(calls) || length(calls) == 0) {
      return(list(content = msg$content, messages = messages))
    }

    for (call in calls) {
      name <- call$`function`$name
      raw_args <- call$`function`$arguments
      args <- if (nzchar(raw_args)) {
        jsonlite::fromJSON(raw_args, simplifyVector = FALSE)
      } else {
        list()
      }

      handler <- handlers[[name]]
      if (is.null(handler)) {
        result <- list(error = paste0("No handler registered for tool '", name, "'"))
      } else {
        result <- tryCatch(
          handler(args),
          error = function(e) list(error = conditionMessage(e))
        )
      }

      if (verbose) {
        cat(sprintf("[tool] %s(%s) -> %s\n", name, raw_args, toJSON(result, auto_unbox = TRUE)))
      }

      messages <- c(
        messages,
        list(list(
          role = "tool",
          tool_call_id = call$id,
          content = toJSON(result, auto_unbox = TRUE)
        ))
      )
    }
  }

  return(list(
    content = NULL,
    messages = messages,
    error = sprintf("max_iter (%d) reached without final answer", max_iter)
  ))
}
