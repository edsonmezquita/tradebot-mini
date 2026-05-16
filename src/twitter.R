box::use(
  httr2,
  digest[ hmac ],
  jsonlite[ toJSON, fromJSON, base64_enc ],
  data.table[ as.data.table, data.table, rbindlist ],
  utils[ URLencode ]
)

# ---- OAuth 1.0a signing ----------------------------------------------------
.enc <- function(x) URLencode(as.character(x), reserved = TRUE, repeated = TRUE)

.oauth1_header <- function(method, url, query_params = list(), body_form_params = list()) {
  oauth <- list(
    oauth_consumer_key = Sys.getenv("TWITTER_API_CONSUMER_KEY"),
    oauth_nonce = paste0(sample(c(0:9, letters), 32, replace = TRUE), collapse = ""),
    oauth_signature_method = "HMAC-SHA1",
    oauth_timestamp = as.character(as.integer(Sys.time())),
    oauth_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
    oauth_version = "1.0"
  )
  # OAuth 1.0a signs OAuth params + query params + (form body if any).
  # JSON bodies are NOT part of the signature.
  all_params <- c(oauth, query_params, body_form_params)
  pairs <- mapply(
    function(k, v) paste0(.enc(k), "=", .enc(v)),
    names(all_params),
    all_params,
    USE.NAMES = FALSE
  )
  pairs <- sort(pairs)
  base_string <- paste(
    toupper(method),
    .enc(url),
    .enc(paste(pairs, collapse = "&")),
    sep = "&"
  )
  signing_key <- paste0(
    .enc(Sys.getenv("TWITTER_API_SECRET_KEY")),
    "&",
    .enc(Sys.getenv("TWITTER_TOKEN_SECRET"))
  )
  oauth$oauth_signature <- base64_enc(
    hmac(signing_key, base_string, algo = "sha1", raw = TRUE)
  )
  paste0(
    "OAuth ",
    paste(
      vapply(
        names(oauth),
        function(k) {
          paste0(.enc(k), '="', .enc(oauth[[k]]), '"')
        },
        character(1)
      ),
      collapse = ", "
    )
  )
}

# ---- helpers ---------------------------------------------------------------
.post_signed_json <- function(url, body_list, query_params = list()) {
  auth <- .oauth1_header(
    method = "POST",
    url = url,
    query_params = query_params
  )
  req <- httr2$request(url) |>
    httr2$req_method("POST") |>
    httr2$req_headers(Authorization = auth, `Content-Type` = "application/json") |>
    httr2$req_body_raw(toJSON(body_list, auto_unbox = TRUE), type = "application/json") |>
    httr2$req_error(is_error = function(r) FALSE)
  if (length(query_params) > 0) {
    req <- httr2$req_url_query(req, !!!query_params)
  }
  resp <- httr2$req_perform(req)
  list(
    status = httr2$resp_status(resp),
    body = httr2$resp_body_json(resp, simplifyVector = FALSE)
  )
}

.get_signed <- function(url, query_params = list()) {
  auth <- .oauth1_header(
    method = "GET",
    url = url,
    query_params = query_params
  )
  req <- httr2$request(url) |>
    httr2$req_method("GET") |>
    httr2$req_headers(Authorization = auth) |>
    httr2$req_error(is_error = function(r) FALSE)
  if (length(query_params) > 0) {
    req <- httr2$req_url_query(req, !!!query_params)
  }
  resp <- httr2$req_perform(req)
  list(
    status = httr2$resp_status(resp),
    body = httr2$resp_body_json(resp, simplifyVector = FALSE)
  )
}

# ---- public API ------------------------------------------------------------

#' Post a tweet (optionally as a reply to another tweet).
#' @param text Tweet body, max 280 chars on free/pay-per-use.
#' @param in_reply_to_tweet_id Optional tweet id this is a reply to.
#' @return list(success, id, text, error_message)
#' @export
post_tweet <- function(text, in_reply_to_tweet_id = NULL) {
  stopifnot(nzchar(text))
  body <- list(text = text)
  if (!is.null(in_reply_to_tweet_id) && nzchar(in_reply_to_tweet_id)) {
    body$reply <- list(in_reply_to_tweet_id = as.character(in_reply_to_tweet_id))
  }
  res <- .post_signed_json("https://api.x.com/2/tweets", body)
  if (res$status == 201L) {
    return(list(
      success = TRUE,
      id = res$body$data$id,
      text = res$body$data$text,
      error_message = NA_character_
    ))
  }
  list(
    success = FALSE,
    id = NA_character_,
    text = text,
    error_message = sprintf("HTTP %d: %s", res$status, toJSON(res$body, auto_unbox = TRUE))
  )
}

#' Look up the authenticated user's id + handle.
#' @return list(id, username, name)
#' @export
get_me <- function() {
  res <- .get_signed("https://api.x.com/2/users/me")
  if (res$status != 200L) {
    stop(sprintf("get_me HTTP %d: %s", res$status, toJSON(res$body, auto_unbox = TRUE)))
  }
  list(
    id = res$body$data$id,
    username = res$body$data$username,
    name = res$body$data$name
  )
}

#' Read recent @-mentions of a user.
#' @param user_id Numeric X user id (as character string).
#' @param since_id Optional — only return mentions newer than this tweet id.
#' @param max_results Max mentions to return per call (5-100, default 20).
#' @return data.table with id, text, author_id, created_at columns (possibly empty).
#' @export
get_mentions <- function(user_id, since_id = NULL, max_results = 20L) {
  url <- sprintf("https://api.x.com/2/users/%s/mentions", user_id)
  q <- list(
    max_results = as.integer(max_results),
    `tweet.fields` = "created_at,author_id,conversation_id"
  )
  if (!is.null(since_id) && nzchar(since_id)) {
    q$since_id <- as.character(since_id)
  }
  res <- .get_signed(url, q)
  if (res$status != 200L) {
    warning(sprintf("get_mentions HTTP %d: %s", res$status, toJSON(res$body, auto_unbox = TRUE)))
    return(data.table())
  }
  data <- res$body$data
  if (is.null(data) || length(data) == 0L) {
    return(data.table())
  }
  rbindlist(
    lapply(data, function(t) {
      list(
        id = t$id,
        text = t$text,
        author_id = t$author_id,
        conversation_id = t$conversation_id,
        created_at = t$created_at
      )
    }),
    fill = TRUE
  )
}

.STATE_PATH <- "state/twitter_state.json"

#' Load persisted Twitter state (user_id, last_mention_id, last_tweet_id, last_tweet_at).
#' Returns an empty list on first run.
#' @export
load_twitter_state <- function() {
  if (!file.exists(.STATE_PATH)) {
    return(list())
  }
  tryCatch(fromJSON(.STATE_PATH), error = function(e) list())
}

#' Persist Twitter state. Pass any subset of fields to merge into existing state.
#' @export
save_twitter_state <- function(...) {
  current <- load_twitter_state()
  updates <- list(...)
  for (k in names(updates)) {
    current[[k]] <- updates[[k]]
  }
  if (!dir.exists(dirname(.STATE_PATH))) {
    dir.create(dirname(.STATE_PATH), recursive = TRUE)
  }
  writeLines(toJSON(current, auto_unbox = TRUE, pretty = TRUE), .STATE_PATH)
  invisible(current)
}

#' Read replies to a specific tweet (via conversation_id search).
#' Note: X search excludes the original tweet by default; we filter further.
#' @param tweet_id The tweet whose replies we want.
#' @return data.table (possibly empty).
#' @export
get_replies_to <- function(tweet_id) {
  url <- "https://api.x.com/2/tweets/search/recent"
  q <- list(
    query = sprintf("conversation_id:%s", as.character(tweet_id)),
    `tweet.fields` = "created_at,author_id,in_reply_to_user_id",
    max_results = 50L
  )
  res <- .get_signed(url, q)
  if (res$status != 200L) {
    warning(sprintf("get_replies HTTP %d: %s", res$status, toJSON(res$body, auto_unbox = TRUE)))
    return(data.table())
  }
  data <- res$body$data
  if (is.null(data) || length(data) == 0L) {
    return(data.table())
  }
  rbindlist(
    lapply(data, function(t) {
      list(
        id = t$id,
        text = t$text,
        author_id = t$author_id,
        created_at = t$created_at
      )
    }),
    fill = TRUE
  )
}
