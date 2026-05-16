box::use(
  alpaca[get_api_keys, get_base_url, get_data_base_url, AlpacaMarketData, AlpacaAccount, AlpacaTrading]
)

KEYS <- get_api_keys()
ALPACA_DATA_ENDPOINT <- get_data_base_url()
ALPACA_API_ENDPOINT <- get_base_url()

#' @export
market <- AlpacaMarketData$new(
  keys = KEYS,
  base_url = ALPACA_API_ENDPOINT,
  data_base_url = ALPACA_DATA_ENDPOINT
)

#' @export
account <- AlpacaAccount$new(
  keys = KEYS,
  base_url = ALPACA_API_ENDPOINT
)

#' @export
trading <- AlpacaTrading$new(
  keys = KEYS,
  base_url = ALPACA_API_ENDPOINT
)
