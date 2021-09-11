# sign up:
# https://coinmarketcap.com/api/
# rate/call limits:
# https://pro.coinmarketcap.com/api/features
# sign in:
# https://pro.coinmarketcap.com/account/
# documentation
# https://coinmarketcap.com/api/documentation/v1/
# *******************************************************************************************************
require("pbapply");require("data.table");require("httr");require("rvest");require("dplyr")
require("lubridate");require("jsonlite")
PASS <- new.env()
assign("apikey","insert_api_key_here",envir = PASS)
# HELPER FUNCTION - Converts timestamp to local timestamp
# format TimeZone
fixTZ = function(timeStamp){
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                     units = "hours")),0)
  as.POSIXct(timeStamp + hours(tmDIFF), tz= Sys.timezone())
}
# *******************************************************************************************************
# https://coinmarketcap.com/api/documentation/v1/#operation/getV1FiatMap
getFiat = function()
{
  # url
  url = paste0("https://pro-api.coinmarketcap.com/v1/fiat/map")
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                        `X-CMC_PRO_API_KEY` = PASS$apikey))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  
  fiat <- dt[["data"]] %>% as.data.frame()
  fiat
}
tmp <- getFiat()
# *********************************************************************************
# Turn it into a function - gets latest listings 
# Pass in max number of listings (limit) & fiat currency to use (ex.USD)
getLatestListings = function(limit,fiat)
{
  # build URL
  url = paste0("https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest",
               "?start=1&limit=",limit,"&convert=",fiat)
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                        `X-CMC_PRO_API_KEY` = PASS$apikey))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  
  # convert to data frame
  bse <- cbind(dt$data$id,
               dt$data$name,
               dt$data$symbol,
               dt$data$slug,
               dt$data$date_added,
               dt$data$max_supply,
               dt$data$circulating_supply,
               dt$data$total_supply) %>% as.data.frame
  # format column names
  colnames(bse) = c("ID","Name","Symbol","Slug","DateAdded","MaxSupply","CirculatingSupply","TotalSupply")
  
  # format DateAdded
  bse$DateAdded <- as.Date(bse$DateAdded)
  
  # quote 
  qte <- dt[["data"]][["quote"]] %>% as.data.frame
  qte[[1]]$price              <- qte[[1]]$price %>% round(digits = 4)
  qte[[1]]$last_updated       <- as.POSIXct(qte[[1]]$last_updated, format="%Y-%m-%dT%H:%M:%S.000Z")
  qte[[1]]$percent_change_1h  <- round(qte[[1]]$percent_change_1h/100,4)
  qte[[1]]$percent_change_24h <- round(qte[[1]]$percent_change_24h/100,4)
  qte[[1]]$percent_change_7d  <- round(qte[[1]]$percent_change_7d/100,4)
  qte[[1]]$percent_change_30d <- round(qte[[1]]$percent_change_30d/100,4)
  qte[[1]]$percent_change_60d <- round(qte[[1]]$percent_change_60d/100,4)
  qte[[1]]$percent_change_90d <- round(qte[[1]]$percent_change_90d/100,4)
  qte[[1]]$market_cap_dominance<-round(qte[[1]]$market_cap_dominance/100,4)
  
  # cbind data & quotes
  df <- cbind(bse,qte)
  
  # return table
  df
}
# TEST Function
tmp = getLatestListings(limit = 5000, fiat = "USD")
# *******************************************************************************************************
# https://coinmarketcap.com/api/documentation/v1/#operation/getV1CryptocurrencyQuotesLatest
getLatestQuote= function(symbol, fiat)
{
  # build URL
  url = paste0("https://pro-api.coinmarketcap.com/v1/cryptocurrency/quotes/latest",
               "?convert=",fiat,"&symbol=",symbol)
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                        `X-CMC_PRO_API_KEY` = PASS$apikey))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  
  # extract quote
  qte <- rbindlist(dt$data[[1]]$quote) %>% as.data.frame()
  # format column types
  qte$price <- round(qte$price, 5)
  qte$percent_change_1h <- round(qte$percent_change_1h/100,5)
  qte$percent_change_24h <- round(qte$percent_change_24h/100,5)
  qte$percent_change_7d <- round(qte$percent_change_7d/100,5)
  qte$percent_change_30d <- round(qte$percent_change_30d/100,5)
  qte$percent_change_60d <- round(qte$percent_change_60d/100,5)
  qte$percent_change_90d <- round(qte$percent_change_90d/100,5)
  qte$market_cap_dominance<-round(qte$market_cap_dominance/100,5)
  qte$last_updated <- fixTZ(as.POSIXct(qte$last_updated, format="%Y-%m-%dT%H:%M:%S.000Z"))
  
  # add Meta
  meta <- as.data.frame(cbind(dt$data[[1]]$id,
                              dt$data[[1]]$name,
                              dt$data[[1]]$symbol,
                              dt$data[[1]]$slug,
                              dt$data[[1]]$num_market_pairs,
                              dt$data[[1]]$date_added,
                              ifelse(is.null(dt$data[[1]]$max_supply), NA,dt$data[[1]]$max_supply),
                              dt$data[[1]]$circulating_supply,
                              dt$data[[1]]$total_supply,
                              dt$data[[1]]$is_active
  ))
  colnames(meta) <- c("id","name","symbol","slug","num_market_pairs",
                      "date_added","max_supply","circulating_supply",
                      "total_supply","is_active")
  meta$date_added <- fixTZ(as.POSIXct(meta$date_added, format="%Y-%m-%dT%H:%M:%S.000Z"))
  # combine meta & qte data
  all <- cbind(meta,qte)
  # return data
  all
  
}
# TEST Function
tmp1 = getLatestQuote(symbol = "BTC", fiat = "USD")
tmp2 = getLatestQuote(symbol = "BTC", fiat = "CAD")
# call multiple quotes:
symbols = c("BTC","ETH","DOGE","ADA","XTZ","USDC")
qte <- pblapply(as.list(symbols), function(x){
  tmp <- try(getLatestQuote(symbol=x, fiat="USD"))
  if(!inherits(tmp, 'try-error'))
    tmp
})
# row bind data
qte <- rbindlist(qte,use.names = TRUE,fill = TRUE)
# *******************************************************************************************************
# https://coinmarketcap.com/api/documentation/v1/#operation/getV1GlobalmetricsQuotesLatest
getLatestMetrics = function()
{
  # build url
  url = paste0("https://pro-api.coinmarketcap.com/v1/global-metrics/quotes/latest")
  
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                        `X-CMC_PRO_API_KEY` = PASS$apikey))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  
  # meta
  meta <- as.data.frame(cbind(dt$data[1:22]))
  # quote data
  qte <- rbindlist(dt[["data"]][["quote"]])  %>% t %>% as.data.frame()
  removed <- c("defi_volume_24h","defi_volume_24h_reported","defi_24h_percentage_change",
               "defi_market_cap","stablecoin_volume_24h","stablecoin_volume_24h_reported",
               "stablecoin_24h_percentage_change","stablecoin_market_cap","derivatives_volume_24h",
               "derivatives_volume_24h_reported","derivatives_24h_percentage_change")
  qte <- as.data.frame(qte)
  qte <- as.data.frame(qte[!(rownames(qte) %in% removed),],
                       row.names= rownames(qte)[!(rownames(qte) %in% removed)])
  qte["last_updated",] <- fixTZ(as.POSIXct(qte["last_updated",],format="%Y-%m-%dT%H:%M:%S.%OSZ")) %>% 
    as.character()
  colnames(qte) <- "V1"
  ALL <- rbind(meta,qte)
  colnames(ALL) <- "Value"
  # return df
  ALL  
}

tmp <- getLatestMetrics()
# *******************************************************************************************************
# https://coinmarketcap.com/api/documentation/v1/#operation/getV1ToolsPriceconversion
cryptoConversionRate = function(amount, fromSymbol, toSymbol)
{
  # url
  url = paste0("https://pro-api.coinmarketcap.com/v1/tools/price-conversion",
               "?amount=",amount,"&symbol=",fromSymbol,"&convert=",toSymbol)
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                        `X-CMC_PRO_API_KEY` = PASS$apikey))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  
  fromCrypto <- as.data.frame(cbind(dt$data$id,
                                    dt$data$symbol,
                                    dt$data$name,
                                    dt$data$amount,
                                    dt$data$last_updated))
  colnames(fromCrypto) <- c("id","fromSymbol","fromName","amount","last_updated")
  fromCrypto$last_updated <- fixTZ(as.POSIXct(fromCrypto$last_updated,
                                              format="%Y-%m-%dT%H:%M:%S.%OSZ")) %>% as.character()
  # in case multiple currency conversions
  nCurr<- length(dt[["data"]][["quote"]])
  tmp <- lapply(as.list(1:nCurr), function(ii){
    df <- rbind(dt$data$quote[[ii]]) %>% as.data.frame()
    toCurrName <- names(dt$data$quote)[[ii]]
    df$toSymbol <- toCurrName
    df <- as.data.frame(df[,c("toSymbol","price")])
    colnames(df)[2] <- "amount"
    df
  })
  tmp <- do.call(cbind,tmp)
  # return conversion(s)
  cbind(fromCrypto,tmp)
}
tmp <- cryptoConversionRate(amount=1, fromSymbol = "BTC", toSymbol = "USD")
tmp <- cryptoConversionRate(amount=100, fromSymbol = "USD", toSymbol = "ETH")
tmp <- cryptoConversionRate(amount=1, fromSymbol = "ETH", toSymbol = "ADA")
# *******************************************************************************************************
# https://coinmarketcap.com/api/documentation/v1/#operation/getV1KeyInfo
planInfo = function()
{
  # url
  url = paste0("https://pro-api.coinmarketcap.com/v1/key/info")
  # GET request
  pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                        `X-CMC_PRO_API_KEY` = PASS$apikey))
  # read in content
  dt<- fromJSON(rawToChar(pg$content))
  
  planInfo <- do.call(rbind,dt[["data"]][["plan"]]) %>% as.data.frame()
  colnames(planInfo) <- "value"
  usage <- do.call(rbind,dt[["data"]][["usage"]]) %>% as.data.frame()
  list(planInfo,usage)
}
tmp <- planInfo()
