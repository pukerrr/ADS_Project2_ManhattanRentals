median_price <- read.csv('../output/median_price_manhattan.csv')

timeSeries <- function(zipcode)
{
  
  selected_zip <- zipcode
  library(dplyr)
  library(forecast)
  
  year <- rep(2011:2017, each = 12)
  year <- append(year, rep(2018,8), after = length(year))
  month <- rep(1:12, 7)
  month <- append(month, 1:8, after = length(month))
  date <- paste(year, month, sep = '-')
  time_series <- data.frame(date)
  
  selected_price <- median_price %>%
    filter(RegionName %in% selected_zip)
  namevector <- as.character(selected_zip)
  value <- subset(selected_price, RegionName %in% selected_zip)[,11:102]
  time_series[,namevector] <- t(value)
  
  # Type 2 time series data
  all_date <- rep(date, length(selected_zip))
  time_series_price <- data.frame(date = all_date)
  value <- subset(selected_price, RegionName %in% selected_zip)[,11:102]
  value <- t(value)
  x <- vector()
  for (i in 1:length(selected_zip)){
    x <- c(x, value[,i])
  }
  
  time_series_price[,'median_price'] <- x
  zipgroup <- rep(selected_zip, each = 92)
  time_series_price[,'zipcode'] <- factor(zipgroup)
  
  pred <- matrix(NA, length(selected_zip), 12)
  for (i in 1:length(selected_zip)){
    ts <- ts(time_series[,i+1])
    model <- auto.arima(ts)
    pred[i,] <- predict(model, 12)$pred
  }
  
  predall <- pred[1,]
  group_zip <- rep(selected_zip[1], 12)
  pre_year <- rep(c(rep(2018, 4), rep(2019, 8)), length(selected_zip))
  pre_month <- rep(c(9:12, 1:8), length(selected_zip))
  pre_date <- paste(pre_year, pre_month, sep = '-')
  if(nrow(pred)>1){
    for (i in 2:nrow(pred)){
      predall <- append(predall, pred[i,], after = length(predall))
      group_zip <- append(group_zip, rep(selected_zip[i], 12), after = length(group_zip))
    }
  }
  
  new_time_series <- data.frame(date = c(as.character(time_series_price$date), pre_date),
                                median_price = c(time_series_price$median_price, predall),
                                zipcode = c(as.character(time_series_price$zipcode), 
                                            as.character(group_zip)))
  new_time_series$date.new <- as.Date(paste0(new_time_series$date,"-28"),"%Y-%m-%d")
  
  return(new_time_series)
}