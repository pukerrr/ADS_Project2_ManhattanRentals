library(shiny)
library(ggplot2)
library(scales)

hospital <- read.csv("../output/hospital_count.csv")
crime <- read.csv("../output/crime_count.csv")
entertainment <- read.csv("../output/entertainment_count.csv")
market <- read.csv("../output/market_count.csv")
restaurant <- read.csv("../output/restaurant_count.csv")
travel <- read.csv("../output/travel_count.csv")
median_price <- read.csv('../output/median_price_manhattan.csv')

year <- rep(2011:2017, each = 12)
year <- append(year, rep(2018,8), after = length(year))
month <- rep(1:12, 7)
month <- append(month, 1:8, after = length(month))
date <- paste(year, month, sep = '-')

df.boxplot <- data.frame(
  Zipcode = c(travel$zipcode,restaurant$ZIPCODE,entertainment$Zip,
               crime$zipcode,market$Zip.Code,hospital$Postcode),
  Category = rep(c("Transit","Restaurants","Entertainment",
                   "Crimes","Grocery","Hospitals"),
                 c(nrow(travel),nrow(restaurant),nrow(entertainment),
                   nrow(crime),nrow(market),nrow(hospital))),
  Count = c(sapply(travel$COUNT.x, sum, na.rm=T) +
              sapply(travel$COUNT.y, sum, na.rm=T),
            restaurant$COUNT,
            sapply(entertainment$COUNT.x, sum, na.rm=T) +
              sapply(entertainment$COUNT.y, sum, na.rm=T) +
              sapply(entertainment$COUNT.x.x, sum, na.rm=T),
            crime$COUNT, market$COUNT, hospital$COUNT)
)

shinyServer(function(input, output) {
    df.box <- reactive( {
        return(subset(df.boxplot, Category == input$category))
      } )
    
    df.point <- reactive( {
      selected_zip <- input$zipcode
      selected_category <- input$category
      df.subset <- df.boxplot %>%
        filter(Zipcode %in% selected_zip & Category %in% selected_category)
      return(df.subset)
    } )
    
    df.time <- reactive( {
      selected_zip <- input$zipcode
      if(length(input$zipcode)>0)
      {
        selected_price <- median_price %>%
          filter(RegionName %in% selected_zip)
        time_series <- data.frame(date)
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
        time_series_price$date.new <- as.Date(paste0(time_series_price$date,"-28"),"%Y-%m-%d")
      }
      else
      {
        time_series_price <- data.frame(NA)
      }
      return(time_series_price)
      } )
    
    # Plot using type 2 time series data
    output$time_series <- renderPlot( {
      ggplot(df.time(), aes(x=date.new, y=median_price, group=zipcode)) +
      geom_line(aes(color=zipcode)) +
      geom_point(aes(color=zipcode)) +
      scale_x_date(breaks="1 year",labels = date_format("%Y")) +
      labs(y="Median Price", x="Year", title=paste("Median Price Time Series for",df.time()$zipcode)) +
      guides(color=FALSE)
      })

    output$boxplot <- renderPlot( {
        ggplot() + 
        geom_boxplot(data=df.box(), aes(x=Category, y=Count, fill=Category),
                     lwd = 1, color="black", fill="palegreen") + 
        geom_point(data=df.point(), aes(x=Category, y=Count), color="red", size=4) +
        ggtitle( "Neighborhood Features by Zipcode" ) + 
        guides(fill=FALSE)
      })
})