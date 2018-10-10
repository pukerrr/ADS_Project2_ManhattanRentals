library(shiny)
library(ggplot2)
library(scales)
library(tidyr)
library(dplyr)
library(forecast)

source("../lib/timeSeries.R")
median_price <- read.csv('../output/median_price_manhattan.csv')
df.boxplot <- read.csv("../output/boxplot_data.csv")

shinyServer(function(input, output) {
    df.box <- reactive( {
      validate(
        need(input$group != "", "")
      )    
      df.subset <- df.boxplot %>%
        filter(Group %in% input$group & Category %in% c(input$food,input$safety,input$night,
                                                        input$transit,input$rec))
      return(df.subset)
      } )
    
    df.point <- reactive( {
      df.subset <- df.boxplot %>%
        filter(Zipcode %in% input$zipcode & Group %in% input$group 
               & Category %in% c(input$food,input$safety,input$night,
                                 input$transit,input$rec))
      return(df.subset)
    } )
    
    df.time <- reactive( {
      validate(
        need(input$zipcode != "", "")
      )
      selected_zip <- input$zipcode
      
      return(timeSeries(selected_zip))
      } )
    
    # Plot using type 2 time series data
    output$time_series <- renderPlot( {
      df <- subset(df.time(), date.new <= as.Date("2018-8-28"))
      ggplot(df, aes(x=date.new, y=median_price, group=zipcode)) +
      geom_line(aes(color=zipcode)) +
      geom_point(aes(color=zipcode)) +
      scale_x_date(breaks="1 year",labels = date_format("%Y")) +
      labs(y="Median Price", x="Year", title="Median Price Time Series",color="Zipcode")
      })
    
    output$prediction <- renderPlot( {
      df <- subset(df.time(), date.new > as.Date("2018-8-28"))
      ggplot(df, aes(x=date.new, y=median_price, group=zipcode)) +
        geom_line(aes(color=zipcode)) +
        geom_point(aes(color=zipcode)) +
        facet_wrap(~zipcode, scales="free") +
        scale_x_date(breaks="1 month",labels = date_format("%Y-%m")) +
        labs(y="Median Price", x="Date", title="Median Price Prediction") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(color=F)
    })
    output$boxplot <- renderPlot( {
      req(nrow(df.box()) > 0)   
      ggplot() +
        geom_boxplot(data=df.box(), aes(x=Category, y=Count, fill=Category),
                     lwd = 1, color="black", fill="palegreen") +
        geom_point(data=df.point(), aes(x=Category, y=Count, color=factor(Zipcode)), size=3) +
        facet_wrap(~Group, scales="free") +
        ggtitle( "Neighborhood Features" ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        guides(fill=FALSE) +
        labs(color="Zipcode")
      })

    observeEvent(input$reset, {
      reset("page")
    })  
})