library(ggmap)
library(ggplot2)
library(dplyr)
library(DT)
library(shiny)
library(leaflet)
library(data.table)
#library(choroplethrZip)
library(devtools)
library(MASS)
library(tigris)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)
library(XML)
library(tidyr)
library(scales)

source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")
source("../lib/timeSeries.R")

median_price <- read.csv('../output/median_price_manhattan.csv')
df.boxplot <- read.csv("../output/boxplot_data.csv")
load("../output/housing_all.RData") 
load("../output/markets.RData")
load("../output/restaurant.RData")
load("../output/subway.RData")
load("../output/bus_stop.RData")

housing_all$price = as.numeric(gsub(",","",housing_all$price))
  
# features
load("../output/restaurant_overall.Rdata")
rest_rawscore = (restaurant_overall$american.prob+restaurant_overall$chinese.prob+restaurant_overall$italian.prob+
  restaurant_overall$japanese.prob+restaurant_overall$pizza.prob)/(restaurant_overall$violation.prob)
rest_score = punif(rest_rawscore, 0, max(rest_rawscore))

load("../output/travel.Rdata") 
travel_score = punif(travel$bus.prob+travel$subway.prob, 0, max(travel$bus.prob+travel$subway.prob))

load("../output/party.RData")
party_score = punif(party$bar.prob+party$club.prob, 0, max(party$bar.prob+party$club.prob))

load("../output/entertainment.RData") 
entertainment_score = punif(entertainment$gallery.prob+entertainment$theatre.prob, 0, max(entertainment$gallery.prob+entertainment$theatre.prob))

load("../output/count_hospital.Rdata")
hospital_score = count.hospital$PROB

load("../output/count_market.Rdata")
market_score = count.market$PROB

load("../output/count_crime.Rdata")
crime_score = 1 - count.crime$PROB

load("../output/count_park.Rdata")
park_score = count.park$PROB

load("../output/count_complaint.Rdata")
complaint_score = 1 - count.complaint$PROB

zip = as.numeric(count.complaint[,1])

all_score = data.frame("zip"=zip,"rest"=rest_score,"travel"=travel_score,
                  "party"=party_score,"entertainment"=entertainment_score,
                  "hospital"=hospital_score,"market"=market_score,
                  "crime"=crime_score,"park"=park_score,"complaint"=complaint_score)


shinyServer(function(input, output,session) {
  
  ############################
  ## Page 1: recommendation ##
  ############################
  
  #############Map#############
  
  output$map <- renderLeaflet({
    leaflet() %>%
      # http://leaflet-extras.github.io/leaflet-providers/preview/
      addProviderTiles('Esri.WorldTopoMap') %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 11.5) %>%
      addMarkers(data=housingFilter(),
                 lng=~lng,
                 lat=~lat,
                 #clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })

  # filter housing data:
  housingFilter=reactive({
    # filter on house info
    # price1 = input$slider2[1]
    # price2 = input$slider2[2]
    bedroom_filter=housing_all$bedrooms==input$min_bedrooms 
    bathroom_filter=housing_all$bathrooms==input$min_bathrooms
    price_filter=housing_all$price>=as.numeric(input$min_price) & housing_all$price<=as.numeric(input$max_price)
    # price_filter=housing_all$price>=as.numeric(input$slider2[1]) & housing_all$price<=as.numeric(input$slider2[2])
    
    # preference of the neighborhood
    
    if(input$select_all){
      combined = rest_score+travel_score+party_score+entertainment_score+
                            hospital_score+market_score+crime_score+park_score+complaint_score
      zcomb = cbind(zip, combined) %>% data.frame()
    } else {
      weights = c(as.numeric(input$first_weight),as.numeric(input$second_weight),as.numeric(input$third_weight))
      mat = t(as.matrix(all_score[,c(input$First,input$Second,input$Third)]))
      combined = rowSums(t(weights*mat))
      zcomb = cbind(zip, combined) %>% data.frame()
    }

    filterh=bedroom_filter & bathroom_filter & price_filter
    f_house = housing_all[filterh,]
    filtered.house = data.frame(merge(f_house,zcomb, by.x="zipcode",by.y="zip", all.x = T))
    ordered_house = filtered.house %>% arrange(desc(combined))
    n=20
    if(sum(filterh)<20){n = sum(filterh)}
    
    return(ordered_house[1:20,])
  })
  
  
  # show data in the map:
  observe({leafletProxy("map")%>%clearGroup("housing_cluster")%>%
      addMarkers(data=housingFilter(),
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })
  
  # show current status of icons:

  showStatus=reactive({
    if (is.null(input$map_bounds)){
      return("cloud")

    }
    else{
      if(input$map_zoom<14){
        return('cloud')
      }
      else{
        return('details')
      }
    }
  })
  # hide and show clouds
  observe({
    if(showStatus()=="cloud"){

      leafletProxy("map") %>%showGroup("housing_cluster")%>%clearGroup("new_added")
    }
    else{
      leafletProxy("map") %>%hideGroup("housing_cluster")

    }
  })

  # show housing details when zoom to one specific level

  observe({
    if(showStatus()=="details"){
      if(nrow(marksInBounds())!=0){
        leafletProxy("map")%>%clearGroup(group="new_added")%>%
          addCircleMarkers(data=marksInBounds(),
                           lat=~lat,
                           lng=~lng,
                           label=~as.character(price),
                           radius=8,
                           stroke=FALSE,
                           fillColor = "blue",
                           fillOpacity=0.7,
                           group="new_added",
                           labelOptions = labelOptions(
                             noHide = T,
                             offset=c(20,-30),
                             opacity=0.7,
                             direction="left",
                             style=list(
                               background="purple",
                               color="white"
                             )
                           )
          )
      }
      else{
        leafletProxy("map")%>%clearGroup(group="new_added")
      }
    }
 })

  # get the housing data in the bounds
  marksInBounds <- reactive({
    if (is.null(input$map_bounds)) return(housing_all[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    return(
      subset(housingFilter(),
             lat>= latRng[1] & lat <= latRng[2] &
               lng >= lngRng[1] & lng <= lngRng[2])
    )
  })

  # sort housing in current zoom level
  observe({
    housing_sort=marksInBounds()
    action=apply(housing_sort,1,function(r){
      addr=r["addr"]
      lat=r["lat"]
      lng=r["lng"]
      paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",addr,'</a>')
    }
    )
    housing_sort$addr=action
    output$recom <- renderDataTable(housing_sort[,c(3:7,1)],
                                    # options = list("sScrollX" = "100%", "bLengthChange" = FALSE),
                                    escape = FALSE)
  })

  # When point in map is hovered, show a popup with housing info
  observe({

    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    if(showStatus()=="details"){
      isolate({
        showPopupHover(event$lat, event$lng,housing=housingFilter())
      })
    }

  })

  # mouseout the point and cancel popup
  observe({

    event <- input$map_marker_mouseout
    if (is.null(event))
      return()

    isolate({
      leafletProxy("map") %>% clearPopups()
    })
  })

  # click name to go to that point
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      lat <- as.numeric(input$goto$lat)
      lng <- as.numeric(input$goto$lng)

      map %>% setView(lng = lng, lat = lat, zoom = 16)
    })
  })
  # hover the list to show info
  observe({
    if (is.null(input$showPop))
      return()
    isolate({
      remove=as.numeric(input$showPop$remove)
      map <- leafletProxy("map")

      if(remove==0){
        lat <- as.numeric(input$showPop$lat)
        lng <- as.numeric(input$showPop$lng)
        showPopupHover(lat, lng,housingFilter())
      }
      else{
        map %>% clearPopups()
      }


    })
  })

  # ## default layer ##
  # bedroom_filter=housing_all$bedrooms==2
  # bathroom_filter=housing_all$bathrooms==1
  # price_filter=housing_all$price>=1000 & housing_all$price<=5000
  #   weights = c(0.5,0.3,0.2)
  #   mat = t(as.matrix(all_score[,c("rest","travel","crime")]))
  #   combined = rowSums(t(weights*mat))
  #   zcomb = cbind(zip, combined) %>% data.frame()
  # filterh=bedroom_filter & bathroom_filter & price_filter
  # f_house = housing_all[filterh,]
  # filtered.house = data.frame(merge(f_house,zcomb, by.x="zipcode",by.y="zip", all.x = T))
  # ordered_house = filtered.house %>% arrange(desc(combined))
  # default_house = ordered_house[1:20,]
  
  ## compare price by median ##
  housingComp=reactive({
    compareHouse = housing_all %>%
      filter(bedrooms == input$bednum & bathrooms == input$bathnum & zipcode == input$zipnum)
    if(nrow(compareHouse) == 0){compareText = "Sorry, we don't have similar apartments in our database"}
    else{
      compMed = median(compareHouse$price)
      if(compMed < input$pricenum){compareText = paste("The price is higher than half of the",input$bednum,"b",
                                                       input$bathnum,"b","apartments in the neighborhood")
      } else {
        compareText = paste("The price is lower than half of the",input$bednum,"b",
                            input$bathnum,"b","apartments in the neighborhood")
      }
    }
    return(compareText)
  })
  output$comp1Text <- renderText({ 
    housingComp()
  })
  
  ############Subway##############
  observeEvent(input$Subway,{
    p<-input$Subway
    proxy<-leafletProxy("map")
    
    if(p==TRUE){
      proxy %>% 
        addMarkers(data=subway, ~lng, ~lat,label = ~Info,icon=icons(
          iconUrl = "../output/icons8-Bus-48.png",
          iconWidth = 7, iconHeight = 7),group="subway")
    }
    else proxy%>%clearGroup(group="subway")
    
  })
  
  ###############bus###############
  observeEvent(input$Bus,{
    p<-input$Bus
    proxy<-leafletProxy("map")
    
    if(p==TRUE){
      proxy %>% 
        addMarkers(data=bus_stop, ~lng, ~lat,label = ~info,icon=icons(
          iconUrl = "../output/icons8-Bus-48.png",
          iconWidth = 7, iconHeight = 7),layerId=as.character(bus_stop$info))
    }
    else proxy%>%removeMarker(layerId=as.character(bus_stop$info))
    
  })
  
  
  ##############Market#####################
  observeEvent(input$Market,{
    p<- input$Market
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy%>%
        addMarkers(lat=markets$latitude, lng=markets$longitude,icon=icons(
          iconUrl = "../output/icons8-Shopping Cart-48.png",
          iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),layerId=as.character(markets$License.Number))
    }
    else{
      proxy %>%
        removeMarker(layerId=as.character(markets$License.Number))
    }
  })
  
  ##############Resturant#####################
  observeEvent(input$Restaurant,{
    p<- input$Restaurant
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy%>%
        addMarkers(lat=restaurant$lat, lng=restaurant$lon,icon=icons(
          iconUrl = "../output/icons8-French Fries-96.png",
          iconWidth = 7, iconHeight = 7, shadowWidth = 7, shadowHeight = 7),layerId=as.character(restaurant$CAMIS))
    }
    else{
      proxy %>%
        removeMarker(layerId=as.character(restaurant$CAMIS))
    }
  })
  

  df.box <- reactive( {
    validate(
      need(input$group != "", "Please select a feature")
    )    
    df.subset <- df.boxplot %>%
      filter(Group %in% input$group & Category %in% c(input$food,input$safety,input$night,
                                                      input$transit,input$rec))
    return(df.subset)
  } )
  
  df.point <- reactive( {
    df.subset <- df.boxplot %>%
      filter(Zipcode %in% input$zipcode & Group %in% input$group & Category %in% c(input$food,input$safety,input$night,
                                                                                   input$transit,input$rec))
    return(df.subset)
  } )
  
  df.time <- reactive( {
    validate(
      need(input$zipcode != "", paste("Please select a zipcode."))
    )  
    validate(
      need(!("10282" %in% input$zipcode), paste("Sorry! No data available for 10282. Please select another zipcode."))
    )
    return(timeSeries(input$zipcode))
  } )
  
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
      labs(color="Zipcode", y="",x="")
  })
  
  observeEvent(input$reset, {
    reset("page")
  })   

})
