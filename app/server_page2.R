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
library(dplyr)
library(tigris)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)
library(XML)
library(tidyr)
library(dplyr)

source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")

load("../output/housing_all.RData") 

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
      setView(lng = -73.971035, lat = 40.775659, zoom = 11) %>%
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
    bedroom_filter=housing_all$bedrooms==input$min_bedrooms 
    bathroom_filter=housing_all$bathrooms==input$min_bathrooms
    price_filter=housing_all$price>=input$min_price & housing_all$price<=input$max_price
    
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

    filter.h=bedroom_filter & bathroom_filter & price_filter
    f_house = housing_all[filter.h,]
    filtered.house = data.frame(merge(f_house,zcomb, by.x="zipcode",by.y="zip", all.x = T))
    ordered_house = filtered.house %>% arrange(desc(combined))
    
    if(sum(filter.h)<20){n = sum(filter.h)} else{n=20}
    
    return(ordered_house[1:n,])
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
    # housing_sort=marksInBounds()
    # action=apply(housing_sort,1,function(r){
    #   addr=r["addr"]
    #   lat=r["lat"]
    #   lng=r["lng"]
    #   paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",addr,'</a>')
    # }
    # )
    # housing_sort$addr=action
    # output$recom <- renderDataTable(housing_sort,
    #                                 # options = list("sScrollX" = "100%", "bLengthChange" = FALSE),
    #                                 escape = FALSE)
    
    if(nrow(housing_sort)!=0){
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
      # output$recom <- renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")],escape=FALSE)
    }
    else{
      # output$recom = renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")])
      output$recom <- renderDataTable(housingFilter()[,c(3:7,1)]
                                      # options = list("sScrollX" = "100%", "bLengthChange" = FALSE),
                                      # escape = FALSE
                                      )
    }
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

  # #############Search###############
  # observeEvent(input$button1,{
  #   url = paste0('http://maps.google.com/maps/api/geocode/xml?address=',input$location,'&sensor=false')
  #   doc = xmlTreeParse(url)
  #   root = xmlRoot(doc)
  #   lati = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lat']]))
  #   long = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lng']]))
  # 
  #   leafletProxy("map") %>%
  #     setView(lng=long, lat=lati,zoom=15)%>%
  #     addMarkers(lng=long,lat=lati,layerId = "1",icon=icons(
  #       iconUrl = "../output/icons8-Location-50.png",iconWidth = 25, iconHeight = 25))
  # })
  #################Clear Choices############
  observeEvent(input$button2,{
    proxy<-leafletProxy("map")
    proxy %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 11) %>%
      removeMarker(layerId="1") %>%
      addMarkers(data=housingFilter(),
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster")
    updateTextInput(session, inputId="location", value = "")
  }

  )

  ## Clear preference choices
  observeEvent(input$no_rec2, {
    updateSliderInput(session, "check2_pr",value = 5400)
    updateSelectInput(session, "check2_ty",selected="")
    updateSelectInput(session, "check2_re",selected="")
    updateSelectInput(session, "check2_tr",selected = "Who Cares.")
    updateSelectInput(session, "check2_cb",selected = "I'm allergic.")
    updateSelectInput(session, "check2_ct",selected = "Netflix for life.")
    updateSelectInput(session, "check2_ma",selected = "Just Amazon.")
  })

  

})
