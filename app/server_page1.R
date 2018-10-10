library(ggmap)
library(ggplot2)
library(dplyr)
library(DT)
source("../lib/showPopupHover.R")
source("../lib/ZillowApi.R")

load("../output/housing_all.RData") 
load("../output/markets.RData")
load("../output/restaurant.RData")
load("../output/subway.RData")
load("../output/bus_stop.RData")
load("../output/nyc.RData")
color <- list(color1 = c('#F2D7D5','#D98880', '#CD6155', '#C0392B', '#922B21','#641E16'),
              color2 = c('#e6f5ff','#abdcff', '#70c4ff', '#0087e6', '#005998','#00365d','#1B4F72'),
              color3 = c("#F7FCF5","#74C476", "#005A32"))
bin <- list(bin1 = c(0,500,1000,1500,2000,2500,3000), bin2 = c(0,1,2,3,4,5,6,7))
pal <- colorBin(color[[1]], bins = bin[[1]])

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
      setView(lng = -73.971035, lat = 40.775659, zoom = 12.5) %>%
      addMarkers(data=housing_all,
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster"
      )
  })
  # filter housing data:
  housingFilter=reactive({
    # filter on house info
    bedroom_filter=housing_all$bedrooms>=input$min_bedrooms 
    bathroom_filter=housing_all$bathrooms>=input$min_bathrooms
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
    filtered.house = data.frame(merge(housing_all[filter.h,],zcomb, by.x="zipcode",by.y="zip", all.x = T))
    ordered_house = filtered.house %>% arrange(desc(combined))
    
    return(ordered_house)
    # market.fil <- if(input$check2_ma == "Love it!"){
    #   1:16
    # } else if(input$check2_ma == "It depends."){
    #   1:32
    # } else {
    #   c(1:46, NA)
    # }
    
    # market.fil <- if(input$check2_ma == 1){
    #   1:16
    # } else if(input$check2_ma == 2){
    #   1:32
    # } else {
    #   c(1:46, NA)
    # }
    # market_logi = sapply(housing_all$`market rank`, function(num) num %in% market.fil)
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
      if(input$map_zoom<16){
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
                           radius=5,
                           stroke=FALSE,
                           fillColor = "green",
                           fillOpacity=0.7,
                           group="new_added",
                           labelOptions = labelOptions(
                             noHide = T,
                             offset=c(20,-15),
                             opacity=0.7,
                             direction="left",
                             style=list(
                               background="green",
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
    if (is.null(input$map_bounds))
      return(housing_all[FALSE,])
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
    if(nrow(housing_sort)!=0){
      action=apply(housing_sort,1,function(r){
        addr=r["addr"]
        lat=r["lat"]
        lng=r["lng"]
        paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",addr,'</a>')
      }
      )
      housing_sort$addr=action
      output$rank <- renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")],escape=FALSE)
    }
    else{
      output$rank=renderDataTable(housing_sort[,c("addr","price","bedrooms","bathrooms")])
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

  #############Search###############
  observeEvent(input$button1,{
    url = paste0('http://maps.google.com/maps/api/geocode/xml?address=',input$location,'&sensor=false')
    doc = xmlTreeParse(url)
    root = xmlRoot(doc)
    lati = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lat']]))
    long = as.numeric(xmlValue(root[['result']][['geometry']][['location']][['lng']]))

    leafletProxy("map") %>%
      setView(lng=long, lat=lati,zoom=15)%>%
      addMarkers(lng=long,lat=lati,layerId = "1",icon=icons(
        iconUrl = "../output/icons8-Location-50.png",iconWidth = 25, iconHeight = 25))
  })
  #################Clear Choices############
  observeEvent(input$button2,{
    proxy<-leafletProxy("map")
    proxy %>%
      setView(lng = -73.971035, lat = 40.775659, zoom = 12) %>%
      removeMarker(layerId="1") %>%
      addMarkers(data=housing_all,
                 lng=~lng,
                 lat=~lat,
                 clusterOptions=markerClusterOptions(),
                 group="housing_cluster")
    updateTextInput(session, inputId="location", value = "")
  }

  )
  #############Clear button###########
  observeEvent(input$clear, {
    leafletProxy('map')%>% setView(lng = -73.971035, lat = 40.775659, zoom = 12)
    
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
  ##############Crime#####################
  observeEvent(input$Crime,{
    p<- input$Crime
    proxy<-leafletProxy("map")
    if(p==TRUE){
      proxy %>%
        addPolygons(data=nyc, fillColor = ~pal(count), color = 'grey', weight = 1,
                    fillOpacity = .6)%>%
        addLegend(pal = pal, values = nyc$count,position="bottomleft")
    }
    else proxy%>%clearShapes()%>%clearControls()
    
  })

  ##Clear
  observeEvent(input$no_rec2, {
    updateSliderInput(session, "check2_pr",value = 5400)
    updateSelectInput(session, "check2_ty",selected="")
    updateSelectInput(session, "check2_re",selected="")
    updateSelectInput(session, "check2_tr",selected = "Who Cares.")
    updateSelectInput(session, "check2_cb",selected = "I'm allergic.")
    updateSelectInput(session, "check2_ct",selected = "Netflix for life.")
    updateSelectInput(session, "check2_ma",selected = "Just Amazon.")
  })
  # 
  # 
  # 
  # ##Recommand
  # # areas  <- reactive({
  # #   cond.ame <- if(is.null(input$check2_re)){"ranking.American <= 46 |is.na(ranking.American) == TRUE"
  # #   } else if("American" %in% input$check2_re){"ranking.American <= 23"
  # #   } else {"ranking.American <= 46 |is.na(ranking.American) == TRUE"}
  # #
  # #   cond.chi <- if(is.null(input$check2_re)){"ranking.Chinese <= 46 |is.na(ranking.Chinese) == TRUE"
  # #   } else if("Chinese" %in% input$check2_re) {"ranking.Chinese <= 23"
  # #   } else {"ranking.Chinese <= 46 |is.na(ranking.Chinese) == TRUE"}
  # #
  # #   cond.ita <-  if(is.null(input$check2_re)){"ranking.Italian <= 46 |is.na(ranking.Italian) == TRUE"
  # #   } else if("Italian" %in% input$check2_re) {"ranking.Italian <= 23"
  # #   } else {"ranking.Italian <= 46 |is.na(ranking.Italian) == TRUE"}
  # #
  # #   cond.jap <- if(is.null(input$check2_re)){"ranking.Japenses <= 46 |is.na(ranking.Japenses) == TRUE"
  # #   } else if("Japanese" %in% input$check2_re) {"ranking.Japenses <= 23"
  # #   } else {"ranking.Japenses <= 46 |is.na(ranking.Japenses) == TRUE"}
  # #
  # #   cond.piz <- if(is.null(input$check2_re)){"ranking.Pizza <= 46 |is.na(ranking.Pizza) == TRUE"
  # #   } else if("Pizza" %in% input$check2_re) {"ranking.Pizza <= 23"
  # #   } else {"ranking.Pizza <= 46 |is.na(ranking.Pizza) == TRUE"}
  # #
  # #   cond.oth <- if(is.null(input$check2_re)){"ranking.Others <= 46 |is.na(ranking.Others) == TRUE"
  # #   } else if("Others" %in% input$check2_re) {"ranking.Others <= 23"
  # #   } else {"ranking.Others <= 46 |is.na(ranking.Others) == TRUE"}
  # #
  # #   trans.fil <- if(input$check2_tr == "It's everything."){
  # #     1:16
  # #   } else if(input$check2_tr == "Emmm."){
  # #     1:32
  # #   } else {
  # #     c(1:46, NA)
  # #   }
  # #
  # #   club.fil <- if(input$check2_cb == "Let's party!"){1:16
  # #   } else if(input$check2_cb == "Drink one or two."){
  # #     1:32
  # #   } else {
  # #     c(1:46, NA)
  # #   }
  # #
  # #   theatre.fil<- if(input$check2_ct == "Theatre goers."){1:16
  # #   } else if(input$check2_ct == "It depends."){
  # #     1:32
  # #   } else {
  # #     c(1:46, NA)
  # #   }
  # #
  # #   market.fil <- if(input$check2_ma == "Love it!"){
  # #     1:16
  # #   } else if(input$check2_ma == "It depends."){
  # #     1:32
  # #   } else {
  # #     c(1:46, NA)
  # #   }
  # #   ranks = read.csv("./data/housing_all.csv", as.is = T)
  # #   logi = sapply(ranks$ranking.market, function(num) num %in% market.fill)
  # #   chosen_zip = ranks[logi,"zipcode"]
  # #   return(chosen_zip)
  # #   # areas <- (housingFilter() %>%
  # #   #             filter(eval(parse(text = cond.ame)), eval(parse(text = cond.chi)), eval(parse(text = cond.ita)),
  # #   #                    eval(parse(text = cond.jap)), eval(parse(text = cond.piz)), eval(parse(text = cond.oth)),
  # #   #                    ranking.trans %in% trans.fil, ranking.bar %in% club.fil,
  # #   #                    ranking.theatre %in% theatre.fil, ranking.market %in% market.fil
  # #   #             ) %>%
  # #   #             select(zipcode))[,1]
  # #   # return(areas)
  # # })
  # # output$recom <- renderDataTable(housingFilter(),
  # #                                 options = list("sScrollX" = "100%", "bLengthChange" = FALSE))
  # 
  output$recom <- renderDataTable(housingFilter()[1:10,3:7],
                                  options = list("sScrollX" = "100%", "bLengthChange" = FALSE))


})
