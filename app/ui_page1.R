library(shiny)
library(leaflet)
library(data.table)
library(plotly)
library(DT)
library(shinydashboard)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(htmltools)

shinyUI(
  fluidPage(
            navbarPage(theme="styles.css", p(class="h","Manhattan Rent"),id = "inTabset",
                       tabPanel("Rental Recommend",fluidPage(
                         fluidRow(
                           column(width=2,
                                  #style = "margin-top: 25px;display:inline-block;margin-right: 0px;margin-left: 120px",
                                  dropdownButton(circle = FALSE,
                                                 label="Min price",  status = "default",
                                                 numericInput(inputId="min_price", label = "choose",value=0, min=0,max=1000000,step=1000)
                                  )
                           ),
                           column(width=2,
                                  #style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                                  dropdownButton(circle = FALSE,
                                                 label="Max price",  status = "default", 
                                                 numericInput(inputId="max_price", value=1000000, label="choose",min=0,max=1000000,step=1000 )
                                  )),
                           column(width=2, 
                                  #style="margin-top: 25px;display:inline-block;margin-right: 10px",
                                  dropdownButton(circle = FALSE,
                                                 label = "Bedrooms", status = "default",
                                                 selectInput(inputId="min_bedrooms", label="choose", choices = c("studio"=0,"1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6)
                                                             
                                                 ))
                                  ),
                
                           column(width=2,
                                  #style = "margin-top: 25px;;display:inline-block;margin-right: 10px;",
                                  dropdownButton(circle = FALSE,
                                                 label = "Bathroom", status = "default",
                                                 selectInput(inputId="min_bathrooms", label="choose", choices = c("studio"=0,"1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6)
                                                             
                                                 )
                                  )),
                           column(width=2, 
                                  #style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                                  actionButton("button2",label="Reset" 
                                               ))),
                         fluidRow(titlePanel("      ")),
                         fluidRow(
                           
                           column(2,
                                  selectInput("First",
                                              label = "First Preference",
                                              choices = c("Restaurants"= "rest",
                                                          "Travel Convenience"="travel",
                                                          "Safety"="crime",
                                                          "Party Places"="party",
                                                          "Entertainment"="entertainment",
                                                          "Hospitals"="hospital",
                                                          "Shopping"="market",
                                                          "Park"="park",
                                                          "Free of Complaints"="complaint"),
                                              selected="rest")),
                                  # selectInput("First",
                                  #             label = "First Preference",
                                  #             choices = c("rest",
                                  #                         "travel",
                                  #                         "crime",
                                  #                         "party",
                                  #                         "entertainment",
                                  #                         "hospital",
                                  #                         "market",
                                  #                         "park",
                                  #                         "complaint"),
                                  #             selected="rest")),
                           column(2, 
                                  sliderInput("first_weight", label = "Weight:",
                                              min = 0, max = 1, value = 0.5, step = 0.1)),
                           column(2,
                                  selectInput("Second",
                                              label = "Second Preference",
                                              choices = c("Restaurants"= "rest",
                                                          "Travel Convenience"="travel",
                                                          "Safety"="crime",
                                                          "Party Places"="party",
                                                          "Entertainment"="entertainment",
                                                          "Hospitals"="hospital",
                                                          "Shopping"="market",
                                                          "Park"="park",
                                                          "Free of Complaints"="complaint"),
                                              selected="travel")),
                                  # selectInput("Second",
                                  #             label = "Second Preference",
                                  #             choices = c("rest",
                                  #                         "travel",
                                  #                         "crime",
                                  #                         "party",
                                  #                         "entertainment",
                                  #                         "hospital",
                                  #                         "market",
                                  #                         "park",
                                  #                         "complaint"),
                                  #             selected="travel")),
                           column(2, 
                                  sliderInput("second_weight", label = "Weight:",
                                              min = 0, max = 1, value = 0.3, step = 0.1)),
                           column(2,
                                  selectInput("Third",
                                              label = "Third Preference",
                                              choices = c("Restaurants"="rest",
                                                          "Travel Convenience"="travel",
                                                          "Safety"="crime",
                                                          "Party Places"="party",
                                                          "Entertainment"="entertainment",
                                                          "Hospitals"="hospital",
                                                          "Shopping"="market",
                                                          "Park"="park",
                                                          "Free of Complaints"="complaint"),
                                              selected="crime")),
                                  # selectInput("Third",
                                  #             label = "Third Preference",
                                  #             choices = c("rest",
                                  #                         "travel",
                                  #                         "crime",
                                  #                         "party",
                                  #                         "entertainment",
                                  #                         "hospital",
                                  #                         "market",
                                  #                         "park",
                                  #                         "complaint"),
                                  #             selected="crime")),
                           column(2, 
                                sliderInput("third_weight", label = "Weight:",
                                            min = 0, max = 1, value = 0.2, step = 0.1)),
                         column(width=2, 
                                checkboxInput("select_all", label = "Select all features", value = FALSE))),
                         fluidRow(
                         column(6,
                                dataTableOutput("recom")
                         ), 
                         column(3,
                                leafletOutput("map", width = "220%", height = 650),
                                
                                absolutePanel(id="legend",
                                              fixed = TRUE,
                                              draggable = TRUE, top = 300, left = "auto", right = 20, bottom = "auto",
                                              width = 125, height = 215,
                                              checkboxInput("Crime", label = "Crime",value= FALSE),
                                              checkboxInput("Bus", label = "Bus",value= FALSE),
                                              checkboxInput("Subway",label="Subway",value = FALSE),
                                              checkboxInput("Market", label = "Market",value = FALSE),
                                              checkboxInput("Restaurant", label = "Restaurant",value= FALSE)                               
                                              
                                )
                               
                         )
                         )
                       )
                       )
            )))