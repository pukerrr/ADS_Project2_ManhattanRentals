library(shiny)
library(shinyjs)
library(leaflet)
library(data.table)
library(plotly)
library(DT)
library(shinydashboard)
library(leaflet.extras)
library(shinythemes)
library(shinyWidgets)
library(htmltools)

# df.boxplot <- read.csv("./output/boxplot_data.csv")
load('./output/df_boxplot.RData')
groups <- unique(df.boxplot$Group)

shinyUI(
  fluidPage(
            navbarPage(theme="styles.css", p(class="h","Manhattan Rent"),id = "inTabset",
                       #the home panel
                       tabPanel("Home",icon = icon("home"),
                                div(class = "home"  ,tags$head(
                                  # Include our custom CSS
                                  includeCSS("www/styles.css")),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  h1("Rentals Discovery in Manhattan", style = "position:absolute;background: rgba(44, 62, 80, 0.5);padding:20px;color:white;font-family: sans-serif;font-size: 450%;left:100px"),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  h1("ADS Fall 2018",br(),"Group 6",style = "position:absolute;color:white;font-family: sans-serif;font-size: 250%;left:100px;padding:20px;background: rgba(44, 62, 80, 0.5)")
                                  
                                )
                                
                       ),
                       
                       
                       tabPanel("All About Rental",fluidPage(
                         h2("Customize My Apartment", #span("My Apartment", style = "font-weight: 300"), 
                            style = "font-family: 'Arial';
                            color: #fff; text-align: center;
                            background-image: url('texturebg2.png');
                            padding: 10px"),
                         h3("I want my apartment to be:",style = "font-family: 'Arial';
                            color: #2D3D50; text-align: left;
                            padding: 5px"),
                         fluidRow(
                           column(width=2,
                                  dropdownButton(circle = FALSE,
                                                 label="Min price",  status = "default",
                                                 numericInput(inputId="min_price", label = "choose",value=1000, min=750,max=1e5,step=250)
                                  )
                           ),
                           column(width=2,
                                  dropdownButton(circle = FALSE,
                                                 label="Max price",  status = "default",
                                                 numericInput(inputId="max_price", value=10000, label="choose",min=750,max=1e5,step=250)
                                  )),
                           column(width=2, 
                                  dropdownButton(circle = FALSE,
                                                 label = "Bedrooms", status = "default",
                                                 selectInput(inputId="min_bedrooms", label="choose", 
                                                             choices = c("1b"=1,"2b"=2,"3b"=3,"4b"=4,"5b"=5,"6b"=6),
                                                             selected = 2
                                                 ))
                                  ),
                           column(width=2,
                                  dropdownButton(circle = FALSE,
                                                 label = "Bathroom", status = "default",
                                                 selectInput(inputId="min_bathrooms", label="choose", choices = c(1,2,3,4,5,6)
                                                 )
                                  ))
                           ),
              
                         h3("I want my neighborhood to be:",style = "font-family: 'Arial';
                            color: #2D3D50; text-align: left;
                            padding: 5px"),
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
                           column(2, 
                                sliderInput("third_weight", label = "Weight:",
                                            min = 0, max = 1, value = 0.2, step = 0.1)),
                         column(width=2, 
                                checkboxInput("select_all", label = "Select all features", value = FALSE)),
                         column(width=1, offset = 4,
                                style = "margin-top: 25px;display:inline-block;margin-right: 0px;",
                                actionButton("reset_map",label="Reset Map"))#,
                         # column(width=2, offset = 4,
                         #        checkboxInput("reset_map", label = "Reset Map", value = FALSE))
                         ),
                         fluidRow(
                         column(6,
                                dataTableOutput("recom")
                         ), 
                         column(3,
                                leafletOutput("map", width = "220%", height = 650),
                                absolutePanel(id="legend",
                                              fixed = F,
                                              draggable = F, top = 60, 
                                              left = "auto", 
                                              right = 115, 
                                              bottom = "auto",
                                              width = 125, height = 215,
                                              checkboxInput("Bus", label = "Bus",value= FALSE),
                                              checkboxInput("Subway",label="Subway",value = FALSE)#,
                                              #checkboxInput("Market", label = "Market",value = FALSE),
                                              #checkboxInput("Restaurant", label = "Restaurant",value= FALSE)                               
                                              
                                )
                         )
                         ),
                         h2("Do I have a Good Deal?",
                            style = "font-family: 'Arial';
                            color: #fff; text-align: center;
                            background-image: url('texturebg2.png');
                            padding: 10px"),
                         br(),
                         fluidRow(
                           p("select in information of the apartment you are considering:"),
                           column(width=2,
                                  numericInput("bednum", label = h4("bedrooms"), value = 2, min = 1, max = 9)
                                  ),
                           column(width=2,
                                  numericInput("bathnum", label = h4("bathrooms"), value = 1, min = 1, max = 9)
                           ),
                           column(width=2,
                                  numericInput("zipnum", label = h4("zip code"), value = 10025,min = 10001, max = 10282)
                           ),
                           column(width=2,
                                  numericInput("pricenum", label = h4("monthly rent"), value = 4000,min = 750, max = 1e5)
                           ),
                           column(width=4,
                                  br(),
                                  br(),
                                  textOutput("comp1Text")
                           )
                         )
                       ),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       br(),
                       p("GR5243 Applied Data Science Section2 Group6")
                       ),
                       tabPanel("Zipcode Comparison",fluidPage(
                         sidebarLayout(
                           sidebarPanel(
                             useShinyjs(),
                             div(
                               id = "page",
                               pickerInput(
                                 inputId = "zipcode", 
                                 label = "Select your Zipcode(s) of Interest (4 max):", 
                                 choices = unique(df.boxplot$Zipcode), 
                                 multiple = TRUE,
                                 selected = 10001,
                                 options = list("max-options" = 4)
                               ),
                               pickerInput(
                                 inputId = "group", 
                                 label = "Select Key Features:", 
                                 choices = paste(groups), 
                                 selected = paste(groups),
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE)
                               ),
                               pickerInput(
                                 inputId = "food", 
                                 label = "Food Preferences:", 
                                 choices = paste(unique(df.boxplot$Category[df.boxplot$Group=="Food"])), 
                                 selected = paste(unique(df.boxplot$Category[df.boxplot$Group=="Food"])), 
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE)
                               ),
                               pickerInput(
                                 inputId = "night", 
                                 label = "Night Life Preferences:", 
                                 choices = paste(unique(df.boxplot$Category[df.boxplot$Group=="Night Life"])), 
                                 selected = paste(unique(df.boxplot$Category[df.boxplot$Group=="Night Life"])), 
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE)
                               ),
                               pickerInput(
                                 inputId = "rec", 
                                 label = "Recreation Preferences:", 
                                 choices = paste(unique(df.boxplot$Category[df.boxplot$Group=="Recreation"])), 
                                 selected = paste(unique(df.boxplot$Category[df.boxplot$Group=="Recreation"])), 
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE)
                               ),
                               pickerInput(
                                 inputId = "safety", 
                                 label = "Safety Preferences:", 
                                 choices = paste(unique(df.boxplot$Category[df.boxplot$Group=="Safety"])), 
                                 selected = paste(unique(df.boxplot$Category[df.boxplot$Group=="Safety"])), 
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE)
                               ),
                               pickerInput(
                                 inputId = "transit", 
                                 label = "Transportation Preferences:", 
                                 choices = paste(unique(df.boxplot$Category[df.boxplot$Group=="Transportation"])), 
                                 selected = paste(unique(df.boxplot$Category[df.boxplot$Group=="Transportation"])), 
                                 multiple = TRUE,
                                 options = list(`actions-box` = TRUE)
                               )
                             ),
                             actionButton("reset", "Reset")
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Time Series",plotOutput("time_series")),
                               tabPanel("Prediction",plotOutput("prediction")),
                               tabPanel("Neighborhood Features", plotOutput("boxplot",height = "700px"))
                             )
                           )
                         )
                       ),
                       p("GR5243 Applied Data Science Section2 Group6")
                       )
            )
            )
  )