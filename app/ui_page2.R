library(shiny)
library(shinyjs)
library(shinyWidgets)
library(plotly)

df.boxplot <- read.csv("../output/boxplot_data.csv")
groups <- sort(unique(df.boxplot$Group))

shinyUI(fluidPage(
  titlePanel("Zipcode Comparisons"),
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
        tabPanel("Neighborhood Features", plotOutput("boxplot"))
        )
      )
    )
  )
)
