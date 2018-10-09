library(shiny)

shinyUI(fluidPage(
  titlePanel("Zipcode Comparisons"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "category",
        "Top Categories:",
        c("Crimes"="Crimes",
          "Public Transit Stops"="Transit",
          "Restaurants"="Restaurants",
          "Entertainment Centers"="Entertainment",
          "Grocery"="Grocery",
          "Hospitals"="Hospitals"),
        selected = c("Transit","Restaurants","Entertainment",
                     "Crimes","Grocery","Hospitals"),
      ),
      selectInput("zipcode", 
                  "Select your Zipcode of Interest:",
                  sort(unique(median_price$RegionName)),
      )
    ),
    mainPanel(
      plotOutput("boxplot"),
      plotOutput("time_series")
      )
    )
  )
)
