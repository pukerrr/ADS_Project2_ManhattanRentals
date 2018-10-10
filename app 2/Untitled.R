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
f_house = housing_all[filter.h,]
filtered.house = data.frame(merge(f_house,zcomb, by.x="zipcode",by.y="zip", all.x = T))
ordered_house = filtered.house %>% arrange(desc(combined))

return(ordered_house[,])

input = list()
input$min_bedrooms = 1
input$min_bathrooms = 1
input$min_price = 1
input$max_price = 1500
input$select_all = FALSE
input$first_weight = 0.5
input$second_weight = 0.3
input$third_weight = 0.2
input$First = "rest"
input$Second = "travel"
input$Third = "crime"



o = ordered_house[1:20,]


library(shiny)

runApp(list(
  
  ui = pageWithSidebar(
    
    headerPanel("'Reset inputs' button example"),
    
    sidebarPanel(
      uiOutput('resetable_input'),
      tags$hr(),
      actionButton("reset_input", "Reset inputs")
    ),
    
    mainPanel(
      h4("Summary"),
      verbatimTextOutput("summary")
    )
    
  ),
  
  server = function(input, output, session) {
    
    output$summary <- renderText({
      return(paste(input$mytext, input$mynumber))
    })
    
    output$resetable_input <- renderUI({
      times <- input$reset_input
      div(id=letters[(times %% length(letters)) + 1],
          numericInput("mynumber", "Enter a number", 20),
          textInput("mytext", "Enter a text", "test"))
    })
    
  }
))