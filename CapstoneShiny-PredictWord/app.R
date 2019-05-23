
source("model.R")

server <- function(input, output) {
  
  output$dataTableOut1 <- renderTable({
    data.frame("Rank" = c("1","2","3"),Prediction = PredictFunction(input$textInput1, type=input$radioInputs))
  })
  
}



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  #titlePanel("Old Faithful Geyser Data"),
  
  
  textInput("textInput1","",""),
  radioButtons("radioInputs","",c("Twitter" = "Twitter","Blog" = "Blog","News" = "News","All" = "All")),
  submitButton("Submit"),
  tableOutput("dataTableOut1")
)




# Run the application 
shinyApp(ui = ui, server = server)

