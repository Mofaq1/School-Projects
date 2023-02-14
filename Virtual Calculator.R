# Project #1
# Creating a Virtual Calculator


library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("A Simple Calculator"), 
  
  # Sidebar with a numeric input and two slider inputs
  sidebarPanel(
    numericInput(inputId = "x",
                 label = "Type a number:",
                 value = 10,
                 step = 1
    ), 
    
    numericInput(inputId = "y",
                 label = "Type another number:",
                 value = 10,
                 step = 1
    ),
    
    actionButton(inputId = "multiply", 
                 label = "Multiply"), 
   
    
    # Adding another button with the label "Add"
     actionButton(inputId = "add", 
                 label = "Add")
    
  ), 
  
 
  
  # Show outputs
  mainPanel(
    textOutput(outputId = "result"),  
  )
  
)

# Server logic
server <- function(input, output) {  
  
  observeEvent(input$multiply, {
    output$result <- renderText({  
      input$x * input$y
    })
    
  })
  
  observeEvent(input$add, {
    output$result <- renderText({  
      input$x + input$y
    })
    
  })
}

# Link ui and server
shinyApp(ui, server)

