library(shiny)
library(plotly)
library(ggplot2)

# Make a dataset to get you started: The dataset "diamonds" is available from ggplots. The following will save it on your computer
# write.csv(diamonds, file="/Users/mohibullahfaqeerzai/Desktop/Project4", row.names = FALSE) # On your computer, you may save D in a different folder

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title = "Uploading Your File"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      ## Create a file upload control
      fileInput(inputId = "file", 
                label = "Choose Your File:",
                accept = c(".txt", ".csv")),
      ## Use html tag hr (horizontal rule) to make a horizontal separator
      hr(),
      ## Make a h5 heading 
      h5("Max file size is 2M"),
      ## Create a checkbox that can be used to specify logical values. 
      checkboxInput(inputId = "header", 
                    label = "Header", 
                    value = TRUE),
      ## Create a set of radio buttons used to select an item from a list.
      radioButtons(inputId = "sep", 
                   label = "Separator",
                   choices = c(Comma = ",", Space = " ", Tab = "\t")),
      
      uiOutput("variable")
    ),
    
    
    # Generate Outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("table")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", plotlyOutput("plot", height = "700px"))
      )
      
    )
  )
)

# Define server logic required to generate desired outputs
server <- function(input, output, session) {
  
  myData <- reactive({ 
    # input$file will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the path to the local file.
    f = input$file
    if (is.null(f)){
      return(NULL)
      
    } else {
      read.table(f$datapath, header = input$header, sep = input$sep)
      
    }
  })
  
  # Create a drop-down menu to choose a variable
  output$variable <- renderUI({
    
    #Code you need to write (a)
    selectInput('id', 'Choose a Variable from the drop-down Menu', names(myData()))
    
  })
  
  # Display the whole table
  output$table <- renderTable({
    
    
    #Code you need to write (b)
    myData()
  })
  
  # Summarize the whole table
  output$summary <- renderPrint({
    
    
    #Code you need to write (c)
    summary(myData())
    
  })
  
  # Plot only the selected variable.
  
  # The code needs to handle both a categorical and numeric variables
  output$plot <- renderPlotly({
    
    #Code you need to write (d)
    if(is.numeric(myData()[[input$id]]))
      plt <- ggplot(myData(), aes(.data[[input$id]])) + geom_histogram()
    else 
      plt <- ggplot(myData(), aes(.data[[input$id]])) + geom_bar()
    ggplotly(plt)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)