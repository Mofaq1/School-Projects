library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(graphics)
library(DT)



con <- dbConnect(SQLite(), dbname = "survey_data.db")
survey = data.frame(Q1 = "", Q2 = "", Q3 = "", Q4 = "")[-1,]
dbWriteTable(con, "Survey", survey, overwrite = FALSE, append = TRUE)



# Defining UI for the application for doing an online survey
ui <- fluidPage(
  # Application title
  titlePanel("Online Survey"),
 
  sidebarLayout(
    sidebarPanel(
  # Radio button function for Question 1
  radioButtons(inputId = "rb", label = "What is your biological sex?",
               c("Male" = "M",
                 "Female" = "F")),
  
  # NumericInput function for Question 2
  numericInput(inputId = "obs", label = "What is your age in years?", "Age"),

  
  # SelectInput function for Question 3
  selectInput(inputId = "id", 
              label = "How much time do you spend on shopping outside on average each week?",
              choices = c("One hour or less", 
                          "More than an hour up to Two hours", 
                          "Two to Three hours", 
                          "More than Three hours")),
 
  
  # textAreaInput function for Question 4
  textAreaInput(inputId = "cap", 
                label = "What is your opinion about the handling of COVID-19 by President Biden?", 
                "Write a paragraph of no more than 100 words"),
   
  
  actionButton(inputId = "save",
               label = "Save"),
    ),
  
  mainPanel(
    DTOutput("datatable"),
    plotOutput(outputId = "rbChart"),
    plotOutput(outputId = "obsChart"),
    plotOutput(outputId = "idChart")
    
  )
  
))


server <-function(input, output, session){
  
  observeEvent(input$save, {
    dbExecute(con, statement = "INSERT INTO Survey VALUES (?, ?, ?, ?)",
    params = list(input$rb, input$obs, input$id, input$cap)
  )
  session$reload()
  })
  
  D = reactive({
    dbGetQuery(con, statement = 'SELECT * FROM Survey')
  })
  
  split.screen(c(2,1)) # Makes Screen 1 and 2
  split.screen(c(1,2), screen=1) # Makes Screen 3 and 4


  
  output$rbChart <- renderPlot(
    {
      screen(2)
      ggplot(D(), aes(x = Q1)) + geom_bar(color = "white", fill = "steelblue")})

  
  output$obsChart <- renderPlot(
    {
      screen(3)
      ggplot(D(), aes(x = as.numeric(Q2))) + geom_histogram(bins = 30, color = "white", fill = "steelblue")})
  
  
  output$idChart <- renderPlot(
    {
      screen(4)
      ggplot(D(), aes(x = Q3)) + geom_bar(color = "white", fill = "steelblue")}) 
  
  ##  Close all screens.
  close.screen( all = TRUE )
  
  output$datatable <- renderDT({
    D()
  })   
}


# Part 5: Run the application 

shinyApp(ui = ui, server = server)

























