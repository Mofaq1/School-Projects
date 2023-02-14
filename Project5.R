library(tidyverse)
library(shiny)
library(DT)
library(highcharter)
library(rvest)


# Create a shiny app that allows the user to filter data for each year selected. 
# It is not a good idea to download the data. Instead, read the date remotely.


url = "https://github.com/washingtonpost/data-police-shootings/releases/download/v0.1/fatal-police-shootings-data.csv"
page = read.csv(url)
data <- data.frame(page) %>% mutate(Year = substr(date,1,4))  

ui <- fluidPage(
  
  titlePanel("Fatal Police Shooting"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Year", "Select Year",choices = 2015:2021)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("DT", DTOutput("dt")),
        tabPanel("Map", highchartOutput("map")),
        tabPanel("Race", plotOutput("race")),
        tabPanel("Gender", plotOutput("gender")),
        tabPanel("Threat", plotOutput("threat")),
        tabPanel("Flee", plotOutput("flee")),
        tabPanel("Age", plotOutput("age")),
        tabPanel("Armed", plotOutput("armed")),
        tabPanel("City", plotOutput("city"))
        
      )
    )
  )
)
server <- function(input, output) {
  D <- reactive({
    filter(data, Year == input$Year)
  })
  
  # (a) Display all cases with the DT package.
  output$dt <- renderDT(
    D()
    
  )
  
  # (b) Create a map with the leaflet or highcharter package showing each person 
  # killed by the police in 2020. 
  # Display the relevant information on the map for each case.
  
  output$map <- renderHighchart({

    myformat1 <- '<table style="background-color:#00FF00">
        <tr>
         <th>state </th>
         <th>city </th>
         <th>name </th>
         <th>manner_of_death </th>
         <th>armed </th>
         <th>age </th>
         <th>flee </th>
         <th>body_camera </th>
         
        </tr>
        <tr>
         <td>{point.state}</td>
         <td>{point.city}</td>
         <td>{point.name}</td>
         <td>{point.manner_of_death}</td>
         <td>{point.armed}</td>
         <td>{point.age}</td>
         <td>{point.flee}</td>
         <td>{point.body_camera}</td>
         
         </tr>
        </tables>'
    data("usgeojson")
     
     highchart() %>%
        hc_title(text = "Each Person Killed by the Police") %>% 
        hc_subtitle(text = "Source: Washington Post") %>% 
        hc_add_series_map(usgeojson, D(), 
                          value = "name", joinBy =c("name", "state"),
                          dataLabels = list(enabled = TRUE,
                                            format = "{point.name}")) %>%
      hc_tooltip(useHTML = TRUE, headerformat = "", pointFormat = myformat1) %>% 
        hc_colorAxis(stops = color_stops()) %>%
        hc_mapNavigation(enabled = TRUE)
    
  }
  )
  
  # (c) Plot the distribution of the variable "race" using a bar graph.
  output$race <- renderPlot({
    ggplot(D(), aes(race)) +
      geom_bar() +
      ggtitle("Race of Victims")+
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
  }   
  
  )   
  
  # (d) Plot the distribution of the variable "gender".
  output$gender <- renderPlot({
    ggplot(D(), aes(gender)) +
      geom_bar() +
      ggtitle("Gender of Victims")+
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
  }
  )
  
  # (e) Plot the distribution of the variable "threat_level". 
  output$threat <- renderPlot({
    ggplot(D(), aes(threat_level)) +
      geom_bar() +
      ggtitle("Threat Level Reported")+
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
  }
  )
  
  # (f) Plot the distribution of the variable "flee".
  output$flee <- renderPlot({
    ggplot(D(), aes(flee)) +
      geom_bar() +
      ggtitle("Did the victim flee?")+
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
  }
  )
  
  # (g) Plot the distribution of the variable "age" (with geom_histogram() or geom_density() in ggplot2).
  output$age <- renderPlot({
    ggplot(D(), aes(age)) +
      geom_density() +
      ggtitle("Age of victims")+
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
  }
  
  )
  
  # (h) Plot the distribution of the variable "armed". 
  output$armed <- renderPlot({
    ggplot(D(), aes(armed)) +
      geom_bar() +
      ggtitle("Was the victim armed?") +
      theme(axis.text.x = element_text(angle = 90))+
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
  }
  
  )
 
  # (i) Plot the top 10 cities that have most victims.
  output$city <- renderPlot({
    top.10 <- top_n(D(),10,D()$id)
    ggplot(top.10,aes(city)) +
      geom_bar() +
      ggtitle("Top 10 cities with most frequent killings") +
      labs(caption = "Summary of the plot") +
      theme(plot.tag.position = "bottomleft")
      
  }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)