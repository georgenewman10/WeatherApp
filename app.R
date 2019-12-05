library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(tidyverse)
library(data.table)
library(plotly)

# Load data
#data_description <- read_csv("")
weather_data <- read_csv("data.csv")
data_description <- c('PRCP', 'TMAX',"TMIN")

weather_data <- weather_data %>% 
  mutate(TDiff = TMAX - TMIN)

weather_data %>% 
  mutate(DATE = as.Date(weather_data$DATE))

weather_data$DATE <- format(as.Date(weather_data$DATE, format = "%m/%d/%Y"), "%Y-%m-%d")

# create data frame
#setDT(data)

# columns
#choices <- c("PRCP","TMAX","TMIN")

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Weather Data Visualizations"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", 
                                label = strong("Data Category"),
                                #choices = unique(weather_data$type),
                                choices = c("Temperature", 'Precipitation', 'Differential'),
                                selected = "Temperature"),
                    
                    # Select DATE range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "1938-06-01", 
                                   end = "2019-11-10",
                                   min = "1938-06-01", max = "2019-11-10"),
                    
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100)),
                                     HTML("Higher values give more smoothness.")
                    )
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "300px")
                  )
                )
)



# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_data <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end DATE."))
    validate(need(input$date[1] < input$date[2], "Error: Start DATE should be earlier than end DATE."))
    
    data <- weather_data %>% 
      select('DATE', 'TMAX', 'TMIN', 'PRCP', 'TDiff') %>% 
      filter(DATE >= input$date[1] & DATE <= input$date[2])
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
  
    data <- selected_data()
    
    graph_color = "#434343"
    
    if(input$type == 'Temperature') {
      
      print('Max Temperature')
      print(max(data$TMAX))
      print('Year')
      print(data$DATE[which.max(data$TMAX)])
    
      date_1 <- format(as.Date(input$date[1], format = "%Y-%m-%d"), "%m-%d-%Y")
      date_2 <- format(as.Date(input$date[2], format = "%Y-%m-%d"), "%m-%d-%Y")
      full_title <- paste("Temperature Patterns in Austin between", date_1, "and", date_2)
      color = "#039403"
      
      plot(x = as.Date(data$DATE), y = data$TMAX, type = "l", xlab = "Date", ylab = 'Temperature (Degrees Fahrenheit)', 
           col = color, fg = graph_color, col.lab = graph_color, col.axis = graph_color, main = full_title)
      
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = as.Date(data$DATE), y = data$TMAX, f = input$f)
            print(smooth_curve$y[1])
            print(smooth_curve$y[length(smooth_curve$y)])
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    }
    
    else if(input$type == 'Precipitation') {
      
      print('Max Precipitation')
      print(max(data$PRCP))
      print('Year')
      print(data$DATE[which.max(data$PRCP)])
      
      date_1 <- format(as.Date(input$date[1], format = "%Y-%m-%d"), "%m-%d-%Y")
      date_2 <- format(as.Date(input$date[2], format = "%Y-%m-%d"), "%m-%d-%Y")
      full_title <- paste("Precipitation in Austin between", date_1, "and", date_2)
      color = "#0273cf"
      
      plot(x = as.Date(data$DATE), y = data$PRCP, type = "l", xlab = "Date", ylab = 'Precipitation (Inches)', 
           col = color, fg = graph_color, col.lab = graph_color, col.axis = graph_color, main = full_title)
      
        # Display only if smoother is checked
      if(input$smoother){
        smooth_curve <- lowess(x = as.Date(data$DATE), y = data$PRCP, f = input$f)
        print(smooth_curve$y[1])
        print(smooth_curve$y[length(smooth_curve$y)])
        lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    }
    
    else if(input$type == 'Differential') {
      
      print('Max Differential')
      print(max(data$TDiff))
      print('Year')
      print(data$DATE[which.max(data$TDiff)])
      
      date_1 <- format(as.Date(input$date[1], format = "%Y-%m-%d"), "%m-%d-%Y")
      date_2 <- format(as.Date(input$date[2], format = "%Y-%m-%d"), "%m-%d-%Y")
      full_title <- paste("Temperature Differential in Austin between", date_1, "and", date_2)
      color = "#ba020e"
      
      plot(x = as.Date(data$DATE), y = data$TDiff, type = "l", xlab = "Date", ylab = 'Temperature Differential (Degrees Fahreheit)', 
           col = color, fg = graph_color, col.lab = graph_color, col.axis = graph_color, main = full_title)
      
      if(input$smoother){
        smooth_curve <- lowess(x = as.Date(data$DATE), y = data$TDiff, f = input$f)
        print(smooth_curve$y[1])
        print(smooth_curve$y[length(smooth_curve$y)])
        lines(smooth_curve, col = "#000000", lwd = 3)
      }
    }
    
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)


