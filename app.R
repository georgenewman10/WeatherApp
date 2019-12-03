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
                                choices = c("Temperature", 'Precipitation'),
                                selected = "Temperature"),
                    
                    # Select DATE range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "1978-01-01", end = "2019-11-07",
                                   min = "1978-01-01", max = "2019-11-07"),
                    
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
                    plotOutput(outputId = "lineplot", height = "300px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://www.google.com/finance/domestic_trends", "Source: Google Domestic Trends", target = "_blank")
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
    #weather_data %>%
      if(input$type == 'Temperature'){
        new_data = weather_data %>% 
          select('DATE', 'TMAX')
        #new_data = new_data[input$DATE==input$date[1],] 
      }
      else if(input$type == 'Precipitation'){
        new_data = weather_data %>% 
            select('DATE', 'PRCP')
        #new_data = new_data[input$DATE==input$date[1],] 
      }
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    color = "#434343"
    par(mar = c(4, 4, 1, 1))
    if(input$type == 'Temperature') {
        plot(x = selected_data()$DATE, y = selected_data()$TMAX, type = "l",
             xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = as.numeric(selected_data()$DATE), y = selected_data()$TMAX, f = input$f)
            print(smooth_curve$y[1])
            print(smooth_curve$y[length(smooth_curve$y)])
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    }
    else if(input$type == 'Precipitation') {
        plot(x = selected_data()$DATE, y = selected_data()$PRCP, type = "l",
             xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = as.numeric(selected_data()$DATE), y = selected_data()$PRCP, f = input$f)
            print(smooth_curve$y[1])
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    }

  })
  
  
  # Pull in description of trend
  #output$desc <- renderText({
  #  trend_text <- filter(data_description, type == input$type) %>% pull(text)
  #  paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  #})
}

# Create Shiny object
shinyApp(ui = ui, server = server)


