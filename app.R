library(shiny)
library(tidyverse)
library(dplyr)

data <- read_delim("UAH-lower-troposphere-long.csv")

ui <- fluidPage(
  titlePanel("UAH Lower Troposphere Temperatures"),
  tabsetPanel(
    tabPanel("Introductory Page", 
             h3("Welcome to my app :D"),
             p("This app will be able to display data collected by the UAH."),
             em("The temperature is measured in Celsius degrees C."),
             br(),
             p("This dataset has", nrow(data), "rows of observation and", ncol(data), "variables."),
             p("Click on the other tabs above to view a plot or a table."),
             br(),
             h4("Sample Data"),
             p("Here is a small sample of random data collected from the dataset."),
             tableOutput("sample_data_table")
    ),
    tabPanel("Plot Page", 
             sidebarLayout(
               sidebarPanel(
                 h3("This is my plot!"),
                 em("Here, you may select various regions to display temperatures,
                    and also showcase a function to show certain trends found in those years."),
                 br(),
                 checkboxInput("show_trend", "Display trends", value = FALSE),
                 checkboxGroupInput("regions", "Select regions to display:", 
                                    c("globe", "globe_land", "globe_ocean", "nh", "nh_land", "nh_ocean", "sh", "sh_land", 
                                      "sh_ocean", "trpcs", "trpcs_land", "trpcs_ocean", "noext", "noext_land", "noext_ocean", 
                                      "soext", "soext_land", "soext_ocean", "nopol", "nopol_land", "nopol_ocean", "sopol", 
                                      "sopol_land", "sopol_ocean", "usa48", "usa49", "aust")),
               ),
               mainPanel(
                 plotOutput(outputId = "plot"),
                 br(),
                 textOutput(outputId = "total_obs")
               )
             )
    ),
    
    tabPanel("Table Page", 
             h3("Temperature Averages"),
             p("This table displays the average temperature by month, year, or decade based on the selected radio button input."),
             radioButtons("avg_type", "Select Average Type:", 
                          c("By Month" = "month", "By Year" = "year", "By Decade" = "decade")),
             br(),
             p(em(textOutput("range_text"))),
             br(),
             dataTableOutput(outputId = "table"))
  )
)


server <- function(input, output) {

  observe({
    total_obs <- data %>%
      filter(region %in% input$regions) %>%
      summarise(total_observations = n())
    output$total_obs <- renderText({
      paste0("Total observations: ", total_obs$total_observations)
    })
  })
  
  
  output$plot <- renderPlot({
    regions_to_display <- input$regions
    filtered_data <- data %>% 
      filter(region %in% regions_to_display)
  

    if (input$show_trend) {
      ggplot(filtered_data, aes(x = year, y = temp, color = region)) +
        geom_point() +
        geom_smooth(se = FALSE)
    } else {
      ggplot(filtered_data, aes(x = year, y = temp, color = region)) +
        geom_point()
    }
  })
  
  
  output$table <- renderDataTable({
    if (input$avg_type == "month") {
      table_data <- data %>%
        group_by(month) %>%
        summarise(temp = mean(temp)) %>%
        arrange(month)
    } else if (input$avg_type == "year") {
      table_data <- data %>%
        group_by(year) %>%
        summarise(temp = mean(temp)) %>%
        arrange(year)
    } else if (input$avg_type == "decade") {
      table_data <- data %>%
        mutate(decade = 10 * floor(year / 10)) %>%
        group_by(decade) %>%
        summarise(temp = mean(temp)) %>%
        arrange(decade)
    }
    range_text <- paste0("Temperature range: ", 
                         format(round(range(table_data$temp), 2), nsmall = 2))
    
    output$range_text <- renderText(range_text)
    
    table_data
    
  })
  
  output$sample_data_table <- renderTable({
    sample_data <- data %>% sample_n(5)
    sample_data
  })

}

shinyApp(ui = ui, server = server)

      