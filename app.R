if(!require(pacman)){install.packages("pacman");
  library(pacman)}
p_load(shiny, ggplot2, gapminder, leaflet, sf, tidycensus, bslib, tidyverse, thematic)
getwd()
##################### data

df <- st_read("t6ej_final.geojson")
mdf <- read.csv("merged_df.csv")

mdf_grp <- mdf %>%
  group_by(Peak, Year) %>%
  summarize(
    av_tti = mean(TTI),
    av_pti = mean(PTI),
    av_bti = mean(BTI),
    av_sev = mean(Severity),
    av_var = mean(Variability)) %>%
  mutate(Year = as.integer(Year))

# Define UI for application
ui <- fluidPage(theme = bslib::bs_theme(
  bg = "#002B36", fg = "#EEE8D5",
  "progress-bar-bg" = "orange"
),
titlePanel("Socio-Economic and Congestion Data Exploration"),
tabsetPanel(
  tabPanel("Line Plot",
           sidebarLayout(
             sidebarPanel(
               selectInput("variable", "Select Variable:",
                           choices = c("TTI" = "av_tti",
                                       "PTI" = "av_pti",
                                       "BTI" = "av_bti"))
             ),
             mainPanel(
               plotOutput("linePlot")
             )
           )),
  tabPanel("Grouped Bar Chart",
           sidebarLayout(
             sidebarPanel(
               selectInput("vars", "Select Variable:",
                           choices = c("Severity" = "Severity",
                                       "Variability" = "Variability"))
             ),
             mainPanel(
               plotOutput("barPlot")
             )
           )),
  tabPanel("Scatter Plot",
           sidebarLayout(
             sidebarPanel(
               selectInput("Impact", "Select Impact:",
                           choices = c("Severity", "Variability")),
               selectInput("Congestion", "Select Congestion:",
                           choices = c("TTI", "PTI", "BTI"))
             ),
             mainPanel(
               plotOutput("scatterPlot")
             )
           )),
  tabPanel("map plot",
           sidebarLayout(
             sidebarPanel(
               selectInput("year", "Select Year:", unique(df$year)),
               selectInput("attribute", "Select Attribute:", c("Poverty", "Disabled", "Foreigner"))
             ),
             mainPanel(
               leafletOutput("map")
             )
           ))
)
)

# Define server logic
server <- function(input, output) {  
  output$linePlot <- renderPlot({
    ggplot(mdf_grp, aes(x = Year, y = get(input$variable), color = Peak)) +
      geom_line() +
      labs(x = "Year", y = input$variable) +
      scale_x_continuous(breaks = 2018:2020)
  })
  
  output$barPlot <- renderPlot({
    mdf_grp_2 <- mdf %>%
      group_by(Peak, Year) %>%
      summarize(value = mean(get(input$vars)))
    
    ggplot(mdf_grp_2, aes(x = Year, y = value, fill = Peak)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Year", y = input$vars)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(mdf, aes_string(x = input$Impact, y = input$Congestion)) +
      geom_point() +
      labs(title = "Scatter Plot",
           x = input$Impact,
           y = input$Congestion) +
      theme_minimal()
  })
  
  output$map <- renderLeaflet({
    filtered_data <- df %>% filter(year == input$year)
    
    leaflet(filtered_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        fillColor = ~colorNumeric("viridis", get(input$attribute))(get(input$attribute)),
        fillOpacity = 0.6,
        color = "",
        weight = 1,
        label = ~paste0(GEOID, "<br>", input$attribute, ": ", round(get(input$attribute), 2))
      ) %>%
      addLegend(
        position = "bottomright",
        title = input$attribute,
        pal = colorNumeric("viridis", filtered_data[[input$attribute]]),
        values = ~get(input$attribute)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
