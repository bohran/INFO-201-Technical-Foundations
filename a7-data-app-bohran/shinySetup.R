# Creates layout of main page by implementing tabs and widgets for users to interact with
my.ui <- fluidPage (
  titlePanel(h2("World Bank: Data on CO2 Emissions")),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Choose a year:", choices = c(1998:2014, "Most Recent")),
      radioButtons('level', " Find out varying CO2 Emission levels within each country given the chosen year:", 
      c("CO2 Emissions (kt)" = "EN.ATM.CO2E.KT", "CO2 Emissions (metrics per capita)" = "EN.ATM.CO2E.PC")
    )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Table", dataTableOutput("table")),
                  tabPanel("Global Map", plotOutput("map", hover = "plot_hover"),
                           p("Selected Country:" , strong(textOutput('selected', inline = TRUE))),
                           p(textOutput('paragraph', inline = TRUE))
                          )
                  )
    )
  )
)

# Renders table displaying Country Codes and CO2 emission data type
my.server <- function(input, output) {
  output$table <- renderDataTable({
    table.data <- filtered.data %>% 
      filter(Series.Code == input$level) %>% 
      select(Country.Code, input$year) 
    
  })
  
  # Renders plot of the world map and displays CO2 emission level ranges
  output$map <- renderPlot({
    world <- map_data("world")
    world <- mutate(world, ISO3 = iso.alpha(region, 3))
    map.emissions <- left_join(world, filtered.data, by = c("ISO3" = "Country.Code")) %>% select(-subregion) 
    map.emissions <- map.emissions %>% filter(Series.Code == input$level)
    p <- ggplot(data = map.emissions) + 
      geom_polygon(aes(x = long, y = lat, group = group, fill=cut(map.emissions[, input$year], 10))) +
      scale_fill_brewer(type = "seq", name = "C02 Emisson Levels") +
      ggtitle("Carbon Dioxide Emissions Throughout the Globe") +
      theme(plot.title = element_text(size = 20)) + 
      coord_quickmap()
   return(p)
  })
  
  # Creates a reactive function for returning country names given the hovered longitude and latitude value
  d <- reactiveValues()
  d$selected.class <- ""
  output$selected <- renderText({
    selected <- GetCountryAtPoint(input$plot_hover$x, input$plot_hover$y)
    return(selected)
  })
  
  # Sentence to explain the map visualization
  output$paragraph <- renderText({
    return("This visualization displays the differences in CO2 emissions either in kilotons(kt) or by metric tons per capita based on the chosen year around the world.")
  })
}

