# install the package first!!!
library(shiny)
library(plotly)
library(dplyr)
library(readr)
library(shinythemes)
library(leaflet)

# reading dataset
data <- read_csv("All.csv")
data

# group 1 data
year_group <- data %>% group_by(Year, Quarter) %>% 
  summarise(year_acc = sum(count))
year_death <- data %>% group_by(Year, Quarter) %>%
  summarise(death_rate = mean(Death))

year_group <- na.omit(year_group)
quarter_group <- data %>% group_by(Year, Quarter) %>% 
  summarise(quarter_acc = sum(count)) 
quarter_death <- data %>% group_by(Year, Quarter)%>%
  summarise(death_rate = mean(Death))
quarter_group  <- na.omit(quarter_group)

# group 2 data
factor_group <- na.omit(data %>% group_by(Light, Speed, Weather, Road) %>% 
                         summarize(factor_acc = sum(as.numeric(count))))

light_group <- na.omit(data %>% group_by(Light) %>% 
  summarize(acc_acount = sum(as.numeric(count))))

speed_group <- na.omit(data %>% group_by(Speed) %>% 
  summarize(acc_acount = sum(as.numeric(count))))

weather_group <- na.omit(data %>% group_by(Weather) %>% 
  summarize(acc_acount = sum(as.numeric(count))))

road_group <- na.omit(data %>% group_by(Road) %>% 
  summarize(acc_acount = sum(as.numeric(count))))

# group 3 data
map_group <- na.omit(data %>% group_by(Region) %>% 
  summarize(avgLat=mean(Lat, na.rm=TRUE),avgLon=mean(Lng, na.rm=TRUE), map_count = sum(as.numeric(count))))

map_group <- unique(map_group)

map_choice <- map_group$Region %>% append("All") %>% as.factor() %>% levels()

# Design the interface of the web application
ui <- fluidPage(theme = shinytheme("cosmo"),
    #----------------------------------Application title--------------------------------
    titlePanel(h2("Factors of motorcycle accident", align = "center")),
    
    #----------------------------------First page----------------------------------
    #Navbar structure for UI
    navbarPage("Factors of accident",
               tabPanel("Time factor", icon = icon("chart-bar"),
                        sidebarLayout(
                          sidebarPanel(
                            actionButton("switch", "Switch Death Rate/Accident amount"),
                            textOutput('button_value'),
                            selectInput(
                              inputId = "Quarter_Select",
                              label = "Select quarter of Plot 1",
                              multiple = F,
                              selected = NULL,
                              choices = c('1', '2', '3', '4'),
                              width = 300),
                            selectInput(
                              inputId = "Year_Select",
                              label = "Select Year of Plot 2",
                              multiple = F,
                              selected = NULL,
                              choices = c('2015', '2016', '2017', '2018', '2019', '2020'),
                              width = 300)),
                            
                            
                          mainPanel(plotlyOutput("vis2", width = 750, height = 480),
                                    plotlyOutput("vis1", width = 750, height = 480)
                          ))),
               #----------------------------------Second page----------------------------------
               
               tabPanel("Environmental factors", icon = icon("chart-bar"),
                        sidebarLayout(
                          sidebarPanel(  
                            selectInput(
                              inputId = "Factor_Select",
                              label = "Select Environmental Factor",
                              multiple = F,
                              selected = NULL,
                              choices = c('Light Condition', 'Speed Zone', 'Weather', 'Road Geometry'),
                              width = 300)),
                            
                            mainPanel(plotlyOutput("vis3"))
                          )
                        ),
               #----------------------------------Third page----------------------------------
               tabPanel("Map", icon = icon("globe-americas"),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "Region", 
                              label = h3("Select Region"), 
                              choices = map_choice,
                              selected = "All"),
                            
                            sliderInput("slider1", "Count selection:",
                                        min = 0,
                                        max = 20,
                                        value = c(0, 20),
                                        step = 1)), 
                          
                          mainPanel(leafletOutput("map"))
                        )
               ))
               
)
               

# Desgin the server side
# Take the input of the user to filter data and draw the plot
server <- function(input, output) {
  # judge which graph to show 'Accident amount' or 'Death rate'
  whichplot <- reactiveVal(TRUE)
  observeEvent(input$switch, {
    whichplot(!whichplot())
  })
  
  which_graph <- reactive({
    if (is.null(input$Year_Select)){
      quarter_group <- quarter_group
      quarter_death <- quarter_death
      
    }
    else{
      quarter_group <- quarter_group %>% filter(Year %in% input$Year_Select)
      quarter_death <- quarter_death %>% filter(Year %in% input$Year_Select)
    }
    # Quarter plot
    p1 <- ggplot(quarter_group, aes(x = Quarter, y = quarter_acc, fill = factor(Quarter))) + 
      geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
      scale_fill_brewer(palette = "Set1") +
      theme_minimal(base_size = 10, base_family = "Georgia") +
      xlab("Quarter") +
      ylab("Accident amount")+
      ggtitle("Quarterly accidents amount")+
      theme(legend.position = "right") + 
      scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F) +
      theme_bw()
    p12 <- ggplot(quarter_death, aes(x = Quarter, y = death_rate, fill = factor(Quarter))) + 
      geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
      scale_fill_brewer(palette = "Set1") +
      theme_minimal(base_size = 10, base_family = "Georgia") +
      xlab("Quarter") +
      ylab("Death Rate")+
      ggtitle("Quarterly death rate")+
      theme(legend.position = "right") + 
      # scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F) +
      theme_bw()
    if (whichplot()) {
      ggplotly(p1)
    } else {
      ggplotly(p12)
    }
  })
  
  which_graph2 <- reactive({
    
    if (is.null(input$Quarter_Select)){
      year_group <- year_group
      year_death <- year_death
      
    }
    else{
      year_group <- year_group %>% filter(Quarter %in% input$Quarter_Select)
      year_death <- year_death %>% filter(Quarter %in% input$Quarter_Select)
    }
    # Year plot
    p2 <- ggplot(year_group, aes(x = Year, y = year_acc, fill = factor(Year))) + 
      geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
      scale_fill_brewer(palette = "Set1") +
      theme_minimal(base_size = 10, base_family = "Georgia") +
      xlab("Year") +
      ylab("Accident amount")+
      ggtitle("Annually accidents amount")+
      theme(legend.position = "right") + 
      # scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F) +
      theme_bw()
    p22 <- ggplot(year_death, aes(x = Year, y = death_rate, fill = factor(Year))) + 
      geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
      scale_fill_brewer(palette = "Set1") +
      theme_minimal(base_size = 10, base_family = "Georgia") +
      xlab("Year") +
      ylab("Death Rate")+
      ggtitle("Annually death rate")+
      theme(legend.position = "right") + 
      # scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F) +
      theme_bw()
    if (whichplot()) {
      ggplotly(p2)
    } else {
      ggplotly(p22)
    }
  })
  
  # page 1
  output$vis1 <- renderPlotly({
    which_graph()
  })
  
  output$vis2 <- renderPlotly({
    which_graph2()
  })
  
  # page 2
  # draw the four environmental condition plots
  p3 <- ggplot(light_group, aes(x = Light, y = acc_acount, fill = factor(Light))) + 
    geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 10, base_family = "Georgia") +
    xlab("Light Condition") +
    ylab("Accident amount")+
    ggtitle("Annually accidents amount")+
    theme(legend.position = "right") + 
    theme_bw()
  
  
  p4 <- ggplot(speed_group, aes(x = Speed, y = acc_acount, fill = factor(Speed))) + 
    geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 10, base_family = "Georgia") +
    theme_minimal() +
    xlab("Speed Zone") +
    ylab("Accident amount")+
    ggtitle("Annually accidents amount")+
    theme(legend.position = "right") + 
    theme_bw()
  
  p5 <- ggplot(weather_group, aes(x = Weather, y = acc_acount, fill = factor(Weather))) + 
    geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 10, base_family = "Georgia") +
    xlab("Weather Condition") +
    ylab("Accident amount")+
    ggtitle("Annually accidents amount")+
    theme(legend.position = "right") + 
    theme_bw()
  
  p6 <- ggplot(road_group, aes(x = Road, y = acc_acount, fill = factor(Road))) + 
    geom_bar(stat = 'identity', alpha=0.6, width=0.6) + 
    scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_size = 10, base_family = "Georgia") +
    xlab("Road Condition") +
    ylab("Accident amount")+
    ggtitle("Annually accidents amount")+
    theme(legend.position = "right") + 
    theme_bw()
  
  
  # determine which plpt to show according the drop down list
  output$vis3 <- renderPlotly({
    if (input$Factor_Select == 'Light Condition'){
      ggplotly(p3)
    }
    else if(input$Factor_Select == 'Speed Zone'){
      ggplotly(p4)
    }
    else if (input$Factor_Select == 'Weather'){
      ggplotly(p5)
    }
    else if (input$Factor_Select == 'Road Geometry'){
      ggplotly(p6)
    }
  })
  
  # page 3
  output$map <- renderLeaflet({
    if (input$Region != "All") {
      map_group <- map_group %>% filter(map_group$Region == input$Region)
    } else {
      map_group <- map_group
    }
    map_group <- map_group %>% filter( map_count>= input$slider1[1] & map_count <= input$slider1[2])
    # draw the map
    map <- map_group %>%
      leaflet() %>%
      addTiles() %>% 
      setView(lng=145,lat=-37,zoom=6) %>%
      addCircleMarkers(lat = ~avgLat, 
                       lng = ~avgLon, 
                       radius = ~map_count,
                       popup = ~paste('Region:',Region,'count:',map_count))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

