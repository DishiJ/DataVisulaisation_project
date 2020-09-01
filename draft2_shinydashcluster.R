install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("shiny")
install.packages("dplyr")
install.packages("htmltools")
install.packages("leaflet")
install.packages("scales")
install.packages("datasets")
install.packages("viridis")
install.packages("plotly")
install.packages("quantmod")
install.packages("gganimate")
install.packages("hrbrthemes")
install.packages("shiny")


library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)
library(htmltools)
library(leaflet)
library(scales)
library(datasets)
library(viridis)
library(plotly)
library(quantmod)

library(gganimate)
library(babynames)
library(hrbrthemes)
library(shiny)
library(shinydashboard)


install.packages("shinydashboard")
library(shinydashboard)

library(shiny)
library(shinydashboard)


#readinf csv file
data <- read.csv("Crashes_Last_Five_Years (1).csv",header=T)
data$ACCIDENT_DATE <- format(as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
data$YEAR_OF_ACCIDENT <- format(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"),"%Y")
#data$MONTH_OF_ACCIDENT <- month(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"))


#ui for shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard to display Accidents in Victoria"),
  dashboardSidebar(sidebarMenu(
    menuItem("Accidents on the Map", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Region Wise", tabName = "widgets", icon = icon("th")),
    menuItem("Light Condition", tabName = "widgets2", icon = icon("th2"))
  )),
  dashboardBody( 
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard", 
              # Application title
              headerPanel("LOCATION WISE ACCIDENTS"),
              
              sidebarLayout(
                sidebarPanel(
                  #dropdown for year selection
                  div(style="display: inline-block;vertical-align:top;width: 200px;",
                      #setting the style of the selectinput
                      selectInput("variable2", "Year Select:", 
                                  #variable2 storing input of method of smoothing
                                  c("2014" = "2014", 
                                    "2015" = "2015",
                                    "2016" = "2016",
                                    "2017" = "2017",
                                    "2018" = "2018",
                                    "2019" = "2019")
                      ))
                  
                ),
                
                
                mainPanel(
                  #h3(textOutput("caption"),align="center"),
                  #caption will store caption for main plot
                  #plotOutput("coralplot",width="100%"),
                  #coralplot to represent main plot while connecting with server.R
                  h3(textOutput("caption"),align="center"),
                  #caption2 will store caption for leaflet map
                  leafletOutput("locationmap1"),
                  h3(textOutput("caption2"),align="center"),
                  #caption2 will store caption for leaflet map
                  leafletOutput("locationmap")
                  #locationmap to represent location map while connecting with server.R
                  
                )
              )) , tabItem(tabName = "widgets",
                           headerPanel("REGION WISE ACCIDENTS"),
                           
                           sidebarLayout(
                             sidebarPanel(div(style="display: inline-block;vertical-align:top;width: 200px;",
                                              #setting the style of the selectinput
                                              selectInput("variable5", "Region Select:", 
                                                          #variable2 storing input of method of smoothing
                                                          c("EASTERN REGION" = "EASTERN REGION", 
                                                            
                                                            "METROPOLITAN NORTH WEST REGION" = "METROPOLITAN NORTH WEST REGION",
                                                            "METROPOLITAN SOUTH EAST REGION" = "METROPOLITAN SOUTH EAST REGION",
                                                            "NORTH EASTERN REGION" = "NORTH EASTERN REGION",
                                                            "NORTHERN REGION" = "NORTHERN REGION",
                                                            "SOUTH WESTERN REGION" = "SOUTH WESTERN REGION" , 
                                                            "WESTERN REGION" = "WESTERN REGION")
                                              ))
                               
                               
                               
                             ),
                             
                             
                             mainPanel(
                               #h3(textOutput("caption"),align="center"),
                               #caption will store caption for main plot
                               #plotOutput("coralplot",width="100%"),
                               #coralplot to represent main plot while connecting with server.R
                               h3(textOutput("caption3"),align="center"),
                               #caption2 will store caption for leaflet map
                               plotlyOutput("area3"),
                               #coralplot to represent main plot while connecting with server.R
                               h3(textOutput("caption5"),align="center"),
                               #caption2 will store caption for leaflet map
                               plotOutput("area5")
                               #locationmap to represent location map while connecting with server.R
                               
                             )
                           )
                           
                           
                           
                           
                           
                           
              ) , tabItem(tabName = "widgets2",
                          headerPanel("LIGHT CONDITION WISE ACCIDENTS AND TYPE OF DRIVER"),
                          
                          sidebarLayout(
                            sidebarPanel(div(style="display: inline-block;vertical-align:top;width: 200px;",
                                             #setting the style of the selectinput
                                             selectInput("variable3", "Region Select:", 
                                                         #variable2 storing input of method of smoothing
                                              c("EASTERN REGION" = "EASTERN REGION", 
                                            
                                              "METROPOLITAN NORTH WEST REGION" = "METROPOLITAN NORTH WEST REGION",
                                              "METROPOLITAN SOUTH EAST REGION" = "METROPOLITAN SOUTH EAST REGION",
                                              "NORTH EASTERN REGION" = "NORTH EASTERN REGION",
                                              "NORTHERN REGION" = "NORTHERN REGION",
                                              "SOUTH WESTERN REGION" = "SOUTH WESTERN REGION" , 
                                              "WESTERN REGION" = "WESTERN REGION")
                                             ))
                              
                              
                              
                            ),
                            
                            
                            mainPanel(
                              #h3(textOutput("caption"),align="center"),
                              #caption will store caption for main plot
                              #plotOutput("coralplot",width="100%"),
                              #coralplot to represent main plot while connecting with server.R
                              h3(textOutput("caption4"),align="center"),
                              #caption2 will store caption for leaflet map
                              plotlyOutput("area4")
                              #locationmap to represent location map while connecting with server.R
                              
                            )
                          )
                          
                          
                          
                          
                          
                          
              )
      
    ))
)
#server for shiny dashboard
server <-  function(input, output) {
  
  
  output$caption <- reactiveText(function() {
    paste("Location of the accidents overall")
  })        #Caption of output plot of sites
  
  
  output$locationmap1 <- renderLeaflet({
    data <- read.csv("Crashes_Last_Five_Years (1).csv",header=T)
    data$ACCIDENT_DATE <- format(as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
    data$YEAR_OF_ACCIDENT <- format(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"),"%Y")
    #data$MONTH_OF_ACCIDENT <- month(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"))
    
    leaflet(data = data) %>% addTiles() %>%
      addMarkers(~LONGITUDE, ~LATITUDE,
                 clusterOptions = markerClusterOptions()) 
  })
  
  
  
  output$caption2 <- reactiveText(function() {
    paste("Location of the accidents in selected year")
  })        #Caption of output plot of sites
  
  
  output$locationmap <- renderLeaflet({
    
    data <- read.csv("Crashes_Last_Five_Years (1).csv",header=T)
    data$ACCIDENT_DATE <- format(as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
    data$YEAR_OF_ACCIDENT <- format(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"),"%Y")
    #data$MONTH_OF_ACCIDENT <- month(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"))
    
    
    if(input$variable2 == "2014") {
      
      data = data[data$YEAR_OF_ACCIDENT == 2014,]
    }
    else if(input$variable2 == "2015")
    {
      
      data = data[data$YEAR_OF_ACCIDENT == 2015,]
    }
    else if(input$variable2 == "2016")
    {
      
      data = data[data$YEAR_OF_ACCIDENT == 2016,]
    }
    else if(input$variable2 == "2017")
    {
      
      data = data[data$YEAR_OF_ACCIDENT == 2017,]
    }
    else if(input$variable2 == "2018")
    {
      
      data = data[data$YEAR_OF_ACCIDENT == 2018,]
    }
    else if(input$variable2 == "2019")
    {
      
      data = data[data$YEAR_OF_ACCIDENT == 2019,]
    }
    
    
    
    leaflet(data = data) %>% addTiles() %>%
      addMarkers(~LONGITUDE, ~LATITUDE,
                 clusterOptions = markerClusterOptions()) 
    
  })
  
  
  
  
  output$caption3 <- reactiveText(function() {
    paste("Accidents according to regions")
  })        #Caption of output plot of sites
  
  
  output$area3 <- renderPlotly({
    
    
    
    data <- read.csv("Crashes_Last_Five_Years (1).csv",header=T)
    data$ACCIDENT_DATE <- format(as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
    data$YEAR_OF_ACCIDENT <- format(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"),"%Y")
    #data$MONTH_OF_ACCIDENT <- month(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"))
    
    new <- data.frame(data %>% group_by(YEAR_OF_ACCIDENT,REGION_NAME) %>% 
                        summarise(total=length(ACCIDENT_NO)))
    new <- new[!(new$REGION_NAME==" "),]
    new$REGION_NAME <- as.character(new$REGION_NAME)
    new$YEAR_OF_ACCIDENT <- as.double(new$YEAR_OF_ACCIDENT)
    
    p <- new %>% 
      ggplot(aes(x=YEAR_OF_ACCIDENT, y=total, fill=REGION_NAME,text=REGION_NAME)) +
      geom_area() +
      scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      theme(legend.position="none") +
      theme_ipsum() +
      theme(legend.position="center") 
    
    q<-  ggplotly(p, tooltip="text")
    print(q)
    
    
    
  })
  
  
  
  
  output$caption5 <- reactiveText(function() {
    paste("Accidents according to regions in light conditions")
  })        #Caption of output plot of sites
  
  
  output$area5 <- renderPlot({
    data <- read.csv("Crashes_Last_Five_Years (1).csv",header=T)
    data$ACCIDENT_DATE <- format(as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
    data$YEAR_OF_ACCIDENT <- format(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"),"%Y")
    
    new2 <- data.frame(data %>% group_by(LIGHT_CONDITION,REGION_NAME) %>% 
                         summarise(total=length(ACCIDENT_NO)))
    new2 <- new2[!(new2$REGION_NAME==" "),]
    
    new2 <- new2[- grep("Unk", new2$LIGHT_CONDITION),]
    new2 <- new2[- grep("unk", new2$LIGHT_CONDITION),]
    
    
    new2$total <- as.double(new2$total)
    #new2$LIGHT_CONDITION <- as.integer(new2$LIGHT_CONDITION)
  
    
    new3 <- new2[new2$REGION_NAME== input$variable5,]
    
    
    
    
    
    # Create test data.
    

    # Compute percentages
    new3$fraction <- new3$total / sum(new3$total)
    
    # Compute the cumulative percentages (top of each rectangle)
    new3$ymax <- cumsum(new3$fraction)
    
    # Compute the bottom of each rectangle
    new3$ymin <- c(0, head(new3$ymax, n=-1))
    
    # Compute label position
    new3$labelPosition <- (new3$ymax + new3$ymin) / 2
    
    # Compute a good label
    new3$label <- paste0(new3$LIGHT_CONDITION, "\n value: ", new3$total)
    
    # Make the plot
    ggplot(new3, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=LIGHT_CONDITION)) +
      geom_rect() +
      #geom_text( x=2, aes(y=labelPosition, label=label, color=LIGHT_CONDITION), size=6) + # x here controls label position (inner / outer)
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      coord_polar(theta="y") +
      xlim(c(-1, 4)) +
      theme_void() +
      theme(legend.position = "right")
    
    
  })
  
  
  
  
  
  
  
  output$caption4 <- reactiveText(function() {
    paste("Accidents based on light condition and type of driver")
  })        #Caption of output plot of sites
  
  
  output$area4 <- renderPlotly({
    data <- read.csv("Crashes_Last_Five_Years (1).csv",header=T)
    data$ACCIDENT_DATE <- format(as.Date(data$ACCIDENT_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
    data$YEAR_OF_ACCIDENT <- format(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"),"%Y")
    #data$MONTH_OF_ACCIDENT <- month(as.Date(data$ACCIDENT_DATE, format="%Y-%m-%d"))
    
    
    new1 <- data.frame(data %>% group_by(LIGHT_CONDITION,REGION_NAME) %>% 
                         summarise(total=sum(OLD_DRIVER)))
    
    new2 <- data.frame(data %>% group_by(LIGHT_CONDITION,REGION_NAME) %>% 
                         summarise(total=sum(YOUNG_DRIVER)))
    
    
    new1 <- new1[!(new1$REGION_NAME==" "),]
    new2 <- new2[!(new2$REGION_NAME==" "),]
    new1 <- new1[- grep("Unk", new1$LIGHT_CONDITION),]
    new1 <- new1[- grep("unk", new1$LIGHT_CONDITION),]
    new2 <- new2[- grep("Unk", new2$LIGHT_CONDITION),]
    new2 <- new2[- grep("unk", new2$LIGHT_CONDITION),]
    
    chk1 <- new1[new1$REGION_NAME == input$variable3,]
    chk2 <- new2[new2$REGION_NAME == input$variable3,]
    chk2$light_old <- chk1$total
    
    head(new1)
    head(new2)
    
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) 
    fig <- fig %>%
      add_trace(
        r = c(chk2$total),
        theta = c("Dusk/Dawn","Day","Dark Street lights on" ,"Dark Street lights off", "Dark No street lights"),
        name = 'Young Driver'
      ) 
    fig <- fig %>%
      add_trace(
        r = c(chk2$light_old),
        theta = c("Dusk/Dawn","Day","Dark Street lights on" ,"Dark Street lights off", "Dark No street lights"),
        name = 'Old Driver'
      ) 
    
    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,5000)
          )
        )
      )
    
    fig
    
    
    
  })
  
  
  
  
  
  
}

shinyApp(ui,server)

