library(readr)
library(wbstats)
library(plotly)
library(maps)
library(ggExtra)
library(data.table)
library(gridExtra)
library(knitr)
library(tidyverse)
library(shiny)
library(lubridate)
library(shinythemes)

athlete_events <- read_csv("athlete_events.csv", col_types = cols(
  ID = col_double(),
  Name = col_character(),
  Sex = col_character(),
  Age = col_double(),
  Height = col_double(),
  Weight = col_double(),
  Team = col_character(),
  NOC = col_character(),
  Games = col_character(),
  Year = col_double(),
  Season = col_character(),
  City = col_character(),
  Sport = col_character(),
  Event = col_character(),
  Medal = col_character()
))

winter_medals <- read_csv("winter.csv", col_types = cols(
  Year = col_double(),
  City = col_character(),
  Sport = col_character(),
  Discipline = col_character(),
  Athlete = col_character(),
  Country = col_character(),
  Gender = col_character(),
  Event = col_character(),
  Medal = col_character()
))

summer_medals <- read_csv("summer.csv", col_types = cols(
  Year = col_double(),
  City = col_character(),
  Sport = col_character(),
  Discipline = col_character(),
  Athlete = col_character(),
  Country = col_character(),
  Gender = col_character(),
  Event = col_character(),
  Medal = col_character()
))

noc_regions <- read_csv("noc_regions.csv", col_types = cols(
  NOC = col_character(),
  region = col_character(),
  notes = col_character()
))


ui <- fluidPage(theme = shinytheme("united"),
                
      navbarPage("The History of the Olympic Games",
                           
                tabPanel("Olympic Characteristics",
                                    
              tabsetPanel(
                                        
                  tabPanel("Winter Games", 
                       
                      h3("What Countries Have Been Successful In The Winter Games?"),
                       
                      br(),
                       
                      sidebarPanel(h4("Medals"),
                                   p("Over the past 22 Winter Olympic Games, not including the 2018 games, the graph on the right shows that during this period of time the Unitied States has collected the largest total amount of medals, with Canada following close behind. This graph shows the top 13 countries to date today with the highest number of medals collected in the Winter Games. Each country has a color coated legend to show what kinds of medals they are collecting as well.")),
                      
                      mainPanel(plotOutput("wintermedalPlot"))),
                                        
                  tabPanel("Summer Games",
                       
                        h3("What Countries Have Been Successful In The Summer Games?"),
                           
                        br(),
                          
                        sidebarPanel(h4("Medals"),
                                     p("Over the past 28 Summer Olympic Games, the graph on the right shows that during this period of time the Unitied States has collected the largest total amount of medals, with no other country coming anywhere close behind to challenge the US total number of medals or any type of medal as well. This graph shows the top 12 countries to date today with the highest number of medals collected in the Summer Games. Each country has a color coated legend to show what kinds of medals they are collecting as well.")),
                            
                        mainPanel(plotOutput("summermedalPlot"))),
                                        
                  tabPanel("Location",
                        
                        h3("Where Were Athletes Coming From Over Time?"),
                        
                        br(),
                        
                        sidebarPanel(h4("Countries"),
                                     p("By looking at the progression of graphs on the right, it is clear from these plots we can see that the geographic representation in the Olympics has expanded over time since the earlier Olympic Games in 1900. However, despite this expansion, the Olympics still have large strides to make to incorporate more of the world. Even in 2014, many regions remain severely underrepresented. These regions include, but are not limitied to, almost all of Africa, South America, the western half of South America, Southwest Asia, the Indonesian islands, and Iceland. Hopefully one day we can look back and see almost all of the world highly represnted in the Games.")),
                        
                        mainPanel(
                          plotOutput("mapsPlot"),
                          br(),
                          plotOutput("maps2Plot"),
                          br(),
                          plotOutput("maps3Plot"))))),
                           
                tabPanel("Athlete Characteristics",
                                    
             tabsetPanel(
                                        
                 tabPanel("Gender",
                      
                      h3("What Have Gender Trends Been Over Time?"),
                      
                      br(),
                      
                      sidebarPanel(h4("Seasonal Trends"),
                                   p("Looking at the overall trends for the growth of male and female athletes over time in both seasons together, it is clear that females have had exponential growth in their involvement in the Games, while the men have grown at a slightly less severe rate.")),
                     
                      mainPanel(
                        plotOutput("winterGenderPlot"),
                        br(),
                        plotOutput("summerGenderPlot"))),
                                        
                  tabPanel("Height",
                      
                      h3("What Are Height Trends Like For Olympic Athletes?"),
                      
                      br(),
                      
                      sidebarPanel(h4("Height")),
                      
                      mainPanel(plotOutput("heightPlot"))),
                   
                    tabPanel("Weight",
                      
                      h3("What Are Weight Trends Like For Olympic Athletes?"),
                      
                      br(),
                      
                      sidebarPanel(h4("Weight")),
                      
                      mainPanel(plotOutput("weightPlot"))))),
                           
                  tabPanel("About",
                      
                      mainPanel(
                          
                          h2("Summary"),
                          h5("This dashboard gives you the option to explore data collected from both the summer and winter Olympic Games throughout the years of 1896 to 2016. You have the option to select and view particular data from a variety of categories, including particular locations, events, medals won, gender, and so much more."),
                            
                          h2("The Data"),
                          h5("For this project I colleted three separate data sets. All of the visualizations are based on these data sets. Part of the data is from", a("Sports Reference", href="www.sports-reference.com"), ", collected in May of 2018 and the other two data sets were provided by the IOC Research and Reference Service and published by The Guardian's Datablog."),
                          h5("One data set contains information for 270,960 different events ranging from the start of the Olympics in 1896 to the 2016 games hosted in Rio. Another data set contains 5,770 different medals won by all different athltes during the winter games, and the last data set contains 30,065 different medals won by all different athletes during the summer games."),
                             
                          h2("About Me"),
                          h5("I am a sophomore undergraduate student at Harvard concentrating in Economics and pursuing a secondary in Global Health and Health Policy. At Harvard, I play on the Women's Lacrosse Team and find myself getting involved in many different organizations in the athletic department. I have grown up my whole life loving sports and as a result I wanted to use this project as an opportunity to use my passion for data science and sports to explore the Olympic Games beyond what you see on television."),
                          h5("Feel free to reach out and contact me at ogill@college.harvard.edu or connect with me on LinkedIn", a("HERE", href="https://www.linkedin.com/in/olly-gill-081899160/")),
                          h5("The source code for this Shiny App can be found at my GitHub")))))


server <- function(input, output) {

    output$wintermedalPlot <- renderPlot({
        winter_medals %>%
            select(Country, Medal, Year) %>%
            group_by(Country, Medal) %>%
            count(Country) %>%
            arrange(desc(n)) %>%
            head(30) %>%
            ggplot(aes(x = Country, y = n, group = Medal, fill = Medal)) + geom_col() +
            labs(title = "The Top Medaling Countries: Winter Olympic Games", subtitle = "Which Countries Over Time Have Succeeded The Most?", x = "Country", y = "Number of Medals")
    })
    
    output$summermedalPlot <- renderPlot({
        summer_medals %>%
            select(Country, Medal, Year) %>%
            group_by(Country, Medal) %>%
            count(Country) %>%
            arrange(desc(n)) %>%
            head(30) %>%
            ggplot(aes(x = Country, y = n, group = Medal, fill = Medal)) + geom_col() +
            labs(title = "The Top Medaling Countries: Sumer Olympic Games", 
                 subtitle = "Which Countries Over Time Have Succeeded The Most?", 
                 x = "Country", 
                 y = "Number of Medals")
    })
    
    output$mapsPlot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by="NOC") %>%
        filter(!is.na(region))
      
      paris <- regions %>% 
        filter(Games == "1900 Summer") %>%
        group_by(region) %>%
        summarize(Paris = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(paris, by="region")
      mappingdata$Paris[is.na(mappingdata$Paris)] <- 0
      
      world <- left_join(world, mappingdata, by="region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Paris)) +
        labs(title = "Paris 1900",
             x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient2(low="white",high = "red")
       
    })
    
    output$maps2Plot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by="NOC") %>%
        filter(!is.na(region))
      
      melbourne <- regions %>% 
        filter(Games == "1956 Summer") %>%
        group_by(region) %>%
        summarize(Melbourne = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(melbourne, by="region")
      mappingdata$Melbourne[is.na(mappingdata$Melbourne)] <- 0
      
      world <- left_join(world, mappingdata, by="region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Melbourne)) +
        labs(title = "Melbourne 1956",
             x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient2(low="white",high = "red")
      
    })
    
    output$maps3Plot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by="NOC") %>%
        filter(!is.na(region))
      
      sochi <- regions %>% 
        filter(Games == "2014 Winter") %>%
        group_by(region) %>%
        summarize(Sochi = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(sochi, by="region")
      mappingdata$Sochi[is.na(mappingdata$Sochi)] <- 0
      
      world <- left_join(world, mappingdata, by="region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Sochi)) +
        labs(title = "Sochi 2014",
             x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_colourbar(title="Athletes")) +
        scale_fill_gradient2(low="white",high = "red")
    })
    
    output$winterGenderPlot <- renderPlot({
        Gender_Per_Year <- athlete_events %>%
            filter(Season == "Winter") %>%
            group_by(Year, Sex) %>%
            count() %>%
            arrange(Year)
        
        Gender_Per_Year %>%
            ggplot(aes(x = Year, y = n, group = Sex, color = Sex)) +
            geom_point() +
            geom_line() +
            labs(title = "The Number of Male and Female Winter Olympic Athletes Over Time", 
                 subtitle = "How has the number of male and female athletes changed over the years?", 
                 x = "Year", 
                 y = "Number of Athletes")
    })
    
    output$summerGenderPlot <- renderPlot({
      Gender_Per_Year <- athlete_events %>%
        filter(Season == "Summer") %>%
        group_by(Year, Sex) %>%
        count() %>%
        arrange(Year)
      
      Gender_Per_Year %>%
        ggplot(aes(x = Year, y = n, group = Sex, color = Sex)) +
        geom_point() +
        geom_line() +
        labs(title = "The Number of Male and Female Summer Olympic Athletes Over Time", subtitle = "How has the number of male and female athletes changed over the years?", x = "Year", y = "Number of Athletes")
    })
    
    output$heightPlot <- renderPlot({
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x=as.factor(Year), y = Height, fill = Sex)) +
        geom_boxplot() +
        labs(title = "Height Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Height (cm)") +
        scale_fill_manual(values=c("pink","blue")) +
        theme(axis.text.x=element_text(size = 5, angle = 30))
    })
    
    output$weightPlot <- renderPlot({
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
        geom_boxplot() +
        labs(title = "Weight Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Weight (kg)") +
        scale_fill_manual(values=c("pink","blue")) +
        theme(axis.text.x=element_text(size = 5, angle = 30))
    })
}


shinyApp(ui = ui, server = server)
