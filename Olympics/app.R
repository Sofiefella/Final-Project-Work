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
library(plotly)
library(shinythemes)

all_data <- left_join(athlete_events, GDP, by = c("Team"))
winter_medals <- read_csv("winter.csv")
summer_medals <- read_csv("summer.csv")
noc_regions <- read_csv("noc_regions.csv")


ui <- fluidPage(theme = shinytheme("superhero"),
                
                br(),
                
                navbarPage("The History of the Olympic Games",
                           
                           tabPanel("Olympic Characteristics",
                                    tabsetPanel(
                                        
                                        tabPanel("Winter Games", 
                                                 h3("What Kind Of Trends Are Shown In The Winter Games?"),
                                                 br(),
                                                 sidebarPanel(
                                                     selectInput("Medals")
                                                 ),
                                                 mainPanel(
                                                     plotlyOutput("wintermedalPlotly"),
                                                     br()
                                                 ),
                                                 br()
                                                 ),
                                        
                                        tabPanel("Summer Games",
                                                 h3("What Kind Of Trends Are Shown In The Summer Games?"),
                                                 br(),
                                                 sidebarPanel(
                                                     selectInput("Medals")
                                                 ),
                                                 mainPanel(
                                                     plotlyOutput("summermedalPlotly"),
                                                     br()
                                                 ),
                                                 br()
                                                 ),
                                        
                                        tabPanel("Location",
                                                 h3("Where Were Athletes Coming From Over Time?"),
                                                 br(),
                                                 sidebarPanel(
                                                     selectInput("Countries")
                                                 ),
                                                 mainPanel(
                                                     plotlyOutput("mapsPlotly"),
                                                     br()
                                                 ),
                                                 br(),
                                                 ),
                                        
                                        )),
                           tabPanel("Athlete Characteristics",
                                    tabsetPanel(
                                        tabPanel("Gender",
                                                 h3("What Have Gender Trends Been Over Time?"),
                                                 br(),
                                                 sidebarPanel(
                                                     selectInput("Season")
                                                 ),
                                                 mainPanel(
                                                     plotlyOutput("winterGender"),
                                                     br(),
                                                 ),
                                                 br(),
                                                 ),
                                        
                                        tabPanel("Height",
                                                 h3("What Are Height Trends Like For Olympic Athletes?"),
                                                 br(),
                                                 sidebarPanel(
                                                   selectInput("Height")
                                                 ),
                                                 mainPanel(
                                                   plotlyOutput("heightPlotly"),
                                                   br()
                                                 ),
                                                 br(),
                                        ),
                                        
                                        tabPanel("Weight",
                                                 h3("What Are Weight Trends Like For Olympic Athletes?"),
                                                 br(),
                                                 sidebarPanel(
                                                   selectInput("Weight")
                                                 ),
                                                 mainPanel(
                                                   plotlyOutput("weightPlotly"),
                                                   br()
                                                 ),
                                                 br(),
                                        )
                
                                    )),
                           
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
                                        
                                        h5("The source code for this Shiny App can be found at my GitHub")
                                        
                                    ))
                                    

))



server <- function(input, output) {

    output$wintermedalPlotly <- renderPlot({
        wintermedal <-
            winter_medals %>%
            select(Country, Medal, Year) %>%
            group_by(Country, Medal) %>%
            count(Country) %>%
            arrange(desc(n)) %>%
            head(30) %>%
            ggplot(aes(x = Country, y = n, group = Medal, fill = Medal)) + geom_col() +
            labs(title = "The Top Medaling Countries: Winter Olympic Games", subtitle = "Which Countries Over Time Have Succeeded The Most?", x = "Country", y = "Number of Medals")
    })
    
    output$summermedalPlotly <- renderPlot({
        summermedal <-
            summer_medals %>%
            select(Country, Medal, Year) %>%
            group_by(Country, Medal) %>%
            count(Country) %>%
            arrange(desc(n)) %>%
            head(30) %>%
            ggplot(aes(x = Country, y = n, group = Medal, fill = Medal)) + geom_col() +
            labs(title = "The Top Medaling Countries: Sumer Olympic Games", subtitle = "Which Countries Over Time Have Succeeded The Most?", x = "Country", y = "Number of Medals")
    })
    
    output$mapsPlotly <- renderPlot({
        regions <- athlete_events %>% 
            left_join(noc_regions, by="NOC") %>%
            filter(!is.na(region))
        
        paris <- regions %>% 
            filter(Games == "1900 Summer") %>%
            group_by(region) %>%
            summarize(Paris = length(unique(ID)))
        
        melbourne <- regions %>% 
            filter(Games == "1956 Summer") %>%
            group_by(region) %>%
            summarize(Melbourne = length(unique(ID)))
        
        sochi <- regions %>% 
            filter(Games == "2014 Winter") %>%
            group_by(region) %>%
            summarize(Sochi = length(unique(ID)))
        
        
        world <- map_data("world")
        
        mappingdata <- tibble(region=unique(world$region))
        
        mappingdata <- mappingdata %>% 
            left_join(paris, by="region") %>%
            left_join(melbourne, by="region") %>%
            left_join(sochi, by="region")
        mappingdata$Paris[is.na(mappingdata$Paris)] <- 0
        mappingdata$Melbourne[is.na(mappingdata$Melbourne)] <- 0
        mappingdata$Sochi[is.na(mappingdata$Sochi)] <- 0
        
        world <- left_join(world, mappingdata, by="region")
        
        p1 <- ggplot(world, aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = Paris)) +
            labs(title = "Paris 1900",
                 x = NULL, y=NULL) +
            theme(axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  panel.background = element_rect(fill = "navy"),
                  plot.title = element_text(hjust = 0.5)) +
            guides(fill=guide_colourbar(title="Athletes")) +
            scale_fill_gradient(low="white",high="red")
        
        p2 <- ggplot(world, aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = Melbourne)) +
            labs(title = "Melbourne 1956",
                 x = NULL, y = NULL) +
            theme(axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  panel.background = element_rect(fill = "navy"),
                  plot.title = element_text(hjust = 0.5)) +
            guides(fill=guide_colourbar(title="Athletes")) +
            scale_fill_gradient2(low = "white", high = "red")
        
        p3 <- ggplot(world, aes(x = long, y = lat, group = group)) +
            geom_polygon(aes(fill = Sochi)) +
            labs(title = "Sochi 2014",
                 x = NULL, y = NULL) +
            theme(axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  panel.background = element_rect(fill = "navy"),
                  plot.title = element_text(hjust = 0.5)) +
            guides(fill=guide_colourbar(title="Athletes")) +
            scale_fill_gradient2(low="white",high = "red")
        
        grid.arrange(p1, p2, p3, ncol=1)
    })
    
    output$winterGenderPlotly <- renderPlot({
        Gender_Per_Year <- athlete_events %>%
            filter(Season == "Winter") %>%
            group_by(Year, Sex) %>%
            count() %>%
            arrange(Year)
        
        Gender_Per_Year %>%
            ggplot(aes(x = Year, y = n, group = Sex, color = Sex)) +
            geom_point() +
            geom_line() +
            labs(title = "The Number of Male and Female Winter Olympic Athletes Over Time", subtitle = "How has the number of male and female athletes changed over the years?", x = "Year", y = "Number of Athletes")
    })
    
    output$heightPlotly <- renderPlot({
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
    
    output$heightPlotly <- renderPlot({
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
        geom_boxplot() +
        labs(title = "Weight Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Weight (lb)") +
        scale_fill_manual(values=c("pink","blue")) +
        theme(axis.text.x=element_text(size = 5, angle = 30))
    })
}


shinyApp(ui = ui, server = server)
