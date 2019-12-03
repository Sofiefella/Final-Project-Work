library(readr)
library(wbstats)
library(plotly)
library(maps)
library(ggExtra)
library(data.table)
library(gridExtra)
library(knitr)
library(highcharter)
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


ui <- fluidPage(theme = shinytheme("flatly"),
                
      navbarPage("The History of the Olympic Games",
                           
                tabPanel("Olympic Characteristics",
                                    
              tabsetPanel(
                                        
                  tabPanel("Trends",
                      
                      h2("What Kinds of Trends Can We See Over The Past 120 Years?"),
                      
                      br(),
                      
                      sidebarPanel(h3("Trends"),
                                   p("The graphs on the right show trends pertaining to the number of athletes, events, and nations participating in the Olympics over time.")),
                                   
                      mainPanel(plotOutput("athletesPlot"),
                                br(),
                                p("As we can see, the number of athletes participating in the Winter Games begins collecting data points much later than the Summer Games, in 1924. As we look at overall trends, it is clear that the number of athletes in the Summer Games have dramatically increased since 1896, whereas the Winter Games have only steadily increased since they began."),
                                br(),
                                plotOutput("eventsPlot"),
                                br(),
                                p("Similarly to the athlete trend, the number of events have also increased for Summer Games at a trend much faster than the Winter. However, the Winter Games may be able to increase their events to that of the Summer Games, as the number of Summer events have seemed to level off since aroung 1996."),
                                br(),
                                plotOutput("nationsPlot"),
                                br(),
                                p("While the number of nations participating in both the Summer and Winter Games has increased, the amount by which they have increased is not to the hundreds or even thousdands that the number of events and athletes have increased by."))),
                
                  tabPanel("Winter Games", 
                       
                      h2("What Countries Have Been Successful In The Winter Games?"),
                       
                      br(),
                       
                      sidebarPanel(h3("Medals"),
                                   p("Over the past 22 Winter Olympic Games, not including the 2018 games, the graph on the right shows that during this period of time the Unitied States has collected the largest total amount of medals, with Canada following close behind. This graph shows the top 13 countries to date today with the highest number of medals collected in the Winter Games. Each country has a color coated legend to show what kinds of medals they are collecting as well.")),
                      
                      mainPanel(plotOutput("wintermedalPlot"),
                                br(),
                                p("By following the legend to the right, you are able to tell how many of each kind of medal the top scoring countries received. While it is clear that some nations do better than others, there is still a wide array of nations that are winning different events."),
                                br(),
                                highchartOutput("agemedalsPlot"),
                                br(),
                                p("By moving your cursor over the different boxes in this boxplot, you will be able to see the maximum, minimum, and median values for the ages of both female and male medaling athletes."),
                                br(),
                                plotOutput("wintermedalsPlot"),
                                br(),
                                p("This graph, similarly to the one above, will show you what the ages of athletes are, who are both medaling or not medaling, over the course of the years that the Winter Olympics have been held."))),
                                        
                  tabPanel("Summer Games",
                       
                        h2("What Countries Have Been Successful In The Summer Games?"),
                           
                        br(),
                          
                        sidebarPanel(h3("Medals"),
                                     p("Over the past 28 Summer Olympic Games, the graph on the right shows that during this period of time the Unitied States has collected the largest total amount of medals, with no other country coming anywhere close behind to challenge the US total number of medals or any type of medal as well. This graph shows the top 12 countries to date today with the highest number of medals collected in the Summer Games. Each country has a color coated legend to show what kinds of medals they are collecting as well.")),
                            
                        mainPanel(plotOutput("summermedalPlot"),
                                  br(),
                                  p("By following the legend to the right, you are able to tell how many of each kind of medal the top scoring countries received. Unlike the Winter Games where it is clear that there are a wide array of nations that are winning different events, in the Summer Games there are one or two nations that win most of the medals, and then the remaining medals are spread evenly across several different nations."),
                                  br(),
                                  highchartOutput("agemedals2Plot"),
                                  br(),
                                  p("By moving your cursor over the different boxes in this boxplot, you will be able to see the maximum, minimum, and median values for the ages of both female and male medaling athletes."),
                                  br(),
                                  plotOutput("summermedalsPlot"),
                                  br(),
                                  p("This graph, similarly to the one above, will show you what the ages of athletes are, who are both medaling or not medaling, over the course of the years that the Summer Olympics have been held."))),
                                        
                  tabPanel("Location",
                        
                        h2("Where Were Athletes Coming From Over Time?"),
                        
                        br(),
                        
                        sidebarPanel(h3("Countries"),
                                     p("By looking at the progression of graphs on the right, it is clear from these plots that the geographic representation in the Olympics has expanded over time since the earlier Olympic Games in 1900. However, despite this gradual expansion, the Olympics still have large strides to make to incorporate more of the world. As we look at the different maps from 1900 to 2014, there is great progress being made, but as we can see in the closer to present years, there is still a lot of room for improvement; even in 2014, many regions remain severely underrepresented. These regions include, but are not limitied to, almost all of Africa, South America, the western half of South America, Southwest Asia, the Indonesian Islands, and Iceland. Hopefully one day we can look back and see almost all of the world highly represnted in the Games.")),
                        
                        mainPanel(plotOutput("mapsPlot"),
                                  br(),
                                  plotOutput("maps5Plot"),
                                  br(),
                                  plotOutput("maps2Plot"),
                                  br(),
                                  plotOutput("maps4Plot"),
                                  br(),
                                  plotOutput("maps3Plot"))))),
                           
                tabPanel("Athlete Characteristics",
                                    
             tabsetPanel(
                                        
                 tabPanel("Gender",
                      
                      h2("What Have Gender Trends Been Over Time?"),
                      
                      br(),
                      
                      sidebarPanel(h3("Seasonal Trends"),
                                   p("Looking at the overall trends for the growth of male and female athletes over time in both seasons together, it is clear that females have had exponential growth in their involvement in the Games, while the men have grown at a slightly less severe rate.")),
                     
                      mainPanel(plotOutput("winterGenderPlot"),
                                br(),
                                p("While females started to compete in the Winter Games after men, the graph above shows that over time the number of female athletes joining the Winter Olympics has steadily increased while the number of male athletes has fluctuated both up and down over the past 120 years."),
                                br(),
                                plotOutput("summerGenderPlot"),
                                br(),
                                p("Similarly to the Winter Games, the number of female athletes that participate in the Summer Games has increased over time. However, in the Summer Games the number of females has increased recently at a very dramatic rate so that the number of females is very close to the most recent number of participating males. For males, it is clear that their participation has fluctuated even more than in the Winter Games."))),
                                          
                  tabPanel("Height & Weight",
                      
                      h2("What Are Body Trends Like For Olympic Athletes?"),
                      
                      br(),
                      
                      sidebarPanel(h3("Height & Weight"),
                                   p("Both of the plots show that for males and females, their corresponding heights and weights have increased slightly over the history of the Games. While these increases are not dramatic, we definitely still see that they have increased over the following years. Something interesting to keep in mind is that while it may look like some athletes stick out of the 'norm', we aren't able to tell by looking at this chart what kind of body types are favored for certain events.")),
                      
                      mainPanel(
                        p("Both height and weight in athletes have followed similar trends as time has gone on from the early games to 2016. In both categories, the height/weight has stayed relatively stable with a slight increase as the time gets closer to present day. As the time gets closer to present day, there is also more variation in the heights and weights of athltes although the general trend has stayed the same. In both cases the statistics for females have stayed below the mens' heights and weights."),
                        br(),
                        plotOutput("heightPlot"),
                        br(),
                        plotOutput("weightPlot"))),
                 
                  tabPanel("Age",
                           
                        h2("What Ages Are Athletes Typically?"),
                        
                        br(),
                        
                        sidebarPanel(h3("Age Trends"),
                                     p("The following graphs on the right take the age variable and explore it on 3 different graphs. On the top, age is displayed on a histogram and on the bottom two graphs, age is displayed on density charts.")),
                        
                        mainPanel(plotOutput("agePlot"),
                                  br(),
                                  p("In this histogram, each bin represents an age group, and the height of the bin represents the number of athletes that fall into this age group. The graph is also faceted by sex, which shows us the age distributions for males and females separately."),
                                  br(),
                                  plotOutput("age2Plot"),
                                  br(),
                                  p("This density plot takes the same data that was used in the histogram, but instead of dividng it by gender and putting the data into bins, it shows the density level at each age."),
                                  br(),
                                  plotOutput("age3Plot"),
                                  br(),
                                  p("This last plot is similar to the one above in that it takes the age data and plots it by the density of each age, but this plot also shows us how the density charts are made by gender. It is very similar to the graph above with an added element of division by sex."))))),
                  
                    tabPanel("Statistical Analysis",
                      
                          h2("What Does The Data Show?"),
                             
                          br(),
                             
                          sidebarPanel(h3("Two Findings:"),
                                       p("The first graph shows a linear regression that was run to show the participation of male and female athletes over time and the resulting best-fit line plotted."),
                                       br(),
                                       p("Below, a table is shown that depicts a different statistic I was interested in, the affect of age on winning a gold medal.")),
                      
                          mainPanel(plotOutput("statsPlot"),
                                    br(),
                                    p("This graph helps show us that over time female participation in the Games has been steadily increasing. If we look back at the 1900 Games to now, we can tell that the growth of female involvement is increasing to hopefully one day catch up with that of the males!"),
                                    br(),
                                    tableOutput("statsTable"),
                                    p("In this table, the average coefficient value, or the slope of the regression line, is shown with 5th and 95th percentile values to give an indication of uncertainty associated with the term. Additionally, its corresponding r-squared value is shown as well in order to give an indication of the linear fit between the two variables."))),
                  
             
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
            ggplot(aes(x = Country, y = n, group = Medal, fill = Medal)) + 
            geom_col() +
            labs(title = "The Top Medaling Countries: Winter Olympic Games", 
                 subtitle = "Which Countries Over Time Have Succeeded The Most?", 
                 x = "Country", 
                 y = "Number of Medals") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$summermedalPlot <- renderPlot({
        summer_medals %>%
            select(Country, Medal, Year) %>%
            group_by(Country, Medal) %>%
            count(Country) %>%
            arrange(desc(n)) %>%
            head(30) %>%
            ggplot(aes(x = Country, y = n, group = Medal, fill = Medal)) + 
            geom_col() +
            labs(title = "The Top Medaling Countries: Sumer Olympic Games", 
                 subtitle = "Which Countries Over Time Have Succeeded The Most?", 
                 x = "Country", 
                 y = "Number of Medals") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$mapsPlot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      paris <- regions %>% 
        filter(Games == "1900 Summer") %>%
        group_by(region) %>%
        summarize(Paris = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(paris, by = "region")
      mappingdata$Paris[is.na(mappingdata$Paris)] <- 0
      
      world <- left_join(world, mappingdata, by = "region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Paris)) +
        labs(title = "Paris 1900",
             x = NULL, 
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange")
       
    })
    
    output$maps5Plot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      amsterdam <- regions %>% 
        filter(Games == "1928 Summer") %>%
        group_by(region) %>%
        summarize(Amsterdam = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(amsterdam, by = "region")
      mappingdata$Amsterdam[is.na(mappingdata$Amsterdam)] <- 0
      
      world <- left_join(world, mappingdata, by = "region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Amsterdam)) +
        labs(title = "Amsterdam 1928",
             x = NULL, 
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange")
      
    })
    
    output$maps2Plot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
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
      
      world <- left_join(world, mappingdata, by = "region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Melbourne)) +
        labs(title = "Melbourne 1956",
             x = NULL, 
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low ="white", high = "orange")
      
    })
    
    output$maps3Plot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      sochi <- regions %>% 
        filter(Games == "2014 Winter") %>%
        group_by(region) %>%
        summarize(Sochi = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(sochi, by = "region")
      mappingdata$Sochi[is.na(mappingdata$Sochi)] <- 0
      
      world <- left_join(world, mappingdata, by = "region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Sochi)) +
        labs(title = "Sochi 2014",
             x = NULL, 
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low ="white", high = "orange")
    })
    
    output$maps4Plot <- renderPlot({
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      montreal <- regions %>% 
        filter(Games == "1984 Summer") %>%
        group_by(region) %>%
        summarize(Montreal = length(unique(ID)))
      
      world <- map_data("world")
      
      mappingdata <- tibble(region=unique(world$region))
      
      mappingdata <- mappingdata %>% 
        left_join(montreal, by = "region")
      mappingdata$Montreal[is.na(mappingdata$Montreal)] <- 0
      
      world <- left_join(world, mappingdata, by = "region")
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Montreal)) +
        labs(title = "Montreal 1976",
             x = NULL, 
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low ="white", high = "orange")
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
            labs(title = "Male & Female Winter Olympic Athletes Over Time", 
                 subtitle = "How has the number of male and female athletes changed over the years?", 
                 x = "Year", 
                 y = "Number of Athletes") +
          theme(plot.title = element_text(size = 16,face = "bold"))
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
        labs(title = "Male & Female Summer Olympic Athletes Over Time", 
             subtitle = "How has the number of male and female athletes changed over the years?", 
             x = "Year", 
             y = "Number of Athletes") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$heightPlot <- renderPlot({
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x = as.factor(Year), y = Height, fill = Sex)) +
        geom_boxplot() +
        labs(title = "Height Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Height (cm)",
             subtitle = "How have Olympic athletes' heights fluctuated over time?") +
        scale_fill_manual(values = c("pink","blue")) +
        theme(axis.text.x = element_text(size = 5, angle = 20)) +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$weightPlot <- renderPlot({
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x = as.factor(Year), y = Weight, fill=Sex)) +
        geom_boxplot() +
        labs(title = "Weight Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Weight (kg)",
             subtitle = "How have Olympic athletes' weights fluctuated over time?") +
        scale_fill_manual(values = c("pink","blue")) +
        theme(axis.text.x = element_text(size = 5, angle = 20)) +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$athletesPlot <- renderPlot({
      counts <- athlete_events %>% 
        filter(Sport != "Art Competitions") %>%
        group_by(Year, Season) %>%
        summarize(Athletes = length(unique(ID)))
      
      ggplot(counts, aes(x = Year, y = Athletes, group = Season, color = Season)) +
        geom_point(size = 1) +
        geom_line() +
        scale_color_manual(values = c("purple","blue")) +
        labs(title = "The Number of Athletes Over Time", 
             x = "Year",
             y = "Athletes",
             subtitle = "Have females or males been more involved?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
      
    })
    
    output$eventsPlot <- renderPlot({
      counts <- athlete_events %>% 
        filter(Sport != "Art Competitions") %>%
        group_by(Year, Season) %>%
        summarize(Events = length(unique(Event)))
      
      ggplot(counts, aes(x = Year, y = Events, group = Season, color = Season)) +
        geom_point(size = 1) +
        geom_line() +
        scale_color_manual(values = c("purple","blue")) +
        labs(title = "The Number of Events Over Time", 
             x = "Year",
             y = "Events",
             subtitle = "Which season hosts more events?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
      
    })
    
    output$nationsPlot <- renderPlot({
      counts <- athlete_events %>% 
        filter(Sport != "Art Competitions") %>%
        group_by(Year, Season) %>%
        summarize(Nations = length(unique(NOC)))
      
      ggplot(counts, aes(x = Year, y = Nations, group = Season, color = Season)) +
        geom_point(size = 1) +
        geom_line() +
        scale_color_manual(values = c("purple","blue")) +
        labs(title = "The Number of Nations Over Time", 
             x = "Year",
             y = "Nations",
             subtitle = "Which season draws more nations from around the world?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
      
    })
    
    output$agemedalsPlot <- renderHighchart({
    age_data <- athlete_events %>%
      drop_na(Age) %>%
      filter(Season == "Winter")
    
    age_data$Medal <- ifelse(is.na(age_data$Medal),"others",
                             ifelse(age_data$Medal== "Gold","Gold",
                                    ifelse(age_data$Medal== "Silver","Silver","Bronze")))
    
    hcboxplot(x = age_data$Age, var = age_data$Sex, var2 = age_data$Medal, outliers = FALSE) %>% 
      hc_chart(type = "row") %>%
      hc_title(text = "Age Distributed: Medal Winners & Others") %>%
      hc_subtitle(text = "Data collected From the Winter Olympics 1896-2016")
    
    })
    
    output$agemedals2Plot <- renderHighchart({
      age_data <- athlete_events %>%
        drop_na(Age) %>%
        filter(Season == "Summer")
      
      age_data$Medal <- ifelse(is.na(age_data$Medal),"others",
                               ifelse(age_data$Medal== "Gold","Gold",
                                      ifelse(age_data$Medal== "Silver","Silver","Bronze")))
      
      hcboxplot(x = age_data$Age, var = age_data$Sex, var2 = age_data$Medal, outliers = FALSE) %>% 
        hc_chart(type = "row") %>%
        hc_title(text = "Age Distributed: Medal Winners & Others") %>%
        hc_subtitle(text = "Data collected From the Summer Olympics 1896-2016")
      
    })
    
    output$agePlot <- renderPlot({
    age <- athlete_events %>%
      na.omit() %>%
      filter(Season == "Summer")
    
    ggplot(age, aes(x = Age))+
      geom_histogram(binwidth = 1, aes(fill = ..count..), color = "purple", fill = "black") +
      facet_wrap(~Sex) +
      labs(title = "Age Distribution of Olympics Athletes",
           subtitle = "Data Taken From the Summer Olympics 1896-2016",
           x = "Age",
           y = "Number of Athletes") +
      theme(legend.position = "none",
            axis.text = element_text(size = 8,face="bold"),
            plot.title = element_text(size = 16,face = "bold"))
    
    })
    
    output$age2Plot <- renderPlot({
      athlete_events %>%
        ggplot(aes(x = Age)) +
        geom_density(color = "black", fill = "red") +
        labs(title = "Age Distribution",
             x = "Age", 
             y = "Density",
             subtitle = "What is the overall density of the different athlete ages?") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$age3Plot <- renderPlot({
      athlete_events %>%
        ggplot(aes(x = Age, fill = Sex)) +
        geom_density(alpha = 0.3) +
        labs(title = "Age Distribution by Sex",
             x = "Age", 
             y = "Density",
             subtitle = "How is the density of ages broken up by sex?") +
        theme_minimal() +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$wintermedalsPlot <- renderPlot({
    wintermedals <- athlete_events %>% 
      group_by(Year, Season, Medal) %>% 
      filter(Season == "Winter") %>%
      summarise(mean = mean(Age, na.rm = TRUE))
    
    ggplot(wintermedals, aes(x = Year, y = mean, group = Medal, color = Medal)) +
      geom_point(size = 1) +
      geom_line()  +
      labs(title = "Age of Athletes Winning Medals",
           subtitle = "What are the typical ages of medaling athletes?",
           x = "Year",
           y = "Average Age") + 
      theme(plot.title = element_text(size = 16,face = "bold"))
    
    })
    
    output$summermedalsPlot <- renderPlot({
      wintermedals <- athlete_events %>% 
        group_by(Year, Season, Medal) %>% 
        filter(Season == "Summer") %>%
        summarise(mean = mean(Age, na.rm = TRUE))
      
      ggplot(wintermedals, aes(x = Year, y = mean, group = Medal, color = Medal)) +
        geom_point(size = 1) +
        geom_line()  +
        labs(title = "Age of Athletes Winning Medals",
             subtitle = "What are the typical ages of medaling athletes?",
             x = "Year",
             y = "Average Age") + 
        theme(plot.title = element_text(size = 16,face = "bold"))
      
    })
    
    output$statsPlot <- renderPlot({
    
      count_per_nation <- athlete_events %>% 
      filter(Year %in% c(1900,1936,1976,1984,2016)) %>%
      group_by(Year, NOC, Sex) %>%
      summarize(Count = length(unique(ID))) %>%
      spread(Sex, Count)
    
    names(count_per_nation)[3:4] <- c("Male","Female")
    count_per_nation$Male[is.na(count_per_nation$Male)] <- 0
    count_per_nation$Female[is.na(count_per_nation$Female)] <- 0
    count_per_nation$Year <- as.factor(count_per_nation$Year)
    
    count_per_nation %>%
      ggplot(aes(x = Male, y = Female, group = Year, color = Year)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Female vs. Male Olympians from participating NOCs",
           x = "Number of Female Participants",
           y = "Number of Male Participants",
           subtitle = "How are the number of males vs. females different?") +
      theme(plot.title = element_text(size = 16,face = "bold"))
    
    })
    
    output$statsTable <- renderTable({
    
      athlete_clean <- athlete_events %>%
        na.omit() %>%
        select(Age, Medal, Sex) %>%
        mutate(gold = as.logical(ifelse(Medal == "Gold", "TRUE", "FALSE")))
      
      model_2 <- lm(data = athlete_clean, formula = gold ~ Age)
      
      conf_ints <- confint_tidy(model_2, conf.level = .9) %>% 
        mutate(label = c("Intercept", "Coefficient")) %>%
        filter(label == "Coefficient")
      
      r_squared <- glance(model_2) %>%
        select(r.squared) %>%
        mutate(label = "Coefficient")
      
      tidy(model_2) %>%
        mutate(label = c("Intercept", "Coefficient")) %>%
        inner_join(conf_ints, by = "label") %>%
        inner_join(r_squared, by = "label") %>%
        select(estimate, conf.low, conf.high, r.squared) %>%
        rename("Coefficient" = estimate,
               "5th Percentile" = conf.low,
               "95th Percentile" = conf.high,
               "R Squared" = r.squared) %>%
        mutate_if(is.numeric, round, digits = 8)
    
    })
}


shinyApp(ui = ui, server = server)
