
# Loading the libraries to run the shiny app.

library(highcharter)
library(maps)
library(shiny)
library(tidyverse)
library(shinythemes)

# Loading in the first big data set. I included col_types to get rid of the col error.

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

# Loading in the second data set. I included col_types to get rid of the col error.

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

# Loading in the third data set. I included col_types to get rid of the col error.

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

# Loading in the fourth data set. I included col_types to get rid of the col error.

noc_regions <- read_csv("noc_regions.csv", col_types = cols(
  NOC = col_character(),
  region = col_character(),
  notes = col_character()
))

# Building the ui page.
# I started with the fluidPage and added in a theme I wanted to make it more detailed.

ui <- fluidPage(theme = shinytheme("flatly"),
      
      # The navbarPage includes all of the tabs that you can click on to look through the project.
                
      navbarPage("The History of the Olympic Games",
                 
                 # The Olympic Characteristics tabPanel includes information on general trends, the 
                 # winter and summer games separately, and where the athletes are coming from. In
                 # each tab I created a new tabPanel with a big title regarding what the tab was about
                 # and then a sidePanel with more indepth information about the specific page.
                 # In the mainPanel I added in my graphs that I made in my rmd file.
                           
                tabPanel("Olympic Characteristics",
              tabsetPanel(
                  tabPanel("Trends",
                      h2("What Kinds of Trends Can We See Over The Past 120 Years?"),
                      br(),
                      sidebarPanel(h3("Trends"),
                                   p("The graphs on the right show trends pertaining to the number of athletes, events, 
                                     and nations participating in the Olympics over time.")),
                      mainPanel(plotOutput("athletesPlot"),
                                br(),
                                p("As we can see, the number of athletes participating in the Winter Games begins 
                                  collecting data points much later than the Summer Games, in 1924. As we look at overall 
                                  trends, it is clear that the number of athletes in the Summer Games have dramatically 
                                  increased since 1896, whereas the Winter Games have only steadily increased since they began."),
                                br(),
                                plotOutput("eventsPlot"),
                                br(),
                                p("Similarly to the athlete trend, the number of events have also increased for Summer Games at 
                                  a trend much faster than the Winter. However, the Winter Games may be able to increase their 
                                  events to that of the Summer Games, as the number of Summer events have seemed to level off 
                                  since aroung 1996."),
                                br(),
                                plotOutput("nationsPlot"),
                                br(),
                                p("While the number of nations participating in both the Summer and Winter Games has increased, 
                                  the amount by which they have increased is not to the hundreds or even thousdands that the 
                                  number of events and athletes have increased by."))),
                  tabPanel("Winter Games", 
                      h2("What Countries Have Been Successful In The Winter Games?"),
                      br(),
                      sidebarPanel(h3("Medals"),
                                   p("Over the past 22 Winter Olympic Games, not including the 2018 games, the graph on the right 
                                     shows that during this period of time the Unitied States has collected the largest total 
                                     amount of medals, with Canada following close behind. This graph shows the top 13 countries 
                                     to date today with the highest number of medals collected in the Winter Games. Each country 
                                     has a color coated legend to show what kinds of medals they are collecting as well.")),
                      mainPanel(plotOutput("wintermedalPlot"),
                                br(),
                                p("By following the legend to the right, you are able to tell how many of each kind of medal the 
                                  top scoring countries received. While it is clear that some nations do better than others, there 
                                  is still a wide array of nations that are winning different events."),
                                br(),
                                highchartOutput("agemedalsPlot"),
                                br(),
                                p("By moving your cursor over the different boxes in this boxplot, you will be able to see the 
                                  maximum, minimum, and median values for the ages of both female and male medaling athletes."),
                                br(),
                                plotOutput("wintermedalsPlot"),
                                br(),
                                p("This graph, similarly to the one above, will show you what the average ages of athletes are, who are 
                                  both medaling or not medaling, over the course of the years that the Winter Olympics have 
                                  been held."))),
                  tabPanel("Summer Games",
                        h2("What Countries Have Been Successful In The Summer Games?"),
                        br(),
                        sidebarPanel(h3("Medals"),
                                     p("Over the past 28 Summer Olympic Games, the graph on the right shows that during this period 
                                       of time the Unitied States has collected the largest total amount of medals, with no other 
                                       country coming anywhere close behind to challenge the US total number of medals or any type 
                                       of medal as well. This graph shows the top 12 countries to date today with the highest 
                                       number of medals collected in the Summer Games. Each country has a color coated legend to 
                                       show what kinds of medals they are collecting as well.")),
                        mainPanel(plotOutput("summermedalPlot"),
                                  br(),
                                  p("By following the legend to the right, you are able to tell how many of each kind of medal the 
                                    top scoring countries received. Unlike the Winter Games where it is clear that there are a wide 
                                    array of nations that are winning different events, in the Summer Games there are one or two 
                                    nations that win most of the medals, and then the remaining medals are spread evenly across 
                                    several different nations."),
                                  br(),
                                  highchartOutput("agemedals2Plot"),
                                  br(),
                                  p("By moving your cursor over the different boxes in this boxplot, you will be able to see the 
                                    maximum, minimum, and median values for the ages of both female and male medaling athletes."),
                                  br(),
                                  plotOutput("summermedalsPlot"),
                                  br(),
                                  p("This graph, similarly to the one above, will show you what the average ages of athletes are, who are 
                                    both medaling or not medaling, over the course of the years that the Summer Olympics have 
                                    been held."))),
                  tabPanel("Location",
                        h2("Where Were Athletes Coming From Over Time?"),
                        br(),
                        sidebarPanel(h3("Countries"),
                                     p("By looking at the progression of graphs on the right, it is clear from these plots that the 
                                       geographic representation in the Olympics has expanded over time since the earlier Olympic 
                                       Games in 1900. However, despite this gradual expansion, the Olympics still have large strides 
                                       to make to incorporate more of the world. As we look at the different maps from 1900 to 2014, 
                                       there is great progress being made, but as we can see in the closer to present years, there 
                                       is still a lot of room for improvement; even in 2014, many regions remain severely 
                                       underrepresented. These regions include, but are not limitied to, almost all of Africa, 
                                       South America, the western half of South America, Southwest Asia, the Indonesian Islands, 
                                       and Iceland. Hopefully one day we can look back and see almost all of the world highly 
                                       represnted in the Games.")),
                        mainPanel(plotOutput("mapsPlot"),
                                  br(),
                                  plotOutput("maps5Plot"),
                                  br(),
                                  plotOutput("maps2Plot"),
                                  br(),
                                  plotOutput("maps4Plot"),
                                  br(),
                                  plotOutput("maps3Plot"))))),
                           
              # The Athlete Characteristics tabPanel includes information on the top 10 athletes and
              # teams in 2016, general height and weight trends, and the age trends of athletes. In
              # each tab I created a new tabPanel with a big title regarding what the tab was about
              # and then a sidePanel with more indepth information about the specific page.
              # In the mainPanel I added in my graphs that I made in my rmd file. 
              
                tabPanel("Athlete Characteristics",
             tabsetPanel(
                  tabPanel("Top 10: 2016",
                      h2("Who Are The Top 10 Athletes? Team?"),
                      br(),
                      sidebarPanel(h3("Top 10 Stats"),
                                   p("The graphs on the right show us the top 10 medaling athletes (top) and the top 10 
                                    medaling teams (bottom) of 2016. By dragging your cursor over the different parts of 
                                    the pie chart you will be able to see how many medals that specific athlete or team 
                                     won during the most rescent Olympic Games of my data set.")),
                      mainPanel(highchartOutput("summer10Plot"),
                                br(),
                                highchartOutput("sport10Plot"),
                                br())),
                  tabPanel("Gender",
                      h2("What Have Gender Trends Been Over Time?"),
                      br(),
                      sidebarPanel(h3("Seasonal Trends"),
                                   p("Looking at the overall trends for the growth of male and female athletes over time 
                                     in both seasons together, it is clear that females have had exponential growth in 
                                     their involvement in the Games, while the men have grown at a slightly less severe 
                                     rate.")),
                      mainPanel(plotOutput("winterGenderPlot"),
                                br(),
                                p("While females started to compete in the Winter Games after men, the graph above shows 
                                  that over time the number of female athletes joining the Winter Olympics has steadily 
                                  increased while the number of male athletes has fluctuated both up and down over the past 
                                  120 years."),
                                br(),
                                plotOutput("summerGenderPlot"),
                                br(),
                                p("Similarly to the Winter Games, the number of female athletes that participate in the Summer 
                                  Games has increased over time. However, in the Summer Games the number of females has 
                                  increased recently at a very dramatic rate so that the number of females is very close to the 
                                  most recent number of participating males. For males, it is clear that their participation 
                                  has fluctuated even more than in the Winter Games."))),
                  tabPanel("Height & Weight",
                      h2("What Are Body Trends Like For Olympic Athletes?"),
                      br(),
                      sidebarPanel(h3("Height & Weight"),
                                   p("Both of the plots show that for males and females, their corresponding heights and weights 
                                     have increased slightly over the history of the Games. While these increases are not 
                                     dramatic, we definitely still see that they have increased over the following years. 
                                     Something interesting to keep in mind is that while it may look like some athletes stick 
                                     out of the 'norm', we aren't able to tell by looking at this chart what kind of body types 
                                     are favored for certain events.")),
                      mainPanel(
                        p("Both height and weight in athletes have followed similar trends as time has gone on from the early 
                          games to 2016. In both categories, the height/weight has stayed relatively stable with a slight increase 
                          as the time gets closer to present day. Recently, there is also more variation in the heights and 
                          weights of athltes although the general trend has stayed the same. In both 
                          cases the statistics for females have stayed below the mens' heights and weights."),
                        br(),
                        plotOutput("heightPlot"),
                        br(),
                        plotOutput("weightPlot"))),
                  tabPanel("Age",
                        h2("What Ages Are Athletes Typically?"),
                        br(),
                        sidebarPanel(h3("Age Trends"),
                                     p("The following graphs on the right take the age variable and explore it on 3 different graphs. 
                                       On the top, age is displayed on a histogram and on the bottom two graphs, age is displayed 
                                       on density charts.")),
                        mainPanel(plotOutput("agePlot"),
                                  br(),
                                  p("In this histogram, each bin represents an age group, and the height of the bin represents the 
                                    number of athletes that fall into this age group. The graph is also faceted by sex, which shows 
                                    us the age distributions for males and females separately."),
                                  br(),
                                  plotOutput("age2Plot"),
                                  br(),
                                  p("This density plot takes the same data that was used in the histogram, but instead of dividing 
                                    it by gender and putting the data into bins, it shows the density level at each age."),
                                  br(),
                                  plotOutput("age3Plot"),
                                  br(),
                                  p("This last plot is similar to the one above in that it takes the age data and plots it by the 
                                    density of each age, but this plot also shows us how the density charts are made by gender. It 
                                    is very similar to the graph above with an added element of division by sex."))))),
                    tabPanel("Statistical Analysis",
                          h2("What Does The Data Show?"),
                          br(),
                          sidebarPanel(h3("Two Findings:"),
                                       p("The first graph shows a linear regression that was run to show the participation of male 
                                         and female athletes over time and the resulting best-fit line plotted."),
                                       br(),
                                       p("Below, a table is shown that depicts a different statistic I was interested in, the 
                                         affect of age on winning a gold medal.")),
                          mainPanel(plotOutput("statsPlot"),
                                    br(),
                                    p("This graph helps show us that over time female participation in the Games has been steadily 
                                      increasing. If we look back at the 1900 Games to now, we can tell that the growth of female 
                                      involvement is increasing to hopefully one day catch up with that of the males!"),
                                    br(),
                                    includeHTML("table.html"),
                                    br(),
                                    br(),
                                    p("This chart shows the relationship between age and winning a gold medal. The intercept value 
                                      shows what the probability is of winning a gold if age is not a factor. The Age row shows us 
                                      that as you get older, your chances of winning a gold decrease at an incredibly slow rate. The 
                                      5th and 95th percentile columns show us the credible interval for how much your chances will 
                                      go down by as you get older by one year."),
                                    br(),
                                    p("I also made versions of this chart using Height or Weight as the variable instead to describe 
                                    gold but for height there was a coefficient of 0.00100 and for weight there was a coefficient 
                                    of 0.00056. Both are very small values that are not very significant"))),
                  
             # In the last panel I added in an about page which gives a summary overview of the project,
             # where I got all of the data for the project, and information about me as a person and how
             # to contact me!
             
                    tabPanel("About",
                      
                      mainPanel(
                          
                          h2("Summary"),
                          h5("This dashboard gives you the option to explore data collected from both the summer
                             and winter Olympic Games throughout the years of 1896 to 2016. You have the option 
                             to select and view particular data from a variety of categories, including particular 
                             locations, events, medals won, gender, and so much more."),
                            
                          h2("The Data"),
                          h5("For this project I colleted four separate data sets. All of the visualizations are 
                             based on these data sets. Part of the data is from", 
                             a("Sports Reference", href="www.sports-reference.com"), ", collected in May of 2018 
                             and two of the other data sets were provided by the IOC Research and Reference Service 
                             and published by The Guardian's Datablog."),
                          h5("One data set contains information for 270,960 different events ranging from the start 
                             of the Olympics in 1896 to the 2016 games hosted in Rio. Another data set contains 
                             5,770 different medals won by all different athltes during the winter games, and the 
                             other data set contains 30,065 different medals won by all different athletes during the 
                             summer games. The last data set contains information on the regions the athletes were from."),
                             
                          h2("About Me"),
                          h5("I am a sophomore undergraduate student at Harvard concentrating in Economics and pursuing 
                             a secondary in Global Health and Health Policy. At Harvard, I play on the Women's Lacrosse 
                             Team and find myself getting involved in many different organizations in the athletic 
                             department. I have grown up my whole life loving sports and as a result I wanted to use 
                             this project as an opportunity to use my passion for data science and sports to explore 
                             the Olympic Games beyond what you see on television."),
                          h5("Feel free to reach out and contact me at ogill@college.harvard.edu or connect with me on 
                             LinkedIn", a("HERE", href="https://www.linkedin.com/in/olly-gill-081899160/")),
                          h5("The source code for this Shiny App can be found at my GitHub", 
                             a("HERE", href="https://github.com/ollygill/Final-Project-Work"))))))

# Creating the server page which holds all of the code to make the graphs that I included
# in the ui part of the project. 
# Almost all of the graphs were made in the rmd file but for some of the graphs that I made
# different versions of I made the base version in the rmd and then copy and pasted the 
# template and changed one or two things in the shiny server in order to make another version
# of the graph.

server <- function(input, output) {

    output$wintermedalPlot <- renderPlot({
      
      # First I took the winter medals data set and selected the variables that I needed to 
      # make the data set. I chose country medal and year. 
      # I then grouped by country to get the medals won per country. I also grouped by medal
      # because I wanted to also know how many of each kind of medal the country won.
      # I then counted by country to know how many each country got.
      # I then arranged by desc so that I could select the top medaling countries. I used head
      # to take the top countries and selected 30 so that I got around 10-15ish countries.
      # I then used ggplot to plot the graph with the right axes and used group and fill for 
      # medal so that the different medals were different colors.
      # I used geom_col to make columns for the data.
      # I added labels and a theme command to make the title large and bolded.
      
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
        theme(plot.title = element_text(size = 16, face = "bold"))
    })
    
    output$summermedalPlot <- renderPlot({
     
      # First I took the summer medals data set and selected the variables that I needed to 
      # make the data set. I chose country medal and year. 
      # I then grouped by country to get the medals won per country. I also grouped by medal
      # because I wanted to also know how many of each kind of medal the country won.
      # I then counted by country to know how many each country got.
      # I then arranged by desc so that I could select the top medaling countries. I used head
      # to take the top countries and selected 30 so that I got around 10-15ish countries.
      # I then used ggplot to plot the graph with the right axes and used group and fill for 
      # medal so that the different medals were different colors.
      # I used geom_col to make columns for the data.
      # I added labels and a theme command to make the title large and bolded.
      
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
        theme(plot.title = element_text(size = 16, face = "bold"))
    })
    
    output$mapsPlot <- renderPlot({
      
      # First I took the athletes data set and joined it with another region data set that I got off
      # the internet. I then used filter to get rid of all the na values in the regions column of the 
      # data set. I stored this all as regions.
      
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      # I then took the region data and filtered for the paris games which were in 1900 and they were
      # the summer games. I then grouped by region to get the region that everyone came from as different
      # groups. I then created a Paris variable that took every athlete and only selected their name
      # once so I wouldn't double count someone coming from one place.
      
      Paris_data <- regions %>% 
        filter(Games == "1900 Summer") %>%
        group_by(region) %>%
        summarize(Paris = length(unique(ID)))
      
      # I then used the map_data command to turn data from the maps package in to a data frame suitable 
      # for plotting with ggplot2. I set the region to world because I wanted to plot the data everywhere
      # in the world. I saved this data.
      
      world <- map_data("world")
      
      # I then created a new data set where region was only listed once. I did this again by using the 
      # unique command so the same regions weren't grouped multiple times.
      
      map_data <- tibble(region = unique(world$region))
      
      # I then took the new map data and joined it together with my Paris data by region.      
      
      map_data <- map_data %>% 
        left_join(Paris_data, by = "region")
      
      # I then took the Paris value in the map_data and got rid of the na values and made them 0s.
      
      map_data$Paris[is.na(map_data$Paris)] <- 0
      
      # I then joined together the updated map data datset and the world data by region.
      
      world <- left_join(world, map_data, by = "region")
      
      # Once I had all the data I created a plot of the data with the long and lat values
      # as the axis variables so that I would be making a map. 
      # I grouped by group so every group of people from a region would be together.
      # I added geom_polygon to create a map and used fill to add in the Paris data.
      # I used guides and scale fill gradient to make a gradient legend that showed the 
      # white regions of the graph to be where few athletes were from and a bright orange
      # where the athletes are concentrated. The legend shows this.
      # I used labs to add a title.
      # I used theme to get rid of the axis ticks and text. I made the background navy like
      # a normal map would be. I made the title in the center of the top.
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Paris)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange") +
        labs(title = "Paris 1900",
             x = NULL,
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5))
    })
    
    output$maps5Plot <- renderPlot({
      
      # First I took the athletes data set and joined it with another region data set that I got off
      # the internet. I then used filter to get rid of all the na values in the regions column of the 
      # data set. I stored this all as regions.
      
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      # I then took the region data and filtered for the amsterdam games which were in 1928 and they were
      # the summer games. I then grouped by region to get the region that everyone came from as different
      # groups. I then created a Amsterdam variable that took every athlete and only selected their name
      # once so I wouldn't double count someone coming from one place.
      
      Amsterdam_data <- regions %>% 
        filter(Games == "1928 Summer") %>%
        group_by(region) %>%
        summarize(Amsterdam = length(unique(ID)))
      
      # I then used the map_data command to turn data from the maps package in to a data frame suitable 
      # for plotting with ggplot2. I set the region to world because I wanted to plot the data everywhere
      # in the world. I saved this data.
      
      world <- map_data("world")
      
      # I then created a new data set where region was only listed once. I did this again by using the 
      # unique command so the same regions weren't grouped multiple times.
      
      map_data <- tibble(region = unique(world$region))
      
      # I then took the new map data and joined it together with my Amsterdam data by region.      
      
      map_data <- map_data %>% 
        left_join(Amsterdam_data, by = "region")
      
      # I then took the Amsterdam value in the map_data and got rid of the na values and made them 0s.
      
      map_data$Amsterdam[is.na(map_data$Amsterdam)] <- 0
      
      # I then joined together the updated map data datset and the world data by region.
      
      world <- left_join(world, map_data, by = "region")
      
      # Once I had all the data I created a plot of the data with the long and lat values
      # as the axis variables so that I would be making a map. 
      # I grouped by group so every group of people from a region would be together.
      # I added geom_polygon to create a map and used fill to add in the Amsterdam data.
      # I used guides and scale fill gradient to make a gradient legend that showed the 
      # white regions of the graph to be where few athletes were from and a bright orange
      # where the athletes are concentrated. The legend shows this.
      # I used labs to add a title.
      # I used theme to get rid of the axis ticks and text. I made the background navy like
      # a normal map would be. I made the title in the center of the top.
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Amsterdam)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange") +
        labs(title = "Amsterdam 1928",
             x = NULL,
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5))
    })
    
    output$maps2Plot <- renderPlot({
      
      # First I took the athletes data set and joined it with another region data set that I got off
      # the internet. I then used filter to get rid of all the na values in the regions column of the 
      # data set. I stored this all as regions.
      
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      # I then took the region data and filtered for the paris games which were in 1956 and they were
      # the summer games. I then grouped by region to get the region that everyone came from as different
      # groups. I then created a Melbourne variable that took every athlete and only selected their name
      # once so I wouldn't double count someone coming from one place.
      
      Melbourne_data <- regions %>% 
        filter(Games == "1956 Summer") %>%
        group_by(region) %>%
        summarize(Melbourne = length(unique(ID)))
      
      # I then used the map_data command to turn data from the maps package in to a data frame suitable 
      # for plotting with ggplot2. I set the region to world because I wanted to plot the data everywhere
      # in the world. I saved this data.
      
      world <- map_data("world")
      
      # I then created a new data set where region was only listed once. I did this again by using the 
      # unique command so the same regions weren't grouped multiple times.
      
      map_data <- tibble(region = unique(world$region))
      
      # I then took the new map data and joined it together with my Melbourne data by region.      
      
      map_data <- map_data %>% 
        left_join(Melbourne_data, by = "region")
      
      # I then took the Melbourne value in the map_data and got rid of the na values and made them 0s.
      
      map_data$Melbourne[is.na(map_data$Melbourne)] <- 0
      
      # I then joined together the updated map data datset and the world data by region.
      
      world <- left_join(world, map_data, by = "region")
      
      # Once I had all the data I created a plot of the data with the long and lat values
      # as the axis variables so that I would be making a map. 
      # I grouped by group so every group of people from a region would be together.
      # I added geom_polygon to create a map and used fill to add in the Melbourne data.
      # I used guides and scale fill gradient to make a gradient legend that showed the 
      # white regions of the graph to be where few athletes were from and a bright orange
      # where the athletes are concentrated. The legend shows this.
      # I used labs to add a title.
      # I used theme to get rid of the axis ticks and text. I made the background navy like
      # a normal map would be. I made the title in the center of the top.
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Melbourne)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange") +
        labs(title = "Melbourne 1956",
             x = NULL,
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5))
    })
    
    output$maps3Plot <- renderPlot({
      
      # First I took the athletes data set and joined it with another region data set that I got off
      # the internet. I then used filter to get rid of all the na values in the regions column of the 
      # data set. I stored this all as regions.
      
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      # I then took the region data and filtered for the paris games which were in 2014 and they were
      # the winter games. I then grouped by region to get the region that everyone came from as different
      # groups. I then created a Sochi variable that took every athlete and only selected their name
      # once so I wouldn't double count someone coming from one place.
      
      Sochi_data <- regions %>% 
        filter(Games == "2014 Winter") %>%
        group_by(region) %>%
        summarize(Sochi = length(unique(ID)))
      
      # I then used the map_data command to turn data from the maps package in to a data frame suitable 
      # for plotting with ggplot2. I set the region to world because I wanted to plot the data everywhere
      # in the world. I saved this data.
      
      world <- map_data("world")
      
      # I then created a new data set where region was only listed once. I did this again by using the 
      # unique command so the same regions weren't grouped multiple times.
      
      map_data <- tibble(region = unique(world$region))
      
      # I then took the new map data and joined it together with my Sochi data by region.      
      
      map_data <- map_data %>% 
        left_join(Sochi_data, by = "region")
      
      # I then took the Sochi value in the map_data and got rid of the na values and made them 0s.
      
      map_data$Sochi[is.na(map_data$Sochi)] <- 0
      
      # I then joined together the updated map data datset and the world data by region.
      
      world <- left_join(world, map_data, by = "region")
      
      # Once I had all the data I created a plot of the data with the long and lat values
      # as the axis variables so that I would be making a map. 
      # I grouped by group so every group of people from a region would be together.
      # I added geom_polygon to create a map and used fill to add in the Sochi data.
      # I used guides and scale fill gradient to make a gradient legend that showed the 
      # white regions of the graph to be where few athletes were from and a bright orange
      # where the athletes are concentrated. The legend shows this.
      # I used labs to add a title.
      # I used theme to get rid of the axis ticks and text. I made the background navy like
      # a normal map would be. I made the title in the center of the top.
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Sochi)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange") +
        labs(title = "Sochi 2014",
             x = NULL,
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5))
    })
    
    output$maps4Plot <- renderPlot({
      
      # First I took the athletes data set and joined it with another region data set that I got off
      # the internet. I then used filter to get rid of all the na values in the regions column of the 
      # data set. I stored this all as regions.
      
      regions <- athlete_events %>% 
        left_join(noc_regions, by = "NOC") %>%
        filter(!is.na(region))
      
      # I then took the region data and filtered for the paris games which were in 1900 and they were
      # the summer games. I then grouped by region to get the region that everyone came from as different
      # groups. I then created a Montreal variable that took every athlete and only selected their name
      # once so I wouldn't double count someone coming from one place.
      
      Montreal_data <- regions %>% 
        filter(Games == "1984 Summer") %>%
        group_by(region) %>%
        summarize(Montreal = length(unique(ID)))
      
      # I then used the map_data command to turn data from the maps package in to a data frame suitable 
      # for plotting with ggplot2. I set the region to world because I wanted to plot the data everywhere
      # in the world. I saved this data.
      
      world <- map_data("world")
      
      # I then created a new data set where region was only listed once. I did this again by using the 
      # unique command so the same regions weren't grouped multiple times.
      
      map_data <- tibble(region = unique(world$region))
      
      # I then took the new map data and joined it together with my Montreal data by region.      
      
      map_data <- map_data %>% 
        left_join(Montreal_data, by = "region")
      
      # I then took the Montreal value in the map_data and got rid of the na values and made them 0s.
      
      map_data$Montreal[is.na(map_data$Montreal)] <- 0
      
      # I then joined together the updated map data datset and the world data by region.
      
      world <- left_join(world, map_data, by = "region")
      
      # Once I had all the data I created a plot of the data with the long and lat values
      # as the axis variables so that I would be making a map. 
      # I grouped by group so every group of people from a region would be together.
      # I added geom_polygon to create a map and used fill to add in the Montreal data.
      # I used guides and scale fill gradient to make a gradient legend that showed the 
      # white regions of the graph to be where few athletes were from and a bright orange
      # where the athletes are concentrated. The legend shows this.
      # I used labs to add a title.
      # I used theme to get rid of the axis ticks and text. I made the background navy like
      # a normal map would be. I made the title in the center of the top.
      
      ggplot(world, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = Montreal)) +
        guides(fill = guide_colourbar(title = "Athletes")) +
        scale_fill_gradient2(low = "white", high = "orange") +
        labs(title = "Montreal 1984",
             x = NULL,
             y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              panel.background = element_rect(fill = "navy"),
              plot.title = element_text(hjust = 0.5))
    })
    
    output$winterGenderPlot <- renderPlot({
      
      # I took the big athlete data set and filtered for the winter season.
      # I grouped by year and sex to see how the trends in male vs female participation
      # changed over the years of the Olympics.
      # I then counted to get the total number of each gender in each year.
      # I then arranged by year to get the most recent numbers at the bottom and the first
      # games at the top of the list.
      
      Gender_Per_Year <- athlete_events %>%
        filter(Season == "Winter") %>%
        group_by(Year, Sex) %>%
        count() %>%
        arrange(Year)
      
      # I took the data I modified above and pipped it into ggplot with year on the x axis
      # and the number of athletes on the y axis. I grouped by sex and made them different
      # colors to make the data pop.
      # I added geom point and geom line to make it a line graph.
      # Labs added in labels and a title and subtitle.
      # I added a theme command to make the title large and bolded.
      
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
      
      # I took the big athlete data set and filtered for the summer season.
      # I grouped by year and sex to see how the trends in male vs female participation
      # changed over the years of the Olympics.
      # I then counted to get the total number of each gender in each year.
      # I then arranged by year to get the most recent numbers at the bottom and the first
      # games at the top of the list.
      
      Gender_Per_Year <- athlete_events %>%
        filter(Season == "Summer") %>%
        group_by(Year, Sex) %>%
        count() %>%
        arrange(Year)
      
      # I took the data I modified above and pipped it into ggplot with year on the x axis
      # and the number of athletes on the y axis. I grouped by sex and made them different
      # colors to make the data pop.
      # I added geom point and geom line to make it a line graph.
      # Labs added in labels and a title and subtitle.
      # I added a theme command to make the title large and bolded.
      
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
      
      # I took the big athlete data set and took out the na values and pipped it into a 
      # ggplot that made Year a factor variable and plotted the height values on the y axis.
      # I then filled by sex so that there would be two separate trends that showed the difference
      # in the two genders.
      # I added geom_boxplot to make a boxplot of the data.
      # I then used scale fill manual so that I could pick the colors of the different graph values.
      # I then had to adjust the size and angle of the year values so that they would be small enough
      # to all fit on the axis without overlapping.
      # I added in labs to make a nice title, subtitle, and axes labels.
      # I used theme to make the title bolded and large.
      
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x = as.factor(Year), y = Height, fill = Sex)) +
        geom_boxplot() +
        scale_fill_manual(values = c("pink","navy")) +
        theme(axis.text.x = element_text(size = 5, angle = 20)) +
        labs(title = "Height Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Height (cm)",
             subtitle = "How have Olympic athletes' heights fluctuated over time?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$weightPlot <- renderPlot({
      
      # I took the big athlete data set and took out the na values and pipped it into a 
      # ggplot that made Year a factor variable and plotted the weight values on the y axis.
      # I then filled by sex so that there would be two separate trends that showed the difference
      # in the two genders.
      # I added geom_boxplot to make a boxplot of the data.
      # I then used scale fill manual so that I could pick the colors of the different graph values.
      # I then had to adjust the size and angle of the year values so that they would be small enough
      # to all fit on the axis without overlapping.
      # I added in labs to make a nice title, subtitle, and axes labels.
      # I used theme to make the title bolded and large.
      
      athlete_events %>% 
        na.omit() %>%
        ggplot(aes(x = as.factor(Year), y = Weight, fill = Sex)) +
        geom_boxplot() +
        scale_fill_manual(values = c("pink","navy")) +
        theme(axis.text.x = element_text(size = 5, angle = 20)) +
        labs(title = "Weight Trends In Athletes Over Time", 
             x = "Olympic Year", 
             y = "Weight (kg)",
             subtitle = "How have Olympic athletes' weights fluctuated over time?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$athletesPlot <- renderPlot({
      
      # First I took the big data set and filtered for sport so that I was only looking at the 
      # main sport events and wasn't looking at the art competitions.
      # I grouped by year and season to get the number of athletes per year per season to look
      # at the trends that differed by winter or summer.
      # I then summarized to create an athletes variable that only took each name, or ID, once 
      # because several of the athletes are listed many times due to being in many events. That 
      # is why I used the length(unique(ID)) command so each name only came up once.
      
      athlete_data <- athlete_events %>% 
        filter(Sport != "Art Competitions") %>%
        group_by(Year, Season) %>%
        summarize(Athletes = length(unique(ID)))
      
      # I then used ggplot to create a plot with the year on the x axis and the number of athletes 
      # on the y axis. I grouped by and colored season to get two separate plots one for each season
      # and I colored them to make them stick out.
      # I added geom point and line to make a line graph of the data.
      # I added scale color manual because I wanted to be able to select the colors of the lines on the
      # graph. I added labs to add in labels and axis titles.
      # I added a theme command to make the title large and bolded.
      
      athlete_data %>%       
        ggplot(aes(x = Year, y = Athletes, group = Season, color = Season)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = c("pink","navy")) +
        labs(title = "The Number of Athletes Over Time", 
             x = "Year",
             y = "Athletes",
             subtitle = "Have females or males been more involved?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$eventsPlot <- renderPlot({
      
      # First I took the big data set and filtered for sport so that I was only looking at the 
      # main sport events and wasn't looking at the art competitions.
      # I grouped by year and season to get per year per season data to look at the trends that 
      # differed by winter or summer.
      # I then summarized to create an events variable that only took each event once 
      # because several of the events are listed many times due to many athletes competeing in 
      # them. That is why I used the length(unique(Event)) command so each name only came up once.
      
      events_data <- athlete_events %>% 
        filter(Sport != "Art Competitions") %>%
        group_by(Year, Season) %>%
        summarize(Events = length(unique(Event)))
      
      # I then used ggplot to create a plot with the year on the x axis and the number of events 
      # on the y axis. I grouped by and colored season to get two separate plots one for each season
      # and I colored them to make them stick out.
      # I added geom point and line to make a line graph of the data.
      # I added scale color manual because I wanted to be able to select the colors of the lines on the
      # graph. I added labs to add in labels and axis titles.
      # I added a theme command to make the title large and bolded.
      
      events_data %>%
        ggplot(aes(x = Year, y = Events, group = Season, color = Season)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = c("pink","navy")) +
        labs(title = "The Number of Events Over Time", 
             x = "Year",
             y = "Events",
             subtitle = "Which season hosts more events?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$nationsPlot <- renderPlot({
      
      # First I took the big data set and filtered for sport so that I was only looking at the 
      # main sport events and wasn't looking at the art competitions.
      # I grouped by year and season to get per year per season data to look at the trends that 
      # differed by winter or summer.
      # I then summarized to create a nations variable that only took each nation once 
      # because several of the nations are listed many times due to many athletes coming from 
      # them. That is why I used the length(unique(NOC)) command so each nation only came up once
      # for each athlete.
      
      nation_data <- athlete_events %>% 
        filter(Sport != "Art Competitions") %>%
        group_by(Year, Season) %>%
        summarize(Nations = length(unique(NOC)))
      
      # I then used ggplot to create a plot with the year on the x axis and nations on the y axis. 
      # I grouped by and colored season to get two separate plots one for each season and I colored 
      # them to make them stick out. 
      # I added geom point and line to make a line graph of the data.
      # I added scale color manual because I wanted to be able to select the colors of the lines on the
      # graph. I added labs to add in labels and axis titles.
      # I added a theme command to make the title large and bolded.
      
      nation_data %>%
        ggplot(aes(x = Year, y = Nations, group = Season, color = Season)) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = c("pink","navy")) +
        labs(title = "The Number of Nations Over Time", 
             x = "Year",
             y = "Nations",
             subtitle = "Which season draws more nations from around the world?") +
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$agemedalsPlot <- renderHighchart({
      
      # I tok the data and got rid of the na values in the Age data column and filtered for 
      # the winter season.
      
      age_data <- athlete_events %>%
        na.omit(Age) %>%
        filter(Season == "Winter")
      
      # I then took the data and made a series of ifelse statements and stored them all as the
      # Medal data in the age_data dataset.
      # First I took the values of medal for which the athletes did not win a medal and I said for
      # those values to come up as 'others'.
      # I then took the values for which gold came up to store then as gold. I did the same for silver.
      # Then for the remaining values I said for them to come up as bronze.
      
      age_data$Medal <- ifelse(is.na(age_data$Medal),"others",
                               ifelse(age_data$Medal== "Gold","Gold",
                                      ifelse(age_data$Medal== "Silver","Silver","Bronze")))
      
      # I then took this new data and made a boxplot with the highcharter plackage which would 
      # allow for me to have a reactive cursor over the boxplot boxes.
      # I put age on the y axis and then did this packages version of faceting my gender and 
      # the type of medal as well.
      # I made the type = column because every example of this type of chart I googled had type set
      # to column.
      # I then added in some details with a title and substitle.
      
      hcboxplot(x = age_data$Age, var = age_data$Sex, var2 = age_data$Medal, outliers = FALSE) %>% 
        hc_chart(type = "column") %>%
        hc_title(text = "Age Distributed: Medal Winners") %>%
        hc_subtitle(text = "Data collected From the Winter Olympics 1896-2016")
    })
    
    output$agemedals2Plot <- renderHighchart({
      
      # I tok the data and got rid of the na values in the Age data column and filtered for 
      # the summer season.
      
      age_data <- athlete_events %>%
        na.omit(Age) %>%
        filter(Season == "Summer")
      
      # I then took the data and made a series of ifelse statements and stored them all as the
      # Medal data in the age_data dataset.
      # First I took the values of medal for which the athletes did not win a medal and I said for
      # those values to come up as 'others'.
      # I then took the values for which gold came up to store then as gold. I did the same for silver.
      # Then for the remaining values I said for them to come up as bronze.
      
      age_data$Medal <- ifelse(is.na(age_data$Medal),"others",
                               ifelse(age_data$Medal== "Gold","Gold",
                                      ifelse(age_data$Medal== "Silver","Silver","Bronze")))
      
      # I then took this new data and made a boxplot with the highcharter plackage which would 
      # allow for me to have a reactive cursor over the boxplot boxes.
      # I put age on the y axis and then did this packages version of faceting my gender and 
      # the type of medal as well.
      # I made the type = column because every example of this type of chart I googled had type set
      # to column.
      # I then added in some details with a title and substitle.
      
      hcboxplot(x = age_data$Age, var = age_data$Sex, var2 = age_data$Medal, outliers = FALSE) %>% 
        hc_chart(type = "column") %>%
        hc_title(text = "Age Distributed: Medal Winners") %>%
        hc_subtitle(text = "Data collected From the Summer Olympics 1896-2016")
    })
    
    output$agePlot <- renderPlot({
      
      # I took the big data set and took out all of the na values.
      # I pipped it into ggplot to make a plot with age on the x axis.
      # I added in geom_histogram to make a histogram with the height representing the number
      # of athletes in each height bin. I added some cool colors in the graph as well.
      # I facted by sex to get two separate graphs by gender.
      # I added in labs to make a nice title and axes labels.
      # I added theme to get rid of the legend, and to make the title large and bold.
      
      athlete_events %>%
        na.omit() %>%
        ggplot(aes(x = Age)) +
        geom_histogram(binwidth = 1, aes(fill = ..count..), color = "black", fill = "pink") +
        facet_wrap(~Sex) +
        labs(title = "Age Distribution of Olympics Athletes",
             subtitle = "Data Taken From the Olympics 1896-2016",
             x = "Age",
             y = "Number of Athletes") +
        theme(legend.position = "none",
              plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$age2Plot <- renderPlot({
      
      # I took the big athlete data set and pipped it into ggplot with age on the x axis.
      # I then added geom_density and made the inside of graph pink with a black border.
      # I added labs to put a title and axes labels on the graph.
      # I added theme_minimal to make the background white.
      
      athlete_events %>%
        ggplot(aes(x = Age)) +
        geom_density(color = "black", fill = "pink") +
        labs(title = "Age Distribution",
             x = "Age", 
             y = "Density",
             subtitle = "What is the overall density of the different athlete ages?") +
        theme(plot.title = element_text(size = 16, face = "bold")) +
        theme_minimal()
    })
    
    output$age3Plot <- renderPlot({
      
      # I took the big athlete data set and pipped it into ggplot with age on the x axis.
      # I also added in fill = sex to get two different charts one for female one for male.
      # I then added geom_density and made the inside of graph pink with a black border.
      # I added labs to put a title and axes labels on the graph.
      # I added theme_minimal to make the background white.
      
      athlete_events %>%
        ggplot(aes(x = Age, fill = Sex)) +
        geom_density(alpha = 0.3) +
        labs(title = "Age Distribution by Sex",
             x = "Age", 
             y = "Density",
             subtitle = "How is the density of ages broken up by sex?") +
        theme(plot.title = element_text(size = 16,face = "bold")) +
        theme_minimal()
    })
    
    output$wintermedalsPlot <- renderPlot({
      
      # I took the big athlete data set and I grouped by year season and medal because I wanted to 
      # find the average age of medal winners and the non medal winners every year the games were hosted
      # for that specific season. I then filtered for the season.
      # I used summarise to make a value mean that finds the average of the age of the groups of people.
      
      wintermedals <- athlete_events %>% 
        group_by(Year, Season, Medal) %>% 
        filter(Season == "Winter") %>%
        summarise(mean = mean(Age, na.rm = TRUE))
      
      # I took this data and piped it into the ggplot function with year on the x axis and then the average
      # age values on the y axis. I grouped by and colored medal so that each type of medal had its own
      # groupings color coated.
      # I added in geom_point and geom_line to make a line graph of the data so that it was easy to visualize.
      # I used labs to add in a title and axes labels and subtitle.
      # I then made the title big and bolded.
      
      wintermedals %>% 
        ggplot(aes(x = Year, y = mean, group = Medal, color = Medal)) +
        geom_point() +
        geom_line()  +
        labs(title = "The Age of Athletes Winning Medals",
             subtitle = "What are the typical ages of medaling athletes?",
             x = "Year",
             y = "Average Age") + 
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$summermedalsPlot <- renderPlot({
      
      # I took the big athlete data set and I grouped by year season and medal because I wanted to 
      # find the average age of medal winners and the non medal winners every year the games were hosted
      # for that specific season. I then filtered for the season.
      # I used summarise to make a value mean that finds the average of the age of the groups of people.
      
      summermedals <- athlete_events %>% 
        group_by(Year, Season, Medal) %>% 
        filter(Season == "Summer") %>%
        summarise(mean = mean(Age, na.rm = TRUE))
      
      # I took this data and piped it into the ggplot function with year on the x axis and then the average
      # age values on the y axis. I grouped by and colored medal so that each type of medal had its own
      # groupings color coated.
      # I added in geom_point and geom_line to make a line graph of the data so that it was easy to visualize.
      # I used labs to add in a title and axes labels and subtitle.
      # I then made the title big and bolded.
      
      summermedals %>% 
        ggplot(aes(x = Year, y = mean, group = Medal, color = Medal)) +
        geom_point() +
        geom_line()  +
        labs(title = "The Age of Athletes Winning Medals",
             subtitle = "What are the typical ages of medaling athletes?",
             x = "Year",
             y = "Average Age") + 
        theme(plot.title = element_text(size = 16,face = "bold"))
    })
    
    output$statsPlot <- renderPlot({
    
      # I took the big athlete data set and filtered for the specific years I wanted. I chose a
      # random group of years that were sort of spread over to cover the trend of time. 
      # I grouped by the year, sex, and region they were from.
      # I then made a variable that accounetd for each athlete once. I didn't want to double count
      # anyone cause that would ruin the point of the graph so that is why I used length(unique(ID)).
      # I then spread the two key variables across the columns of data.
      
      nation_numbers <- athlete_events %>% 
        filter(Year %in% c(1900,1936,1976,1984,2016)) %>%
        group_by(Year, NOC, Sex) %>%
        summarize(number = length(unique(ID))) %>%
        spread(Sex, number)
      
      # I then used the names replacement function to change values in the naiton_numbers data set.
      # I then used [3:4] in order to change the 3rd element to Male and the 4th element to Female.
      # I then got rid of the na values for Male and then Female in the nation_numbers data set and 
      # replaced them with 0s.
      
      names(nation_numbers)[3:4] <- c("Male","Female")
      nation_numbers$Male[is.na(nation_numbers$Male)] <- 0
      nation_numbers$Female[is.na(nation_numbers$Female)] <- 0
      nation_numbers$Year <- as.factor(nation_numbers$Year)
      
      # I then took the data and pipped it into a ggplot function with male of the y axis and female
      # on the x axis. For some reason when I put them in the correct x and y pairings the data was
      # showing up backwards so I had to manually put them in the opposite labels in the labs command.
      # I grouped by year to get the number of the gender every year and made them different colors to pop.
      # I then added in geom_point and an abline to make a line trend through the graph.
      # I added in geom_smooth to make a linear model of the trend of points every year.
      # I added in labs to give the graph axes labels and a title and subtitle.
      
      nation_numbers %>%
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
    
    output$summer10Plot <- renderHighchart({
      
      # I took the athlete data set and filtered for the summer season in the year that I wanted. 
      # I also got rid of the na values in the medal column.
      # I grouped by name to get each individual athlete with their own set of data.
      # I then used summarise to count the number of medals that each athlete won.
      # I arranged by desc so that I could select the top 10 athletes using head.
      
      athlete <- athlete_events %>%
        filter(!is.na(Medal), Season =='Summer', Year == 2016) %>%
        group_by(Name) %>%
        summarize(medal = n()) %>% 
        arrange(desc(medal)) %>% 
        head(n = 10)
      
      # I then took the highchart function and made the height of the graph 800px.
      # I then used the labels command to put in the two variables I was plotting: name and medals.
      # I specified the colors I wanted the graph to use going from red with the top number of medals
      # to light yellow for the lowest. I also specified to make a pie chart and then 
      # adjusted the center values to make the graph a certain size and place (I experimented with
      # these values until I liked the numbers I chose.)
      
      highchart(height = "800px") %>% 
        hc_add_series_labels_values(athlete$Name, athlete$medal, 
                                    colors = substr(heat.colors(10), 0, 7),
                                    type = "pie", colorByPoint = TRUE, center = c('46%', '40%'))
    })
    
    output$sport10Plot <- renderHighchart({
      
      # I took the athlete data set and filtered for the summer season in the year that I wanted. 
      # I also got rid of the na values in the medal column.
      # I grouped by sport to get each individual sport with their own set of data.
      # I then used summarise to count the number of medals that each sport won.
      # I arranged by desc so that I could select the top 10 sports using head.
      
      Sport <- athlete_events %>%
        filter(!is.na(Medal), Season=='Summer', Year == 2016) %>%
        group_by(Sport) %>%
        summarize(medal=n()) %>%
        arrange(desc(medal)) %>%
        head(n=10)
      
      # I then took the highchart function and made the height of the graph 800px.
      # I then used the labels command to put in the two variables I was plotting: sport and medals.
      # I specified the colors I wanted the graph to use going from red with the top number of medals
      # to light yellow for the lowest. I also specified to make a pie chart and then 
      # adjusted the center values to make the graph a certain size and place (I experimented with
      # these values until I liked the numbers I chose.)
      
      highchart(height = "700px") %>% 
        hc_add_series_labels_values(Sport$Sport, Sport$medal, 
                                    colors = substr(heat.colors(10), 0 , 7),
                                    type = "pie", colorByPoint = TRUE, center = c('46%', '40%')) 
    })
    
    getPage <-function() {
  
      # Here I followed the command I found online for how to render an html in your server output.
      # I made a gt table in my rmd file and saved it as an html and then in this part of my code
      # all I had to go was render the output for this html.
      
      return(includeHTML("table.html"))}
    
    output$inc <- renderUI({getPage()})
    
}

# The last part I kept from the shiny base template in order to help it run and deploy.

shinyApp(ui = ui, server = server)
