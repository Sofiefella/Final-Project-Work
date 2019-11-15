
library(shiny)
library(tidyverse)
library(janitor)
library(lubridate)
library(plotly)
library(gt)
library(shinythemes)

all_data <- left_join(athlete_events, GDP, by = c("Team"))
winter_medals <- read_csv("winter.csv")
summer_medals <- read_csv("summer.csv")


ui <- fluidPage(theme = shinytheme("superhero"),
                
                br(),
                
                navbarPage("The History of the Olympic Games",
                           
                           tabPanel("Olympic Characteristics",
                                    tabsetPanel(
                                        tabPanel("Winter Games"),
                                        tabPanel("Summer Games"),
                                        tabPanel("Location")
                                        
                                        )),
                           tabPanel("Athlete Characteristics",
                                    tabsetPanel(
                                        tabPanel("Gender"),
                                        tabPanel("Age"),
                                        tabPanel("Medals")
                                        
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


    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
