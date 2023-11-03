#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(usmap)
library(rsconnect)
library(shinydashboard)

data <- read.csv("accident.csv")

# Define the order of weekdays
weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Convert 'DAY_WEEKNAME' column to a factor with specified order
data$DAY_WEEKNAME <- factor(data$DAY_WEEKNAME, levels = weekday_order)

Variables <- c("STATENAME", "DAY_WEEKNAME")
Month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November","December")

# Define UI for application that draws a histogram

# Define the UI for the Shiny app
ui <- dashboardPage(
    dashboardHeader(title = "Car Accidents"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Fatalities", tabName = "Fatalities"),
            menuItem("Accidents_by_Weather", tabName = "Accidents_by_Weather")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "Fatalities",
                fluidRow(
                    selectInput(inputId = "Variables", label = "Variable", choices = c("STATENAME", "DAY_WEEKNAME")),
                    selectInput(inputId = "Month", label = "Month", choices = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November","December")),
                    box(
                        title = "Plot 1",
                        plotOutput("Plot1")
                    ),
                    box(
                        title = "Plot 2",
                        plotOutput("Plot2")
                    )
                )
            ),
            tabItem(
                tabName = "Accidents_by_Weather",
                fluidRow(
                    selectizeInput("state_input", "Select State(s):", choices = unique(data$STATENAME), selected = "Alabama", multiple = TRUE),
                    selectizeInput("month_input", "Select Month(s):", choices = unique(data$MONTHNAME), selected = "January", multiple = TRUE),
                    plotOutput("weather_plot")
                )
            )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Plot1 <- renderPlot({     
        
        if (input$Variables == "STATENAME"){
            
            if (input$Month == "January"){
                
                # Filter the data for January
                january_data <- subset(data, Month == "January")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = january_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            else if (input$Month == "February"){
                
                # Filter the data for January
                february_data <- subset(data, Month == "February")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = february_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") + 
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            else if (input$Month == "March"){
                
                # Filter the data for January
                march_data <- subset(data, Month == "March")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = march_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            
            else if (input$Month == "April"){
                
                # Filter the data for January
                april_data <- subset(data, Month == "April")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = april_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}  
            
            
            else if (input$Month == "May"){
                
                # Filter the data for January
                may_data <- subset(data, Month == "May")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = may_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            
            else if (input$Month == "June"){
                
                # Filter the data for January
                june_data <- subset(data, Month == "June")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = june_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            
            else if (input$Month == "July"){
                
                
                # Filter the data for January
                july_data <- subset(data, Month == "July")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = july_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} 
            
            
            
            else if (input$Month == "August"){
                
                
                # Filter the data for January
                august_data <- subset(data, Month == "August")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = august_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} 
            
            
            else if (input$Month == "September"){
                
                # Filter the data for January
                september_data <- subset(data, Month == "September")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = september_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} 
            
            
            
            else if (input$Month == "October"){
                
                # Filter the data for January
                october_data <- subset(data, Month == "October")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = october_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            else if (input$Month == "November"){
                
                # Filter the data for January
                november_data <- subset(data, Month == "November")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = november_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}  
            
            
            else {
                
                # Filter the data for January
                december_data <- subset(data, Month == "December")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = december_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_state, aes(x = STATENAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by State", x = "State", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} }
        
        
        
        else {
            if (input$Month == "January"){
                
                # Filter the data for January
                january_data <- subset(data, Month == "January")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =january_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            else if (input$Month == "February"){
                
                # Filter the data for January
                february_data <- subset(data, Month == "February")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =february_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            else if (input$Month == "March"){
                
                # Filter the data for January
                march_data <- subset(data, Month == "March")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =march_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            
            else if (input$Month == "April"){
                
                april_data <- subset(data, Month == "April")
                # Filter the data for January
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =april_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}  
            
            
            else if (input$Month == "May"){
                
                # Filter the data for January
                may_data <- subset(data, Month == "May")                
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =may_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            
            else if (input$Month == "June"){
                
                # Filter the data for January
                june_data <- subset(data, Month == "June")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =june_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            
            else if (input$Month == "July"){
                
                
                # Filter the data for January
                july_data <- subset(data, Month == "July")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =july_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} 
            
            
            
            else if (input$Month == "August"){
                
                
                # Filter the data for January
                august_data <- subset(data, Month == "August")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =august_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} 
            
            
            else if (input$Month == "September"){
                
                # Filter the data for January
                september_data <- subset(data, Month == "September")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =september_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} 
            
            
            
            else if (input$Month == "October"){
                
                # Filter the data for January
                october_data <- subset(data, Month == "October")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =october_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}
            
            
            
            else if (input$Month == "November"){
                
                # Filter the data for January
                november_data <- subset(data, Month == "November")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =november_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))}  
            
            
            else {
                
                # Filter the data for December
                december_data <- subset(data, Month == "December")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =december_data, FUN = sum)
                # Create a bar plot using ggplot2
                ggplot(fatal_sum_day, aes(x = DAY_WEEKNAME, y = FATALS)) +
                    geom_bar(stat = "identity", fill = "hotpink4") +
                    labs(title = "Fatalities by Days", x = "Days", y = "Total Fatalities") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))} }
        
        
    }) 
    
    output$Plot2 <- renderPlot({ 
        
        if (input$Variables == "STATENAME"){
            
            if (input$Month == "January"){
                
                
                # Use the aggregate function to count accidents by STATE
                january_data <- subset(data, Month == "January")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = january_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = january_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
                
            }
            
            else if (input$Month == "February"){
                
                # Use the aggregate function to count accidents by STATE
                february_data <- subset(data, Month == "February")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = february_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = february_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
                
            }
            
            
            else if (input$Month == "March"){
                
                # Use the aggregate function to count accidents by STATE
                march_data <- subset(data, Month == "March")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = march_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = march_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
                
            }
            
            else if (input$Month == "April"){
                
                # Use the aggregate function to count accidents by STATE
                april_data <- subset(data, Month == "April")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = april_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = april_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }   
            
            else if (input$Month == "May"){     
                # Use the aggregate function to count accidents by STATE
                may_data <- subset(data, Month == "May")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = may_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = may_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }    
            
            else if (input$Month == "June"){   
                # Use the aggregate function to count accidents by STATE
                june_data <- subset(data, Month == "June")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = june_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = june_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }
            
            else if (input$Month == "July"){
                
                # Use the aggregate function to count accidents by STATE
                july_data <- subset(data, Month == "July")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = july_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = july_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }
            
            
            else if (input$Month == "August"){
                
                # Use the aggregate function to count accidents by STATE
                august_data <- subset(data, Month == "August")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = august_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = august_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }
            
            else if (input$Month == "September"){  
                
                # Use the aggregate function to count accidents by STATE
                september_data <- subset(data, Month == "September")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = september_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = september_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            } 
            
            
            else if (input$Month == "October"){ 
                
                # Use the aggregate function to count accidents by STATE
                october_data <- subset(data, Month == "October")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = october_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = october_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            } 
            
            
            else if (input$Month == "November"){
                
                # Use the aggregate function to count accidents by STATE
                november_data <- subset(data, Month == "November")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = november_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = november_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }
            
            
            else {
                
                # Use the aggregate function to count accidents by STATE
                december_data <- subset(data, Month == "December")
                fatal_sum_state <- aggregate(FATALS ~ STATENAME, data = december_data, FUN = sum)
                accidents_by_state <- aggregate(ST_CASE ~ STATENAME, data = december_data, FUN = length)
                
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_state)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_state, accidents_by_state, by = "STATENAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                result <- result %>%
                    rename(state = STATENAME)
                
                #Plot a density USA map for Death by Drug Overdose per Capita in Different States
                plot_usmap(data = result, values = "FATAL_RATE", include = c(), labels = TRUE, color = "black") +
                    scale_fill_continuous(low = "white", high = "hotpink4", name = "Fatality Rate", label = scales::comma) +
                    labs(title = "States of USA", subtitle = "Fatality Rate by States") +
                    theme(legend.position = "right")
            }
        }
    

        else {
            if (input$Month == "January"){
                # Filter the data for January
                january_data <- subset(data, Month == "January")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =january_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = january_data, FUN = length)
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            else if (input$Month == "February"){
                
                # Filter the data for January
                february_data <- subset(data, Month == "February")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =february_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = february_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            else if (input$Month == "March"){
                
                # Filter the data for January
                march_data <- subset(data, Month == "March")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =march_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = march_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            else if (input$Month == "April"){
                
                april_data <- subset(data, Month == "April")
                # Filter the data for January
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =april_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = april_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            else if (input$Month == "May"){
                
                # Filter the data for January
                may_data <- subset(data, Month == "May")                
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =may_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = may_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            
            else if (input$Month == "June"){
                
                # Filter the data for January
                june_data <- subset(data, Month == "June")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =june_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = june_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            else if (input$Month == "July"){
                
                
                # Filter the data for January
                july_data <- subset(data, Month == "July")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =july_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = july_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            else if (input$Month == "August"){
                
                
                # Filter the data for January
                august_data <- subset(data, Month == "August")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =august_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = august_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            else if (input$Month == "September"){
                
                # Filter the data for January
                september_data <- subset(data, Month == "September")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =september_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = september_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            
            else if (input$Month == "October"){
                
                # Filter the data for January
                october_data <- subset(data, Month == "October")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =october_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = october_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            
            
            else if (input$Month == "November"){
                
                # Filter the data for January
                november_data <- subset(data, Month == "November")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =november_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = november_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
            
            
            
            else {
                
                # Filter the data for December
                december_data <- subset(data, Month == "December")
                fatal_sum_day <- aggregate(FATALS ~ DAY_WEEKNAME, data =december_data, FUN = sum)
                accidents_by_day <- aggregate(ST_CASE ~ DAY_WEEKNAME, data = december_data, FUN = length)
                
                fatal_sum_day$percentage <- round((fatal_sum_day$FATALS/sum(fatal_sum_day$FATALS))*100)
                # Rename the ST_CASE column to ACCIDENTS for clarity
                names(accidents_by_day)[2] <- "ACCIDENTS"
                
                # Merge the two data frames by 'STATE' and calculate the fatality rate
                result <- merge(fatal_sum_day, accidents_by_day, by = "DAY_WEEKNAME") %>%
                    mutate(FATAL_RATE = FATALS / ACCIDENTS)
                
                # Create a pie chart for fatalities by DAY_WEEKNAME
                ggplot(result, aes(x = "", y = FATAL_RATE, fill = DAY_WEEKNAME)) +
                    geom_bar(width = 1, stat = "identity") +
                    geom_text(aes(label = paste0(percentage, "%")), position = position_stack(vjust = 0.5)) +  # Add percentage labels
                    coord_polar("y") +
                    labs(title = "Fatality Rate by Day of the Week",
                         fill = "Day of the Week") +
                    theme_minimal()}
            
        }
        
        
    })

    #Bar plot of weather
    filtered_data <- reactive({
        filter(data, STATENAME == input$state_input, MONTHNAME == input$month_input)
    })
    
    output$weather_plot <- renderPlot({
        filtered_data() %>%
            group_by(WEATHERNAME) %>%
            summarise(count = n()) %>%
            arrange(desc(count)) %>%
            ggplot(aes(x = reorder(WEATHERNAME, -count), y = count)) +
            geom_bar(stat = "identity", fill = "hotpink4") +
            labs(x = "Weather", y = "Average Number of Accidents") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)

