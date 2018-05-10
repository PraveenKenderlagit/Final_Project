
library(cdcfluview)
library(tidyverse)

## Code to use later in Shiny:

Data1 <- ilinet() # extracting the influenza data from CDC website

S1 <- Data1 %>%  # filtering the main data file based on weeks.
  filter(year > 2000, week <= 13 & week >0) %>%  
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)
S2 <- Data1 %>%
  filter(year > 2000, week <= 13 +13 & week >0+13) %>%
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)
S3 <- Data1 %>% 
  filter(year > 2000, week <= 13 +13 + 13 & week >0+13 +13) %>% 
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)

S4 <- Data1 %>%
  filter(year > 2000, week <= 13 +13 +13+13 & week >0+13 +13+13) %>%  
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)

# Season 1: Influenza cases reported for age group and year for first season: weeK:
S1S <- summarise(group_by(S1, year), sum(age_0_4), sum(age_5_24), sum(age_25_64), sum(age_65)) # summing the data for each year in season 1

colnames(S1S)[2:5] <- c('age_0_4', 'age_5_24', 'age_25_64', 'age_65')

S1SS <- S1S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S2S <- summarise(group_by(S2, year), sum(age_0_4), sum(age_5_24), sum(age_25_64), sum(age_65)) # summing the data for each year in season 2

colnames(S2S)[2:5] <- c('age_0_4', 'age_5_24', 'age_25_64', 'age_65')

S2SS <- S2S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S3S <- summarise(group_by(S3, year), sum(age_0_4), sum(age_5_24), sum(age_25_64), sum(age_65)) # summing the data for each year in season 1

colnames(S3S)[2:5] <- c('age_0_4', 'age_5_24', 'age_25_64','age_65')

S3SS <- S3S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S4S <- summarise(group_by(S4, year), sum(age_0_4), sum(age_5_24), sum(age_25_64), sum(age_65)) # summing the data for each year in season 1

colnames(S4S)[2:5] <- c('age_0_4', 'age_5_24', 'age_25_64', 'age_65')

S4SS <- S4S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

# For Mortality plots: 

library(lubridate)

Mortalitydata <- pi_mortality()

MD <- arrange(Mortalitydata, year(Mortalitydata$wk_start))

attach(MD)

D2 <- subset(Data1, year > 2008) # subsetting the Data1 to make equivalent and corresponding columns for same weeks in mortality data.
D3 <- D2[40:(nrow(D2)-2),] # Mortality data has not yet updated so, to make the equvalancy between the two data files, the last 2 weeks data is removed.

MDD3 <- cbind(year(wk_start), year(wk_end), year_wk_num, number_influenza, number_pneumonia, D3$ilitotal) # combined datafile;

colnames(MDD3) <- c("year", "wk_end","week" , "Influenza", "Pneumonia", "totalillness")

MDD3 <- as.tibble(MDD3)

SumMDD3 <- summarise(group_by(MDD3, year), sum(Influenza), sum(Pneumonia), sum(totalillness))

colnames(SumMDD3) <- c("year", "Influenza", "Pneumonia", "totalillness")

z <- SumMDD3 %>%  gather (c(Influenza, Pneumonia, totalillness), key = Illness, value = cases, na.rm = T)


## Making All Seasons for plots in Shiny:

Seas1 <- S1S %>%  mutate(Season = "Season1")
Seas2 <- S2S %>%  mutate(Season = "Season2")
Seas3 <- S3S %>%  mutate(Season = "Season3")
Seas4 <- S4S %>%  mutate(Season = "Season4")

AllSeasons1 <- rbind(Seas1, Seas2, Seas3, Seas4)

AllSeasons <- AllSeasons1 %>% gather (c(age_0_4:age_65), key = Age, value = cases, na.rm = T)

## Making AWY beforehand for plots in Shiny: 

AWY <- Data1 %>%  filter (year > 2000) %>% select(year, week, ilitotal, age_0_4, age_5_24, age_25_64, age_65) # selects total reports, year, week, and age group cases reproted

AWY1 <- AWY %>%  gather(c(age_0_4:age_65), key = Age, value = Cases, na.rm = T) # groups according to age, but can be used to display total reprots too


## Making TCPYA1 for plots in Shiny:

# AWY <- Data1 %>%  filter (year > 2000) %>% select(year, week, ilitotal, age_0_4, age_5_24, age_25_64, age_65) # selects total reports, year, week, and age group cases reproted

TCPYA <- summarise(group_by(AWY, year), sum(ilitotal), sum(age_0_4),sum (age_5_24), sum(age_25_64), sum(age_65)) # Total Cases Per Year and Age

colnames(TCPYA)[2:6] <- c("ilitotal", "age_0_4", "age_5_24", "age+25_64", "age_65")

TCPYA1 <- TCPYA %>%  gather(c(age_0_4:age_65), key = Age, value = Cases, na.rm = T)


# Choice Names used in Shiny:

Ages <- c("age_0_4", "age_5_24", "age_25_64", "age_65")
Season <- c("Season1", "Season2", "Season3","Season4")
Years <- c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
Illness <- c("Influenza", "Pneumonia")

library(shiny)
library(shinydashboard)


ui <- fluidPage(
  
  titlePanel("CAS MA 615: Final Project: Spread of Influenza"),
  
  navlistPanel(
    "Influenza Reports and Mortality rates",
    tabPanel("Influenza Reports for Seasons and Age group",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(box(selectInput("age", label = "Choose age group", choice = Ages, selected = "age_0_4"))),
                 
                 fluidRow(selectInput("Season", choices = Season, label = "Choose Seasons"))
                 
               ),
               mainPanel(
                 plotOutput("plot1"),
                 plotOutput("plot2")
                 
               )
             )
    ),
    tabPanel("Influenza Reports for each Season and each Age group",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(box(selectInput("age2", label = "Choose age group", choices = Ages, selected = "age_0_4"))),
                 fluidRow(box(selectInput("Seasons", choices = Season, label = "Choose Season")))
               ),
               
               mainPanel(
                 plotOutput("plot4")
               ) 
               
             )
    ),
    tabPanel("Total Influenza cases reported for all Weeks for each year and Age group",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(box(selectInput("age3", label = "Choose age group", choice = Ages, selected = "age_0_4"))),
                 sliderInput("Year", "Years", 2001, 2018, 2018)
                 
               ),
               mainPanel(
                 plotOutput("plot9"),
                 plotOutput("plot5")
                 
               )
             )
    ),
    tabPanel("Total Influenza cases reported for all years for each week and Age group",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(box(selectInput("age4", label = "Choose age group", choice = Ages, selected = "age_0_4"))),
                 sliderInput("Week", "Weeks", 1,52,10)
                 
               ),
               mainPanel(
                 plotOutput("plot6"),
                 plotOutput("plot7")
               )
             )
    ),
    
    tabPanel("Morality rates due to Influenza and Pneumonia",
             sidebarLayout(
               sidebarPanel(
                 fluidRow(box(selectInput("illness", label = "Choose Illness", choice = Illness, selected = "Influenza")))
               ),
               mainPanel(
                 plotOutput("plot8")
               )
             
             )
    )
  )
)

server <- function (input, output){
  
  
    output$plot1 <- renderPlot({
      
            
    
      Ndata <- AllSeasons %>% filter(Age == input$age)
    
      B <- ggplot(Ndata, aes(x = year, y = cases, color = Season, shape = Season)) + geom_point() + geom_line()+
            labs(x = "Year", y = "Influenza Cases Reported", title = "Influenza Cases Reports for All Seasons as a function of Age group")
      B
    
    })
  
    output$plot2 <- renderPlot({
    
      Seasondata <- AllSeasons %>% filter (Season == input$Season)
    
      C <- ggplot(Seasondata, aes(x = year, y = cases, color = Age, shape = Season)) + geom_point() + geom_line() +
            labs(x = "Year", y = "Influenza Cases Reported", title = "Influenza Cases Reports for All Age groups as a function of Season")
    
      C
    
    })
    
    output$plot4 <- renderPlot({
      
      SAdata <- AllSeasons %>%  filter (Season == input$Seasons, Age == input$age2)
      
      D <- ggplot(SAdata, aes(x = year, y = cases, color = Age, shape = Season)) + geom_point() + geom_line() + 
        labs(x = "Year", y = "Influza Reported Cases", title = "Influenza Cases Reported for each Age Group and each Season")
      
      D
    })  
    
    output$plot5 <- renderPlot({
      
            
      AgeWeekYeardata <- AWY1 %>%  filter (Age == input$age3, year == input$Year) # provides single year, single age group data at a time
      
      E <- ggplot(AgeWeekYeardata, aes(x = week, color = Age)) + 
        geom_point(aes(y = ilitotal), color = "black") + geom_line(aes(y = ilitotal), color = "black") + 
        geom_point(aes(y = Cases)) + geom_line(aes(y = Cases)) +
        labs(x = "Weeks", y = "Cases", title = "Total Cases Reported per Week as a function of each Year and each Age Group")
      E
      
    })
    
    
    output$plot9 <- renderPlot({
      
      AWY2 <- summarise(group_by(AWY, week), sum(ilitotal), sum(age_0_4), sum(age_5_24), sum(age_25_64), sum(age_65))
     
       colnames(AWY2)[2:6] <- c("ilitotal", "age_0_4", "age_5_24", "age+25_64", "age_65")
      
       TotalCasesPerWeekforAgeGroup <- AWY2 %>%  gather(c(age_0_4:age_65), key = Age, value = Cases, na.rm = T)
      
       Total_Week_Age <- TotalCasesPerWeekforAgeGroup %>%  filter(Age == input$age3)
      
      J <- ggplot(Total_Week_Age, aes(x = week, color = Age)) + geom_point(aes(y = Cases)) + geom_line(aes(y = Cases)) +
            geom_point(aes(y = ilitotal), color = 'black') + geom_line(aes(y = ilitotal), color = 'black') + 
              labs(x = "WEEKS", y = "Total Influenza Cases Reported", title = "Total Influenza Cases Reported Per week for each age group")
              
      J
      
    })
    
    output$plot6 <- renderPlot({
      
      
      TotalCasesYearAge <- TCPYA1 %>%  filter (Age == input$age4)
      
      F <- ggplot(TotalCasesYearAge, aes(x = year,color = Age)) + 
        geom_point(aes( y = Cases)) + geom_line(aes(y = Cases)) +
        geom_point(aes(y = ilitotal)) + geom_line(aes(y = ilitotal), color = "black") + 
        labs(x = "Year", y = "Cases reported", title = "Total Influenza Cases Reported for All age groups per Year")
      
      F
      
    })
    
     
    output$plot7 <- renderPlot({
      
      AgenWeek <- Data1 %>% filter (year >2000) %>%  select(year, week, age_0_4, age_5_24, age_25_64,age_65, ilitotal)
      
      AW1 <- summarise(group_by(AgenWeek, year), sum(age_0_4), sum(age_5_24), sum(age_65), sum(ilitotal))
      
      AW2 <- AgenWeek %>%  gather (c(age_0_4:age_65), key = Age, value = Cases, na.rm = T) # Week data per Year for Age
      
      AW3 <- AW2 %>%  filter (Age == input$age4, week == input$Week)
      
      G <- ggplot(AW3, aes(year, Cases, color = Age)) + geom_point()+ geom_line() + 
            labs(x = "Year", y = "Influenza Cases reported", title = "Influenza Cases Reported for each Year as a function of each Week and Age group")
      G
      
    })
    
    
    output$plot8 <- renderPlot({
      
      MortalityRates <- z %>%  filter (Illness == input$illness)
      
      H <- ggplot(MortalityRates, aes(year, cases, color = Illness)) + geom_point() + geom_line() + 
            labs(x = "Year", y = "Deadths", title = "Total Deadths due to Infleunza, Pneumonia")
      H
      
    })
      
    
}

shinyApp(ui,server)
  

