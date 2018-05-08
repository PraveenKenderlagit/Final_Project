
library(cdcfluview)
library(tidyverse)


## Making plots for each season per all years: 

Data1 <- ilinet() # extracting the influenza data from CDC website

S1 <- Data1 %>%  # filtering the main data file based on weeks.
  filter(year > 2000 & year < 2018, week <= 13 & week >0) %>%  
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)
S2 <- Data1 %>%
  filter(year > 2000 & year < 2018, week <= 13 +13 & week >0+13) %>%
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)
S3 <- Data1 %>% 
  filter(year > 2000 & year < 2018, week <= 13 +13 + 13 & week >0+13 +13) %>% 
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)

S4 <- Data1 %>%
  filter(year > 2000 & year < 2018, week <= 13 +13 +13+13 & week >0+13 +13+13) %>%  
  select(region, year, week, age_0_4,age_5_24, age_25_64,age_65)

# Season 1: Influenza cases reported for age group and year for first season: weeK:
S1S <- summarise(group_by(S1, year), sum(age_0_4), sum(age_25_64), sum(age_5_24), sum(age_65)) # summing the data for each year in season 1

colnames(S1S)[2:5] <- c('age_0_4','age_25_64', 'age_5_24', 'age_65')

S1SS <- S1S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S1SSPlot <- ggplot(S1SS, aes(x = year, y = cases, group = Age, color = Age)) + 
            geom_point() + geom_line() +
            labs(x = "Year", y = "Influenxa cases", title = "Total number of influenza cases for season 1 per age group")

S1SSPlot

# Season 2:Influenza cases reported for age group and year for second season: weeK:
S2S <- summarise(group_by(S2, year), sum(age_0_4), sum(age_25_64), sum(age_5_24), sum(age_65)) # summing the data for each year in season 2

colnames(S2S)[2:5] <- c('age_0_4','age_25_64', 'age_5_24', 'age_65')

S2SS <- S2S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S2SSPlot <- ggplot(S2SS, aes(x = year, y = cases, group = Age, color = Age)) + 
            geom_point() + geom_line() +
            labs(x = "Year", y = "Influenxa cases", title = "Total number of influenza cases for season 2 per age group")

S2SSPlot

# Season 3:Influenza cases reported for age group and year for third season: weeK:
S3S <- summarise(group_by(S3, year), sum(age_0_4), sum(age_25_64), sum(age_5_24), sum(age_65)) # summing the data for each year in season 1

colnames(S3S)[2:5] <- c('age_0_4','age_25_64', 'age_5_24', 'age_65')

S3SS <- S3S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S3SSPlot <- ggplot(S3SS, aes(x = year, y = cases, group = Age, color = Age)) + 
            geom_point() + geom_line() +
            labs(x = "Year", y = "Influenxa cases", title = "Total number of influenza cases for season 3 per age group")

S3SSPlot


# Season 4: Influenza cases reported for age group and year for last season: weeK:
S4S <- summarise(group_by(S4, year), sum(age_0_4), sum(age_25_64), sum(age_5_24), sum(age_65)) # summing the data for each year in season 1

colnames(S4S)[2:5] <- c('age_0_4','age_25_64', 'age_5_24', 'age_65')

S4SS <- S4S %>%  gather (c(age_0_4, age_5_24, age_25_64, age_65), key = Age, value = cases, na.rm = T)

S4SSPlot <- ggplot(S4SS, aes(x = year, y = cases, group = Age, color = Age)) + 
            geom_point() + geom_line() +
            labs(x = "Year", y = "Influenxa cases", title = "Total number of influenza cases for season 4 per age group")

S4SSPlot

multiplot(S1SSPlot, S2SSPlot, S3SSPlot, S4SSPlot, cols = 2) # Extracted from this: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/


# What do I have: Age groups across different seasons;

## Illness reported for national per year;

timeseries <- Data1 %>% select(year, week, ilitotal) %>% filter(year >2000)
timesum <- summarise(group_by(Data1, year), sum(ilitotal))

colnames(timesum)[2] <- 'ilitotal'

a <- ggplot(timesum, aes(year, ilitotal)) + geom_point() + geom_line() +
        labs(x = "Year", y = "Total number of influnza cases reported", title = "Total influnza cases reported per year")
a

b <- ggplot(timeseries, aes(week, ilitotal))+ 
      geom_point(aes(color = year)) + 
      xlab("Weeks") + ylab("Total number people ill with influenza") + 
      ggtitle("Total number of cases reported per week for 2001 - 2018")
b

##  Age groups and weeks;


AgeandWeekData <- Data1 %>% filter (year >2000) %>%  select(year, week, age_0_4, age_5_24, age_25_64,age_65)

A <- AgeandWeekData %>%  gather (c(age_0_4,age_5_24,age_25_64,age_65), key = Age, value = cases, na.rm = T) # even this doesn't work

#A1 <- summarise(group_by(AgeandWeekData, year), sum(age_0_4), sum(age_5_24), sum(age_25_64), sum(age_65))

#colnames(A)[2:5] <- c('age_0_4', 'age_5_24', 'age_25_64', 'age_65')

A2 <- summarise(group_by(A, Age, year), sum(cases))

colnames(A2)[3] <- 'cases'

A3 <- ggplot(A2, aes(x = year, y = cases, group = Age, color = Age)) + 
      geom_point() + geom_line() + 
      labs(x = "Year", y = "Cases", title = "Total number of influenza cases per Age group and as a function of Year")
A3

## Age data as a function of Weeks;

Weekdata <- summarise(group_by(AgeandWeekData, week), sum(age_0_4), sum(age_5_24), sum(age_65))

colnames(Weekdata)[2:4] <- c('age_0_4', 'age_5_24', 'age_65')

W1 <- Weekdata %>% gather(c(age_0_4,age_5_24, age_65), key = Age, value = cases, na.rm = T)

W2 <- ggplot(W1, aes(x = week, y = cases, grou = Age, color = Age)) + 
      geom_point() + geom_line() +
      labs(x = "Weeks", y = "influenza Cases", title = "Total Number of Influenza cases per age group for each week")
W2

# multiplot(A3,W2)

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

Mortality_Overall_cases_plot <- ggplot(z, aes(x = year, y = cases, group = Illness, color = Illness)) + geom_point() + geom_line() + 
                                labs(x = "Year", y = "Cases", title = "Total Cases reported and Mortality deadths")

Mortality_Overall_cases_plot

#Plot a time series graph for overall influenza cases for National average.
#Evaluate the total number of cases reported across the age groups and weeks. 
# and compare them using t - statistics. 
#Find the correlation between the pediatric death and number of cases reported in that age range. 


#I have a state data, can I use it to do the analysis considering each state as a data point (number of cases reported) and compare to the level of spread?  
  
#  If you want me to do the text analysis of articles published in new york times for specific season (I can use chunk of 3 months as a season) and its correlation with the spread of Flu, I can do that. 
#



