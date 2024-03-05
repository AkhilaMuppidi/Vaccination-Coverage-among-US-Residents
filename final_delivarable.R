#import and read the data set
d <- read.csv("C:/Users/akhil/Downloads/Vaccination_Coverage_among_Pregnant_Women.csv")
View(d)

#look at the first few rows of the data set
head(d)

#look at the last few rows of the data set
tail(d)

#look at the column names of the data set
colnames(d)

#look at the summary of the data set
summary(d)

#Pull the data of any state from influenza/tdap vaccine, here we took Alaska
#no need to create a subset of influenza vaccination in Alaska as there is no data for Tdap in Alaska 
alaska <- subset(d, Geography == "Alaska")
alaska
View(alaska)
str(alaska)

#Our data has non - numericals, so in order to eliminate those rows, we used the conditional !=
alaska_omit_na <- subset(alaska, Estimate....!="NR*")
alaska_omit_na
View(alaska_omit_na)
str(alaska_omit_na)


#plot to see the estimate of vaccination during influenza season of Hispanic people in Alaska
##creating a subset of estimated vaccine percentage of Hispanic in Alaska during influenza season
hispanic_alaska <- subset(alaska_omit_na, Dimension=="Hispanic")
hispanic_alaska
View(hispanic_alaska)

##to do the plotting we need to arrange the survey year in ascending/descending order, we chose ascending order
###we chose order function to arrange in ascending order and sorted
survey_year_asc <- order(hispanic_alaska$Survey.Year.Influenza.Season)
survey_year_asc


hispanic_alaska[survey_year_asc,]
  
x = hispanic_alaska[survey_year_asc, ]$Survey.Year.Influenza.Season
y = hispanic_alaska$Estimate....
plot(x, y, xlab = "Influenza Season(year)", ylab = "Estimated Vaccine Coverage(%)",
     main = "Influenza Vaccine Coverage in Alaska Among Hispanics(2013 - 2020)", 
     type = "o", pch = 20, lwd = 2, col = "blue")

#creating a bar diagram of 5 states with vaccine estimate in a year.
#Virginia, Washington, Pennsylvania, Delaware, Michigan are the states we chose

#Creating a subset of Tdap vaccine
tdap_vaccine <- subset(d, Vaccine == "Tdap")
tdap_vaccine

#summary of tdap vaccine
summary(tdap_vaccine)

#creating a subset of tdap vaccines in Virginia in 2020 of white, non- Hispanic 
##We used "&" conditional to group the rows
tdap_2020_virginia <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" & 
                               Geography == "Virginia" & Dimension == "White, Non-Hispanic")
tdap_2020_virginia
View(tdap_2020_virginia)


#creating a subset of tdap vaccines in Michigan in 2020 of white, non-Hispanic
tdap_2020_michigan <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                               & Geography == "Michigan" & Dimension == "White, Non-Hispanic")
tdap_2020_michigan
View(tdap_2020_michigan)

#creating a subset of tdap vaccines in Pennsylvania in 2020 of white, non-Hispanic
tdap_2020_pennsylvania <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                             & Geography == "Pennsylvania" & Dimension == "White, Non-Hispanic")
tdap_2020_pennsylvania
View(tdap_2020_pennsylvania)

#creating a subset of tdap vaccines in Washington in 2020 of white, non-Hispanic
tdap_2020_washington <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                                 & Geography == "Washington" & Dimension == "White, Non-Hispanic")
tdap_2020_washington
View(tdap_2020_washington)

#creating a subset of tdap vaccines in Delaware in 2020 of white, non-Hispanic
tdap_2020_delaware <- subset(tdap_vaccine, Survey.Year.Influenza.Season == "2020" 
                               & Geography == "Delaware" & Dimension == "White, Non-Hispanic")
tdap_2020_delaware
View(tdap_2020_delaware)

#now, create a bar plot of vaccination % in Virginia, Michigan, Pennsylvania, Washington, Delaware
##we used  as.numeric function to avoid binary error
data <- c(as.numeric(tdap_2020_virginia$Estimate....), as.numeric(tdap_2020_michigan$Estimate....), 
          as.numeric(tdap_2020_pennsylvania$Estimate....), as.numeric(tdap_2020_washington$Estimate....),
          as.numeric(tdap_2020_delaware$Estimate....))
states <- c("Virginia", "Michigan", "Pennsylvania", "Washington", "Delaware")

barplot(data, xlab = "States", ylab = "Vaccination %", main = "Estimated Tdap Vaccine Coverage of 
White, Non-Hispanics in 2020 among 5 States", names.arg = states, col = "purple", border = "red",
ylim = c(0,100))

#creating a pie chart of estimated vaccine % from 2015-2020 of black, non-Hispanic people in United States

##Creating a subset of Tdap vaccine
tdap_vaccine <- subset(d, Vaccine == "Tdap")
tdap_vaccine
View(tdap_vaccine)


##creating a subset of vaccinated tdap % of black, non-Hispanic people in United States in 2016
tdap_2016_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2016"
                       & Dimension == "Black, Non-Hispanic")
tdap_2016_us


##creating a subset of vaccinated tdap % of black, non-Hispanic people in United States in 2017
tdap_2017_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2017"
                       & Dimension == "Black, Non-Hispanic")
tdap_2017_us

##creating a subset of vaccinated tdap % of black, non-Hispanic people in United States in 2018
tdap_2018_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2018"
                       & Dimension == "Black, Non-Hispanic")
tdap_2018_us


##creating a subset of vaccinated tdap % of black, non-Hispanic people in United States in 2019
tdap_2019_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2019"
                       & Dimension == "Black, Non-Hispanic")
tdap_2019_us


##creating a subset of vaccinated tdap % of black, non-Hispanic people in united states in 2020
tdap_2020_us <- subset(tdap_vaccine, Geography == "United States" & Survey.Year.Influenza.Season == "2020"
                       & Dimension == "Black, Non-Hispanic")
tdap_2020_us

values <- c(as.numeric(tdap_2016_us$Estimate....), as.numeric(tdap_2017_us$Estimate....), 
            as.numeric(tdap_2018_us$Estimate....), as.numeric(tdap_2019_us$Estimate....), 
            as.numeric(tdap_2020_us$Estimate....))
years <- c("2016", "2017", "2018", "2019", "2020")
pct= round(values/sum(values)*100)
pct
new_labels <- paste(years, "-", pct, "%", sep="")
pie(values, labels = new_labels, main = "Tdap Vaccination % of Black, Non-Hispanics in United States from
    2016-2020", col = rainbow(5))

library(plotrix)

pie3D(values, labels = new_labels, main = "Tdap Vaccination % of Black, Non-Hispanics in United States from
    2016-2020", col = rainbow(5),radius = 1, explode = 0.0)

