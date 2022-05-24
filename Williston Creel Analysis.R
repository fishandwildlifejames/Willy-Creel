###Williston Creel 2021 Data Analysis#####
### Created by James Morgan, with the spritual and mental support of Kristen Peck and other past Fish Bios###

###INSTALL PACKAGES###
install.packages("readxl")
install.packages("ggplot2")
install.packages("CRAN")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("plotrix")
library(readxl)
library(ggplot2)
library(CRAN)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)

library(plotrix)
###Read in XL file, create Month column, ####
data <-read_excel("//Sfp.idir.bcgov/s140/S40023/Environmental Stewardship/Fish/Funding/FFSBC/2021-2022/WillistonCreel/Filemaker Files/Williston Creel Data Combined.xlsx")

data$SurveyDate <- as.Date(data$SurveyDate) 
data$Month <- months(data$SurveyDate)

###Create effort table- calculate rod hours###

effort <-
  data %>%
  mutate(Rodhours = as.numeric(Totalfishingtime) * Rods)
  Totalrodhours <- sum(effort$Rodhours, na.rm=TRUE)
  
###Interviews by Stratum#####
effort %>%
  count(Statum, na.rm=TRUE)

WD_Anal <- effort[grepl('WD', effort$Statum), ] 
  Totalrodhours_WD <- sum(WD_Anal$Rodhours, na.rm=TRUE)
  
WD_Analsumm <- WD_Anal %>% 
  group_by(Month) %>%
 summarise(total_Anglers_month = sum(Rods)) %>% 
  mutate(Survey_Days = c(2, 3, 3, 2)) %>% 
mutate(Avg_Anglers_WD = total_Anglers_month / Survey_Days) %>% 
  mutate(SD_Avg_Anglers = sd(total_Anglers_month / Survey_Days), na.rm = TRUE) %>% 
   mutate(non_survey_days = c(20, 19, 20, 20)) %>% 
  mutate(extrap_WD = non_survey_days * Avg_Anglers_WD) %>% 




ggplot(data = WD_Analsumm) +
  geom_bar(stat = 'identity', mapping = aes(x= Month, y = extrap_WD))

  
WE_Anal <- effort[grepl('WE', effort$Statum), ]

  
Totalrodhours_WE <- sum(WE_Anal$Rodhours, na.rm=TRUE) 
  
 WE_Analsumm <- WE_Anal %>% 
    group_by(Month) %>% 
    summarise(total_Anglers_month = sum((Rods), na.rm=TRUE)) %>%   
    mutate(Survey_Days = c(4, 3, 5, 3, 3, 0)) %>% 
    mutate(Avg_Anglers_WE = total_Anglers_month / Survey_Days) %>% 
  mutate(non_survey_days = c(4, 6, 3, 6, 4, 0)) %>% 
    mutate(extrap_WE = non_survey_days * Avg_Anglers_WE)

#### ANGLER DAY CALCULATION - WEEKDAY####
  
  
Total_anglers_WD <- sum(WD_Analsumm$extrap_WD)

Average_Fishing_Trip_WD <- Totalrodhours_WD / Total_anglers_WD

Anglersbymonth <- data %>%
    drop_na(Month) %>%
  group_by(Month) %>%
  summarise(
    count = n(), 
    
  )

Anglersbylaunch <- data %>%
  drop_na(Statum) %>%
  group_by(BoatLaunch) %>%
  summarise(
    count = n()
  )
Anglersbyzone <- data %>% 
  drop_na(Zone) %>% 
  group_by(Zone) %>% 
  summarize(
    count = n()
  )
ggplot(data = Anglersbyzone) +
  geom_bar(stat='identity', mapping = aes(x = Zone, y = count, fill = count))

Anglersbystatum <- data %>%
  drop_na(Statum) %>%
  group_by(Statum) %>%
  summarise(
    count = n()
  )
ggplot(data = Anglersbymonth) +
  geom_bar(stat='identity', mapping = aes(x = Month, y = count, fill = count)) +
  scale_x_discrete(limits = month.name)

ggplot(data = Anglersbylaunch) +
  geom_bar(stat = 'identity', mapping = aes(x = BoatLaunch, y = count, fill = count))

ggplot(data = Anglersbystatum) +
  geom_bar(stat = 'identity', mapping = aes(x = Statum, y = count, fill = count))

totaleffortsumm <- merge(WD_Analsumm, WE_Analsumm,by="Month") %>% 
  mutate(totalmontheffort = extrap_WD + extrap_WE)

ggplot(data = totaleffortsumm) +
  geom_bar(stat='identity', mapping = aes(x = Month, y = totalmontheffort, fill = totalmontheffort))
  

### SAME BUT FOR WEEEKEND####

Total_anglers_WE <- sum(WE_Analsumm$extrap_WE, na.rm=TRUE)

Average_Fishing_Trip_WE <- Totalrodhours_WE / Total_anglers_WE

### 5715 total rod hours per stratum

