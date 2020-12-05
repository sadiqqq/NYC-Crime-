# filter main data for Manhattn
manhattan1<- filter(crime_data, BORO_NM == "MANHATTAN")
# counting the number of observations
manhattan1 %>% add_count(CMPLNT_FR_DT)
# using chron library for Date variable, 
library(chron)
# ex: as.Date('1915-6-16') [1] "1915-06-16"
# counting the number of crimes per day
manhattan_perday<- count(manhattan1, CMPLNT_FR_DT)
# change per day counting days to Date format and order them
manhattan_perday<-manhattan_perday%>% mutate(date = as.Date(CMPLNT_FR_DT, format = '%m/%d/%Y')) %>% arrange(date)
# incident dummy source into r 
library("readxl")
dummy<- read.csv("Book1.csv")
# make Manhattan data(6000) compatible with main data that has more that 7000 entries
dummy$nprime <- ifelse(dummy$date %in% manhattan_perday$date,manhattan_perday$n,0)