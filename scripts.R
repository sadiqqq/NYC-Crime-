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
# shorten and limit both manhattan and dummy datasets to after 2006
dummy_06<- filter(dummy, date >= "2006-01-01")
manhattan_perday06<- filter(manhattan_perday, date >= "2006-01-01" )

# make Manhattan data(6000) compatible with main data that has more that 7000 entries
dummy$nprime <- ifelse(dummy$date %in% manhattan_perday$date,manhattan_perday$n,0)
# now my variables for regression are nprimee counting of dauily crime in Manhattan since 2006 & Newdummyconditional( which is 1 when an incident occured) 