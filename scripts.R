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

#d <- seq(as.Date('2014-10-01'), as.Date('2014-10-9'), by=1) for future reference

#fixed_effects<- data.frame(date = dummy_06$date)
#fixed_effects<-fixed_effects%>% mutate(totalpopulation = 1)


# intrapolating linearly and extrapolating total population year by year
fixed_effects$totalpopulation<-ifelse(grepl("2006", fixed_effects$date), 1566403,fixed_effects$totalpopulation
# 1566403+4868 yearly increase from 2006 to 2010.....

# white population variable created
fixed_effects<- mutate(fixed_effects, whitepop= 1 )

# I found a more convenient way ; [ I omit last three rows for being able to use rep, seq functions]
fixed_effects<- fixed_effects[-(5111:5113),]

# First made a sequence of 14 numbers using 2006 and 20019 values and yearly step
a<-seq(632317,1052144, by=32294)
#then using rep function fill in the white pop variable
fixed_effects$whitepop<-rep(a,each = 365)






