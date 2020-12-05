# filter main data for Manhattn
manhattan1<- filter(crime_data, BORO_NM == "MANHATTAN")
# counting the number of observations
manhattan1 %>% add_count(CMPLNT_FR_DT)
# using chron library for Date variable, 
library(chron)
# ex: as.Date('1915-6-16') [1] "1915-06-16"
# counting the number of crimes per day
manhattn_perday<- count(manhattan1, CMPLNT_FR_DT)