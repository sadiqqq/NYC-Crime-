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

# assigning black population variable and using two seqences representingg two trends before and after 2010 in black population

fixed_effects<- mutate(fixed_effects, blackpop= 1 )
a<- c(seq(217070,205340, by = -2932),seq(214736,289909, by = 9396))
fixed_effects$blackpop<-rep(a,each = 365)

# the same repeated for hispanic origins
fixed_effects<- mutate(fixed_effects, hispanicpop =1)
a<- c(seq(409272,403577, by = -1423),seq(405062,416949, by = 1485))
fixed_effects$hispanicpop<-rep(a,each = 365)

# web scraping
man_census<-read_html(url("https://www.census.gov/quickfacts/newyorkcountymanhattanboroughnewyork","rb"))
highhtml<-html_nodes(man_census,"span")
hightitle<-html_text(highhtml)
valuehtml<- html_nodes(man_census,"td")
> highvalue<-html_text(valuehtml)

# High grad & college grad & povertyrate& median incomevariables created
fixed_effects<- mutate(fixed_effects, highgrad=1)
fixed_effects$highgrad<- rep(c(0.846,0.846,0.846,0.846,0.846,0.849,0.846,0.85,0.855,0.86,0.87,0.87,0.87,0.87), each = 365)
fixed_effects$collegegrad<- rep(c(0.57,0.57,0.57,0.57,0.57,0.577,0.57,0.57,0.581,0.589,0.608,0.608,0.608,0.608), each = 365)
fixed_effects$povertyrate<- rep(c(0.186,0.188,0.188,0.177,0.169,0.166,0.178,0.176,0.175,0.177,0.141,0.141,0.141,0.141), each = 365)
fixed_effects$medianincome<- rep(c(43573,45290,45290,63704,68402,68295,64971,67204,68370,69659,82459,82459,82459,82459), each = 365)


manreg<-lm( dummy_06$nprime ~ dummy_06$NewdummyConditional + fixed_effects$totalpopulation+fixed_effects$whitepop+fixed_effects$blackpop+fixed_effects$hispanicpop+ fixed_effects$highgrad+fixed_effects$collegegrad+fixed_effects$totalpopulation+fixed_effects$povertyrate+fixed_effects$medianincome)
summary(manreg) # there would be a problem!!!! I will go for poisson regression





# I restricted my scope to 2014-2018. First create a dummmy for brutality intense months

citywidedummy<-append(citywidedummy,h), h<- rep(0,25)

# make manhattan fixed effects short and compatible with citywide dummy
# all datasets are restricted to 1825 observations and fixed effect vector is made for all boroughs
man_fixed<- man_fixed[-(1826)]
man_fixed<- man_fixed[-(1826),]
citywidedummy<- citywidedummy[-(1826),]
brook_fixed<- man_fixed

#  Brooklyn F.E.s:

brook_fixed$totalpopulation<- rep(seq(2543764,2582830, by = 9766),each = 365)
 brook_fixed$whitepop<- rep(seq(1238866,1278500, by = 7962),each = 365)
 brook_fixed$blackpop<- 880745
 brook_fixed$hispanicpop<- 493320
 brook_fixed$highgrad<- rep(c(.807,.807, .824,.824,.824),each = 365)
 brook_fixed$collegegrad<- rep(c(.352,.352, .375,.375,.375),each = 365)
 View(brook_fixed)
 brook_fixed$povertyrate<- rep(c(.198,.198, .178,.178,.178),each = 365)
 brook_fixed$medianincome<- rep(c(52782,52782, 60231,60231,60231),each = 365)
 
 # Brooklyn counts:
 # Total crime
 brooklyn1<- filter(crime_data, BORO_NM == "BROOKLYN")
 brooklyn1<- count(brooklyn1, CMPLNT_FR_DT)
 brooklyn1<-brooklyn1%>% mutate(date = as.Date(CMPLNT_FR_DT, format = '%m/%d/%Y')) %>% arrange(date
 brooklyn1<-filter(brooklyn1, date >= "2014-01-01" & date <="2018-12-30")
 brooklyncounts<-brook_fixed
 brooklyncounts$totalcrime<-ifelse(  brooklyncounts$date %in% brooklyn1$date,brooklyn1$n,0)
 brooklyncounts<-brooklyncounts[-c(2:9)]
 
 # Misdemeanor crime
  brooklyn1<- filter(crime_data, BORO_NM == "BROOKLYN" & LAW_CAT_CD == "MISDEMEANOR")
  brooklyn1<- count(brooklyn1, CMPLNT_FR_DT)
  brooklyn1<-brooklyn1%>% mutate(date = as.Date(CMPLNT_FR_DT, format = '%m/%d/%Y')) %>% arrange(date)
  brooklyn1<-filter(brooklyn1, date >= "2014-01-01" & date <="2018-12-30")
  brooklyncounts$misdemeanor<-ifelse(  brooklyncounts$date %in% brooklyn1$date,brooklyn1$n,0)
  
  # same code repeated for felony, violent crime and black crime
  
  # Manhattan counts: total, mis, felony, black
  manhattan1<- filter(crime_data, BORO_NM == "MANHATTAN")
  manhattan1<- count(manhattan1, CMPLNT_FR_DT)
  manhattan1<-manhattan1%>% mutate(date = as.Date(CMPLNT_FR_DT, format = '%m/%d/%Y')) %>% arrange(date)
  manhattan1<-filter(manhattan1, date >= "2014-01-01" & date <="2018-12-30")
  manhattancounts<-brooklyncounts
  manhattancounts$totalcrime<-ifelse(  manhattancounts$date %in% manhattan1$date,manhattan1$n,0)
  
  # the bronx, queens, staten island counts...
  # F.E.s for all remained boroughs have been created
  
  # binding all boroughs counts into one single data frame
  counts<- rbind(manhattancounts,brooklyncounts,bronxcounts,queenscounts,statencounts)
  
  # brutality intense month created
  intensedummy<- as.data.frame(rep(citywidedummy, times = 5))
  
  # First stage Poisson regression using this formula
  firststagereg<-glm(formula = counts$blackcrime ~ intensedummy$`rep(citywidedummy, times = 5)`,
                     +              family = poisson)
  summary(firststagereg)
  
  # all estimates significant at 1% level
  # Second stage poisson regression includinf demographic fixed effects
  secondstagereg<-glm(formula = counts$blackcrime ~ intensedummy$`rep(citywidedummy, times = 5)` + demog_fixed$totalpopulation+ demog_fixed$whitepop + demog_fixed$blackpop+ demog_fixed$hispanicpop + demog_fixed$highgrad + demog_fixed$collegegrad + demog_fixed$povertyrate + demog_fixed$medianincome, family = poisson)
  summary(secondstagereg)
  # still significant estimates but lesser magnitudes
  
  # new dummy
  
  citywidedummy1<- rep(c(0,0,1,0,0,0,1,1,0,1,0,0  ,0,0,0,1,1,0,1,1,0,0,1,0,  0,1,0,1,0,0,0,0,1,1,0,0,  0,1,0,1,0,1,1,0,0,0,1,0,  0,0,1,0,1,1,0,0,0,0,0,0), each =30)
  citywidedummy1<- rep(c(), each =30),, citywidedummy<-append(citywidedummy,h), h<- rep(0,25)
  
  
  # borough fixed effects created
  borough_fixed<- counts
  borough_fixed<-borough_fixed %>% rename( manhattan = totalcrime, brooklyn = misdemeanor, bronx = felony, queens = violation, staten = blackcrime)
  borough_fixed$manhattan<- 0
  borough_fixed$manhattan[1:1825]<- 1
  borough_fixed$brooklyn<- 0
  borough_fixed$brooklyn[1826:3650]<- 1
  borough_fixed$bronx<- 0
  borough_fixed$bronx[3651:5475]<- 1
  borough_fixed$queens<- 0
  borough_fixed$queens[5476:7300]<- 1
  borough_fixed$staten<- 0
  borough_fixed$staten[7301:9125]<- 1
  
 # Second Stage regression with borough F.E.s
  secondstagereg<-glm(formula = counts$blackcrime ~ intensedummy1$citywidedummy1 + demog_fixed$totalpopulation+ demog_fixed$whitepop + demog_fixed$blackpop+ demog_fixed$hispanicpop + demog_fixed$highgrad + demog_fixed$collegegrad + demog_fixed$povertyrate + demog_fixed$medianincome  + borough_fixed$manhattan + borough_fixed$brooklyn+ borough_fixed$bronx+ borough_fixed$queens+ borough_fixed$staten, family = poisson)
  summary(secondstagereg)
  
  
  
   # year fixed effects created
  year_fixed<- counts
  year_fixed$`2014`<- 0
   
  
   
  year_fixed$`2014`<- ifelse(grepl("2014", year_fixed$date), 1, year_fixed$`2014`)
   
  year_fixed$`2015`<- 0
  year_fixed$`2015`<- ifelse(grepl("2015", year_fixed$date), 1, year_fixed$`2015`)
   
  year_fixed$`2016`<- 0
  year_fixed$`2016`<- ifelse(grepl("2016", year_fixed$date), 1, year_fixed$`2016`)
   
  year_fixed$`2017`<- 0
  year_fixed$`2017`<- ifelse(grepl("2017", year_fixed$date), 1, year_fixed$`2017`)
   
  year_fixed$`2018`<- 0
  year_fixed$`2018`<- ifelse(grepl("2018", year_fixed$date), 1, year_fixed$`2018`)
  
  
  # third stage regression with year_fixed
  thirdstagereg<-glm(formula = counts$blackcrime ~ intensedummy1$citywidedummy1 + demog_fixed$totalpopulation+ demog_fixed$whitepop + demog_fixed$blackpop+ demog_fixed$hispanicpop + demog_fixed$highgrad + demog_fixed$collegegrad + demog_fixed$povertyrate + demog_fixed$medianincome  + borough_fixed$manhattan + borough_fixed$brooklyn+ borough_fixed$bronx+ borough_fixed$queens+ borough_fixed$staten +  year_fixed$`2014`+  year_fixed$`2015`+  year_fixed$`2016`+  year_fixed$`2017`+  year_fixed$`2018`, family = poisson)
  summary(thirdstagereg)
  
  # week F.E.s
  week_fixed<-week_fixed[-c(6)]
  week<-rep(c(1 ,1, 1, 1, 1, 1, 1,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), times= 325)
  week<- append(week,c(1 ,1, 1, 1, 1, 1, 1,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  week_fixed$week1<- week
 
   # third stage regression with year_fixed & week_fixed
  thirdstagereg<-glm(formula = counts$blackcrime ~ intensedummy1$citywidedummy1 + demog_fixed$totalpopulation+ demog_fixed$whitepop + demog_fixed$blackpop+ demog_fixed$hispanicpop + demog_fixed$highgrad + demog_fixed$collegegrad + demog_fixed$povertyrate + demog_fixed$medianincome  + borough_fixed$manhattan + borough_fixed$brooklyn+ borough_fixed$bronx+ borough_fixed$queens+ borough_fixed$staten +  year_fixed$`2014`+  year_fixed$`2015`+  year_fixed$`2016`+  year_fixed$`2017`+  year_fixed$`2018`+ week_fixed$week1+ week_fixed$week2+ week_fixed$week3, family = poisson)
  summary(thirdstagereg)
  