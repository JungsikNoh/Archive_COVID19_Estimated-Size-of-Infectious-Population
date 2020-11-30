## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX
## 
## Run COVID19_LatentActualInfectiousPopulation.R -> summariesOverRegions.R
##      -> reporter.R
#
# Updates: 
# J Noh, 08/24/2020. Adjust caption size due to transition to 'cairo' graphics. 

##
##  1. source functions ####################################
##  

# options(bitmapType = 'cairo')

#curDate = Sys.Date(); print(curDate)
curDate = '2020-09-02'

#setwd( )
print(getwd())

source(file.path(getwd(), 'cvd_subftns.R'))
source(file.path(getwd(), 'cvd_county_LatentInfections.R'))
source(file.path(getwd(), 'cvd_state_LatentInfections.R'))
source(file.path(getwd(), 'cvd_country_LatentInfections.R'))

library(ggplot2)
library(data.table)
library(formattable)
library(ggpubr)
#library(RColorBrewer)
#library(ggsci)
library(wesanderson)
library(tseries)
library(penalized)

##
##  2. Fetch data   ###########################################
##

## Countries
# fetch JHU
urlJhu = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
#urlJhu = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
jhudat = read.csv(urlJhu, head=T)

# china curation
idPrv = which(jhudat$Country.Region=='China')
chinaPrv = jhudat[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='China', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat = rbind(chinaDP, jhudat[-idPrv, ])
head(jhudat)
# canada curation
idPrv = which(jhudat$Country.Region=='Canada')
chinaPrv = jhudat[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='Canada', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat = rbind(chinaDP, jhudat[-idPrv, ])
head(jhudat)

write.csv(jhudat, file.path(getwd(), 'JHU_CSSE_covid19_confirmed_global.csv'))

##
## fetch CSSE deaths_global
urlJhu2 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
jhudat2 = read.csv(urlJhu2, head=T)

# china curation
idPrv = which(jhudat2$Country.Region=='China')
chinaPrv = jhudat2[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat2)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='China', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat2 = rbind(chinaDP, jhudat2[-idPrv, ])
head(jhudat2)
# canada curation
idPrv = which(jhudat2$Country.Region=='Canada')
chinaPrv = jhudat2[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat2)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='Canada', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat2 = rbind(chinaDP, jhudat2[-idPrv, ])
head(jhudat2)

write.csv(jhudat2, file.path(getwd(), 'JHU_CSSE_covid19_deaths_global.csv'))

##
## fetch CSSE recovered_global
urlJhu3 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'
jhudat3 = read.csv(urlJhu3, head=T)

# china curation
idPrv = which(jhudat3$Country.Region=='China')
chinaPrv = jhudat3[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat3)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='China', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat3 = rbind(chinaDP, jhudat3[-idPrv, ])
head(jhudat3)
# canada curation
idPrv = which(jhudat3$Country.Region=='Canada')
chinaPrv = jhudat3[idPrv, ]
chinaPrvTS = chinaPrv[, 5:ncol(jhudat3)]
chinaTS = colSums(chinaPrvTS)
chinaHead = data.frame(Province.State='', 'Country.Region'='Canada', Lat=NA, Long=NA)
chinaDP = cbind(chinaHead, rbind(chinaTS))
jhudat3 = rbind(chinaDP, jhudat3[-idPrv, ])
head(jhudat3)

write.csv(jhudat3, file.path(getwd(), 'JHU_CSSE_covid19_recovered_global.csv'))


## States
# fetch states data from covidtracking.com
url2 = 'https://covidtracking.com/api/v1/states/daily.csv'
covidtrackingDat = read.csv(url2, head=T)
# daily input dataset
write.csv(covidtrackingDat, file.path(getwd(), 'covidtracking_dot_com.csv'))
head(covidtrackingDat[, 1:7])


## TX counties
## fetch TX county data from JHU
url_jhuCounty = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
jhuCountyDat = read.csv(url_jhuCounty, head=T)
# counties in Texas
ind = (jhuCountyDat$Province_State == 'Texas')
TXcountyDat = jhuCountyDat[ind, ]
# daily input dataset
write.csv(TXcountyDat, file.path(getwd(), 'TXcounty_confirmed_JHUCSSE.csv'))
tail(TXcountyDat)

## fetch TX county deaths from JHU
url_jhuCounty2 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv'
jhuCountyDat2 = read.csv(url_jhuCounty2, head=T)
# counties in Texas
ind = (jhuCountyDat2$Province_State == 'Texas')
TXcountyDat2 = jhuCountyDat2[ind, ]
# daily input dataset
write.csv(TXcountyDat2, file.path(getwd(), 'TXcounty_deaths_JHUCSSE.csv'))
tail(TXcountyDat2)


## Population data
# csv input files
basicDatasetsDir = file.path(getwd(), 'basicDatasets')
populationData = read.csv(file.path(basicDatasetsDir, 'usItalyKorea_Population2020UN.csv'))
stpopulationData =
  read.csv(file.path(basicDatasetsDir, 'USstatesPopulation_USCensusBureau_simplified.csv'))

# UNpop2019Dat
UNpop2019Dat = read.csv(file.path(basicDatasetsDir, 'UN_WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES_2019.csv'))

# read TX county population 
popTXcounty= read.csv(file.path(basicDatasetsDir, 'worldpopulationReviewdotcom_2020_TexasCounty.csv'))



##
## 3. Global parameters   ############################
##

#totalCases_threshold_toSetStart = 100 
# Infection Fatality Rate
#ifr0 = 0.01
ifr0 = 0.0066 
ifrL = 0.0039
ifrU = 0.0133

## 
##  4. Top 50 countries   ###################################
## 

t1 = Sys.time()
numCntr = 50

jhudatL = jhudat[, c(2, ncol(jhudat))]
head(jhudatL)
scases = sort(jhudatL[, 2], index.return = T, decreasing = T)
jhudatL2 = jhudatL[scases$ix, ]
print(jhudatL2[1:numCntr, ])
namesTop20 = as.character(jhudatL2$Country.Region[1:numCntr])

# Korea is not in top 25 (04/21 revised)
namesTop20 = c(namesTop20, 'Korea, South')

# name curation -.-;; 
namesTop20_1 = namesTop20
namesTop20_1[(namesTop20 == 'US')] = 'United States of America'
namesTop20_1[(namesTop20 == 'Korea, South')] = 'Republic of Korea'
namesTop20_1[(namesTop20 == 'Iran')] = 'Iran (Islamic Republic of)'
namesTop20_1[(namesTop20 == 'Russia')] = 'Russian Federation'
namesTop20_1[(namesTop20 == 'Bolivia')] = 'Bolivia (Plurinational State of)'


# pop preprocessing
countryNamePop = data.frame(Region = namesTop20, namesTop20_1, pop2019 = 1:length(namesTop20))
for (i in 1:nrow(countryNamePop)){
  pop0 = UNpop2019Dat$X2019[which(UNpop2019Dat$Region == namesTop20_1[i])]
  countryNamePop$pop2019[i] = pop0
}

t1=Sys.time()
# run countries 
outLst_country = list()
for (i in 1:(numCntr+1)){
  stname = as.character(countryNamePop$Region[i])
  tmp = cvd_country_LatentInfections(curDate, stname, jhudat, jhudat2, jhudat3, 
                                countryNamePop, ifr0, ifrL, ifrU)
  outLst_country[[i]] = tmp
  #print(tmp)
}
t2=Sys.time(); t2-t1




## 
##  5. TX Counties from JHU   ##############################
## 

numCnty = 15 


# county sorting
TXcountyDatL = TXcountyDat[, c(6, ncol(TXcountyDat))]
head(TXcountyDatL)
scases = sort(TXcountyDatL[, 2], index.return = T, decreasing = T)
TXcountyDatL2 = TXcountyDatL[scases$ix, ]
print(TXcountyDatL2[1:numCnty, ])
sortedCounties = as.character(TXcountyDatL2$Admin2[1:numCnty])
print(sortedCounties)


# run counties 
outLst_county = list()
for (i in 1:numCnty){
  cntyname = sortedCounties[i]
  tmp = cvd_county_LatentInfections(curDate, cntyname, TXcountyDat, TXcountyDat2, 
                                     popTXcounty, ifr0, ifrL, ifrU)
  outLst_county[[i]] = tmp
  tail(tmp)
}
t2=Sys.time(); t2-t1


## 
##  6. States   #############################################
##

# total 56 states
curPos = covidtrackingDat$positive[1:56]
scases = sort(curPos, index.return = T, decreasing = T)
sortedStates = as.character(covidtrackingDat$state[scases$ix[1:56]])
print(sortedStates)

# make 50+'DC' states
data(state)
myStAbb = c(state.abb, 'DC')
stInd = match(sortedStates, myStAbb)
sortedStates2 = sortedStates[!is.na(stInd)]
print(sortedStates2)


# run states
t1 = Sys.time()
numState = length(sortedStates2)
StateAbb = sortedStates2
outLst = list()
for (i in 1:numState){
  stname = StateAbb[i]
  tmp = cvd_state_LatentInfections(curDate, stname, covidtrackingDat, 
                                   stpopulationData, ifr0, ifrL, ifrU)
  outLst[[i]] = tmp # stdat6
  tail(tmp)
}
t2 = Sys.time(); t2-t1



## EOF    #######
