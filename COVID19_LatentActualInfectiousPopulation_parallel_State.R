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

options(bitmapType = 'cairo')

#curDate = Sys.Date(); print(curDate)
curDate = '2020-11-29'
print(curDate)

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

library(parallel)
library(doParallel)

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
print(head(jhudat))

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
print(head(covidtrackingDat[, 1:7]))


Sys.sleep(5)

## TX counties
## fetch TX county data from JHU
url_jhuCounty = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
jhuCountyDat = read.csv(url_jhuCounty, head=T)
# counties in Texas
ind = (jhuCountyDat$Province_State == 'Texas')
TXcountyDat = jhuCountyDat[ind, ]
# daily input dataset
write.csv(TXcountyDat, file.path(getwd(), 'TXcounty_confirmed_JHUCSSE.csv'))
print(tail(TXcountyDat))


## fetch TX county deaths from JHU
url_jhuCounty2 = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv'
jhuCountyDat2 = read.csv(url_jhuCounty2, head=T)
# counties in Texas
ind = (jhuCountyDat2$Province_State == 'Texas')
TXcountyDat2 = jhuCountyDat2[ind, ]
# daily input dataset
write.csv(TXcountyDat2, file.path(getwd(), 'TXcounty_deaths_JHUCSSE.csv'))
print(tail(TXcountyDat2))


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
cl = makeCluster(51)
registerDoParallel(cl)
clusterCall(cl, function() options(bitmapType = 'cairo'))
print(cl)

t1 = Sys.time()
numState = length(sortedStates2)
StateAbb = sortedStates2
outLst = list()

outLst <- foreach(i = 1:numState, 
                  .packages = c('ggplot2', 'data.table','formattable',
                                'ggpubr','RColorBrewer','ggsci','wesanderson','tseries','penalized')) %dopar% {
                                  stname = StateAbb[i]
                                  tmp = cvd_state_LatentInfections(curDate, stname, covidtrackingDat, 
                                                                   stpopulationData, ifr0, ifrL, ifrU)
                                  #outLst[[i]] = tmp  
                                }
stopCluster(cl)

t2 = Sys.time(); print(t2-t1)


## Summaries Over regions

##
## 0. set up  ####

# curDate = '2020-08-10'
print(curDate)

library(ggplot2)
library(ggrepel)
library(matlab)
#library(RColorBrewer)

source(file.path(getwd(), 'summariesOverRegions_subftns.R'))

##
##  2. state_summary    ####
##

rgnNames = sortedStates2 

# pop preprocessing
data(state)
myStAbb = c(state.abb, 'DC')
myStName = c(state.name, 'District of Columbia')

popVec = as.numeric()
for (i in 1:length(sortedStates2)){
  stname = sortedStates2[i]
  stInd = which(myStAbb == stname)
  stfullname = myStName[stInd]
  stfullname2 = paste0('.', stfullname)
  stInd2 = which(stpopulationData$States == stfullname2)
  Xpop = stpopulationData$Est2019_Population[stInd2]
  #
  popVec[i] = Xpop
}


outLst_file = list()
for (i in 1:length(rgnNames)){
  stname = rgnNames[i]
  opath = file.path(getwd(), 'output', 'states', stname, paste0(curDate))
  oDF = read.csv(file=file.path(opath, paste0(stname, '_outputDF.csv')))
  outLst_file[[i]] = oDF
}
myLst = outLst_file 


outsumPath = file.path(getwd(), 'output', 'state_summary', paste0(curDate))
if (!dir.exists(outsumPath)) dir.create(outsumPath, recursive = T)
output_current = file.path(getwd(), 'output', 'states_current' )

## plot summaries
cvd_plotSummaries(cumAsctRate_nudge_x = 20, cumAsctRate_nudge_y = 2)



##
##  states report   ####
## 

head(sortedStates2)

##
repname2 = 'REPORT_STATE.md'

sink(file.path(getwd(), repname2))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# States in the U.S. \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(sortedStates2)){
  cat(paste0(i, '. ', sortedStates2[i], ' <p>\n'))
    fnametmp1 = paste0(sortedStates2[i], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/states_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(sortedStates2[i], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/states_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
   
    fnametmp3 = paste0(sortedStates2[i], '_estInfections.png')
    getImg3 = paste0('<img src="/output/states_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(sortedStates2[i], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/states_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
sink()


##
repname2 = 'REPORT_STATE_RECENT8w.md'

sink(file.path(getwd(), repname2))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# States in the U.S. \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(sortedStates2)){
  cat(paste0(i, '. ', sortedStates2[i], ' <p>\n'))
  fnametmp1 = paste0(sortedStates2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/states_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(sortedStates2[i], '_Recent_NewCasesEstConfirmed.png')
  getImg2 = paste0('<img src="/output/states_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(sortedStates2[i], '_Recent_estInfections.png')
  getImg3 = paste0('<img src="/output/states_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(sortedStates2[i], '_Recent_estInfectionsNewCases.png')
  getImg4 = paste0('<img src="/output/states_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
sink()


## 3
repname1 = 'Daily_confirmed_raw_state.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# States in the U.S. \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in seq(1, length(sortedStates2)-1, 2)){
  cat(paste0('|  ', i, '. ', sortedStates2[i], '  |  ', i+1, '. ', sortedStates2[i+1], '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(sortedStates2[i], '_newCases.png')
  fnametmp2 = paste0(sortedStates2[i+1], '_newCases.png')
  cat(paste0(  '|  ![img](/output/states_current/', fnametmp1, ')  ', 
               '|  ![img](/output/states_current/', fnametmp2, ')  |  \n\n'  ))
}
  cat(paste0('|  ', 51, '. ', sortedStates2[51], '  |  ', 52, '. NA', '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(sortedStates2[51], '_newCases.png') 
  cat(paste0(  '|  <img src="/output/states_current/', fnametmp1, '" width="49.5%"/> ', 
               '|   NA  |  \n\n'  ))

sink()
 


## 4
repname1 = 'Test_positivity_rate_state.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# States in the U.S. \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in seq(1, length(sortedStates2)-1, 2)){
  cat(paste0('|  ', i, '. ', sortedStates2[i], '  |  ', i+1, '. ', sortedStates2[i+1], '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(sortedStates2[i], '_testPositiveRate.png')
  fnametmp2 = paste0(sortedStates2[i+1], '_testPositiveRate.png')
  cat(paste0(  '|  ![img](/output/states_current/', fnametmp1, ')  ', 
               '|  ![img](/output/states_current/', fnametmp2, ')  |  \n\n'  ))
}
cat(paste0('|  ', 51, '. ', sortedStates2[51], '  |  ', 52, '. NA', '  |  \n'))
cat(paste0('|  :---   |   :---   |  \n'))

fnametmp1 = paste0(sortedStates2[51], '_testPositiveRate.png') 
cat(paste0(  '|  <img src="/output/states_current/', fnametmp1, '" width="49.5%"/> ', 
             '|   NA  |  \n\n'  ))

sink()

## 5
repname1 = 'Daily_rates_state.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# States in the U.S. \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(sortedStates2)){
  cat(paste0(i, '. ', sortedStates2[i], ' <p>\n'))
  fnametmp1 = paste0(sortedStates2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/states_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(sortedStates2[i], '_cnvd_AscertainmentRate.png')
  getImg2 = paste0('<img src="/output/states_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(sortedStates2[i], '_estCumIncidence.png')
  getImg3 = paste0('<img src="/output/states_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(sortedStates2[i], '_estTransmissionRate.png')
  getImg4 = paste0('<img src="/output/states_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}

sink()





## EOF    #######
