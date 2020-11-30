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
##  1. country_summary ####
##

rgnNames = countryNamePop$Region[1:50]
popVec = countryNamePop$pop2019[1:50]

outLst_country = list(); 
for (i in 1:length(rgnNames)){
  stname = rgnNames[i]
  opath = file.path(getwd(), 'output', 'countries', stname, paste0(curDate))
  oDF = read.csv(file=file.path(opath, paste0(stname, '_outputDF.csv')))
  outLst_country[[i]] = oDF
}

myLst = outLst_country[1:50]
 
outsumPath = file.path(getwd(), 'output', 'country_summary', paste0(curDate))
if (!dir.exists(outsumPath)) dir.create(outsumPath, recursive = T)
output_current = file.path(getwd(), 'output', 'countries_current' )

## plot summaries
cvd_plotSummaries()


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
##  3. TX_county_summary   ####
##

rgnNames = sortedCounties 

# pop preprocessing  
popVec = as.numeric()
myStName = popTXcounty$CTYNAME
for (i in 1:length(sortedCounties)){
  cntyname = sortedCounties[i]
  stInd = which(myStName == paste0(cntyname, ' County'))
  Xpop = popTXcounty$Pop[stInd]
  #
  popVec[i] = Xpop
}
print(rgnNames)
print(popVec)

outLst_file = list()
for (i in 1:length(rgnNames)){
  stname = rgnNames[i]
  opath = file.path(getwd(), 'output', 'TX_counties', stname, paste0(curDate))
  oDF = read.csv(file=file.path(opath, paste0(stname, '_outputDF.csv')))
  outLst_file[[i]] = oDF
}
myLst = outLst_file 

outsumPath = file.path(getwd(), 'output', 'TX_county_summary', paste0(curDate))
if (!dir.exists(outsumPath)) dir.create(outsumPath, recursive = T)
output_current = file.path(getwd(), 'output', 'TX_counties_current' )

## plot summaries

cvd_plotSummaries(cumAsctRate_nudge_x = 10, cumAsctRate_nudge_y = 1)



 
##
##  EOF
