## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX
## 

# assume input datasets are already loaded to workspace
print(getwd())
print(curDate)

##
##  country report  ####
##

head(countryNamePop)

# curation
cname = as.character(countryNamePop$Region)

# name curation for white spaces
whspInd = rep(0, nrow(countryNamePop))
cname2 = rep(NA, nrow(countryNamePop))
for (i in 1:(numCntr+1)){
  x = cname[i]
  y = chartr(' ', '\u00a0', x)
  whspInd[i] = (x != y)
  cname2[i] = y
} 
# change filename
outPath3 = file.path(getwd(), 'output', 'countries_current')
for (i in which(whspInd==1)){
  #print(i)
  listoffiles = list.files(path = outPath3, pattern = as.character(cname[i]))
  listoffiles2 = chartr(cname[i], cname2[i], listoffiles)
  if (length(listoffiles) != 0) {
    file.copy(file.path(outPath3, listoffiles), 
              file.path(outPath3, listoffiles2), overwrite = T)
  }
}


## 1
repname1 = 'REPORT_COUNTRY.md'

sink(file.path(getwd(), repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:numCntr){
  cat(paste0(i, '. ', cname2[i], ' <p>\n'))
    fnametmp1 = paste0(cname2[i], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(cname2[i], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
    fnametmp3 = paste0(cname2[i], '_estInfections.png')
    getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(cname2[i], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
# one more country
  cat(paste0(cname2[51], ' <p>\n'))
    fnametmp1 = paste0(cname2[51], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(cname2[51], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
    fnametmp3 = paste0(cname2[51], '_estInfections.png')
    getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(cname2[51], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')

sink()



## 2
repname1 = 'REPORT_COUNTRY_RECENT8w.md'

sink(file.path(getwd(), repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:numCntr){
  cat(paste0(i, '. ', cname2[i], ' <p>\n'))
  fnametmp1 = paste0(cname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(cname2[i], '_Recent_NewCasesEstConfirmed.png')
  getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(cname2[i], '_Recent_estInfections.png')
  getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(cname2[i], '_Recent_estInfectionsNewCases.png')
  getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
# one more country
cat(paste0(cname2[51], ' <p>\n'))
fnametmp1 = paste0(cname2[51], '_newCases7d.png')
getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
fnametmp2 = paste0(cname2[51], '_Recent_NewCasesEstConfirmed.png')
getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
cat(paste0('> ', getImg1, getImg2))
cat('\n\n')

fnametmp3 = paste0(cname2[51], '_Recent_estInfections.png')
getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
fnametmp4 = paste0(cname2[51], '_Recent_estInfectionsNewCases.png')
getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
cat(paste0('> ', getImg3, getImg4))
cat('\n\n', '<p>&nbsp;</p>', '\n\n')

sink()


## 3
repname1 = 'Daily_confirmed_raw_country.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in seq(1, numCntr, 2)){
  cat(paste0('|  ', i, '. ', cname2[i], '  |  ', i+1, '. ', cname2[i+1], '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(cname2[i], '_newCases.png')
  fnametmp2 = paste0(cname2[i+1], '_newCases.png')
  cat(paste0(  '|  ![img](/output/countries_current/', fnametmp1, ')  ', 
               '|  ![img](/output/countries_current/', fnametmp2, ')  |  \n\n'  ))
}
# one more country
  cat(paste0( cname2[51], '  \n'))
  fnametmp1 = paste0(cname2[51], '_newCases.png') 
  cat(paste0( '<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ', 
              '   \n\n'  ))
   
sink()


## 4
repname1 = 'Daily_rates_country.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# Countries (top 50)\n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:numCntr){
  cat(paste0(i, '. ', cname2[i], ' <p>\n'))
  fnametmp1 = paste0(cname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(cname2[i], '_cnvd_AscertainmentRate.png')
  getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(cname2[i], '_estCumIncidence.png')
  getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(cname2[i], '_estTransmissionRate.png')
  getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
# one more country
cat(paste0(cname2[51], ' <p>\n'))
fnametmp1 = paste0(cname2[51], '_newCases7d.png')
getImg1 = paste0('<img src="/output/countries_current/', fnametmp1, '" width="49.5%"/> ')
fnametmp2 = paste0(cname2[51], '_cnvd_AscertainmentRate.png')
getImg2 = paste0('<img src="/output/countries_current/', fnametmp2, '" width="49.5%"/> ')
cat(paste0('> ', getImg1, getImg2))
cat('\n\n')

fnametmp3 = paste0(cname2[51], '_estCumIncidence.png')
getImg3 = paste0('<img src="/output/countries_current/', fnametmp3, '" width="49.5%"/> ')
fnametmp4 = paste0(cname2[51], '_estTransmissionRate.png')
getImg4 = paste0('<img src="/output/countries_current/', fnametmp4, '" width="49.5%"/> ')
cat(paste0('> ', getImg3, getImg4))
cat('\n\n', '<p>&nbsp;</p>', '\n\n')

sink()



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




##
##  county report   ####
##

head(sortedCounties)

# curation
Ctname = as.character(sortedCounties)

# name curation for white spaces
whspInd = rep(0, length(sortedCounties))
Ctname2 = rep(NA, length(sortedCounties))
for (i in 1:(numCnty)){
  x = Ctname[i]
  y = chartr(' ', '\u00a0', x)
  whspInd[i] = (x != y)
  Ctname2[i] = y
} 
# change filename
outPath3 = file.path(getwd(), 'output', 'TX_counties_current')
for (i in which(whspInd==1)){
  #print(i)
  listoffiles = list.files(path = outPath3, pattern = as.character(Ctname[i]))
  listoffiles2 = chartr(Ctname[i], Ctname2[i], listoffiles)
  if (length(listoffiles) != 0) {
    file.copy(file.path(outPath3, listoffiles), 
              file.path(outPath3, listoffiles2), overwrite = T)
  }
}


## 1
repname3 = 'REPORT_TX_COUNTY.md'

sink(file.path(getwd(), repname3))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15) \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(Ctname)){
  cat(paste0(i, '. ', Ctname2[i], ' <p>\n'))
    fnametmp1 = paste0(Ctname2[i], '_newCases7d.png')
    getImg1 = paste0('<img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ')
    fnametmp2 = paste0(Ctname2[i], '_NewCasesEstConfirmed.png')
    getImg2 = paste0('<img src="/output/TX_counties_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
    fnametmp3 = paste0(Ctname2[i], '_estInfections.png')
    getImg3 = paste0('<img src="/output/TX_counties_current/', fnametmp3, '" width="49.5%"/> ')
    fnametmp4 = paste0(Ctname2[i], '_estTotalCases.png')
    getImg4 = paste0('<img src="/output/TX_counties_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
sink()



## 2
repname3 = 'REPORT_TX_COUNTY_RECENT8w.md'

sink(file.path(getwd(), repname3))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15) \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(Ctname)){
  cat(paste0(i, '. ', Ctname2[i], ' <p>\n'))
  fnametmp1 = paste0(Ctname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(Ctname2[i], '_Recent_NewCasesEstConfirmed.png')
  getImg2 = paste0('<img src="/output/TX_counties_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(Ctname2[i], '_Recent_estInfections.png')
  getImg3 = paste0('<img src="/output/TX_counties_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(Ctname2[i], '_Recent_estInfectionsNewCases.png')
  getImg4 = paste0('<img src="/output/TX_counties_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}
sink()



## 3
repname1 = 'Daily_confirmed_raw_TX_county.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15) \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in seq(1, length(Ctname2)-1, 2)){
  cat(paste0('|  ', i, '. ', Ctname2[i], '  |  ', i+1, '. ', Ctname2[i+1], '  |  \n'))
  cat(paste0('|  :---   |   :---   |  \n'))
  
  fnametmp1 = paste0(Ctname2[i], '_newCases.png')
  fnametmp2 = paste0(Ctname2[i+1], '_newCases.png')
  cat(paste0(  '|  ![img](/output/TX_counties_current/', fnametmp1, ')  ', 
               '|  ![img](/output/TX_counties_current/', fnametmp2, ')  |  \n\n'  ))
}
cat(paste0('|  ', 15, '. ', Ctname2[15], '  |  ', 16, '. NA', '  |  \n'))
cat(paste0('|  :---   |   :---   |  \n'))

fnametmp1 = paste0(Ctname2[15], '_newCases.png') 
cat(paste0(  '|  <img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ', 
             '|   NA  |  \n\n'  ))

sink()


## 4
repname1 = 'Daily_rates_TX_county.md'

sink(file.path(getwd(), 'doc', repname1))
cat('<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">')
cat('\n\n', '<p>&nbsp;</p>', '\n\n', '<p>&nbsp;</p>', '\n\n')
cat(paste0('## ', curDate, ', COVID-19 Time Series', '\n'))
cat('# TX counties (top 15)  \n')
cat('\n\n', '<p>&nbsp;</p>', '\n\n')
for (i in 1:length(Ctname2)){
  cat(paste0(i, '. ', Ctname2[i], ' <p>\n'))
  fnametmp1 = paste0(Ctname2[i], '_newCases7d.png')
  getImg1 = paste0('<img src="/output/TX_counties_current/', fnametmp1, '" width="49.5%"/> ')
  fnametmp2 = paste0(Ctname2[i], '_cnvd_AscertainmentRate.png')
  getImg2 = paste0('<img src="/output/TX_counties_current/', fnametmp2, '" width="49.5%"/> ')
  cat(paste0('> ', getImg1, getImg2))
  cat('\n\n')
  
  fnametmp3 = paste0(Ctname2[i], '_estCumIncidence.png')
  getImg3 = paste0('<img src="/output/TX_counties_current/', fnametmp3, '" width="49.5%"/> ')
  fnametmp4 = paste0(Ctname2[i], '_estTransmissionRate.png')
  getImg4 = paste0('<img src="/output/TX_counties_current/', fnametmp4, '" width="49.5%"/> ')
  cat(paste0('> ', getImg3, getImg4))
  cat('\n\n', '<p>&nbsp;</p>', '\n\n')
}

sink()



##  EOF


