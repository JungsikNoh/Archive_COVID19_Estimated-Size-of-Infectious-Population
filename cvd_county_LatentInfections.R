## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX 


cvd_county_LatentInfections = function(curDate, cntyname, TXcountyDat, TXcountyDat2, 
                                  popTXcounty, ifr0, ifrL, ifrU){
  #
  # Estimating unidentified infectious cases
  # Jungsik Noh, UTSW, Dallas, TX  
  
  outPath = file.path(getwd(), 'output', 'TX_counties', cntyname, 
                      paste0(curDate))
  if (!dir.exists(outPath)) dir.create(outPath, recursive = T)
  outPath2 = file.path(getwd(), 'output', 'TX_counties_current')
  if (!dir.exists(outPath2)) dir.create(outPath2, recursive = T)
  
  #
  head(TXcountyDat) 
  class(TXcountyDat)
  names(TXcountyDat) 
  stname = cntyname
  
  ## X population: popTXcounty 
  myStName = popTXcounty$CTYNAME
  stInd = which(myStName == paste0(cntyname, ' County'))
  if (length(stInd) == 0){ return() }
  Xpop = popTXcounty$Pop[stInd]
  
  stdat1 = TXcountyDat[TXcountyDat$Admin2 == stname, ]
  #stdat2 = stdat1[nrow(stdat1):1, ]
  stdat1_body = stdat1[, c(12:ncol(stdat1))]
  stdat3 = data.frame(date = colnames(stdat1_body), val = cbind(as.numeric(stdat1_body)))
  colnames(stdat3) = c('date', 'positive')
  tail(stdat3)
  # deaths
  stdat12 = TXcountyDat2[TXcountyDat2$Admin2 == stname, ]
  stdat1_body2 = stdat12[, c(13:ncol(stdat12))]   # good job JHU
  val = cbind(as.numeric(stdat1_body2))
  stdat3$death = val
  stdat3$numTests = NaN
  stdat3$recovered = NaN
  tail(stdat3)
  #if (!all(is.na(stdat3$recovered))) {
  stdat3$recovered[is.nan(stdat3$recovered)] = 0
  if (all(stdat3$recovered == 0)) {stdat3$recovered = NaN}
  #}
  stdat3$death[is.na(stdat3$death)] = 0
  
  # preprocessing: make sure non-decreasing TS
  # all(cummax(stdat3$positive) == stdat3$positive)
  stdat3$positive = cummax(stdat3$positive)
  stdat3$death = cummax(stdat3$death)
    
  
  # data since 20200314 ->0305 xx
  id0 = which(stdat3$date == 'X3.13.20')
  ids = seq(from=id0, to = nrow(stdat3), by = 1)
  stdat4 = stdat3[ids, ]
  #stdat4 = stdat3
  #
  date1 = stdat4$date
  #date_mmdd =       # date1 %% 10000
  #date_mm = as.numeric(substr(date1, 2, 2))           # date_mmdd %/% 100
  #date_dd = as.numeric(substr(date1, 4, 5))           # date_mmdd %% 100 
  tmp = unlist(strsplit(as.character(date1), '[.]'))
  mm_tmp = tmp[seq(1, length(tmp), by=3)]
  date_mm = as.numeric(substr(mm_tmp, 2, 3))  
  date_dd = as.numeric(tmp[seq(2, length(tmp), by=3)])

  mydates = as.Date('2020-03-05')
  for (i in 1:length(date1)){
    mydates[i] = as.Date(paste0("2020-", date_mm[i], "-", date_dd[i]))
  }
  
  #
  stdat4$mydates = mydates
  
  ## Daily new confirmed cases
  dif_cases = c(NA, diff(stdat4$positive))  
  dif_tests = c(NA, diff(stdat4$numTests))  
  dif_recovered = c(NA, diff(stdat4$recovered))  
  dif_death = c(NA, diff(stdat4$death))  
  #d_cases = dif_cases/2
  #d_tests = dif_tests/2
  stdat4$dif_cases = dif_cases
  stdat4$dif_tests = dif_tests
  stdat4$dif_recovered = dif_recovered
  stdat4$dif_death = dif_death
  
  ##
  ##  modules
  ##
  print('==')
  print(stname)
  print('==')
  outdat = describe_df(curDate, stname, stdat4, Xpop, outPath, outPath2, ifr0, ifrL, ifrU)
 
  #return(outdat) 
}


## 
## EOF
