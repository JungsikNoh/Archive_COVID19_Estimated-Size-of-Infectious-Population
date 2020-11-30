## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX 


cvd_country_LatentInfections = function(curDate, stname, jhudat, jhudat2, jhudat3, 
                                   countryNamePop, ifr0, ifrL, ifrU){
  #
  # Estimating unidentified infectious cases
  # Jungsik Noh, UTSW, Dallas, TX  
  
  outPath = file.path(getwd(), 'output', 'countries', stname, 
                      paste0(curDate))
  if (!dir.exists(outPath)) dir.create(outPath, recursive = T)
  outPath2 = file.path(getwd(), 'output', 'countries_current')
  if (!dir.exists(outPath2)) dir.create(outPath2, recursive = T)
  
  #
  head(jhudat) 
  class(jhudat)
  names(jhudat)  
  
  ## X population: countryNamePop 
  myStName = countryNamePop$Region
  stInd = which(myStName == paste0(stname))
  if (length(stInd) == 0){ return() }
  Xpop = countryNamePop$pop2019[stInd]
  
  # positive
  ind = which((jhudat$Country.Region == stname) & (jhudat$Province.State == ''))
  stdat1 = jhudat[ind, ]
  
  #stdat2 = stdat1[nrow(stdat1):1, ]
  stdat1_body = stdat1[, c(5:ncol(stdat1))]
  stdat3 = data.frame(date = colnames(stdat1_body), val = cbind(as.numeric(stdat1_body)))
  colnames(stdat3) = c('date', 'positive')
  tail(stdat3)
  # deaths
  ind = which((jhudat2$Country.Region == stname) & (jhudat2$Province.State == ''))
  stdat12 = jhudat2[ind, ]
  stdat1_body2 = stdat12[, c(5:ncol(stdat12))]   # 
  val2 = cbind(as.numeric(stdat1_body2))
  stdat3$death = val2
  
  stdat3$numTests = NA 
  # recovered
  ind = which((jhudat3$Country.Region == stname) & (jhudat3$Province.State == ''))
  stdat13 = jhudat3[ind, ]
  stdat1_body3 = stdat13[, c(5:ncol(stdat13))]   #  
  val3 = cbind(as.numeric(stdat1_body3))
  stdat3$recovered = val3
  
  tail(stdat3)
  #if (!all(is.na(stdat3$recovered))) {
  stdat3$recovered[is.na(stdat3$recovered)] = 0
  if (all(stdat3$recovered == 0)) {stdat3$recovered = NaN}
  #}
  stdat3$death[is.na(stdat3$death)] = 0
  
  # preprocessing: make sure non-decreasing TS
  # all(cummax(stdat3$positive) == stdat3$positive)
  stdat3$positive = cummax(stdat3$positive)
  stdat3$death = cummax(stdat3$death)
  stdat3$numTests = cummax(stdat3$numTests)
  stdat3$recovered = cummax(stdat3$recovered)
    
  
  # data since 2020....0225        20200314 ->0305 xx
  # starting date V2
  iddeath = which(stdat3$death > 0)[1]
  id0 = max(1, iddeath + 2)
  #id0 = which(stdat3$date == 'X4.1.20')
  
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
