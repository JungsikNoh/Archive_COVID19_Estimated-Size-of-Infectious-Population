##
##  COVID-19 subfunctions   ##############################
##  07/25/2020, Jungsik Noh, UT Southwestern Medical Center, Dallas, TX
##  updates
# 8/12/2020, Add Prevalence Plot. 'xxx_current' folder gathers plots for reporting.


cummax <- function(x) {
  l <- length(x)
  y <- numeric(l)
  y[1] <- x[1]
  for(i in 2:l) y[i] <- max(x[i],y[i-1])
  return(y)
}

zsc <- function(x){ (x - mean(x, na.rm=T))/sd(x, na.rm=T)}

naFilter <- function(x, w){
  y = rep(NA, length(x))
  N = length(w)
  for (i in 1:length(x)){
    stid = max(i-N+1, 1)
    tmp = x[stid:i]
    y[i] = mean(tmp, na.rm=T)
  }
  return(y)
} 

naFilter_2sides <- function(x, w){
  y = rep(NA, length(x))
  N = length(w)
  N2 = (N-1)/2
  for (i in 1:length(x)){
    stid = max(i-N2, 1)
    endid = min(i+N2, length(x))
    tmp = x[stid:endid]
    y[i] = mean(tmp, na.rm=T)
  }
  return(y)
} 

####

EM_confirmRate_k <- function(stdat6, sparam1, sparam2){
  
  numT = nrow(stdat6)
  # stdat6$confirmRate_pre = confirmRate  
  # stdat6$Einfected_pre = stdat6$Einfected
  # stdat6$Epositive_pre = stdat6$Epositive
  # plot(confirmRate, ylim=c(0,100))
    
  for (k in 1:1000){
    
    ## Expectation step
    #dif_Epos_k = stdat6$dif_pos7 * (100/stdat6$confirmRate_pre)    k = k+1
    
    ## Maximization 
    tmp = c(NA, stdat6$Einfected_pre[1:(nrow(stdat6)-1)])
    NCC7PerEI_k = stdat6$dif_pos7 / tmp * 100
    NCC7PerEI_k[stdat6$dif_pos7 == 0] = NaN
    # 08/01
    itstrt = which(is.finite(stdat6$confirmRate_pre))[1]
    NCC7PerEI_k[1:(itstrt-1)] = NaN
    #stdat6$NCC7PerEinfected_k = naFilter_2sides(NCC7PerEI_k, wList[[j]])
    stdat6$NCC7PerEinfected_k = NCC7PerEI_k
    
    #
    #fit0 = lm(stdat6$confirmRate_pre[-1] ~ 0 + stdat6$NCC7PerEinfected_k[-1])
    indforsp = (!is.finite(stdat6$confirmRate_pre) | !is.finite(NCC7PerEI_k))
    a1 = stdat6$confirmRate_pre[!indforsp]
    a2 = NCC7PerEI_k[!indforsp]
    #tmp2 = stdat6$NCC7PerEinfected_k[-1] * mean(a1)/mean(a2)
    
    # log-transform, [0.1%, 99%]-> [1%,99%]
    xx = 1:(numT)
    xx = xx[!indforsp]
    a11 = pmax(pmin(a1, 99), 1) / 100
    a21 = pmax(pmin(a2, 99), 0.1) / 100
    a1log = log10( a11 )
    a2log = log10( a21 )
    
    #tmp2 = zsc( a2log ) * sd(a1log) + mean(a1log)
    ssp2 = smooth.spline(xx, a2log, spar = sparam2)
    yy2 = 100 * 10^ssp2$y 
    #ssp2 = smooth.spline(xx, a21, spar = sparam)  
    #yy2 = 100 * ssp2$y 
    #lines(yy2, col=2)
    ssp1 = smooth.spline(xx, a1log, spar = sparam1)
    yy1_0 = 100 * 10^ssp1$y 
    yy1 = yy1_0 * mean(a1) / mean(yy1_0)
    #ssp1 = smooth.spline(xx, a11, spar = sparam)
    #yy1 = 100 * ssp1$y
    #lines(yy1, col=3)
    yy2_2 = yy2^2; yy2_3 = yy2^3; yy2_4 = yy2^4; yy2_5 = yy2^5; yy2_6 = yy2^6
    #sy2_2 = ssp2$y^2; sy2_3 = ssp2$y^3
    #xx fit0 = lm(ssp1$y ~ ssp2$y + sy2_2 + sy2_3 )
    #fit0 = lm(yy1 ~ yy2 + yy2_2 + yy2_3 )
    # 09/02 revised
    #fit0 = lm(yy1 ~ yy2 + yy2_2 + yy2_3 + yy2_4 )
    
    # 09/12 to speed up
    fit0 = lm(yy1 ~ yy2 + yy2_2 + yy2_3 ) ; #print(fit0$coefficients)
    yy1_k = as.numeric(fit0$fitted.values)
    
    # 07/31, from VT example, non-negative constraints needed. 
    #if (any(is.na(coef(fit0))) | (coef(fit0)[2] < 0) | (coef(fit0)[3] < 0)){
    if (any(is.na(coef(fit0))) | (coef(fit0)[2] < 0) ){
      #print('Coef < 0 => penalized')
      pen = penalized(yy1, ~ yy2 + yy2_2 + yy2_3  , ~ 1, lambda1=0, lambda2=0, positive = T, trace=F, maxiter=100)
      yy1_k = as.numeric(fitted(pen))
      #print(coef(pen))
    }
    
    
    #yy1_k = pmax(pmin(yy1_k, 99), 0.1)
    
    #yy1_k = 100 * 10^a1log_k
    #yy3 = a1_k
    
    yy3 = yy1_k * mean(a1) / mean(yy1_k)
    #yy3 = yy1_k * mean(a11*100) / mean(yy1_k)
    

    # mean preserving scaling, 06/02
    max3 = max(yy3); min3 = min(yy3); m3 = mean(yy3)
    scmax = (max3 - m3) / (99 - m3)
    scmin = (min3 - m3) / (1 - m3)
    sc0 = max(scmax, scmin, 1)
    yy3sc = m3 + (yy3 - m3) / sc0
    
    ## truncation, 07/22
    #yy3sc = pmax(pmin(yy3, 99), 0.1)
    ##yy3sc = yy3sc * mean(a1) / mean(yy3sc)
    ##print(mean(yy3sc))
    
    #lines(yy3, col=k)
    # method 2 adjusting STD too
    #yy4 = 100 * 10^( zsc(ssp2$y) * 0.8 * sd(ssp1$y) +  mean(ssp1$y) )
    #yy4 = zsc(yy2) * 0.9 * sd(a1) + mean(a1)
    #lines(yy4, col=k)
    
    # bdry condition, < 99%
    #tmp2 = pmin(tmp2, 99)
    
    yy3aug = rep(NA, length(stdat6$confirmRate_pre))
    yy3aug[!indforsp] = yy3sc
    #stdat6$confirmRate_k = c(NA, yy3)
    stdat6$confirmRate_k = yy3aug
    # plot(yy2,yy1); lines(yy2, yy3aug[-1], col=k) 
    #mean(a1); mean(yy1); mean(yy3)
    
    #confirmRate_k = c(NA, as.numeric(fit0$fitted.values))
    
    #stdat6$confirmRate7_k = naFilter_2sides(confirmRate_k, wList[[j]])
    ##stdat6$confirmRate_k = confirmRate_k
    
    #confirmRate_k = stdat6$dif_pos7 / dif_Epos_k * 100
    #stdat6$confirmRate7_k = naFilter_2sides(confirmRate_k, w7Gauss)
    
    #stdat6$recRate7_k = naFilter_2sides(stdat6$dailyErecoveredPerEinfected_k, w7Gauss)
    
    ##
    ## Expectation step
    dif_Epos_k = stdat6$dif_pos7 * (100/stdat6$confirmRate_k)
    #dif_Epos_k[!is.finite(dif_Epos_k)] = 0
    dif_Epos_k[stdat6$dif_pos7 == 0] = 0
    # 08/01
    itstrt = which(is.finite(stdat6$confirmRate_k))[1]
    stVal = stdat6$Epositive[itstrt]
    if (!is.finite(stVal)) stVal = 0
    stdat6$Epositive_k = c(stdat6$Epositive[1:(itstrt-1)], stVal, 
                           stVal + cumsum(dif_Epos_k[(itstrt+1):numT]))
    #
    stdat6$Erecovered_k = stdat6$Erecovered
    stdat6$Einfected_k = stdat6$Epositive_k - stdat6$death7 - stdat6$Erecovered_k
    
    # boundary condition
    stdat6$Einfected_k = pmax(stdat6$Einfected_k, 1, na.rm=T)
    stdat6$Einfected_k = pmax(stdat6$Einfected_k, 5*dif_Epos_k, na.rm=T) 
    
    # 05/31, bdry condition adjustment
    tmp = c(NA, diff(stdat6$Einfected_k))
    tmp2 = tmp + stdat6$dif_dea7 + stdat6$dif_Erecovered
    dif_Epos_k2 = pmax(dif_Epos_k, tmp2)
    stdat6$Epositive_k = c(stdat6$Epositive[1:(itstrt-1)], stVal, 
                           stVal + cumsum(dif_Epos_k2[(itstrt+1):numT]))
    stdat6$Einfected_k = stdat6$Epositive_k - stdat6$death7 - stdat6$Erecovered_k
    
    # 07/23, explode
    #stdat6$confirmRate_k = stdat6$dif_pos7 / pmax(dif_Epos_k2, 1/7) * 100
    #stdat6$confirmRate_k[stdat6$dif_pos7 == 0] = NaN
    
    
    #plot(tmp2, type='l')
    #lines(dif_Epos_k, col=2)
    #plot(stdat6$Einfected_k2, type='l')
    #lines(stdat6$Einfected_k, col=2)
    
    # new IR_k
    tmp = c(NA, stdat6$Einfected_k[1:(nrow(stdat6)-1)])
    stdat6$dailyDeathPerEinfected_k = stdat6$dif_dea7 / tmp * 100
    stdat6$dailyDeathPerEinfected_k[stdat6$dif_dea7 == 0] = NA
    stdat6$dailyDeathPerEinfected_k[!is.finite(stdat6$dailyDeathPerEinfected_k)] = NA
    
    tmp = c(NA, stdat6$Einfected_k[1:(nrow(stdat6)-1)])
    stdat6$dailyRecoveredPerEinfected_k = stdat6$dif_rec7 / tmp * 100
    #stdat6$dailyErecoveredPerEinfected_k = dif_Erec_k / tmp * 100
    stdat6$dailyErecoveredPerEinfected_k = stdat6$dif_Erecovered / tmp * 100
    
    #
    
    stdat6$confirmRate_pre = stdat6$confirmRate_k 
    #recRate_pre = stdat6$recRate7_k
    
    #
    mDR = mean(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    sdDR = sd(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    cvDR = sdDR/mDR * 100
    #print(cvDR);     print('==')
    #print(rev(stdat6$Einfected_k)[1])
    
    # tolerance, |f_k - f_k-1|/|f_k-1| < 0.00001, L1-norm
    mabsDif = mean(abs(stdat6$Einfected_k - stdat6$Einfected_pre), na.rm=T)
    L1f_pre = mean(abs(stdat6$Einfected_pre), na.rm=T)
    L1val = mabsDif / L1f_pre
    if (is.finite(L1val) & (L1val < 1e-4)) {
      print(k)
      print(rev(stdat6$Einfected_pre)[1])
      print(rev(stdat6$Einfected_k)[1])
      print(cvDR)
      #print(sparam)
      print(c(sparam1, sparam2))
      print('====')
      break
    }
    #if (mod(k, 1) == 0) { 
    #  print(rev(stdat6$Einfected_k)[1]); print(L1val) 
    #  lines(yy3aug, col=k/1) }
    stdat6$Einfected_pre = stdat6$Einfected_k
  }
  
  output = list(stdat6=stdat6, k=k, cvDR=cvDR)
  return(output)
}

##

CI_Estep <- function(stdat6, confirmRate_k, IFcoef, itod, dtor){
  
  numT = nrow(stdat6)
  # for the case when confirmed cases are too large (09/21)
  # death7 => Epositive, Erecovered
  #numT = nrow(stdat6)
  tmp = stdat6$death7[(itod+1):numT] * IFcoef
  stdat6$Epositive = c(tmp, rep(NA, itod))
  stdat6$Epositive[1] = max(stdat6$Epositive[1], stdat6$positive[1])
  
  tmp = stdat6$death7[2:(numT-dtor)] * (IFcoef - 1)
  stdat6$Erecovered = c(rep(0, (dtor+1)), tmp)
  
  stdat6$dif_Erecovered = c(NA, diff(stdat6$Erecovered))
  stdat6$dif_Epositive = c(NA, diff(stdat6$Epositive))
  # bdry condition
  stdat6$dif_Epositive = pmax(stdat6$dif_Epositive, stdat6$dif_pos7)
  stVal0 = stdat6$Epositive[1]
  stdat6$Epositive = c(stVal0, stVal0 + cumsum(stdat6$dif_Epositive[2:numT]))
  
  # for the case when confirmed cases are too large (09/21)
  shiftedEpos = c( rep(0, itod), stdat6$Epositive[1:(numT-itod)] )
  shiftedEposMinusDeath = pmax( shiftedEpos - stdat6$death7, stdat6$death7 * (IFcoef - 1) )
  tmpRec = c(rep(0, dtor), shiftedEposMinusDeath[1:(numT-dtor)] ) 
  stdat6$Erecovered = pmax( stdat6$Erecovered, tmpRec )
  #stdat6$dif_Erecovered = c(NA, diff(stdat6$Erecovered))
  
  # 09/21
  Erecovered_tmp = stdat6$Erecovered
  
  ## Expectation step
  dif_Epos_k = stdat6$dif_pos7 * (100/confirmRate_k)
  #dif_Epos_k[!is.finite(dif_Epos_k)] = 0
  dif_Epos_k[stdat6$dif_pos7 == 0] = 0
  # 08/01
  itstrt = which(is.finite(stdat6$confirmRate_k))[1]
  stVal = stdat6$Epositive[itstrt]
  if (!is.finite(stVal)) stVal = 0
  Epositive_k = c(stdat6$Epositive[1:(itstrt-1)], stVal, 
                         stVal + cumsum(dif_Epos_k[(itstrt+1):numT]))
  
  #
  Erecovered_k = Erecovered_tmp
  Einfected_k = Epositive_k - stdat6$death7 - Erecovered_k
  
  dif_Erecovered_k = c(NA, diff(Erecovered_k))
  
  # boundary condition
  Einfected_k = pmax(Einfected_k, 1, na.rm=T)
  Einfected_k = pmax(Einfected_k, 5*dif_Epos_k, na.rm=T) 
  
  # 05/31, bdry condition adjustment
  tmp = c(NA, diff(Einfected_k))
  tmp2 = tmp + stdat6$dif_dea7 + dif_Erecovered_k
  dif_Epos_k2 = pmax(dif_Epos_k, tmp2)
  # 08/01
  Epositive_k = c(stdat6$Epositive[1:(itstrt-1)], stVal, 
                  stVal + cumsum(dif_Epos_k2[(itstrt+1):numT]))
  Einfected_k = Epositive_k - stdat6$death7 - Erecovered_k
  
  out = list(Epositive_k = Epositive_k, Einfected_k = Einfected_k)
  return(out)
}

##

initialConfirmRate <- function(stdat6, IFcoef, itod, dtor){
  
  
  #### EstInfected, EstRecovered (initial)  
  
  # initial estimates
  # death7 => Epositive, Erecovered
  numT = nrow(stdat6)
  tmp = stdat6$death7[(itod+1):numT] * IFcoef
  stdat6$Epositive = c(tmp, rep(NA, itod))
  stdat6$Epositive[1] = max(stdat6$Epositive[1], stdat6$positive[1])
  
  tmp = stdat6$death7[2:(numT-dtor)] * (IFcoef - 1)
  stdat6$Erecovered = c(rep(0, (dtor+1)), tmp)
  
  stdat6$dif_Erecovered = c(NA, diff(stdat6$Erecovered))
  stdat6$dif_Epositive = c(NA, diff(stdat6$Epositive))
  # bdry condition
  stdat6$dif_Epositive = pmax(stdat6$dif_Epositive, stdat6$dif_pos7)
  stVal0 = stdat6$Epositive[1]
  stdat6$Epositive = c(stVal0, stVal0 + cumsum(stdat6$dif_Epositive[2:numT]))
  
  # for the case when confirmed cases are too large (09/21)
  shiftedEpos = c( rep(0, itod), stdat6$Epositive[1:(numT-itod)] )
  shiftedEposMinusDeath = pmax( shiftedEpos - stdat6$death7, stdat6$death7 * (IFcoef - 1) )
  tmpRec = c(rep(0, dtor), shiftedEposMinusDeath[1:(numT-dtor)] ) 
  stdat6$Erecovered = pmax( stdat6$Erecovered, tmpRec )
  stdat6$dif_Erecovered = c(NA, diff(stdat6$Erecovered))
  
  
  r2 = stdat6$dif_Epositive / pmax(stdat6$dif_pos7, 0.5)    # 0.5, instead of 0
  r2[!is.finite(r2)] = NA
  
  weight0 = rep(1,7)/7
  #weight14 = rep(1, 14)/14
  #stdat6$r2_14 = filter(r2, weight14, 'convolution', sides = 1)
  stdat6$r2_7 = naFilter(r2, weight0)
  prj_newCases = stdat6$r2_7[numT-itod] * stdat6$dif_pos7[(numT-itod+1):numT]
  stdat6$dif_Epositive[(numT-itod+1):numT] = prj_newCases
  stdat6$Epositive[(numT-itod+1):numT] = stdat6$Epositive[numT-itod] + cumsum(prj_newCases)
  
  stdat6$Einfected = stdat6$Epositive - stdat6$death7 - stdat6$Erecovered
  
  # boundary condition
  stdat6$Einfected = pmax(stdat6$Einfected, 3*stdat6$dif_Epositive, na.rm=T)
  stdat6$Einfected = pmax(stdat6$Einfected, 1, na.rm=T)
  
  # 05/31, bdry condition adjustment
  tmp = c(NA, diff(stdat6$Einfected))
  tmp2 = tmp + stdat6$dif_dea7 + stdat6$dif_Erecovered
  init_dif_Epos2 = pmax(stdat6$dif_Epositive, tmp2)
  stVal = stdat6$Epositive[1]
  if (!is.finite(stVal)) stVal = 0
  stdat6$Epositive = c(stVal, stVal + cumsum(init_dif_Epos2[2:numT]))
  stdat6$Einfected = stdat6$Epositive - stdat6$death7 - stdat6$Erecovered
  stdat6$dif_Epositive = c(NA, diff(stdat6$Epositive))
  

  ## Maximization of confirmRate7, reRate7
  # wList..
  
  confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive, 1/7) * 100
  confirmRate[stdat6$dif_pos7 == 0] = NaN
  # from Galveston example, correct initial condition
  itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
  confirmRate[1:(itstrt)] = NaN
  #stdat6$confirmRate7 = naFilter_2sides(confirmRate, weight16)
  #stdat6$confirmRate_pre = confirmRate  
  
  return(confirmRate)
  
}



## 
# 05/08/2020

describe_df <- function(curDate, stname, stdat4, Xpop, outPath, outPath2,
                              ifr0, ifrL, ifrU, itod = 18, dtor = 7){
  
  
  ## pop adjustment: stdat5 xx
  stdat5 = data.frame(date = stdat4$mydates)
  #stdat5 = cbind(stdat5, round(stdat4[, 2:5] * (1/Xpop * 10^6), 6))
  stdat5 = cbind(stdat5, stdat4[, 2:5] )
  
  # infected: total cases - recovered - death
  stdat5$infected = stdat5$positive - stdat5$recovered - stdat5$death
  
  stdat5$dif_cases = c(NA, diff(stdat5$positive))  
  stdat5$dif_tests = c(NA, diff(stdat5$numTests))  
  stdat5$dif_recovered = c(NA, diff(stdat5$recovered))  
  stdat5$dif_death = c(NA, diff(stdat5$death))  
  stdat5$dif_infected = c(NA, diff(stdat5$infected)) 
  
  
  ## Daily new confirmed cases (2davg)
  stdat6 = data.frame(date = stdat4$mydates)
  stdat6$positive = stdat5$positive
  stdat6$death = stdat5$death
  stdat6$recovered = stdat5$recovered
  stdat6$dif_cases = stdat5$dif_cases
  stdat6$dif_death = stdat5$dif_death
  
  stdat6$positive2 = filter(stdat5$positive, c(1,1)/2, 'convolution', sides=1)
  stdat6$numTests2 = filter(stdat5$numTests, c(1,1)/2, 'convolution', sides=1)
  stdat6$recovered2 = filter(stdat5$recovered, c(1,1)/2, 'convolution', sides=1)
  stdat6$death2 = filter(stdat5$death, c(1,1)/2, 'convolution', sides=1)
  stdat6$infected2 = filter(stdat5$infected, c(1,1)/2, 'convolution', sides=1)
  
  stdat6$dif_cases2 = c(NA, diff(stdat6$positive2))
  stdat6$dif_tests2 = c(NA, diff(stdat6$numTests2))
  stdat6$dif_recovered2 = c(NA, diff(stdat6$recovered2))
  stdat6$dif_death2 = c(NA, diff(stdat6$death2))
  stdat6$dif_infected2 = c(NA, diff(stdat6$infected2))
  
  # mov avg (7dMA): due to week-seasonality or weekend effect
  weight0 = rep(1,7)/7
  stdat6$dif_cases7 = filter(stdat6$dif_cases2, weight0, 'convolution', sides=1)  
  stdat6$dif_tests7 = filter(stdat6$dif_tests2, weight0, 'convolution', sides=1)  
  stdat6$dif_recovered7 = filter(stdat6$dif_recovered2, weight0, 'convolution', sides=1)  
  stdat6$dif_death7 = filter(stdat6$dif_death2, weight0, 'convolution', sides=1)  
  stdat6$dif_infected7 = filter(stdat6$dif_infected2, weight0, "convolution", sides=1)  
  
  ## 05/14, 06/02 >=0
  stdat6$positive7 = naFilter(stdat5$positive, weight0)
  stdat6$death7 = naFilter(stdat5$death, weight0)
  stdat6$recovered7 = naFilter(stdat5$recovered, weight0)
  stdat6$dif_pos7 = pmax(0, c(NA, diff(stdat6$positive7)))
  stdat6$dif_dea7 = pmax(0, c(NA, diff(stdat6$death7)))
  stdat6$dif_rec7 = pmax(0, c(NA, diff(stdat6$recovered7)))
  # 08/02
  stdat6$numTests7 = filter(stdat5$numTests, weight0, 'convolution', sides=1)
  stdat6$dif_tst7 = c(NA, diff(stdat6$numTests7))
  
  
  ## 7d tests
  #stdat6$numTests7 = naFilter(stdat5$numTests, weight0)
  #stdat6$dif_numTests7 = pmax(0, c(NA, diff(stdat6$numTests7)))
  #stdat6$testPos = stdat5$dif_cases / stdat5$dif_tests * 100
  #stdat6$testPos7 = filter(stdat6$testPos, weight0, 'convolution', sides=1) 
  
  #testsFreq = dif_tests / dif_cases
  #stdat6$pctTestPos2 = stdat6$dif_cases2 / stdat6$dif_tests2 * 100
  #stdat6$pctTestPos7 = stdat6$dif_cases7 / stdat6$dif_tests7 * 100
  #stdat6$pctTestPos77 = stdat6$dif_pos7 / stdat6$dif_numTests7 * 100
  stdat6$testPosRate7 = stdat6$dif_pos7 / stdat6$dif_tst7 * 100
  
  
  # GR of positive
  #tmp = diff(stdat5$positive) / stdat5$positive[-nrow(stdat5)] * 100
  #tmp[is.infinite(tmp) | (stdat5$positive[-nrow(stdat5)] < 1)] <- NA
  #stdat5$gr_positive = round(c(NA, tmp), 8)
  #stdat5$gr2_positive = filter(stdat5$gr_positive, c(1,1)/2, "convolution", sides=1)
  #stdat5$gr7_positive = filter(stdat5$gr_positive, weight0, "convolution", sides=1)
  
  
  #plot(stdat5$gr_infected2, type='l', ylim=c(0, 50))
  #lines(stdat5$gr_dif_cases2, col=2)
  #lines(stdat5$gr_positive2, col=3)
  #lines(stdat5$gr2_positive2, col=4)
  
  # inc_dif_cases2
  tmp = diff(stdat6$dif_cases2) 
  stdat6$inc_dif_cases2 = c(NA, tmp)
  tmp = diff(stdat6$dif_death2)  
  stdat6$inc_dif_death2 = c(NA, tmp) 
  tryCatch( {
    cout = ccf(stdat6$inc_dif_cases2, stdat6$inc_dif_death2, na.action = na.pass, 
             lag.max = 20, ylab='Cross-correlation', 
             main = 'CCF of diff of new cases and new deaths')
    ind = which.max(cout$acf)
    lag0 = cout$lag[ind[1]]
    lines(x=c(lag0, lag0), y=c(0, cout$acf[ind[1]]), col=2 )
    #
    ff1name = paste0(stname, '_ff1.png')
    png(file.path(outPath, ff1name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    cout = ccf(stdat6$inc_dif_cases2, stdat6$inc_dif_death2, na.action = na.pass, 
               lag.max = 20, ylab='Cross-correlation', 
               main = 'CCF of diff of new cases and new deaths')
    lines(x=c(lag0, lag0), y=c(0, cout$acf[ind[1]]), col=2 )
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }, error = function(e) print(paste0('CCF the second Error for ', stname)))
  
  # 7d ma
  stdat6$inc7_dif_cases2 = filter(stdat6$inc_dif_cases2, weight0, 'convolution', sides=1) 
  stdat6$inc7_dif_death2 = filter(stdat6$inc_dif_death2, weight0, 'convolution', sides=1) 
  
  # grwoth rate, gr_dif_death2, threshold: 10, 1
  tmp = diff(stdat6$dif_cases2) / stdat6$dif_cases2[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$dif_cases2[-nrow(stdat6)] < 20)] <- NA
  stdat6$gr_dif_cases2 = round(c(NA, tmp), 4)
  tmp = diff(stdat6$dif_death2) / stdat6$dif_death2[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$dif_death2[-nrow(stdat6)] < 4)] <- NA
  stdat6$gr_dif_death2 = round(c(NA, tmp), 4)
  #
  stdat6$gr7_dif_cases2 = filter(stdat6$gr_dif_cases2, weight0, 'convolution', sides=1) 
  stdat6$gr7_dif_death2 = filter(stdat6$gr_dif_death2, weight0, 'convolution', sides=1) 
  
  # lagged NDNCC
  tmp = c(rep(NA, 7), stdat6$dif_cases7[1:(nrow(stdat6)-7)])
  tmp[is.infinite(tmp)] = NA
  stdat6$laggedNDNCC7 = stdat6$dif_death7 / tmp * 100
  
  # TD/TCC, ratios etc
  stdat6$CFRts = stdat6$death2 / stdat6$positive2 * 100
  stdat6$CRRts = stdat6$recovered2 / stdat6$positive2 * 100
  
  # IR
  tmp = c(NA, stdat6$infected2[1:(nrow(stdat6)-1)])
  stdat6$dailyDeathPerInfected2 = stdat6$dif_death2 / tmp * 100
  tmp = c(NA, stdat6$infected2[1:(nrow(stdat6)-1)])
  stdat6$dailyRecoveredPerInfected2 = stdat6$dif_recovered2 / tmp * 100
  tmp = c(NA, stdat6$infected2[1:(nrow(stdat6)-1)])
  stdat6$NCCPerInfected2 = stdat6$dif_cases2 / tmp * 100
  
  # IR 7d
  tmp = c(NA, stdat6$infected2[1:(nrow(stdat6)-1)])
  stdat6$dailyDeath7PerInfected2 = stdat6$dif_death7 / tmp * 100
  tmp = c(NA, stdat6$infected2[1:(nrow(stdat6)-1)])
  stdat6$dailyRecovered7PerInfected2 = stdat6$dif_recovered7 / tmp * 100
  tmp = c(NA, stdat6$infected2[1:(nrow(stdat6)-1)])
  stdat6$NCC7PerInfected2 = stdat6$dif_cases7 / tmp * 100
  
  ####  Growth Rates  ######!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # GR of Infected, gr_infected2
  tmp = diff(stdat6$infected2) / stdat6$infected2[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$infected2[-nrow(stdat6)] < 20)] <- NA
  mad0 = mad(tmp, na.rm=T); m0 = median(tmp, na.rm=T)
  tmp[ which((tmp - m0)/mad0 > 30) ] <- NA              # 30*sigma outlier
  stdat6$gr_infected2 = round(c(NA, tmp), 8)
  stdat6$gr7_infected2 = filter(stdat6$gr_infected2, weight0, "convolution", sides=1)
  stdat6$dif_gr_infected2 = c(NA, diff(stdat6$gr_infected2))
  stdat6$dif7_gr_infected2 = filter(stdat6$dif_gr_infected2, weight0, "convolution", sides=1)
  
  # gr_positive2
  tmp = diff(stdat6$positive2) / stdat6$positive2[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$positive2[-nrow(stdat6)] < 20)] <- NA
  mad0 = mad(tmp, na.rm=T); m0 = median(tmp, na.rm=T)
  tmp[ which((tmp - m0)/mad0 > 30) ] <- NA              # 30*sigma outlier
  stdat6$gr_positive2 = round(c(NA, tmp), 8)
  stdat6$gr7_positive2 = filter(stdat6$gr_positive2, weight0, "convolution", sides=1)
  stdat6$dif_gr_positive2 = c(NA, diff(stdat6$gr_positive2))
  stdat6$dif7_gr_positive2 = filter(stdat6$dif_gr_positive2, weight0, "convolution", sides=1)
  
  #  gr_death2
  tmp = diff(stdat6$death2) / stdat6$death2[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$death2[-nrow(stdat6)] < 4)] <- NA
  mad0 = mad(tmp, na.rm=T); m0 = median(tmp, na.rm=T)
  tmp[ which((tmp - m0)/mad0 > 30) ] <- NA              # 30*sigma outlier
  stdat6$gr_death2 = round(c(NA, tmp), 8)
  stdat6$gr7_death2 = filter(stdat6$gr_death2, weight0, "convolution", sides=1)
  stdat6$dif_gr_death2 = c(NA, diff(stdat6$gr_death2))
  stdat6$dif7_gr_death2 = filter(stdat6$dif_gr_death2, weight0, "convolution", sides=1)
  
  #  gr_recovered2
  tmp = diff(stdat6$recovered2) / stdat6$recovered2[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$recovered2[-nrow(stdat6)] < 1)] <- NA
  mad0 = mad(tmp, na.rm=T); m0 = median(tmp, na.rm=T)
  tmp[ which((tmp - m0)/mad0 > 30) ] <- NA              # 30*sigma outlier
  stdat6$gr_recovered2 = round(c(NA, tmp), 8)
  stdat6$gr7_recovered2 = filter(stdat6$gr_recovered2, weight0, "convolution", sides=1)
  stdat6$dif_gr_recovered2 = c(NA, diff(stdat6$gr_recovered2))
  stdat6$dif7_gr_recovered2 = filter(stdat6$dif_gr_recovered2, weight0, "convolution", sides=1)
  
  
  
  ######################  new figures 
  # palette
  mpal = wes_palette("BottleRocket2", 5)
  
  # pctTestPos7 -> testPos7
  
  
  # dual y axes
  refVal = rev(stdat6$positive)[1]
  refVal2 = rev(stdat6$death)[1]
  ax2 = 0.8 * mean(stdat6$positive, na.rm=T) / mean(stdat6$death, na.rm=T)
  f3 <- ggplot(data = stdat6) +
    geom_line(aes(x = date, y = positive, color='Total cases'), alpha=0.5, size=1.5) +
    geom_point(aes(x = date, y = positive), alpha=0.5, size=1.5, color=mpal[2]) +
    geom_line(aes(x = date, y = death * ax2, color='Total deaths' ), alpha=0.5, size=1.5) +
    geom_point(aes(x = date, y = death * ax2 ), alpha=0.5, size=1.5, color=mpal[3]) +
    #geom_line(aes(x = date, y = recovered2), alpha=0.5, size=1.5, color=mpal[1]) +
    #geom_point(aes(x = date, y = recovered2), alpha=0.5, size=2, color=mpal[1]) +
    
    scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
    theme_bw() +
    scale_y_continuous(name = "No. of Cases", labels = function(x) comma(x,0),
                       sec.axis = sec_axis(~. / ax2, name='No. of Deaths', 
                                           labels = function(x) comma(x,0))) +
    scale_color_manual("", values = c('Total cases' = mpal[2], 'Total deaths' = mpal[3]))
  
  f3out <- f3 + 
    labs(title = 'Total Confirmed Cases/Deaths', x='', y='', 
         subtitle = paste0(stname, ', as of ', curDate, 
                           ':  ', comma(refVal,0), ' / ', comma(refVal2,0))) + 
    theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
          plot.subtitle = element_text(size=rel(1.3)),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.text.y = element_text(color = mpal[2]), 
          axis.text.y.right = element_text(color = mpal[3]), 
          axis.title = element_text(size=rel(1.5)), 
          axis.title.y = element_text(color=mpal[2]), 
          axis.title.y.right = element_text(color=mpal[3]),
          legend.position = 'top', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1)))
  print(f3out)
  f3name = paste0(stname, '_totalConfirmed.png')
  png(file.path(outPath, f3name), width=8, height=4, units = "in", res=300)
  Sys.sleep(2)
  print(f3out)
  Sys.sleep(2)
  dev.off()
  Sys.sleep(2)
  
  
  
  # dual y axes
  refVal = rev(stdat6$dif_pos7)[1]
  refVal2 = rev(stdat6$dif_dea7)[1]
  ax2 = 0.8 * mean(stdat6$dif_pos7, na.rm=T) / mean(stdat6$dif_dea7, na.rm=T)
  f5 <- ggplot(data = stdat6) +
    geom_line(aes(x = date, y = dif_pos7, color='New cases'), alpha=0.5, size=1.5) +
    geom_point(aes(x = date, y = dif_pos7), alpha=0.5, size=1.5, color=mpal[2]) +
    geom_line(aes(x = date, y = dif_dea7 * ax2, color='New deaths' ), alpha=0.5, size=1.5) +
    geom_point(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.5, size=1.5, color=mpal[3]) +
    #geom_line(aes(x = date, y = dif_recovered7), alpha=0.5, size=1.5, color=mpal[1]) +
    #geom_point(aes(x = date, y = dif_recovered7), alpha=0.5, size=2, color=mpal[1]) +
    
    scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
    theme_bw() +
    scale_y_continuous(name = "No. of Cases", labels = function(x) comma(x,0),
                       sec.axis = sec_axis(~. / ax2, name='No. of Deaths', 
                                           labels = function(x) comma(x,0))) +
    scale_color_manual("", values = c('New cases' = mpal[2], 'New deaths' = mpal[3]))
  
  f5out <- f5 + 
    labs(title = 'Daily Confirmed New Cases/Deaths*', x='', y='', 
         subtitle = paste0(stname, ', as of ', curDate, 
                           ':  ', comma(refVal,0), ' / ', comma(refVal2,0)), 
         caption = '*7-day rolling average') +  
    theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
          plot.subtitle = element_text(size=rel(1.3)),
          plot.caption = element_text(size=rel(1), hjust = 0, margin = margin(-10,0,0,0) ), 
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.text.y = element_text(color = mpal[2]), 
          axis.text.y.right = element_text(color = mpal[3]), 
          axis.title = element_text(size=rel(1.5)), 
          axis.title.y = element_text(color=mpal[2]), 
          axis.title.y.right = element_text(color=mpal[3]),
          legend.position = 'top', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1)))
  print(f5out)
  f5name = paste0(stname, '_newCases7d.png')
  png(file.path(outPath, f5name), width=8, height=4, units = "in", res=300)
  Sys.sleep(2)
  print(f5out)
  Sys.sleep(2)
  dev.off()
  Sys.sleep(2)
  
  
  # new cases not smoothed to monitor the today's number, f55, daily not 7d
  refVal = rev(stdat6$dif_cases)[1]
  refVal2 = rev(stdat6$dif_death)[1]
  #ax2 = 0.8 * mean(stdat6$dif_cases, na.rm=T) / mean(stdat6$dif_death, na.rm=T)
  ax2 = 1 * max(stdat6$dif_cases, na.rm=T) / max(stdat6$dif_death, na.rm=T)
  f55 <- ggplot(data = stdat6) +
    geom_line(aes(x = date, y = dif_cases, color='New cases'), alpha=0.5, size=1.5) +
    geom_point(aes(x = date, y = dif_cases), alpha=0.5, size=1.5, color=mpal[2]) +
    geom_line(aes(x = date, y = dif_death * ax2, color='New deaths' ), alpha=0.5, size=1.5) +
    geom_point(aes(x = date, y = dif_death * ax2 ), alpha=0.5, size=1.5, color=mpal[3]) +
    #geom_line(aes(x = date, y = dif_recovered7), alpha=0.5, size=1.5, color=mpal[1]) +
    #geom_point(aes(x = date, y = dif_recovered7), alpha=0.5, size=2, color=mpal[1]) +
    
    scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
    theme_bw() +
    scale_y_continuous(name = "No. of Cases", labels = function(x) comma(x,0),
                       sec.axis = sec_axis(~. / ax2, name='No. of Deaths', 
                                           labels = function(x) comma(x,0))) +
    scale_color_manual("", values = c('New cases' = mpal[2], 'New deaths' = mpal[3]))
  
  f55out <- f55 + 
    labs(title = 'Daily New Cases/Deaths', x='', y='', 
         subtitle = paste0(stname, ', as of ', curDate, 
                           ':  ', comma(refVal,0), ' / ', comma(refVal2,0))) +  
    theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
          plot.subtitle = element_text(size=rel(1.3)),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.text.y = element_text(color = mpal[2]), 
          axis.text.y.right = element_text(color = mpal[3]), 
          axis.title = element_text(size=rel(1.5)), 
          axis.title.y = element_text(color=mpal[2]), 
          axis.title.y.right = element_text(color=mpal[3]),
          legend.position = 'top', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1)))
  print(f55out)
  f55name = paste0(stname, '_newCases.png')
  png(file.path(outPath, f55name), width=8, height=4, units = "in", res=300)
  Sys.sleep(2)
  print(f55out)
  Sys.sleep(2)
  dev.off()
  Sys.sleep(2)
  
  
  # infected
  
  ####
  #### EstInfected, EstRecovered (initial) 
  
  IFcoef = 1/ifr0
  IFcoefU = 1/ifrL
  IFcoefL = 1/ifrU
  
  # initial estimates
  # death7 => Epositive, Erecovered
  numT = nrow(stdat6)
  tmp = stdat6$death7[(itod+1):numT] * IFcoef
  stdat6$Epositive = c(tmp, rep(NA, itod))
  stdat6$Epositive[1] = max(stdat6$Epositive[1], stdat5$positive[1])
  
  tmp = stdat6$death7[2:(numT-dtor)] * (IFcoef - 1)
  stdat6$Erecovered = c(rep(0, (dtor+1)), tmp)
  
  stdat6$dif_Erecovered = c(NA, diff(stdat6$Erecovered))
  stdat6$dif_Epositive = c(NA, diff(stdat6$Epositive))
  # bdry condition
  stdat6$dif_Epositive = pmax(stdat6$dif_Epositive, stdat6$dif_pos7)
  stVal0 = stdat6$Epositive[1]
  stdat6$Epositive = c(stVal0, stVal0 + cumsum(stdat6$dif_Epositive[2:numT]))
  
  # for the case when confirmed cases are too large (09/21)
  shiftedEpos = c( rep(0, itod), stdat6$Epositive[1:(numT-itod)] )
  shiftedEposMinusDeath = pmax( shiftedEpos - stdat6$death7, stdat6$death7 * (IFcoef - 1) )
  tmpRec = c(rep(0, dtor), shiftedEposMinusDeath[1:(numT-dtor)] ) 
  stdat6$Erecovered = pmax( stdat6$Erecovered, tmpRec )
  stdat6$dif_Erecovered = c(NA, diff(stdat6$Erecovered))
  
  
  r2 = stdat6$dif_Epositive / pmax(stdat6$dif_pos7, 0.5)    # 0.5, instead of 0
  r2[!is.finite(r2)] = NA
  
  #weight14 = rep(1, 14)/14
  #stdat6$r2_14 = filter(r2, weight14, 'convolution', sides = 1)
  stdat6$r2_7 = naFilter(r2, weight0)
  prj_newCases = stdat6$r2_7[numT-itod] * stdat6$dif_pos7[(numT-itod+1):numT]
  stdat6$dif_Epositive[(numT-itod+1):numT] = prj_newCases
  stdat6$Epositive[(numT-itod+1):numT] = stdat6$Epositive[numT-itod] + cumsum(prj_newCases)
  
  stdat6$Einfected = stdat6$Epositive - stdat6$death7 - stdat6$Erecovered
  
  # boundary condition
  stdat6$Einfected = pmax(stdat6$Einfected, 3*stdat6$dif_Epositive, na.rm=T)
  stdat6$Einfected = pmax(stdat6$Einfected, 1, na.rm=T)
  
  # 05/31, bdry condition adjustment
  tmp = c(NA, diff(stdat6$Einfected))
  tmp2 = tmp + stdat6$dif_dea7 + stdat6$dif_Erecovered
  init_dif_Epos2 = pmax(stdat6$dif_Epositive, tmp2)
  stVal = stdat6$Epositive[1]
  if (!is.finite(stVal)) stVal = 0
  stdat6$Epositive = c(stVal, stVal + cumsum(init_dif_Epos2[2:numT]))
  stdat6$Einfected = stdat6$Epositive - stdat6$death7 - stdat6$Erecovered
  stdat6$dif_Epositive = c(NA, diff(stdat6$Epositive))
  
  
  # GR Einfected
  tmp = diff(stdat6$Einfected) / stdat6$Einfected[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$Einfected[-nrow(stdat6)] < 20)] <- NA
  stdat6$gr_Einfected = round(c(NA, tmp), 4)
  
  stdat6$gr7_Einfected = filter(stdat6$gr_Einfected, weight0, 'convolution', sides=1) 
  stdat6$dif_gr_Einfected = c(NA, diff(stdat6$gr_Einfected))
  
  stdat6$dif7_gr_Einfected = filter(stdat6$dif_gr_Einfected, weight0, 
                                    'convolution', sides = 1)
  
  tmp = diff(stdat6$Erecovered) / stdat6$Erecovered[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$Erecovered[-nrow(stdat6)] < 20)] <- NA
  stdat6$gr_Erecovered = round(c(NA, tmp), 4)
  
  stdat6$gr7_Erecovered = filter(stdat6$gr_Erecovered, weight0, 'convolution', sides=1) 
  stdat6$dif_gr_Erecovered = c(NA, diff(stdat6$gr_Erecovered))
  
  stdat6$dif7_gr_Erecovered = filter(stdat6$dif_gr_Erecovered, weight0, 
                                     'convolution', sides = 1)
  ##
  stdat6$dif7_Epositive = filter(stdat6$dif_Epositive, weight0, 'convolution', sides=1) 
  stdat6$dif7_Erecovered = filter(stdat6$dif_Erecovered, weight0, 'convolution', sides=1) 
  
  ####
  # new IR, 06/02 dif_dea7>0
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$dailyDeathPerEinfected = stdat6$dif_dea7 / tmp * 100
  stdat6$dailyDeathPerEinfected[stdat6$dif_dea7 == 0] = NA
  
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$dailyRecoveredPerEinfected = stdat6$dif_rec7 / tmp * 100
  stdat6$dailyErecoveredPerEinfected = stdat6$dif_Erecovered / tmp * 100
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$NCC7PerEinfected = stdat6$dif_pos7 / tmp * 100
  stdat6$NCC7PerEinfected[stdat6$dif_pos7 == 0] = NaN
  # 08/01
  itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
  stdat6$NCC7PerEinfected[1:(itstrt)] = NaN
  
  # 08/10
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$difEposPerEinfected = stdat6$dif_Epositive / tmp * 100
  stdat6$difEposPerEinfected[stdat6$dif_pos7 == 0] = NaN
  itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
  stdat6$difEposPerEinfected[1:(itstrt)] = NaN
  
  
  ## 1d Case-Fatality-Rate, ECase-Fatality-Rate
  id_gr10 = which(stdat6$death > 10)[1]
  stdat6$CFR = stdat6$death / stdat6$positive * 100
  stdat6$CFR[!is.finite(stdat6$CFR)] = NaN
  stdat6$CFR[c(1:(id_gr10-1))] = NA
  
  stdat6$CFR7 = stdat6$death7 / stdat6$positive7 * 100
  stdat6$CFR7[!is.finite(stdat6$CFR7)] = NaN
  stdat6$CFR7[c(1:(id_gr10-1))] = NA
  
  stdat6$ECFR = stdat6$death / stdat6$Epositive * 100
  stdat6$ECFR[!is.finite(stdat6$ECFR)] = NaN
  stdat6$ECFR[c(1:(id_gr10-1))] = NA
  
  stdat6$ECFR7 = stdat6$death7 / stdat6$Epositive * 100
  stdat6$ECFR7[!is.finite(stdat6$ECFR7)] = NaN
  stdat6$ECFR7[c(1:(id_gr10-1))] = NA
  
  # 08/02
  tmp = c(rep(NA, itod), stdat6$Epositive[1:(numT - itod)])
  stdat6$EIFRadj = stdat6$death / tmp * 100
  
  tmp = c(rep(NA, itod), stdat6$positive[1:(numT - itod)])
  stdat6$CFRadj = stdat6$death7 / tmp * 100
  stdat6$CFRadj[c(1:(id_gr10-1))] = NaN
  
  ##  05/14 update
  ## Maximization of confirmRate7, reRate7
  # wList..
  
  confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive, 1/7) * 100
  confirmRate[stdat6$dif_pos7 == 0] = NaN
  # from Galveston example, correct initial condition
  itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
  confirmRate[1:(itstrt)] = NaN
  #stdat6$confirmRate7 = naFilter_2sides(confirmRate, weight16)
  stdat6$confirmRate_pre = confirmRate  
  
  #stdat6$recRate7 = naFilter_2sides(stdat6$dailyErecoveredPerEinfected, weight16)
  #
  
  # infected, g3
  {
    ax2 = 0.8 * mean(stdat6$Einfected, na.rm=T) / mean(stdat6$dif_Epositive, na.rm=T)
    g3 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = Einfected, color='Est-currently infected cases'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = Einfected), alpha=0.5, size=1.5, color='purple3') +
      geom_line(aes(x = date, y = dif_pos7 * ax2, color='Confirmed new cases' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_pos7 * ax2 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive * ax2, color='Est-new cases' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Epositive * ax2 ), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Erecovered * ax2, color='Est-recovered' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Erecovered * ax2 ), alpha=0.2, size=1.5, color=mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Infected Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of New Cases', 
                                             labels = function(x) comma(x,0))) +
      scale_color_manual("", breaks = c('Est-currently infected cases', 'Est-new cases', 'Est-recovered', 'Confirmed new cases'),
                         values = c('Est-currently infected cases' = 'purple3', 
                                    'Confirmed new cases' = mpal[2], 
                                    'Est-new cases' = mpal[2], 
                                    'Est-recovered' = mpal[1])) +
      guides(color = guide_legend(override.aes = list(alpha = c(.5, .2, .2, .5)))) # alphabetic order
    
    
    g3out <- g3 + 
      labs(title = 'Initial Estimated Time Courses', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[2]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[2]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(g3out)
    g3name = paste0(stname, '_g3.png')
    png(file.path(outPath, g3name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(g3out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  # infected vs new death, g4
  {
    refVal = rev(stdat6$Einfected)[1]
    ax2 = 0.8 * mean(stdat6$Einfected, na.rm=T) / mean(stdat6$dif_dea7, na.rm=T)
    g4 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = Einfected), alpha=0.5, size=1.5, color='purple3') +
      geom_point(aes(x = date, y = Einfected), alpha=0.5, size=2, color='purple3') +
      geom_line(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.5, size=2, color=mpal[3]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Infected Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of New Deaths', 
                                             labels = function(x) comma(x,0)))
    g4out <- g4 + 
      labs(title = 'Initial Est-Current Infections/New Deaths', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  ', comma(refVal,0))) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(g4out)
    g4name = paste0(stname, '_g4.png')
    png(file.path(outPath, g4name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(g4out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  # daily death per Einfected, g1
  {
    refLineVal = rev(stdat6$dailyDeathPerEinfected)[1]
    refLineVal2 = rev(stdat6$dailyErecoveredPerEinfected)[1]
    ax2 = 0.8 * mean(stdat6$dailyDeathPerEinfected, na.rm=T) / mean(stdat6$dailyErecoveredPerEinfected, na.rm=T)
    #
    mDR = mean(stdat6$dailyDeathPerEinfected, na.rm=T)
    sdDR = sd(stdat6$dailyDeathPerEinfected, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    g1 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = dailyDeathPerEinfected), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = dailyDeathPerEinfected), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      geom_line(aes(x = date, y = dailyErecoveredPerEinfected * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = dailyErecoveredPerEinfected * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", 
                         sec.axis = sec_axis(~. / ax2, name='Percentage'))
    g1out <- g1 + 
      coord_cartesian(ylim = c(0, max(stdat6$dailyDeathPerEinfected, na.rm=T) * 1.2)) +
      labs(title = 'Daily Rate of Deaths/Est-Recovered Per Est-Infections', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ', CV of death rates: ', round(cvDR), '%')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(g1out)
    g1name = paste0(stname, '_g1.png')
    png(file.path(outPath, g1name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(g1out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  # confirmRate7, g2
  {
    refLineVal = rev(stdat6$confirmRate_pre)[1]
    refLineVal2 = rev(stdat6$NCC7PerEinfected)[1]
    ax2 = 0.7 * mean(stdat6$confirmRate_pre, na.rm=T) / mean(stdat6$NCC7PerEinfected, na.rm=T)
    #
    mDR = mean(stdat6$confirmRate_pre, na.rm=T)
    sdDR = sd(stdat6$confirmRate_pre, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    g2 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = confirmRate_pre, color='Ascertainment rate'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = confirmRate_pre), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      geom_line(aes(x = date, y = NCC7PerEinfected * ax2, color='Detected transmission rate' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = NCC7PerEinfected * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", 
                         sec.axis = sec_axis(~. / ax2, name='Percentage')) +
      scale_color_manual("", values = c('Ascertainment rate' = mpal[3], 
                                        'Detected transmission rate' = mpal[1]))
    
    g2out <- g2 + 
      coord_cartesian(ylim = c(0, max(stdat6$confirmRate_pre, na.rm=T) * 1.2)) +
      labs(title = '(Initial) Ascertainment Rates and Detected Transmission Rates', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(g2out)
    g2name = paste0(stname, '_g2.png')
    png(file.path(outPath, g2name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(g2out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  # Est-transmission rate, g21
  { 
    refLineVal2 = rev(stdat6$difEposPerEinfected)[1] 
    ##
    g21 <- ggplot(data = stdat6) + 
      geom_line(aes(x = date, y = difEposPerEinfected  , color='Est-transmission rate' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = difEposPerEinfected  ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 , color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage") +
      scale_color_manual("", values = c('Est-transmission rate' = mpal[1]))
    
    g21out <- g21 + 
      coord_cartesian(ylim = c(0, max(stdat6$difEposPerEinfected, na.rm=T) * 1.2)) +
      labs(title = '(Initial) Est-Transmission Rates', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  ', round(refLineVal2,1), ' %')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[1]), 
            #axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[1]), 
            #axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(g21out)
    g21name = paste0(stname, '_g21.png')
    png(file.path(outPath, g21name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(g21out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # CFR, ECFR, g5
  {
    refLineVal = rev(stdat6$ECFR)[1]
    refLineVal2 = rev(stdat6$CFR)[1]
    refLineVal3 = rev(stdat6$EIFRadj)[1]
    refLineVal4 = rev(stdat6$CFRadj)[1]
    ax2 = 0.7 * mean(stdat6$ECFR, na.rm=T) / mean(stdat6$CFR, na.rm=T)
    ymax0 = max(c(stdat6$ECFR, stdat6$EIFRadj), na.rm=T)
    #
    mDR = mean(stdat6$ECFR, na.rm=T)
    sdDR = sd(stdat6$ECFR, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    
    g5 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = ECFR), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = ECFR), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      geom_line(aes(x = date, y = EIFRadj), alpha=0.5, size=1.5, color='purple3') +
      geom_point(aes(x = date, y = EIFRadj), alpha=0.5, size=1.5, color='purple3') +
      geom_hline(yintercept = refLineVal3, color = 'purple3') +
      geom_line(aes(x = date, y = CFR * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = CFR * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      geom_line(aes(x = date, y = CFRadj * ax2 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = CFRadj * ax2 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_hline(yintercept = refLineVal4 * ax2, color = mpal[2]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", 
                         sec.axis = sec_axis(~. / ax2, name='Percentage'))
    g5out <- g5 + 
      coord_cartesian(ylim = c(0, ymax0 * 1.2)) +
      labs(title = 'Estimated/Observed Case Fatality Rate', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(g5out)
    g5name = paste0(stname, '_g5.png')
    png(file.path(outPath, g5name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(g5out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  ####
  #### Optimization with ma window sizes
  
  #spar0 = c(seq(-1, 2, by=0.3) )
  spar01 = c(seq(0, 2, by=0.4) )
  spar02 = c(seq(1, 2, by=0.05) )
  # outMat
  #outMat = matrix(NA, 6, length(spar0))
  outMat = array(NA, dim = c(6, length(spar01), length(spar02)))
  
for (i in 1:length(spar01)){
  for (j in 1:length(spar02)){ 
    
    confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive, 1/7) * 100
    confirmRate[stdat6$dif_pos7 == 0] = NaN
    # from Galveston example, correct initial condition
    itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
    confirmRate[1:(itstrt)] = NaN
    #stdat6$confirmRate7 = naFilter_2sides(confirmRate, wList[[j]])
    
    stdat6$confirmRate_pre = confirmRate  
    stdat6$Einfected_pre = stdat6$Einfected
    stdat6$Epositive_pre = stdat6$Epositive
    # plot(confirmRate, type='l')
    
    # EM iterations (itermax = 5000)
    EMout <- EM_confirmRate_k(stdat6, spar01[i], spar02[j])
    stdat6 = EMout$stdat6
    k = EMout$k
    cvDR = EMout$cvDR
    
    # lines(stdat6$confirmRate_k, col=2, lwd=4)
    
    if (k >= 2000){
      print('== Not converged ==')
      print(k)
      print(rev(stdat6$Einfected_pre)[1])
      print(rev(stdat6$Einfected_k)[1])
      print(cvDR)
      print(c(spar01[i], spar02[j]))
      print('======')
      # 09/02 revised
      #stdat6$Epositive_k = rep(NA, numT)
      #stdat6$Einfected_k = rep(NA, numT)
    }  
    
    # outdf
    TEC18dea7_Rate =  stdat6$death7[numT] / stdat6$Epositive_k[numT - itod] * 100
    TECdea7_Rate =  stdat6$death7[numT] / stdat6$Epositive_k[numT] * 100
    
    outdf = data.frame(lastEinfected_k = stdat6$Einfected_k[numT], 
                       mDR=NA, sdDR=NA, cvDR=cvDR, 
                       TECdea7_Rate=TECdea7_Rate, TEC18dea7_Rate=TEC18dea7_Rate)
    outMat[, i, j] = cbind(as.numeric(outdf))
    
  }
}
  
  
  #### Optimization: end
  
  ## select min of cvDR
  ##
  convId = !is.finite(outMat[1, , ])
  cvDRvec = outMat[4, , ]
  cvDRvec[convId] = NA
  #ind = which.min(cvDRvec)
  ind = which(cvDRvec == min(cvDRvec), arr.ind = TRUE)
  #spar00 = spar0[ind]
  # bug fix, 09/25
  spar00 = c(spar01[ind[nrow(ind),1]], spar02[ind[nrow(ind),2]])
  
  
  # again for plots with the selected parm
  confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive, 1/7) * 100
  confirmRate[stdat6$dif_pos7 == 0] = NaN
  # from Galveston example, correct initial condition
  itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
  confirmRate[1:(itstrt)] = NaN
  #stdat6$confirmRate7 = naFilter_2sides(confirmRate, wList0)
  ## Iteration-2
  stdat6$confirmRate_pre = confirmRate  
  stdat6$Einfected_pre = stdat6$Einfected
  stdat6$Epositive_pre = stdat6$Epositive
  
  # spar00
  # EM iterations (itermax = 5000)
  EMout <- EM_confirmRate_k(stdat6, spar00[1], spar00[2])
  stdat6 = EMout$stdat6
  k = EMout$k
  cvDR = EMout$cvDR
  
  # lines(stdat6$confirmRate_k, col=2, lwd=4)
  
  
  print('==')
  print(stname)
  print(k)
  print(rev(stdat6$Einfected_pre)[1])
  print(rev(stdat6$Einfected_k)[1])
  print(cvDR)
  print(spar00)
  print('====')
  stdat6$dif_Epositive_k = c(NA, diff(stdat6$Epositive_k))
  
  ## 
  ## 2nd Round
    
    stdat6$dif_Epositive_1st = stdat6$dif_Epositive_k
    stdat6$Einfected_1st = stdat6$Einfected_k
    stdat6$Epositive_1st = stdat6$Epositive_k
    
    #spar0 = c(seq(-1, 2, by=0.3) )
    spar01 = c(seq(0, 2, by=0.4) )
    spar02 = c(seq(1, 2, by=0.05) )
    # outMat2
    #outMat2 = matrix(NA, 6, length(spar0))
    outMat2 = array(NA, dim = c(6, length(spar01), length(spar02)))
    
  for (i in 1:length(spar01)){
    for (j in 1:length(spar02)){ 
      
      confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive_1st, 1/7) * 100
      confirmRate[stdat6$dif_pos7 == 0] = NaN
      # from Galveston example, correct initial condition
      itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
      confirmRate[1:(itstrt)] = NaN
      #stdat6$confirmRate7 = naFilter_2sides(confirmRate, wList[[j]])
      ## Iteration-2
      #stdat6$confirmRate_pre = stdat6$confirmRate7  
      stdat6$confirmRate_pre = confirmRate  
      stdat6$Einfected_pre = stdat6$Einfected_1st
      stdat6$Epositive_pre = stdat6$Epositive_1st
      # plot(confirmRate, type='l')
      
      # EM iterations (itermax = 5000)
      EMout <- EM_confirmRate_k(stdat6, spar01[i], spar02[j])
      stdat6 = EMout$stdat6
      k = EMout$k
      cvDR = EMout$cvDR
      
      # lines(stdat6$confirmRate_k, col=2, lwd=4)
      
      if (k >= 2000){
        print('== Not converged ==')
        print(k)
        print(rev(stdat6$Einfected_pre)[1])
        print(rev(stdat6$Einfected_k)[1])
        print(cvDR)
        #print(spar0[j])
        print(c(spar01[i], spar02[j]))
        print('======')
        # 09/02 revised
        #stdat6$Epositive_k = rep(NA, numT)
        #stdat6$Einfected_k = rep(NA, numT)
      }  
      
      # outdf
      TEC18dea7_Rate =  stdat6$death7[numT] / stdat6$Epositive_k[numT - itod] * 100
      TECdea7_Rate =  stdat6$death7[numT] / stdat6$Epositive_k[numT] * 100
      
      outdf = data.frame(lastEinfected_k = stdat6$Einfected_k[numT], 
                         mDR=NA, sdDR=NA, cvDR=cvDR, 
                         TECdea7_Rate=TECdea7_Rate, TEC18dea7_Rate=TEC18dea7_Rate)
      outMat2[, i, j] = cbind(as.numeric(outdf))
      
    }
  }
    
    
    ## select min of cvDR
    ##
    convId = !is.finite(outMat2[1, , ])
    cvDRvec = outMat2[4, , ]
    cvDRvec[convId] = NA 
    #ind = which.min(cvDRvec)
    ind = which(cvDRvec == min(cvDRvec), arr.ind = TRUE)
    #spar00 = spar0[ind]
    spar00 = c(spar01[ind[nrow(ind),1]], spar02[ind[nrow(ind),2]])
    
    
    # again for plots with the selected parm
    confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive_1st, 1/7) * 100
    confirmRate[stdat6$dif_pos7 == 0] = NaN
    # from Galveston example, correct initial condition
    itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
    confirmRate[1:(itstrt)] = NaN
    #stdat6$confirmRate7 = naFilter_2sides(confirmRate, wList0)
    ## Iteration-2
    stdat6$confirmRate_pre = confirmRate  
    stdat6$Einfected_pre = stdat6$Einfected_1st
    stdat6$Epositive_pre = stdat6$Epositive_1st
    
    # spar00
    # EM iterations
    EMout <- EM_confirmRate_k(stdat6, spar00[1], spar00[2])
    stdat6 = EMout$stdat6
    k = EMout$k
    cvDR = EMout$cvDR
    
    # lines(stdat6$confirmRate_k, col=4, lwd=4)
    
    
    print('==')
    print(stname)
    print(k)
    print(rev(stdat6$Einfected_pre)[1])
    print(rev(stdat6$Einfected_k)[1])
    print(cvDR)
    print(spar00)
    print('====')
    stdat6$dif_Epositive_k = c(NA, diff(stdat6$Epositive_k))
    
    # 08/10
    tmp = c(NA, stdat6$Einfected_k[1:(nrow(stdat6)-1)])
    stdat6$difEposPerEinfected_k = stdat6$dif_Epositive_k / tmp * 100
    stdat6$difEposPerEinfected_k[stdat6$dif_pos7 == 0] = NaN
    itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
    stdat6$difEposPerEinfected_k[1:(itstrt)] = NaN
  
  ##
  ## 2nd Round: end
    
  ##  
  ## 10/09 update: run EM with finer grids when the fit is poor. 
  ##
  if (FALSE){
    # re-optimization 
    spar01 = c(seq(0, 2, by=0.4) )
    spar02 = c(seq(1.01, 2, by=0.01) ) 
    outMat = array(NA, dim = c(6, length(spar01), length(spar02)))
    
    for (i in 1:length(spar01)){
      for (j in 1:length(spar02)){ 
        
        confirmRate = stdat6$dif_pos7 / pmax(stdat6$dif_Epositive, 1/7) * 100
        confirmRate[stdat6$dif_pos7 == 0] = NaN
        # from Galveston example, correct initial condition
        itstrt = which(stdat6$positive7 < stdat6$Epositive)[1]
        confirmRate[1:(itstrt)] = NaN
        #stdat6$confirmRate7 = naFilter_2sides(confirmRate, wList[[j]])
        
        stdat6$confirmRate_pre = confirmRate  
        stdat6$Einfected_pre = stdat6$Einfected
        stdat6$Epositive_pre = stdat6$Epositive
        # plot(confirmRate, type='l')
        
        # EM iterations (itermax = 5000)
        EMout <- EM_confirmRate_k(stdat6, spar01[i], spar02[j])
        stdat6 = EMout$stdat6
        k = EMout$k
        cvDR = EMout$cvDR
        
        # lines(stdat6$confirmRate_k, col=2, lwd=4)
        
        if (k >= 2000){
          print('== Not converged ==')
          print(k)
          print(rev(stdat6$Einfected_pre)[1])
          print(rev(stdat6$Einfected_k)[1])
          print(cvDR)
          print(c(spar01[i], spar02[j]))
          print('======')
          # 09/02 revised
          #stdat6$Epositive_k = rep(NA, numT)
          #stdat6$Einfected_k = rep(NA, numT)
        }  
        
        # outdf
        TEC18dea7_Rate =  stdat6$death7[numT] / stdat6$Epositive_k[numT - itod] * 100
        TECdea7_Rate =  stdat6$death7[numT] / stdat6$Epositive_k[numT] * 100
        
        outdf = data.frame(lastEinfected_k = stdat6$Einfected_k[numT], 
                           mDR=NA, sdDR=NA, cvDR=cvDR, 
                           TECdea7_Rate=TECdea7_Rate, TEC18dea7_Rate=TEC18dea7_Rate)
        outMat[, i, j] = cbind(as.numeric(outdf))
        
      }
    }
    
    ## select min of cvDR 
    convId = !is.finite(outMat[1, , ])
    cvDRvec = outMat[4, , ]
    cvDRvec[convId] = NA
    #ind = which.min(cvDRvec)
    ind = which(cvDRvec == min(cvDRvec), arr.ind = TRUE)
    #spar00 = spar0[ind]
    # bug fix, 09/25
    spar00 = c(spar01[ind[nrow(ind),1]], spar02[ind[nrow(ind),2]])
    
  }
    
    
    
    
  
  ##  
  ## Confidence interval
  # 09/22
  confirmRate_L0 = initialConfirmRate(stdat6, IFcoefU, itod, dtor)
  (mL = mean(confirmRate_L0, na.rm = T)); (m_k = mean(stdat6$confirmRate_k, na.rm = T))
  stdat6$confirmRate_k_L = pmax(stdat6$confirmRate_k * min(mL/m_k, 0.95), 0.5)
  
  # lines(stdat6$confirmRate_k_L, col=2); lines(stdat6$confirmRate_k_U, col=3)
    
  #stdat6$confirmRate_k_L = pmax(stdat6$confirmRate_k * ifrL/ifr0, 0.5)
  stdat6$confirmRate_k_U = pmin(stdat6$confirmRate_k * ifrU/ifr0, 99)
  
  #tmp = stdat6$death7[2:(numT-dtor)] * (IFcoefU - 1)
  #stdat6$Erecovered_U = c(rep(0, (dtor+1)), tmp)
  #tmp2 = stdat6$death7[2:(numT-dtor)] * (IFcoefL - 1)
  #stdat6$Erecovered_L = c(rep(0, (dtor+1)), tmp2)
  
  outU = CI_Estep(stdat6, stdat6$confirmRate_k_L, IFcoefU, itod, dtor)
  stdat6$Epositive_U = outU$Epositive_k
  stdat6$Einfected_U = pmax(outU$Einfected_k, stdat6$Einfected_k)
  #
  outL = CI_Estep(stdat6, stdat6$confirmRate_k_U, IFcoefL, itod, dtor)
  stdat6$Epositive_L = outL$Epositive_k
  stdat6$Einfected_L = pmin(outL$Einfected_k, stdat6$Einfected_k)
  
  stdat6$dif_Epositive_U = c(NA, diff(stdat6$Epositive_U))
  stdat6$dif_Epositive_L = c(NA, diff(stdat6$Epositive_L))
  #
  stdat6$dif_Epositive_L = pmin(stdat6$dif_Epositive_L, stdat6$dif_Epositive_k)
  stVal0 = stdat6$Epositive_L[1]
  stdat6$Epositive_L = c(stVal0, stVal0 + cumsum(stdat6$dif_Epositive_L[2:numT]))
  
  ## CI: end
  
  
  # confirmRate7, d2
  {
    refLineVal = rev(stdat6$confirmRate_k)[1]
    refLineVal2 = rev(stdat6$NCC7PerEinfected_k)[1]
    ax2 = 0.5 * mean(stdat6$confirmRate_k, na.rm=T) / mean(stdat6$NCC7PerEinfected_k, na.rm=T)
    #
    mDR = mean(stdat6$confirmRate_k, na.rm=T)
    sdDR = sd(stdat6$confirmRate_k, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    gg2 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = confirmRate_k, color='Ascertainment rate'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = confirmRate_k), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      geom_line(aes(x = date, y = NCC7PerEinfected_k * ax2, color='Detected transmission rate' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = NCC7PerEinfected_k * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", 
                         sec.axis = sec_axis(~. / ax2, name='Percentage')) +
      scale_color_manual("", values = c('Ascertainment rate' = mpal[3], 
                                        'Detected transmission rate' = mpal[1]))
    
    gg2out <- gg2 + 
      coord_cartesian(ylim = c(0, max(stdat6$confirmRate_k, na.rm=T) * 1.2)) +
      labs(title = '(Converged) Ascertainment Rates and Detected Transmission Rates', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ', splinePar: [', spar00[1], ' ', spar00[2], ']')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg2out)
    gg2name = paste0(stname, '_cnvd_AscertainmentRate.png')
    png(file.path(outPath, gg2name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg2out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }      
  # confirmRate7 only, ggg2
  {
    refLineVal = rev(stdat6$confirmRate_k)[1]
    ##
    ggg2 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = confirmRate_k, color='Ascertainment rate'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = confirmRate_k), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage") +
      scale_color_manual("", values = c('Ascertainment rate' = mpal[3]))
    
    ggg2out <- ggg2 + 
      coord_cartesian(ylim = c(0, max(stdat6$confirmRate_k, na.rm=T) * 1.2)) +
      labs(title = 'Converged Ascertainment Rates', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(ggg2out)
    ggg2name = paste0(stname, '_cnvd_AscertainmentRate2.png')
    png(file.path(outPath, ggg2name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(ggg2out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  } 
  
  
  # Est-transmission rate, gg21
  { 
    refLineVal2 = rev(stdat6$difEposPerEinfected_k)[1] 
    ##
    gg21 <- ggplot(data = stdat6) + 
      geom_line(aes(x = date, y = difEposPerEinfected_k  , color='Est-transmission rate' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = difEposPerEinfected_k  ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 , color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage") +
      scale_color_manual("", values = c('Est-transmission rate' = mpal[1]))
    
    gg21out <- gg21 + 
      coord_cartesian(ylim = c(0, max(stdat6$difEposPerEinfected_k, na.rm=T) * 1.2)) +
      labs(title = '(Converged) Est-Transmission Rates', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  ', round(refLineVal2,1), ' %')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[1]), 
            #axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[1]), 
            #axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg21out)
    gg21name = paste0(stname, '_estTransmissionRate.png')
    png(file.path(outPath, gg21name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg21out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # daily death per Einfected, gg1
  # 08/23 rstudio
  tmpftn <- function(){
  {
    refLineVal = rev(stdat6$dailyDeathPerEinfected_k)[1]
    refLineVal2 = rev(stdat6$dailyRecoveredPerEinfected_k)[1]
    ax2 = 0.8 * mean(stdat6$dailyDeathPerEinfected_k, na.rm=T) / mean(stdat6$dailyRecoveredPerEinfected_k, na.rm=T)
    #
    mDR = mean(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    sdDR = sd(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    gg1 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = dailyDeathPerEinfected_k), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = dailyDeathPerEinfected_k), alpha=0.5, size=2, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      geom_line(aes(x = date, y = dailyRecoveredPerEinfected_k * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = dailyRecoveredPerEinfected_k * ax2 ), alpha=0.5, size=2, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", 
                         sec.axis = sec_axis(~. / ax2, name='Percentage'))
    gg1out <- gg1 + 
      coord_cartesian(ylim = c(0, max(stdat6$dailyDeathPerEinfected_k, na.rm=T) * 1.2)) +
      labs(title = 'Daily Rate of Deaths/Recovered Per Est-Infections', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ', CV of death rates: ', round(cvDR), '%')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg1out)
    gg1name = paste0(stname, '_gg1.png')
    png(file.path(outPath, gg1name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg1out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  }
  
  
  # daily death per Einfected, gg11
  {
    refLineVal = rev(stdat6$dailyDeathPerEinfected_k)[1]
    refLineVal2 = rev(stdat6$dailyRecoveredPerEinfected_k)[1]
    ax2 = 0.8 * mean(stdat6$dailyDeathPerEinfected_k, na.rm=T) / mean(stdat6$dailyRecoveredPerEinfected_k, na.rm=T)
    #
    mDR = mean(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    sdDR = sd(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    gg11 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = dailyDeathPerEinfected_k), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = dailyDeathPerEinfected_k), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      #geom_line(aes(x = date, y = dailyRecoveredPerEinfected_k * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      #geom_point(aes(x = date, y = dailyRecoveredPerEinfected_k * ax2 ), alpha=0.5, size=2, color=mpal[1]) +
      #geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage")
    
    gg11out <- gg11 + 
      coord_cartesian(ylim = c(0, max(stdat6$dailyDeathPerEinfected_k, na.rm=T) * 1.2)) +
      labs(title = 'Daily Rate of Deaths Per Est-Infections', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ', CV of death rates: ', round(cvDR), '%')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg11out)
    gg11name = paste0(stname, '_cnvdDeathRate.png')
    png(file.path(outPath, gg11name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg11out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # infected, d3
  dif_Epos_k3 = c(NA, diff(stdat6$Epositive_k))
  {
    refVal = rev(stdat6$Einfected_k)[1]
    refVal2 = rev(dif_Epos_k3)[1]
    ax2 = 0.8 * mean(stdat6$Einfected_k, na.rm=T) / mean(dif_Epos_k3, na.rm=T)
    gg3 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = Einfected_k, color='Est-currently infected cases'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = Einfected_k), alpha=0.5, size=1.5, color='purple3') +
      geom_line(aes(x = date, y = dif_pos7 * ax2, color='Confirmed new cases' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_pos7 * ax2 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epos_k3 * ax2, color='Est-new cases' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Epos_k3 * ax2 ), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Erecovered * ax2, color='Est-recovered' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Erecovered * ax2 ), alpha=0.2, size=1.5, color=mpal[1]) +
      #geom_line(aes(x = date, y = Einfected_U), alpha=0.5, size=1, color = 'purple2') +
      #geom_line(aes(x = date, y = Einfected_L), alpha=0.5, size=1, color = 'purple2') +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Infected Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of New Cases', 
                                             labels = function(x) comma(x,0))) +
      scale_color_manual("", breaks = c('Est-currently infected cases', 'Est-new cases', 'Est-recovered', 'Confirmed new cases'),
                         values = c('Est-currently infected cases' = 'purple3', 
                                    'Confirmed new cases' = mpal[2], 
                                    'Est-new cases' = mpal[2], 
                                    'Est-recovered' = mpal[1])) +
      guides(color = guide_legend(override.aes = list(alpha = c(.5, .2, .2, .5)))) # alphabetic order
    
    gg3out <- gg3 + 
      labs(title = 'Estimated Currently Infected/New Cases', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ': ', comma(refVal,0), ' / ', comma(refVal2,0))) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[2]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[2]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg3out)
    gg3name = paste0(stname, '_estInfectionsNewCases.png')
    png(file.path(outPath, gg3name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg3out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # infected vs new death, gg4
  {
    refVal = rev(stdat6$Einfected_k)[1]
    refVal2 = refVal / Xpop * 100
    ax2 = 0.8 * mean(stdat6$Einfected_k, na.rm=T) / mean(stdat6$dif_dea7, na.rm=T)
    refValU = rev(stdat6$Einfected_U)[1]
    refValU2 = refValU / Xpop * 100
    refValL = rev(stdat6$Einfected_L)[1]
    refValL2 = refValL / Xpop * 100
    #
    mDR = mean(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    sdDR = sd(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    cvDR = sdDR/mDR * 100
	if (cvDR < 100){
		captionCVDR = paste0('CV of daily death rates (', round(cvDR,0), '% < 100%) suggests the estimation quality is good.')
	} else {
		captionCVDR = paste0('CV of daily death rates (', round(cvDR,0), '% > 100%) suggests the estimation quality is poor.')
	}
    
    gg4 <- ggplot(data = stdat6) +
      geom_ribbon(aes(x = date, ymax = Einfected_U, ymin = Einfected_L), fill='purple1', alpha=0.1) +
      geom_line(aes(x = date, y = Einfected_k, color='Est-currently infected cases'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = Einfected_k), alpha=0.5, size=1.5, color='purple3') +
      geom_line(aes(x = date, y = dif_dea7 * ax2, color='New deaths' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_line(aes(x = date, y = Einfected_U), alpha=0.2, size=1, color = 'purple2') +
      geom_line(aes(x = date, y = Einfected_L), alpha=0.2, size=1, color = 'purple2') +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Infected Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of New Deaths', 
                                             labels = function(x) comma(x,0))) +
      scale_color_manual("", values = c('Est-currently infected cases' = 'purple3', 
                                        'New deaths' = mpal[3]))
    
    gg4out <- gg4 + 
      labs(title = 'Estimated Currently Infected Cases*', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  \n', comma(refVal,0), ' (', comma(refVal2,2), '% of pop.) ', 
                             ' [', comma(refValL,0), '\u2013', comma(refValU,0), '] ',
                             ' [', comma(refValL2,2), '%\u2013', comma(refValU2,2), '%]'), 
           caption = paste0('*Under-reporting-adjusted number of cases which have not yet',
           ' an outcome (recovery or death) \n', captionCVDR)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            plot.caption = element_text(size = rel(0.9), hjust = 0, margin=margin(-10,0,0,0)), 
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg4out)
    gg4name = paste0(stname, '_estInfections.png')
    png(file.path(outPath, gg4name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg4out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # CI Est new cases
  #dif_Epos_k3 = c(NA, diff(stdat6$Epositive_k))
  {
    refVal = rev(stdat6$dif_Epositive_k)[1]
    refVal2 = rev(stdat6$dif_pos7)[1]
    refValU = rev(stdat6$dif_Epositive_U)[1]
    refValL = rev(stdat6$dif_Epositive_L)[1]
	#
    mDR = mean(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    sdDR = sd(stdat6$dailyDeathPerEinfected_k, na.rm=T)
    cvDR = sdDR/mDR * 100
	if (cvDR < 100){
		captionCVDR = paste0('CV of daily death rates (', round(cvDR,0), '% < 100%) suggests the estimation quality is good.')
	} else {
		captionCVDR = paste0('CV of daily death rates (', round(cvDR,0), '% > 100%) suggests the estimation quality is poor.')
	}
    
    
    #ax2 = 0.8 * mean(stdat6$Einfected_k, na.rm=T) / mean(dif_Epos_k3, na.rm=T)
    gg33 <- ggplot(data = stdat6) +
      geom_ribbon(aes(x = date, ymax = dif_Epositive_U, ymin = dif_Epositive_L), fill=mpal[2], alpha=0.1) +
      geom_line(aes(x = date, y = dif_pos7, color='Confirmed new cases' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_pos7 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_k, color='Est-new cases' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Epositive_k ), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_U), alpha=0.2, size=1, color = mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_L), alpha=0.2, size=1, color = mpal[2]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of New Cases", labels = function(x) comma(x,0)) +
      scale_color_manual("", breaks = c('Est-new cases', 'Confirmed new cases'),
                         values = c('Est-new cases' = mpal[2], 
                                    'Confirmed new cases' = mpal[2])) +
      guides(color = guide_legend(override.aes = list(alpha = c(.2, .5)))) # alphabetic order
    
    gg33out <- gg33 + 
      labs(title = 'Estimated*/Confirmed New Cases', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ': ', comma(refVal,0), ' / ', comma(refVal2,0), 
                             ' [', comma(refValL,0), '\u2013', comma(refValU,0), ']'), 
           caption = paste0('*Under-reporting-adjusted number of new infected individuals on each day \n', captionCVDR)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            plot.caption = element_text(size = rel(1), hjust = 0, margin=margin(-10,0,0,0)), 
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[2]), 
            #axis.text.y.right = element_text(color = mpal[2]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            #axis.title.y.right = element_text(color=mpal[2]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg33out)
    gg33name = paste0(stname, '_NewCasesEstConfirmed.png')
    png(file.path(outPath, gg33name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg33out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }  
  
  
  ## 7d Case-Fatality-Rate, ECase-Fatality-Rate
  id_gr10 = which(stdat6$death > 10)[1] 
  
  stdat6$ECFR_k = stdat6$death / stdat6$Epositive_k * 100
  stdat6$ECFR_k[!is.finite(stdat6$ECFR_k)] = NaN
  stdat6$ECFR_k[c(1:(id_gr10-1))] = NA
  
  stdat6$ECFR7_k = stdat6$death7 / stdat6$Epositive_k * 100
  stdat6$ECFR7_k[!is.finite(stdat6$ECFR7_k)] = NaN
  stdat6$ECFR7_k[c(1:(id_gr10-1))] = NA
  
  # 08/02
  tmp = c(rep(NA, itod), stdat6$Epositive_k[1:(numT - itod)])
  stdat6$EIFRadj_k = stdat6$death / tmp * 100
  
  
  # CFR, ECFR, gg44
  {
    refLineVal = rev(stdat6$ECFR_k)[1]
    refLineVal2 = rev(stdat6$CFR)[1]
    refLineVal3 = rev(stdat6$EIFRadj_k)[1]
    refLineVal4 = rev(stdat6$CFRadj)[1]
    ax2 = 0.7 * mean(stdat6$ECFR_k, na.rm=T) / mean(stdat6$CFR, na.rm=T)
    ymax0 = max(c(stdat6$ECFR_k, stdat6$EIFRadj_k), na.rm=T)
    #
    mDR = mean(stdat6$ECFR_k, na.rm=T)
    sdDR = sd(stdat6$ECFR_k, na.rm=T)
    cvDR = sdDR/mDR * 100
    ##
    
    gg44 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = ECFR_k), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = ECFR_k), alpha=0.5, size=2, color=mpal[3]) +
      geom_hline(yintercept = refLineVal, color = mpal[3]) +
      geom_line(aes(x = date, y = EIFRadj_k), alpha=0.5, size=1.5, color='purple3') +
      geom_point(aes(x = date, y = EIFRadj_k), alpha=0.5, size=2, color='purple3') +
      geom_hline(yintercept = refLineVal3, color = 'purple3') +
      geom_line(aes(x = date, y = CFR * ax2 ), alpha=0.5, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = CFR * ax2 ), alpha=0.5, size=2, color=mpal[1]) +
      geom_hline(yintercept = refLineVal2 * ax2, color = mpal[1]) +
      geom_line(aes(x = date, y = CFRadj * ax2 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = CFRadj * ax2 ), alpha=0.5, size=2, color=mpal[2]) +
      geom_hline(yintercept = refLineVal4 * ax2, color = mpal[2]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", 
                         sec.axis = sec_axis(~. / ax2, name='Percentage'))
    gg44out <- gg44 + 
      coord_cartesian(ylim = c(0, ymax0 * 1.2)) +
      labs(title = 'Est-Infection Fatality Rate (adj)/Case Fatality Rate', 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),    
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[3]), 
            axis.text.y.right = element_text(color = mpal[1]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[3]), 
            axis.title.y.right = element_text(color=mpal[1]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg44out)
    gg44name = paste0(stname, '_gg44.png')
    png(file.path(outPath, gg44name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg44out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  print(rev(stdat6$Einfected_k)[1])
  
  ##
  ##  Recent 4 week plots of Infections, New Cases
  
  rctdat = stdat6[(numT-55):numT, ]
  
  # infected, d3
  {
    refVal = rev(rctdat$Einfected_k)[1]
    refVal2 = rev(rctdat$dif_Epositive_k)[1]
    ax2 = 0.8 * mean(rctdat$Einfected_k, na.rm=T) / mean(rctdat$dif_Epositive_k, na.rm=T)
    ggg3 <- ggplot(data = rctdat) +
      geom_line(aes(x = date, y = Einfected_k, color='Est-currently infected cases'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = Einfected_k), alpha=0.5, size=1.5, color='purple3') +
      geom_line(aes(x = date, y = dif_pos7 * ax2, color='Confirmed new cases' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_pos7 * ax2 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_k * ax2, color='Est-new cases' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Epositive_k * ax2 ), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Erecovered * ax2, color='Est-recovered' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Erecovered * ax2 ), alpha=0.2, size=1.5, color=mpal[1]) +
      #geom_line(aes(x = date, y = Einfected_U), alpha=0.5, size=1, color = 'purple2') +
      #geom_line(aes(x = date, y = Einfected_L), alpha=0.5, size=1, color = 'purple2') +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Infected Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of New Cases', 
                                             labels = function(x) comma(x,0)), limits = c(0, NA)) +
      scale_color_manual("", breaks = c('Est-currently infected cases', 'Est-new cases', 'Est-recovered', 'Confirmed new cases'),
                         values = c('Est-currently infected cases' = 'purple3', 
                                    'Confirmed new cases' = mpal[2], 
                                    'Est-new cases' = mpal[2], 
                                    'Est-recovered' = mpal[1])) +
      guides(color = guide_legend(override.aes = list(alpha = c(.5, .2, .2, .5)))) # alphabetic order
    
    ggg3out <- ggg3 + 
      labs(title = 'Estimated Currently Infected/New Cases', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ': ', comma(refVal,0), ' / ', comma(refVal2,0))) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[2]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[2]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(ggg3out)
    ggg3name = paste0(stname, '_Recent_estInfectionsNewCases.png')
    png(file.path(outPath, ggg3name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(ggg3out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # infected vs new death, gg4
  {
    refVal = rev(rctdat$Einfected_k)[1]
    refVal2 = refVal / Xpop * 100
    ax2 = 0.8 * mean(rctdat$Einfected_k, na.rm=T) / mean(rctdat$dif_dea7, na.rm=T)
    refValU = rev(rctdat$Einfected_U)[1]
    refValU2 = refValU / Xpop * 100
    refValL = rev(rctdat$Einfected_L)[1]
    refValL2 = refValL / Xpop * 100
    
    ggg4 <- ggplot(data = rctdat) +
      geom_ribbon(aes(x = date, ymax = Einfected_U, ymin = Einfected_L), fill='purple1', alpha=0.1) +
      geom_line(aes(x = date, y = Einfected_k, color='Est-currently infected cases'), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = Einfected_k), alpha=0.5, size=1.5, color='purple3') +
      geom_line(aes(x = date, y = dif_dea7 * ax2, color='New deaths' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_line(aes(x = date, y = Einfected_U), alpha=0.2, size=1, color = 'purple2') +
      geom_line(aes(x = date, y = Einfected_L), alpha=0.2, size=1, color = 'purple2') +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Infected Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of New Deaths', 
                                             labels = function(x) comma(x,0)), limits = c(0, NA)) +
      scale_color_manual("", values = c('Est-currently infected cases' = 'purple3', 
                                        'New deaths' = mpal[3]))
    
    ggg4out <- ggg4 + 
      labs(title = 'Estimated Currently Infected Cases*', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  \n', comma(refVal,0), ' (', comma(refVal2,2), '% in pop) ', 
                             ' [', comma(refValL,0), '\u2013', comma(refValU,0), '] ',
                             ' [', comma(refValL2,2), '%\u2013', comma(refValU2,2), '%]'), 
           caption = paste0('*Under-reporting-adjusted number of cases which have not yet',
                            ' an outcome (recovery or death) \n', captionCVDR)) +  
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            plot.caption = element_text(size=rel(0.9), hjust=0,margin=margin(-10,0,0,0)), 
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(ggg4out)
    ggg4name = paste0(stname, '_Recent_estInfections.png')
    png(file.path(outPath, ggg4name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(ggg4out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # CI Est new cases
  {
    refVal = rev(rctdat$dif_Epositive_k)[1]
    refVal2 = rev(rctdat$dif_pos7)[1]
    refValU = rev(rctdat$dif_Epositive_U)[1]
    refValL = rev(rctdat$dif_Epositive_L)[1]
    
    #ax2 = 0.8 * mean(stdat6$Einfected_k, na.rm=T) / mean(dif_Epos_k3, na.rm=T)
    ggg33 <- ggplot(data = rctdat) +
      geom_ribbon(aes(x = date, ymax = dif_Epositive_U, ymin = dif_Epositive_L), fill=mpal[2], alpha=0.1) +
      geom_line(aes(x = date, y = dif_pos7, color='Confirmed new cases' ), alpha=0.5, size=1.5) +
      geom_point(aes(x = date, y = dif_pos7 ), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_k, color='Est-new cases' ), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = dif_Epositive_k ), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_U), alpha=0.2, size=1, color = mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive_L), alpha=0.2, size=1, color = mpal[2]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of New Cases", labels = function(x) comma(x,0), limits = c(0, NA)) +
      scale_color_manual("", breaks = c('Est-new cases', 'Confirmed new cases'),
                         values = c('Est-new cases' = mpal[2], 
                                    'Confirmed new cases' = mpal[2])) +
      guides(color = guide_legend(override.aes = list(alpha = c(.2, .5)))) # alphabetic order
    
    ggg33out <- ggg33 + 
      labs(title = 'Estimated*/Confirmed New Cases', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ': ', comma(refVal,0), ' / ', comma(refVal2,0), 
                             ' [', comma(refValL,0), '\u2013', comma(refValU,0), ']'), 
           caption = paste0('*Under-reporting-adjusted number of new infected individuals on each day \n', captionCVDR)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)), 
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[2]), 
            #axis.text.y.right = element_text(color = mpal[2]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            #axis.title.y.right = element_text(color=mpal[2]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(ggg33out)
    ggg33name = paste0(stname, '_Recent_NewCasesEstConfirmed.png')
    png(file.path(outPath, ggg33name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(ggg33out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }    
  
  
  
  ##
  ## update from EM
  
  stdat6$Einfected = stdat6$Einfected_k
  stdat6$Epositive = stdat6$Epositive_k 
  stdat6$dif_Epositive = c(NA, diff(stdat6$Epositive))
  
  
  # GR Einfected, 06/02 (from < 20 => < 1 => < 20, 07/31)
  tmp = diff(stdat6$Einfected) / stdat6$Einfected[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$Einfected[-nrow(stdat6)] < 20)] <- NA
  stdat6$gr_Einfected = round(c(NA, tmp), 4)
  
  stdat6$gr7_Einfected = filter(stdat6$gr_Einfected, weight0, 'convolution', sides=1) 
  stdat6$dif_gr_Einfected = c(NA, diff(stdat6$gr_Einfected))
  
  stdat6$dif7_gr_Einfected = filter(stdat6$dif_gr_Einfected, weight0, 
                                    'convolution', sides = 1)
  
  tmp = diff(stdat6$Erecovered) / stdat6$Erecovered[-nrow(stdat6)] * 100
  tmp[is.infinite(tmp) | (stdat6$Erecovered[-nrow(stdat6)] < 20)] <- NA
  stdat6$gr_Erecovered = round(c(NA, tmp), 4)
  
  stdat6$gr7_Erecovered = filter(stdat6$gr_Erecovered, weight0, 'convolution', sides=1) 
  stdat6$dif_gr_Erecovered = c(NA, diff(stdat6$gr_Erecovered))
  
  stdat6$dif7_gr_Erecovered = filter(stdat6$dif_gr_Erecovered, weight0, 
                                     'convolution', sides = 1)
  ##
  stdat6$dif7_Epositive = filter(stdat6$dif_Epositive, weight0, 'convolution', sides=1) 
  stdat6$dif7_Erecovered = filter(stdat6$dif_Erecovered, weight0, 'convolution', sides=1) 
  
  ####
  # new IR
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$dailyDeathPerEinfected = stdat6$dif_dea7 / tmp * 100
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$dailyRecoveredPerEinfected = stdat6$dif_rec7 / tmp * 100
  stdat6$dailyErecoveredPerEinfected = stdat6$dif_Erecovered / tmp * 100
  tmp = c(NA, stdat6$Einfected[1:(nrow(stdat6)-1)])
  stdat6$NCC7PerEinfected = stdat6$dif_pos7 / tmp * 100
  
  
  ## 05/14
  ##
  ##
  
  grplots <- function()  {
    # gr_infected2, dif_gr_infected2
    refLineVal = rev(stdat6$gr_Einfected)[1]
    refLineVal2 = rev(stdat6$dif_gr_Einfected)[1]
    a = mean(stdat6$gr_Einfected, na.rm=T)
    #ax2 = 0.8 * a / mean(stdat6$dif_gr_infected2, na.rm=T)
    
    gg5 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = gr_Einfected), alpha=0.5, size=1.5, color='purple3') +
      geom_point(aes(x = date, y = gr_Einfected), alpha=0.5, size=2, color='purple3') +
      geom_hline(yintercept = refLineVal, color = 'purple3') +
      geom_line(aes(x = date, y = dif_gr_Einfected + a  ), alpha=0.5, size=1.5, color='green3') +
      geom_point(aes(x = date, y = dif_gr_Einfected + a   ), alpha=0.5, size=2, color='green3') +
      geom_hline(yintercept = refLineVal2 + a , color = mpal[5]) +
      geom_hline(yintercept = 0  , color = 'black') +
      geom_hline(yintercept = 0 + a  , color = 'black') +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Grwoth Rate (%)", 
                         sec.axis = sec_axis(~. - a, name='Diff of Growth Rate (%)'))
    gg5out <- gg5 + 
      #coord_cartesian(ylim = c(-10, 20)) +
      labs(title = paste('(Diff of) Growth Rate of Est-Infections,', round(refLineVal, 1), round(refLineVal2,1)),
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = 'green3'), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color='green3'),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg5out)
    gg5name = paste0(stname, '_gg5.png')
    png(file.path(outPath, gg5name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg5out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
    
    # gr_infected2, dif_gr_infected2
    refLineVal = rev(stdat6$gr7_Einfected)[1]
    refLineVal2 = rev(stdat6$dif7_gr_Einfected)[1]
    a =  mean(stdat6$gr7_Einfected, na.rm=T) #/ mean(stdat6$gr_positive2, na.rm=T)
    #b = 0.8 * sd(stdat6$dif7_gr_infected2, na.rm=T) / sd(stdat6$gr7_infected2, na.rm=T)
    
    gg6 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = gr7_Einfected), alpha=0.5, size=1.5, color='purple3') +
      geom_point(aes(x = date, y = gr7_Einfected), alpha=0.5, size=2, color='purple3') +
      geom_hline(yintercept = refLineVal, color = 'purple3') +
      geom_line(aes(x = date, y = dif7_gr_Einfected + a ), alpha=0.5, size=1.5, color='green3') +
      geom_point(aes(x = date, y = dif7_gr_Einfected + a   ), alpha=0.5, size=2, color='green3') +
      geom_hline(yintercept = refLineVal2 + a  , color = 'green3') +
      geom_hline(yintercept = 0 + a  , color = 'black') +
      geom_hline(yintercept = 0   , color = 'black') +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Grwoth Rate (%)", 
                         sec.axis = sec_axis(~. - a, name='Diff of Growth Rate (%)'))
    gg6out <- gg6 + 
      #coord_cartesian(ylim = c(-10, 20)) +
      labs(title = paste('(Diff of) Growth Rate of Est-Infections (7-day avg),', round(refLineVal, 1), round(refLineVal2,1)),
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = 'green3'), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color='green3'),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg6out)
    gg6name = paste0(stname, '_gg6.png')
    png(file.path(outPath, gg6name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg6out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
    
    
    # GRs  Einfected
    gg7 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = gr7_Einfected), alpha=0.5, size=1.5, color='purple3') +
      geom_point(aes(x = date, y = gr7_Einfected), alpha=0.5, size=2, color='purple3') +
      #geom_hline(yintercept = refLineVal, color = 'blue') +
      geom_line(aes(x = date, y = gr7_death2  ), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = gr7_death2    ), alpha=0.5, size=2, color=mpal[3]) +
      #geom_line(aes(x = date, y = gr7_Erecovered   ), alpha=0.5, size=1.5, color=mpal[1]) +
      #geom_point(aes(x = date, y = gr7_Erecovered    ), alpha=0.5, size=2, color=mpal[1]) +
      
      geom_hline(yintercept = 0  , color = 'black') + 
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "GR of Est-Infected Cases (%)", 
                         sec.axis = sec_axis(~. , name='GR of Deaths (%)'))
    gg7out <- gg7 + 
      #coord_cartesian(ylim = c(-10, 20)) +
      labs(title = paste('Growth Rates of Est-Infections/Total Deaths (7-d avg)'), 
           x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'purple3'), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.3)), 
            axis.title.y = element_text(color='purple3'), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg7out)
    gg7name = paste0(stname, '_gg7.png')
    png(file.path(outPath, gg7name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg7out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
    
    # Dif7 GRs
    gg8 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = dif7_gr_Einfected), alpha=0.5, size=1.5, color='green3') +
      geom_point(aes(x = date, y = dif7_gr_Einfected), alpha=0.5, size=2, color='green3') +
      #geom_hline(yintercept = refLineVal, color = 'blue') +
      geom_line(aes(x = date, y = dif7_gr_death2  ), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = dif7_gr_death2    ), alpha=0.5, size=2, color=mpal[3]) +
      #geom_line(aes(x = date, y = dif7_gr_Erecovered   ), alpha=0.5, size=1.5, color=mpal[1]) +
      #geom_point(aes(x = date, y = dif7_gr_Erecovered    ), alpha=0.5, size=2, color=mpal[1]) +
      
      geom_hline(yintercept = 0  , color = 'black') + 
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Diff-GR of Est-Infected Cases (%)", 
                         sec.axis = sec_axis(~. , name='Diff-GR of Deaths (%)'))
    gg8out <- gg8 + 
      #coord_cartesian(ylim = c(-5, 5)) +
      labs(title = paste('Diff of GRs of Est-Infections/Total Deaths (7-d avg),', round(refLineVal, 1), round(refLineVal2,1)),
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = 'green3'), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.3)), 
            axis.title.y = element_text(color='green3'), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(gg8out)
    gg8name = paste0(stname, '_gg8.png')
    png(file.path(outPath, gg8name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(gg8out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  # ccf
  tryCatch( {
    cout = ccf(stdat6$dif_gr_Einfected, stdat6$dif_gr_death2, na.action = na.pass, 
               lag.max = 20, ylab='Cross-correlation')
    ind = which.max(cout$acf)
    lag0 = cout$lag[ind[1]]
    lines(x=c(lag0, lag0), y=c(0, cout$acf[ind[1]]), col=2 )
    #
    f4name = paste0(stname, '_ff4.png')
    png(file.path(outPath, f4name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    cout = ccf(stdat6$dif_gr_Einfected, stdat6$dif_gr_death2, na.action = na.pass, 
               lag.max = 20, ylab='Cross-correlation', 
               main = 'CCF of dif_gr_Einfected and dif_gr_death2')
    lines(x=c(lag0, lag0), y=c(0, cout$acf[ind[1]]), col=2 )
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }, error = function(e) print(paste0('CCF the second Error for ', stname)))
  
  
  # recent GRs
  numT = nrow(stdat6)
  rctdat = stdat6[(numT-55):numT, ]
  
  gg9 <- ggplot(data = rctdat) +
    geom_line(aes(x = date, y = gr_Einfected), alpha=0.5, size=1.5, color='purple3') +
    geom_point(aes(x = date, y = gr_Einfected), alpha=0.5, size=2, color='purple3') +
    #geom_hline(yintercept = refLineVal, color = 'blue') +
    geom_line(aes(x = date, y = gr_death2  ), alpha=0.5, size=1.5, color=mpal[3]) +
    geom_point(aes(x = date, y = gr_death2    ), alpha=0.5, size=2, color=mpal[3]) +
    #geom_line(aes(x = date, y = gr_Erecovered   ), alpha=0.5, size=1.5, color=mpal[1]) +
    #geom_point(aes(x = date, y = gr_Erecovered    ), alpha=0.5, size=2, color=mpal[1]) +
    
    geom_hline(yintercept = 0  , color = 'black') + 
    
    scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
    theme_bw() +
    scale_y_continuous(name = "GR of Est-Infected Cases (%)", 
                       sec.axis = sec_axis(~. , name='GR of Deaths (%)'))
  gg9out <- gg9 + 
    #coord_cartesian(ylim = c(-10, 20)) +
    labs(title = paste('Growth Rates of Est-Infections/Total Deaths'), 
         x='', y='Percentage', 
         subtitle = paste0(stname, ', as of ', curDate)) +  
    theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
          plot.subtitle = element_text(size=rel(1.3)),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.text.y = element_text(color = 'purple3'), 
          axis.text.y.right = element_text(color = mpal[3]), 
          axis.title = element_text(size=rel(1.3)), 
          axis.title.y = element_text(color='purple3'), 
          axis.title.y.right = element_text(color=mpal[3]),
          legend.position = 'top', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1)))
  print(gg9out)
  gg9name = paste0(stname, '_gg9.png')
  png(file.path(outPath, gg9name), width=8, height=4, units = "in", res=300)
  Sys.sleep(2)
  print(gg9out)
  Sys.sleep(2)
  dev.off()
  Sys.sleep(2)
  
  
  # recent Dif GRs Einfected
  numT = nrow(stdat6)
  rctdat = stdat6[(numT-55):numT, ]
  
  gg10 <- ggplot(data = rctdat) +
    geom_line(aes(x = date, y = dif_gr_Einfected), alpha=0.5, size=1.5, color='green3') +
    geom_point(aes(x = date, y = dif_gr_Einfected), alpha=0.5, size=2, color='green3') +
    #geom_hline(yintercept = refLineVal, color = 'blue') +
    geom_line(aes(x = date, y = dif_gr_death2  ), alpha=0.5, size=1.5, color=mpal[3]) +
    geom_point(aes(x = date, y = dif_gr_death2    ), alpha=0.5, size=2, color=mpal[3]) +
    #geom_line(aes(x = date, y = dif_gr_Erecovered   ), alpha=0.5, size=1.5, color=mpal[1]) +
    #geom_point(aes(x = date, y = dif_gr_Erecovered    ), alpha=0.5, size=2, color=mpal[1]) +
    
    geom_hline(yintercept = 0  , color = 'black') + 
    
    scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
    theme_bw()+
    scale_y_continuous(name = "Diff-GR of Est-Infected Cases (%)", 
                       sec.axis = sec_axis(~. , name='Diff-GR of Deaths (%)'))
  gg10out <- gg10 + 
    #coord_cartesian(ylim = c(-10, 20)) +
    labs(title = paste('Diff of GRs of Est-Infections/Total Deaths'), 
         x='', y='Percentage', 
         subtitle = paste0(stname, ', as of ', curDate)) + 
    theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
          plot.subtitle = element_text(size=rel(1.3)),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(angle = 30, hjust = 1), 
          axis.text.y = element_text(color = 'green3'), 
          axis.text.y.right = element_text(color = mpal[3]), 
          axis.title = element_text(size=rel(1.3)), 
          axis.title.y = element_text(color='green3'), 
          axis.title.y.right = element_text(color=mpal[3]),
          legend.position = 'top', legend.title = element_text(size= rel(1)), 
          legend.text = element_text(size = rel(1)))
  print(gg10out)
  gg10name = paste0(stname, '_gg10.png')
  png(file.path(outPath, gg10name), width=8, height=4, units = "in", res=300)
  Sys.sleep(2)
  print(gg10out)
  Sys.sleep(2)
  dev.off()
  Sys.sleep(2)
  
  ## rr
  rr = rctdat$gr_Einfected
  acf(rr)
  aa = rctdat$dif_gr_Einfected
  acf(aa)
  
  
  ####
  ####  new figures 2
  ##  pie(rep(1, 7), col = rainbow(7))
  
  #mpal = rainbow(7)
  mpal = wes_palette("BottleRocket2", 5)
  
  {
    pp = 1/Xpop * 10^6 
    curTC = rev(stdat4$positive)[1]
    curDth = rev(stdat4$death)[1]
    EstTC = rev(stdat6$Epositive)[1]
    # dual y axes
    ax2 = 0.8 * mean(stdat6$positive, na.rm=T) / mean(stdat6$death, na.rm=T)
    h3 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = positive), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = positive), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = Epositive), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = Epositive ), alpha=0.2, size=1.5, color=mpal[2]) +
      
      geom_line(aes(x = date, y = death * ax2  ), alpha=0.4, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = death * ax2  ), alpha=0.4, size=1.5, color=mpal[3]) +
      geom_line(aes(x = date, y = recovered ), alpha=0.4, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = recovered ), alpha=0.4, size=1.5, color=mpal[1]) +
      geom_line(aes(x = date, y = Erecovered ), alpha=0.2, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = Erecovered ), alpha=0.2, size=1.5, color=mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of Deaths', 
                                             labels = function(x) comma(x,0)))
    h3out <- h3 + 
      labs(title = 'Estimated/Reported Total Cases and Deaths', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ', E.Cases Per-Mil: ', comma(EstTC /Xpop*10^6,0))) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[2]), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(h3out)
    h3name = paste0(stname, '_h3.png')
    png(file.path(outPath, h3name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(h3out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # dual y axes
  {
    ax2 = 0.8 * mean(stdat6$dif_pos7, na.rm=T) / mean(stdat6$dif_dea7, na.rm=T)
    h4 <- ggplot(data = stdat6) +
      geom_line(aes(x = date, y = dif_pos7), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = dif_pos7), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = dif_Epositive), alpha=0.2, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = dif_Epositive), alpha=0.2, size=1.5, color=mpal[2]) +
      
      geom_line(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.4, size=1.5, color=mpal[3]) +
      geom_point(aes(x = date, y = dif_dea7 * ax2 ), alpha=0.4, size=1.5, color=mpal[3]) +
      geom_line(aes(x = date, y = dif_rec7), alpha=0.4, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = dif_rec7), alpha=0.4, size=1.5, color=mpal[1]) +
      geom_line(aes(x = date, y = dif_Erecovered), alpha=0.2, size=1.5, color=mpal[1]) +
      geom_point(aes(x = date, y = dif_Erecovered), alpha=0.2, size=1.5, color=mpal[1]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of Deaths', 
                                             labels = function(x) comma(x,0)))
    
    h4out <- h4 + 
      labs(title = 'Estimated/Reported New Cases and Deaths', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[2]), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(h4out)
    h4name = paste0(stname, '_h4.png')
    png(file.path(outPath, h4name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(h4out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # 08/12, est-daily-prevalence
  ## 08/12, daily prevalence
  stdat6$prevalence = stdat6$Epositive / Xpop * 100
  stdat6$prevalence_L = stdat6$Epositive_L / Xpop * 100
  stdat6$prevalence_U = pmin(stdat6$Epositive_U / Xpop * 100, 99)
  
  { 
    refVal = rev(stdat6$prevalence)[1]
    refVal1 = rev(stdat6$prevalence_L)[1]
    refVal2 = rev(stdat6$prevalence_U)[1]
    h5 <- ggplot(data = stdat6) +
      geom_ribbon(aes(x = date, ymax = prevalence_U, ymin = prevalence_L), fill=mpal[2], alpha=0.1) +
      geom_line(aes(x = date, y = prevalence), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = prevalence), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = prevalence_U), alpha=0.1, size=1.5, color=mpal[2]) +
      geom_point(aes(x = date, y = prevalence_L), alpha=0.1, size=1.5, color=mpal[2]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "Percentage", labels = function(x) comma(x,0) )
    
    h5out <- h5 + 
      labs(title = 'Estimated Cumulative Incidence', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  ', round(refVal,1), '%', 
                             ' [', round(refVal1,1), '%\u2013', comma(refVal2,1), '%]' )) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.7)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[2]), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(h5out)
    h5name = paste0(stname, '_estCumIncidence.png')
    png(file.path(outPath, h5name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(h5out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  # total case, 08/20
  
  {
    pp = 1/Xpop * 10^6 
    curTC = rev(stdat4$positive)[1]
    curDth = rev(stdat4$death)[1]
    EstTC = rev(stdat6$Epositive)[1]
    refValL = rev(stdat6$Epositive_L)[1]
    refValU = rev(stdat6$Epositive_U)[1]
    # dual y axes
    ax2 = 0.6 * mean(stdat6$Epositive, na.rm=T) / mean(stdat6$death, na.rm=T)
    h6 <- ggplot(data = stdat6) +
      geom_ribbon(aes(x = date, ymax = Epositive_U, ymin = Epositive_L), fill=mpal[2], alpha=0.1) +
      geom_line(aes(x = date, y = Epositive_U), alpha=0.2, size=1, color = mpal[2]) +
      geom_line(aes(x = date, y = Epositive_L), alpha=0.2, size=1, color = mpal[2]) +
      
      geom_line(aes(x = date, y = positive, color='Confirmed total cases'), alpha=0.4, size=1.5) +
      geom_point(aes(x = date, y = positive), alpha=0.4, size=1.5, color=mpal[2]) +
      geom_line(aes(x = date, y = Epositive, color='Est-total cases'), alpha=0.2, size=1.5) +
      geom_point(aes(x = date, y = Epositive ), alpha=0.2, size=1.5, color=mpal[2]) +
      
      geom_line(aes(x = date, y = death * ax2, color='Total deaths'  ), alpha=0.4, size=1.5) +
      geom_point(aes(x = date, y = death * ax2  ), alpha=0.4, size=1.5, color=mpal[3]) +
      
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() +
      scale_y_continuous(name = "No. of Cases", labels = function(x) comma(x,0),
                         sec.axis = sec_axis(~. / ax2, name='No. of Deaths', 
                                             labels = function(x) comma(x,0))) + 
      scale_color_manual("", breaks = c('Total deaths', 'Confirmed total cases', 'Est-total cases'),
                         values = c('Total deaths' = mpal[3], 
                                    'Confirmed total cases' = mpal[2], 
                                    'Est-total cases' = mpal[2])) +
      guides(color = guide_legend(override.aes = list(alpha = c(.4, .4, .2))))
    
    h6out <- h6 + 
      labs(title = 'Total Deaths and Confirmed/Estimated* Total Cases', x='', y='', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  \n', comma(curDth,0), ' / ', comma(curTC,0), 
                             ' / ', comma(EstTC,0), 
                             ' [', comma(refValL,0), '\u2013', comma(refValU,0), ']'), 
           caption = '*Under-reporting-adjusted number of cumulative cases') + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
            plot.subtitle = element_text(size=rel(1.2)),
            plot.caption = element_text(size=rel(1), hjust = 0, margin = margin(-10,0,0,0) ), 
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.text.y = element_text(color = mpal[2]), 
            axis.text.y.right = element_text(color = mpal[3]), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(h6out)
    h6name = paste0(stname, '_estTotalCases.png')
    png(file.path(outPath, h6name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(h6out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
  
  ####
  ##  Output
  ####
  
  stdat6$cumIncidence = stdat6$prevalence
  stdat6$cumIncidence_L = stdat6$prevalence_L
  stdat6$cumIncidence_U = stdat6$prevalence_U
  
  varNames = c('date', 'positive', 'death', 'recovered', 'positive7', 'death7', 'recovered7', 
               'dif_pos7', 'dif_dea7', 'dif_rec7', 'testPosRate7', 'Einfected', 'Epositive', 
               'dif_Epositive', 'dif_Erecovered', 'Einfected_U', 'Einfected_L', 
               'Epositive_U', 'Epositive_L', 'confirmRate_k_U', 'confirmRate_k_L',
               'dif_Epositive_U', 'dif_Epositive_L', 'CFR', 'CFRadj', 'ECFR_k', 
               'EIFRadj_k', 'confirmRate_k', 'NCC7PerEinfected_k', 
               'cumIncidence', 'cumIncidence_U', 'cumIncidence_L', 'difEposPerEinfected_k')
  outDat = stdat6[varNames]
  outDat$ascertainmentRate = stdat6$confirmRate_k
  outDat$docuTransmissionRate = stdat6$NCC7PerEinfected_k
  
  # csv output
  outname1 = paste0(stname, '_outputDF.csv')
  fname1 = file.path(outPath, outname1)
  write.csv(outDat, fname1, row.names = FALSE)
  
  ##
  ## output_current
  #listoffiles = list.files(path = outPath)
  if (length(list.files(path = outPath, pattern = '*testPositiveRate*')) != 0) {
    testPosname = list.files(path = outPath, pattern = '*testPositiveRate*')
    file.copy(from= file.path(outPath, testPosname), to=outPath2, overwrite = T, recursive = T)
  }
  
  listoffiles = as.character()
  listoffiles[1] = list.files(path = outPath, pattern = '*totalConfirmed*')
  listoffiles[2:3] = list.files(path = outPath, pattern = '*newCases*')
  listoffiles[4] = list.files(path = outPath, pattern = '*cnvd_AscertainmentRate*')
  listoffiles[5] = list.files(path = outPath, pattern = '*cnvdDeathRate*')
  listoffiles[6:7] = list.files(path = outPath, pattern = '*NewCasesEstConfirmed*')
  listoffiles[8:9] = list.files(path = outPath, pattern = '*estInfections.png')
  listoffiles[10:11] = list.files(path = outPath, pattern = '*estInfectionsNewCases*')
  listoffiles[12] = list.files(path = outPath, pattern = '*estCumIncidence*') 
  listoffiles[13] = list.files(path = outPath, pattern = '*estTransmissionRate*')
  listoffiles[14] = list.files(path = outPath, pattern = '*estTotalCases*')
  
  file.copy(from= file.path(outPath, listoffiles), to=outPath2, overwrite = T, recursive = T)

  return(outDat)
}


#### EOF
