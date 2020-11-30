## COVID-19: Estimating the Size of Latent Actual Infectious Population
## Jungsik Noh, UTSW, Dallas, TX 


cvd_state_LatentInfections = function(curDate, stname, covidtrackingDat, 
                                      stpopulationData, ifr0, ifrL, ifrU){
  #options(bitmapType = 'cairo')
  # (no pop adj.)
  ## X population: USstatesPopulation_USCensusBureau_simplified 
  data(state)
  myStAbb = c(state.abb, 'DC')
  myStName = c(state.name, 'District of Columbia')
  
  stInd = which(myStAbb == stname)
  if (length(stInd) == 0){ return() }
  
  outPath = file.path(getwd(), 'output', 'states', stname, 
                      paste0(curDate))
  if (!dir.exists(outPath)) dir.create(outPath, recursive = T)
  outPath2 = file.path(getwd(), 'output', 'states_current')
  if (!dir.exists(outPath2)) dir.create(outPath2, recursive = T)
  
  
  stfullname = myStName[stInd]
  stfullname2 = paste0('.', stfullname)
  #stpopulationData = read.csv('USstatesPopulation_USCensusBureau_simplified.csv')
  stInd2 = which(stpopulationData$States == stfullname2)
  Xpop = stpopulationData$Est2019_Population[stInd2]
  
  #
  head(covidtrackingDat) 
  class(covidtrackingDat)
  names(covidtrackingDat)
  head(covidtrackingDat$date)
  
  stdat1 = covidtrackingDat[covidtrackingDat$state == stname, ]
  stdat2 = stdat1[nrow(stdat1):1, ]
  stdat3 = data.frame(date = stdat2$date, positive = stdat2$positive,
              numTests = stdat2$totalTestResults, recovered = stdat2$recovered,  
              death = stdat2$death)
  #colnames(stdat3) = c('date', stname, 'numTests')
  #if (!all(is.na(stdat3$recovered))) {
    stdat3$recovered[is.na(stdat3$recovered)] = 0
    if (all(stdat3$recovered == 0)) {stdat3$recovered = NaN}
  #}
  stdat3$death[is.na(stdat3$death)] = 0
  stdat3$positive[is.na(stdat3$positive)] = 0
  stdat3$numTests[is.na(stdat3$numTests)] = 0
  
  
  # preprocessing: make sure non-decreasing TS
  # all(cummax(stdat3$positive) == stdat3$positive)
  stdat3$positive = cummax(stdat3$positive)
  stdat3$death = cummax(stdat3$death)
  stdat3$numTests = cummax(stdat3$numTests)
  stdat3$recovered = cummax(stdat3$recovered)
  
  
  # data since 20200314 ->0305 xx
  #iddeath = which(stdat3$death > 0)[1]
  #id0 = max(1, iddeath + 2)
  id0 = which(stdat3$date == '20200313')
  
  ids = seq(from=id0, to = nrow(stdat3), by = 1)
  stdat4 = stdat3[ids, ]
  #stdat4 = stdat3
  #
  date1 = stdat4$date
  date_mmdd = date1 %% 10000
  date_mm = date_mmdd %/% 100
  date_dd = date_mmdd %% 100 
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
  
  # 08/03
  weight0 = rep(1,7)/7
  stdat4$positive7 = naFilter(stdat4$positive, weight0)
  stdat4$dif_pos7 = pmax(0, c(NA, diff(stdat4$positive7)))
  stdat4$numTests7 = filter(stdat4$numTests, weight0, 'convolution', sides=1)
  stdat4$dif_tst7 = c(NA, diff(stdat4$numTests7))
  
  stdat4$testPosRate7 = stdat4$dif_pos7 / stdat4$dif_tst7 * 100
  
  # pctTestPos7 -> testPos7 -> testPosRate7
  {
    mpal = wes_palette("BottleRocket2", 5)
    refLineVal = rev(stdat4$testPosRate7)[1]
    f2 <- ggplot(data = stdat4) +
      geom_line(aes(x = mydates, y = testPosRate7), alpha=0.5, size=1.5, color=mpal[2]) +
      geom_point(aes(x = mydates, y = testPosRate7), alpha=0.5, size=1.5, color=mpal[2]) +
      #geom_line(aes(x = date, y = testPosRate7 ), alpha=0.5, size=1.5, color=mpal[3]) +
      #geom_point(aes(x = date, y = testPosRate7), alpha=0.5, size=1.5, color=mpal[3]) +
      geom_hline(yintercept = refLineVal) +
      scale_x_date(date_labels = "%b%d", date_minor_breaks = "1 week", date_breaks = "1 week") +
      theme_bw() #+
    #scale_y_continuous(name = "No. of Cases", 
    #                   sec.axis = sec_axis(~. / ax2, name='No. of Deaths'))
    f2out <- f2 + 
      coord_cartesian(ylim = c(0, max(30, max(stdat4$testPosRate7, na.rm=T)))) +
      labs(title = paste0('Test Positive Rate (7d avg)'), 
           x='', y='Percentage', 
           subtitle = paste0(stname, ', as of ', curDate, 
                             ':  ', round(refLineVal,1),'%')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),   
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1)),
            axis.text.x = element_text(angle = 30, hjust = 1), 
            axis.title = element_text(size=rel(1.5)), 
            axis.title.y = element_text(color=mpal[2]), 
            axis.title.y.right = element_text(color=mpal[3]),
            legend.position = 'top', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    print(f2out)
    f2name = paste0(stname, '_testPositiveRate.png')
    png(file.path(outPath, f2name), width=8, height=4, units = "in", res=300)
    Sys.sleep(2)
    print(f2out)
    Sys.sleep(2)
    dev.off()
    Sys.sleep(2)
  }
  
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
