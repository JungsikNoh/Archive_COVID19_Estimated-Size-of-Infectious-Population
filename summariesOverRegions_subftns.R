## Summaries Over regions Sub-functions
## Jungsik Noh, UTSW, Dallas, TX, 08/10/2020


cvd_plotSummaries <- function(cumAsctRate_nudge_x=40, cumAsctRate_nudge_y=4){

  ## taking global variables as input (myLst, rgnNames, outsumPath, popVec)
  ## crude CFR, ascertainment rate, Est-adj-Infection Fatality Rate, etc.   ####
  
  regns = data.frame(EIFRadj=NA, crudeCFR=NA, cumAsctRate=NA, rcnt2wAsctRate=NA, 
                     pctEstTotalCases=NA, pctEstTotalCases_L=NA, pctEstTotalCases_U=NA,
                     pctTotalCases=NA, pctEstInfection=NA, 
                     pctEstInfection_L=NA, pctEstInfection_U=NA, 
                     cumAsctRate_L=NA, cumAsctRate_U=NA, rcntAsct_L=NA, rcntAsct_U=NA, 
                     testPosRate7=NA, rcntAsct1w=NA, 
                     rcntAsct4w=NA, deathRateTot=NA, deathRate4w=NA, cvDR=NA)
  
  n = length(myLst)
  for (i in 1:n){
    numT = nrow(myLst[[i]])
    cfr = myLst[[i]]$CFR[numT]
    #ecfrk = myLst[[i]]$ECFR_k[numT]
    #epos18day = myLst[[i]]$Epositive[numT-itod]
    #eifr = myLst[[i]]$death7[numT] / epos18day * 100
    # death7/epos18day
    eifradj = myLst[[i]]$EIFRadj_k[numT]
    conf = mean(myLst[[i]]$confirmRate_k, na.rm=T)
    tmp = myLst[[i]]$positive / myLst[[i]]$Epositive * 100
    cumAsct = min(tmp[numT], 100)
    cumAsct_L = myLst[[i]]$positive[numT] / myLst[[i]]$Epositive_U[numT] * 100
    cumAsct_U = myLst[[i]]$positive[numT] / myLst[[i]]$Epositive_L[numT] * 100
    cumAsct_U = pmin(cumAsct_U, 99)
    
    pos2w = myLst[[i]]$positive7[numT] - myLst[[i]]$positive7[numT-14]
    Epos2w = myLst[[i]]$Epositive[numT] - myLst[[i]]$Epositive[numT-14]
    rcntAsct = pos2w / Epos2w * 100
    
    Epos2w_U = myLst[[i]]$Epositive_U[numT] - myLst[[i]]$Epositive_U[numT-14]
    rcntAsct_L = pos2w / Epos2w_U * 100
    Epos2w_L = myLst[[i]]$Epositive_L[numT] - myLst[[i]]$Epositive_L[numT-14]
    rcntAsct_U = pos2w / Epos2w_L * 100
    rcntAsct_U = pmin(rcntAsct_U, 99)
    
    #
    EposRate = myLst[[i]]$Epositive[numT] / popVec[i] * 100
    EposRate_L = myLst[[i]]$Epositive_L[numT] / popVec[i] * 100
    EposRate_U = myLst[[i]]$Epositive_U[numT] / popVec[i] * 100
    
    posRate = myLst[[i]]$positive[numT] / popVec[i] * 100
    estInfection = myLst[[i]]$Einfected[numT] / popVec[i] * 100
    estInfection_L = myLst[[i]]$Einfected_L[numT] / popVec[i] * 100
    estInfection_U = myLst[[i]]$Einfected_U[numT] / popVec[i] * 100
    #
    testPosRate7 = myLst[[i]]$testPosRate7[numT]
    pos1w = myLst[[i]]$positive7[numT] - myLst[[i]]$positive7[numT-7]
    Epos1w = myLst[[i]]$Epositive[numT] - myLst[[i]]$Epositive[numT-7]
    rcntAsct1w = pos1w / Epos1w * 100
    # 08/16
    pos4w = myLst[[i]]$positive7[numT] - myLst[[i]]$positive7[numT-28]
    Epos4w = myLst[[i]]$Epositive[numT] - myLst[[i]]$Epositive[numT-28]
    rcntAsct4w = pos4w / Epos4w * 100
    
    deathRateTot = ( myLst[[i]]$death[numT]) / popVec[i] * 100
    deathRate4w = (myLst[[i]]$death[numT] - myLst[[i]]$death[numT-28]) / popVec[i] * 100
    #
    tmp = c(NA, myLst[[i]]$Einfected[1:(nrow(myLst[[i]])-1)])
    dailyDeathPerEinfected_k = myLst[[i]]$dif_dea7 / tmp * 100
    dailyDeathPerEinfected_k[myLst[[i]]$dif_dea7 == 0] = NA
    dailyDeathPerEinfected_k[!is.finite(dailyDeathPerEinfected_k)] = NA
    mDR = mean(dailyDeathPerEinfected_k, na.rm=T)
    sdDR = sd(dailyDeathPerEinfected_k, na.rm=T)
    cvDR = sdDR/mDR * 100
    
    # 
    regns[i, ] = c(eifradj, cfr, cumAsct, rcntAsct, 
                   EposRate, EposRate_L, EposRate_U, posRate, 
                   estInfection, estInfection_L, estInfection_U, 
                   cumAsct_L, cumAsct_U, rcntAsct_L, rcntAsct_U, 
                   testPosRate7, rcntAsct1w, 
                   rcntAsct4w, deathRateTot, deathRate4w, cvDR)
  }
  row.names(regns) = rgnNames
  
  # sort
  sout = sort.int(regns$cumAsctRate, index.return = T) 
  regns2 = regns[sout$ix, ]
  #
  write.csv(regns2, file = file.path(outsumPath, 'regns2.csv'))
  
  
  mean(regns$EIFRadj)
  sd(regns$EIFRadj)
  2*sd(regns$EIFRadj)/sqrt(n)
  
  #
  min(regns$EIFRadj)
  #plot(regns$cumAsctRate, regns$EIFRadj)
  #text(regns$cumAsctRate, regns$EIFRadj, rgnNames, cex=0.9)
  (ifrcor = cor.test(regns$cumAsctRate, regns$EIFRadj, method = 's'))
  
  min(regns$crudeCFR)
  #plot(regns$cumAsctRate, regns$crudeCFR)
  #text(regns$cumAsctRate, regns$crudeCFR, rgnNames, cex=.7)
  (cfrcor = cor.test(regns$cumAsctRate, regns$crudeCFR, method = 's'))
  
  ##
  ## figures ####
  
  mpal = wes_palette("BottleRocket2", 5)
  
  midCap = floor(n * 3/5)
  mycol = c(n:1)
  myjet = jet.colors(n+ midCap + 1)
  myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
  
  {
    s1 <- ggplot(data = regns2) + 
      geom_point(aes(x = cumAsctRate, y = crudeCFR, color=mycol), alpha=0.5, size=3) +
      geom_label_repel(
        aes(label = row.names(regns2),  x = cumAsctRate, y = crudeCFR, color=mycol, alpha=0.9), 
        nudge_x = cumAsctRate_nudge_x,
        nudge_y = cumAsctRate_nudge_y,
        #direction = 'x',
        segment.alpha = 0.2,
        #vjust = 1,
        size = 3
      ) +
      theme_bw() +
      #scale_color_manual(values = jet.colors(n)) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, max(regns2$crudeCFR) * 1.2)) +
      scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(x='Ascertainment Rate (%)', y='Case Fatality Rate (%)', 
           subtitle = paste0('As of ', curDate, ', sprmnCor= ', round(cfrcor$estimate,2), 
                             ', P=', round(cfrcor$p.value, 4))) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1.5)),
            #axis.text.x = element_text(angle = 0, hjust = 1), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.5)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s1)
    s1name = paste0('CFRvsAsctRate.png')
    png(file.path(outsumPath, s1name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s1)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  
  ## EIFRadj
  mycol = c(n:1)
  rgnNames2 = row.names(regns2)
  ind_headtail = union(c(1:10), seq(n-9, n))
  rgnNames2[-ind_headtail] = ""
  #num1 = sum(regns2$EIFRadj > 0.66)
  #num2 = n - num1
  ind1 = which(regns2$EIFRadj > 0.66)
  
  {
    s2 <- ggplot(data = regns2, aes(x = cumAsctRate, y = EIFRadj, color=mycol, label = rgnNames2)) + 
      geom_point(alpha=0.5, size=3) +
      geom_label_repel(
        data = subset(regns2, EIFRadj > 0.66 ),
        aes(label = rgnNames2[ind1], color=mycol[ind1], alpha=0.9), 
        nudge_x = 0,
        nudge_y = 0.3,
        #direction = 'x',
        segment.alpha = 0.2,
        vjust = 0,
        size = 3
      ) +
      geom_label_repel(
        data = subset(regns2, EIFRadj <= 0.66 ),
        aes(label = rgnNames2[-ind1], color=mycol[-ind1], alpha=0.9), 
        nudge_x = 0,
        nudge_y = -0.5,
        #direction = 'x',
        segment.alpha = 0.2,
        vjust = 0,
        size = 3
      ) +
      theme_bw() +
      #scale_color_manual(values = jet.colors(n)) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, max(regns2$EIFRadj) * 1.2)) +
      scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(x='Ascertainment Rate (%)', y='Est-Infection Fatality Rate (%)', 
           subtitle = paste0('As of ', curDate, ', sprmnCor= ', round(ifrcor$estimate,2), 
                             ', P=', round(ifrcor$p.value, 4))) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1.5)),
            #axis.text.x = element_text(angle = 0, hjust = 1), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.5)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s2)
    s2name = paste0('IFRvsAsctRate.png')
    png(file.path(outsumPath, s2name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s2)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  
  
  ##
  ##  Bar charts    ####
  
  # Asct rate
  regns2$name = factor(row.names(regns2), levels = row.names(regns2))
  
  midCap = floor(n * 3/5)
  mycol = c(n:1)
  myjet = jet.colors(n+ midCap + 1)
  myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
  
  {
    s11 <- ggplot(data = regns2) + 
      geom_col(aes(x = name, y = cumAsctRate, fill = name ), alpha=1) +
      geom_errorbar(aes(x = name, ymin= cumAsctRate_L, ymax= cumAsctRate_U), 
                    width = 0.4) +
      theme_bw() +
      scale_fill_manual("", values = myjet[n:1]) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      #scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      #scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(y='Ascertainment Rate (%)', x='',  
           subtitle = paste0('As of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1.5)),
            axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.5)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s11)
    s11name = paste0('cumAsctRate.png')
    png(file.path(outsumPath, s11name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s11)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  # recent Asct rate
  regns2$rcnt2wAsctRate[!is.finite(regns2$rcnt2wAsctRate)] = Inf
  sorted2 = sort.int(regns2$rcnt2wAsctRate, decreasing = F, index.return = T, na.last = NA)
  levelsorted = regns2$name[sorted2$ix]
  # regns2$rcnt2wAsctRate[sorted2$ix]
  regns2$rcnt2wAsctRate[!is.finite(regns2$rcnt2wAsctRate)] = NA
  
  regns2$rowid = as.character(c(1:n))
  
  regns2$name2 = factor(row.names(regns2), levels = levelsorted)

  
  midCap = floor(n * 3/5)
  mycol = c(n:1)
  myjet = jet.colors(n+ midCap + 1)
  myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
  
  {
    s12 <- ggplot(data = regns2) + 
      geom_col(aes(x = name2, y = rcnt2wAsctRate, fill = name ), alpha=1) + 
      geom_errorbar(aes(x = name2, ymin= rcntAsct_L, ymax= rcntAsct_U), 
                    width = 0.4) +
      theme_bw() +
      scale_fill_manual("", breaks = as.character(c(1:n)), values = myjet[n:1]) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      #scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      #scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(y='Ascertainment Rate \n(%, recent 2 weeks)', x='',  
           subtitle = paste0('As of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1.5)),
            axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.3)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s12)
    s12name = paste0('recent2w_AsctRate.png')
    png(file.path(outsumPath, s12name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s12)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  
  # recent Asct rate 1 week
  sorted2 = sort.int(regns2$rcntAsct1w, decreasing = F, index.return = T)
  levelsorted = regns2$name[sorted2$ix]
  regns2$rowid = as.character(c(1:n))
  
  regns2$name2 = factor(row.names(regns2), levels = levelsorted)
  
  midCap = floor(n * 3/5)
  mycol = c(n:1)
  myjet = jet.colors(n+ midCap + 1)
  myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
  
  {
    s12 <- ggplot(data = regns2) + 
      geom_col(aes(x = name2, y = rcntAsct1w, fill = name ), alpha=1) + 
      theme_bw() +
      scale_fill_manual("", breaks = as.character(c(1:n)), values = myjet[n:1]) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      #scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      #scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(y='Ascertainment Rate \n(%, recent 1 weeks)', x='',  
           subtitle = paste0('As of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1.5)),
            axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.3)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s12)
    s12name = paste0('recent1w_AsctRate.png')
    png(file.path(outsumPath, s12name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s12)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  
  # cumIncidence rate
  sortedByEpos = sort.int(regns2$pctEstTotalCases, decreasing = T, index.return = T)
  levelsorted = regns2$name[sortedByEpos$ix]
  regns2$rowid = as.character(c(1:n))
  
  regns2$name2 = factor(row.names(regns2), levels = levelsorted)
  
  midCap = floor(n * 3/5)
  mycol = c(n:1)
  myjet = jet.colors(n+ midCap + 1)
  myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
  
  {
    s13 <- ggplot(data = regns2) + 
      geom_col(aes(x = name2, y = pctEstTotalCases, fill = name ), alpha=1) +
      geom_errorbar(aes(x = name2, ymin= pctEstTotalCases_L, ymax= pctEstTotalCases_U), 
                    width = 0.4) +
      theme_bw() +
      scale_fill_manual("", breaks = as.character(c(1:n)), values = myjet[n:1]) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      #scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, NA)) +
      #scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(y='% of the Population', x='', 
           title='Under-reporting-adjusted Total COVID-19 Cases',
           subtitle = paste0('As of ', curDate) ) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
            plot.subtitle = element_text(size=rel(1.3)),
            #plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)),
            axis.text = element_text(size = rel(1.2)),
            axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.5)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s13)
    s13name = paste0('pctEstTotalCases.png')
    png(file.path(outsumPath, s13name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s13)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  
  # Currently Infectious Cases

    # region name marked by cvDR
    idcvDR = which(regns2$cvDR > 100)
    rgnNamesMarked = row.names(regns2)
    marks = rep("*", n); marks[-idcvDR] = "" 
    rgnNamesMarked = paste0(row.names(regns2), marks) 
    regns2$name3 = rgnNamesMarked
  
  sorted2 = sort.int(regns2$pctEstInfection, decreasing = T, index.return = T)
  levelsorted = regns2$name3[sorted2$ix]
  regns2$rowid = as.character(c(1:n))
  
  regns2$name2 = factor(regns2$name3, levels = levelsorted)
   
  midCap = floor(n * 3/5)
  mycol = c(n:1)
  myjet = jet.colors(n+ midCap + 1)
  myjet = myjet[-c((round(n/2)):(round(n/2)+midCap))]
  
  # for log10 trans
  #regns2$pctEstInfection_log = pmax(log10(regns2$pctEstInfection), log10(0.001)) + 3
  {
    s15 <- ggplot(data = regns2) + 
      geom_col(aes(x = name2, y = pctEstInfection, fill = name), alpha=1) +
      geom_errorbar(aes(x = name2, ymin= pctEstInfection_L, ymax=pctEstInfection_U), 
                    width = 0.4) +
      theme_bw() +
      scale_fill_manual("", breaks = as.character(c(1:n)), values = myjet[n:1]) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      scale_y_continuous(limits = c(0, NA)) +
      #coord_trans( y="log10", ylim = c(0.01, NA)) +
      #scale_y_log10( ) +
      #scale_x_continuous(limits = c(0, max(regns2$cumAsctRate) * 1.2), breaks = seq(0,100,20)) +
      labs(y='% of the Population', x='', 
           title='Currently Infected COVID-19 Cases**',
           subtitle = paste0('As of ', curDate), 
           caption = paste0('* indicates regions where the estimation quality is poor. \n', 
             '**Under-reporting-adjusted number of cases which have not yet an outcome (recovery or death)')) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(1.5)),  
            plot.subtitle = element_text(size=rel(1.3)),
            plot.caption = element_text(size=rel(1), hjust=0, margin=margin(-10,0,0,0)), 
            axis.text = element_text(size = rel(1.2)),
            axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5)), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.5)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s15)
    s15name = paste0('pctEstInfection.png')
    png(file.path(outsumPath, s15name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s15)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
  
  ##
  ## asctRate vs testPos for states
  tmp = regns2$testPosRate7
  tmp2 = regns2$rcntAsct1w
  ind = which(!is.finite(tmp) | (tmp > 50))
  
  regns3 = regns2[c('name', 'testPosRate7', 'rcntAsct1w', 'rcnt2wAsctRate') ]
  regns3 = regns3[-ind, ]
  row.names(regns3) = regns3$name
  
if (nrow(regns3) > 0){
  rgnNames3 = row.names(regns3)
  n3 = nrow(regns3)
  mycol = c(n3:1)
  midCap = floor(n3 * 3/5)
  mycol = c(n3:1)
  myjet = jet.colors(n3+ midCap + 1)
  myjet = myjet[-c((round(n3/2)):(round(n3/2)+midCap))]
  
  {
    s1 <- ggplot(data = regns3) + 
      geom_point(aes(x = testPosRate7, y = rcntAsct1w, color=mycol), alpha=0.5, size=3) +
      geom_label_repel(
        aes(label = row.names(regns3),  x = testPosRate7, y = rcntAsct1w, color=mycol, 
            alpha=0.9), 
        nudge_x = 0,
        nudge_y = 0,
        #direction = 'x',
        segment.alpha = 0.2,
        #vjust = 1,
        size = 3
      ) +
      theme_bw() +
      #scale_color_manual(values = jet.colors(n)) +
      #scale_color_gradientn(colors = jet.colors(n)) +
      scale_color_gradientn(colors = myjet) +
      #scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
      #scale_color_gradientn(colors = brewer.pal(11, 'Spectral')) +
      scale_y_continuous(limits = c(0, max(regns3$rcntAsct1w) * 1.2)) +
      scale_x_continuous(limits = c(0, max(regns3$testPosRate7) * 1.2), breaks = seq(0,100,20)) +
      labs(x='Test Positivity Rate (%)', y='Ascertainment Rate (%, recent 1 week)', 
           subtitle = paste0('As of ', curDate)) + 
      theme(plot.title = element_text(hjust = 0.5, size=rel(2)),  
            plot.subtitle = element_text(size=rel(1.3)),
            axis.text = element_text(size = rel(1.5)),
            #axis.text.x = element_text(angle = 0, hjust = 1), 
            #axis.text.y = element_text(color = mpal[2]),  
            axis.title = element_text(size=rel(1.5)), 
            #axis.title.y = element_text(color=mpal[2]),  
            legend.position = 'none', legend.title = element_text(size= rel(1)), 
            legend.text = element_text(size = rel(1)))
    
    print(s1)
    s1name = paste0('AsctRateVsTestPos.png')
    png(file.path(outsumPath, s1name), width=8, height=4, units = "in", res=300)
    Sys.sleep(1)
    print(s1)
    Sys.sleep(1)
    dev.off()
    Sys.sleep(1)
  }
}
  
  ## output_current 
  
  listoffiles = as.character()
  listoffiles = list.files(path = outsumPath)  
   
  file.copy(from= file.path(outsumPath, listoffiles), to=output_current, overwrite = T, recursive = T)

}



##  EOF
