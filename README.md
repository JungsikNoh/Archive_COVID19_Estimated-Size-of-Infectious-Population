<img align="right"  height="100" src="/doc/utsw-master-logo-cmyk+BI.png">

 <p>&nbsp;</p> 

 <p>&nbsp;</p> 

# Estimation of daily ascertainment rates of COVID-19 cases unveils actual sizes of currently infected populations in countries and U.S. states  

 <p>&nbsp;</p> 
 
> <img src="/output/countries_current/US_newCases7d.png" width="49.5%"/> <img src="/output/countries_current/US_NewCasesEstConfirmed.png" width="49.5%"/> 

> <img src="/output/countries_current/US_estInfections.png" width="49.5%"/> <img src="/output/countries_current/US_estTotalCases.png" width="49.5%"/> 

 <p>&nbsp;</p> 

Since it began to spread in China around December 2019, the coronavirus disease 2019 (COVID-19) has caused more than 800,000 confirmed deaths all over the world as of August 22, 2020 according to [Worldometer](https://www.worldometers.info/coronavirus).

Massive diagnostic testing has confirmed >23 million cases globally so far, and daily numbers of new infections and deaths have been collected and monitored as an effort to contain the virus. 

However, in many countries, we still do not know how many actual infections have occurred, and more importantly how many individuals are currently infected, because of substantial undocumented infections. The currently infected population is the cause of future infections and deaths. The actual size of the infected population is what we should know for effective contact tracing and controlling this pandemic. 

Infection-Fatality-Rate (IFR) can give us a hint to estimate the actual number of infections under the assumption that the number of undocumented deaths is negligible, though this assumption may not be valid in some regions. [Verity et al. (2020, Lancet)](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext) presented an IFR estimate of 0.66% (0.39%&ndash;1.33%, 95%-confidence interval) based on early pandemic data in China. In other words, one death comes after about 150 infections on average, which suggests that the worldwide 23 million infections are too small to account for the 800,000 deaths as of August 22, 2020. 
 
A computational pipeline proposed here estimates daily ascertainment rate of COVID-19 cases, under-reporting-adjusted number of new infections, and the actual size of currently infected population in each country and U.S. state. The estimates are based on the publicly available data of daily confirmed cases and deaths, and the key pandemic parameter estimates such as the IFR and the mean duration from infection to death or recovery presented by [Verity et al](https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext). 

This webpage presents daily reports of COVID-19 data and the estimates of actual infections for 50 countries with the most infections, 50 U.S. states, and 15 Texas counties with the most confirmed cases. The estimation results show severe under-reporting of the cases in many countries and regions. Strikingly, the number of total actual cases is estimated to be 20 times more than the reported cases in the U.K., and five times more in the U.S. as of August 22, 2020. In Peru, the actual size of currently infected population is estimated to be 8.1% of the total population (4.0%&ndash;13.7%). The estimate for Brazil is 4.5% of the population (2.3%&ndash;7.7%), as of August 22, 2020. Because the number of documented deaths is assumed to be accurate in this estimation, if it has been under-reported, then the infection estimates should be adjusted upward accordingly. 


## Case-Fatality-Rates differ across countries mainly because ascertainment rates are varying widely. 

- The ascertainment rate is the ratio of confirmed infections to actual infections, which include the individuals who were once infected but not tested. 
The figures below show that a high case-fatality-rate indicates severe under-reporting of COVID-19 cases, that is, a low ascertainment rate overall since the beginning.
After under-reporting-adjustment, the obtained infection-fatality-rates no longer correlate with the ascertainment rates. 


> <img src="/output/countries_current/CFRvsAsctRate.png" width="49.5%"/> <img src="/output/countries_current/IFRvsAsctRate.png" width="49.5%"/>

> <img src="/output/states_current/CFRvsAsctRate.png" width="49.5%"/> <img src="/output/states_current/IFRvsAsctRate.png" width="49.5%"/>

<p>&nbsp;</p>

## Daily Report of COVID-19 Time Series

1. Countries
   1. [Estimated number of new cases and currently infected cases](REPORT_COUNTRY.md)    
   2. [Recent 8 weeks of estimated number of new cases and currently infected cases](REPORT_COUNTRY_RECENT8w.md)
   3. [Daily confirmed cases/deaths (not rolling average)](doc/Daily_confirmed_raw_country.md)
   4. [Summary](doc/Summary_country.md)
   5. [Estimated daily rates of ascertainment, cumulative incidence and transmission](doc/Daily_rates_country.md)
   
2. U.S. states
   1. [Estimated number of new cases and currently infected cases](REPORT_STATE.md)    
   2. [Recent 8 weeks of estimated number of new cases and currently infected cases](REPORT_STATE_RECENT8w.md)
   3. [Daily confirmed cases/deaths (not rolling average)](doc/Daily_confirmed_raw_state.md)
   4. [Summary](doc/Summary_state.md)
   5. [Estimated daily rates of ascertainment, cumulative incidence and transmission](doc/Daily_rates_state.md)
   6. [Test positivity rate](doc/Test_positivity_rate_state.md)
   
3. TX counties
   1. [Estimated number of new cases and currently infected cases](REPORT_TX_COUNTY.md)
   2. [Recent 8 weeks of estimated number of new cases and currently infected cases](REPORT_TX_COUNTY_RECENT8w.md)
   3. [Daily confirmed cases/deaths (not rolling average)](doc/Daily_confirmed_raw_TX_county.md)
   4. [Summary](doc/Summary_TX_county.md)
   5. [Estimated daily rates of ascertainment, cumulative incidence and transmission](doc/Daily_rates_TX_county.md)
   
 

## Links

- An article at Medium.com explains how undocumented infections make confusion in interpreting daily counts of COVID-19 confirmed cases. 
  - [*The Actual Highest Number of Daily COVID-19 Cases in the US Is Estimated to Be About 400,000 in April*](https://nohjssunny.medium.com/the-actual-highest-number-of-daily-covid-19-cases-in-the-us-is-estimated-to-be-about-400-000-in-e91bf1cce8e0?sk=b7155837d4e78bb767d8fa61c860cab7) 
- A manuscript at medrxiv.org: [*Estimation of the fraction of COVID-19 infected people in U.S. states and countries worldwide*](https://www.medrxiv.org/content/10.1101/2020.09.26.20202382v1)
- [COVID-19 Time Series: Look at Us Today, See Yourself Tomorrow](https://github.com/JungsikNoh/COVID-19_LookAtUsToday_SeeYourselfTomorrow) 
  - A previous pipeline for monitoring COVID-19 confirmed cases over time for countries and regions in the U.S. 



## Output

- Daily updated estimates for each region can be found at, for example, /output/countries/US/2020-mm-dd/US_outputDF.csv. Visualization plots are in the same folder.  
- Daily updated summaries for 50 countries or 50 U.S. states can be found at, for example, /output/country_summary/2020-mm-dd/regns2.csv.


## Data sources

- The number of confirmed cases/deaths for countries and Texas counties are from COVID-19 data repository of Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19). Updated around 11pm CT.
- Data at the state level are from the covidtracking project (https://covidtracking.com/). Updated around 4pm CT.


## Contact

Jungsik Noh (jungsik.noh@utsouthwestern.edu)

