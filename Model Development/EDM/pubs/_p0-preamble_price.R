
scenarios <- read.csv(file=paste0('../pubs/figs/Uproj_scenario.csv'))
pars_price_fuel <- read.csv(file=paste0('../output_',spp,'/',spp,'_pars_price_fuel.csv'))
fitstats_price_fuel <- read.csv(file=paste0('../output_',spp,'/',spp,'_fitstats_price_fuel.csv'))
fuel_price <- read.csv(file='../Data/Fuelprice_UPLOAD_8722.csv')

price_mod_results <- read.csv(file=paste0('./figs/BSH_price_fuel_mod_land.csv'))


pars_merge <- pars_price_fuel %>% select(par, 'BSH_G20023_price_fuel'=est)

top_pars_tab2 <- full_join(top_pars_tab, pars_merge, by=c('par'))


##filter down fitstats to merge for table
fitstats_price_fuel <- fitstats_price_fuel %>% select(stats, 'BSH_G20023_price_fuel'=est)
top_fitstats <- top_fitstats%>%filter(ind==0) %>% select(stats, 'BSH_G20023'=est)

top_fitstats2 <- full_join(top_fitstats,fitstats_price_fuel, by=c('stats'))


##format and merge fuel price
fuel_price2 <- fuel_price %>%
  filter(SEASON !='JFMA') %>%
  rename(QUAD=SEASON) %>%
  mutate(
    YEAR2 = ifelse(QUAD=='SOND', YEAR + 0.5, YEAR),
    SEASON = ifelse(QUAD=='SOND','Fall',
                    ifelse(QUAD=='MJJA','Summer',
                           ifelse(QUAD=='JFMA','Winter','Missing')))
  )
cpue_land_Gfuel <- left_join(cpue_land_G, fuel_price2, by=c('YEAR','SEASON','YEAR2'))


####**DEFINE THE CATCHABILITY FROM THE PREVIOUS MODEL TO ESTIMATE HARVEST RATE**
####**REFIT THE MODEL, COMPARE WITH THE BEST MODEL IN A PLOT**
####**NEXT SCRIPT WILL READ IN THE DATA AND ADD PROJECTIONS + ERROR BARS**
q=0.401774546

###working off G_20023 (best run from SEDAR87 work)
cpue_land_G <- cpue_land_Gfuel %>% select(-c('GULF','logcpue','tail10mp','LBS_Imports','imp_10mp','imp_100mp',
                                         'SEAS_SIZE','tempbotm','tempbotm_std','salbotm', 'salbotm_std'))
cpue_land_G_nooutlier <- cpue_land_G %>% mutate(hrate= q*tailmp/CPUE)  %>% filter(hrate<0.95)
pricemod_G  <- lm( hrate ~  log(Price_GOM) * SIZE + log(P_Fuel_Real), data=cpue_land_G_nooutlier)
summary(pricemod_G)

coef <- pricemod_G$coefficients


pmod1 <- price_mod_results %>% mutate(
  Year=substr(timestep,0,4)
)

pmod2 <- pmod1 %>%
            group_by(Year, scenario) %>%
            summarize(Revenue=sum(revenue))

pmod3 <- pmod2 %>% filter(Year>2020)


pmod11 <- price_mod_results %>% mutate(
  year=substr(timestep,1,4) )
pmod12 <- pmod11 %>%
  group_by(year, scenario, fscenario)%>%
  summarize(totrev = sum(revenue)) %>%
  filter(year>2022, fscenario=='status.quo')
pmod13 <- pmod12 %>%
  group_by(scenario) %>%
  summarize(rev_avg = mean(totrev))
