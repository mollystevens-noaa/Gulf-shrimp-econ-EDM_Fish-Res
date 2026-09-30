
###**Define parameters / species**
sedar='SEDAR 87'
sedartype='Benchmark Assessment'
region='Gulf'
termyear = 2022
startyear= 1987

spp='BSH'
species='Brown Shrimp'
scientific='Farfantepenaeus aztecus'
survey='SEAMAP'
survey2='TPWD'
landunit = 'tailmp'
Tseas = 2

nursmo1 = 'February' ##start month in nursery for env indices
nursmo2 = 'May' ##end month in nursery for env indices 

nfore <- 80
tsave <- 6

###**Load functions**
# Not in
'%nin%' <- Negate('%in%')



##**EDM**##
#source("../EDM/00_load-libraries.R")
load('../Data/bsh_agg.RData')

pars <- read.csv(file=paste0('../output_',spp,'/',spp,'_pars.csv'))
fitstats <- read.csv(file=paste0('../output_',spp,'/',spp,'_fitstats.csv'))

topmodfit <- read.csv(file=paste0('../output_',spp,'/',spp,'_TOPMODEL.csv'))
msyref    <- read.csv(file=paste0('../output_',spp,'/',spp,'_msyref.csv'))
msypeel   <-  read.csv(file=paste0('../output_',spp,'/',spp,'_msytop-peel-flags.csv'))

msytop1   <-  read.csv(file=paste0('../output_',spp,'/',spp,'_msytop.csv'))
msytop2   <-  msytop1 %>% filter(DROPFLAGS==0)

msyfin   <-  read.csv(file=paste0('../output_',spp,'/',spp,'_msyfinal.csv'))


Fstatus   <-  read.csv(file=paste0('../output_',spp,'/Fstatus-series_',spp,'.csv'))


##take top msy to merge (then set) with the peeled runs
pull1 <- msytop1 %>% select(name )  ##top 5 mods
pull2 <- msytop2 %>% select(spp.run )
topmod_peel <- left_join(pull1, msypeel) %>%
  relocate(spp.run, name) #%>%
 # select(spp.run, name, msy, fmsy,Bmsy,Bmsy_landunit,msy_fac, msy_drop, msy_drop10)


###**adapt this to sum by run, drop any that fails msy_drop**
topmodfit_msy1 <- left_join(msytop2, topmodfit, by=c('spp.run')) %>%
filter( spp.run=='BSH_G20023') #'BSH_G10323', spp.run=='BSH_G21023' |

topmodfit_msy2 <-  topmodfit_msy1  %>%
  select(spp.run, msy_annual, Fmsy_annual, Umsy_seasonal=Umsy, Umsy_annual,land_max,Bmsy,Bmsy_landunit,
         df_0, R2_0, R2scaled_0, R2_out_0, R2scaled_out_0, rho_0,
                                                    #bLarge_0, bMedium_0, bSmall_0  ##EDIT FOR WSH
                                                    b_0
  )

####**PULL THE TOP RUNS CONSIDERED AND GENERATE TABLES, INFO TO PULL FIGURES**
top_runs     <- topmodfit_msy2 %>% select(spp.run)
top_pars     <- left_join(top_runs,pars)
top_fitstats <- left_join(top_runs, fitstats)

##pull only top hierarchical models
top_pars <- top_pars %>%
                  filter(ind==0)
top_pars_tab <- top_pars %>%
                  pivot_wider(
                    id_cols='par', values_from='est', names_from='spp.run'
                  )


winter <- landings2 %>%
             mutate(SEASON=ifelse(MONTH<5, 'WINTER','OTHER')) %>%
             group_by(SEASON)%>%
               summarize(lb_tw = sum(HEADSOFF_LBS)) %>%
             mutate( percent = lb_tw / sum(lb_tw))
