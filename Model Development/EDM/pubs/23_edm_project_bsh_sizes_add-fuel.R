####################################################################################################
source("./ModelDevelopment/EDM/00_load-libraries.R")
source("./ModelDevelopment/EDM/03_bsh_ctrl_expansion.R")
source("./Functions/fig_format_export.R")
####################################################################################################
## Load data and functions
load('./ModelDevelopment/EDM/Data/bsh_agg.RData')
spp <- 'BSH'
species <- 'Brown Shrimp'
land.unit <- 'tailmp'
Tseas <- 2 ##seasonal time steps within the year if used (e.g. pop=SEASON, time=YEAR2, etc)
nfore <- 10
tsave <- 6 ##**incorporate below in data call and in outputs**
ctrl_all <- ctrl_all %>% filter(eval=='Y')%>% filter(is.na(z1))
ytrans="gr2"
##########################################

###***READ IN BEST MODEL AND BEST MODEL WITH EX-VESSEL PRICE AND CO-PLOT RESULTS**


###**FIX THIS SCRIPT ; MAYBE MOVE X2 ELSEWHERE SINCE IT MIGHT NEED TO BE APPLIED TO FMSY TOO**

topmodfit <- read.csv(file=paste0('./ModelDevelopment/EDM/output_',spp,'/',spp,'_TOPMODEL.csv'))
ctrl_fit <- readRDS(file=paste0('./ModelDevelopment/EDM/output_',spp,'/',spp,'_ctrlfit_all')) %>%
  relocate(spp.run, fullrun)

BSH_Fstatus_pop <- read.csv(file=paste0('./ModelDevelopment/EDM/output_BSH/Fstatus-popseries_BSH.csv'))
BSH_Fstatus     <- read.csv(file=paste0('./ModelDevelopment/EDM/output_BSH/Fstatus-series_BSH.csv'))


##pull recent harvest rates
Uproj <- BSH_Fstatus %>% filter(YEAR>=2020) %>%
             group_by(spp.run) %>%
             summarize(
               U_20_22 = mean(Urate)
             )
#Uproj$U22 <- WSH_Fstatus %>% filter(YEAR==2022) %>% select(Urate)

##**Read in and view seamap survey data**
seamap_update <- read.csv(file=paste0('./ModelDevelopment/EDM/output_BSH/BSH_SurM_SEAMAPT_8724_09182025.csv'))
table(seamap_update$YEAR, seamap_update$MONTH, useNA='always')
table(seamap_update$YEAR, seamap_update$STATZONE, useNA='always')

##**from ./DataProcessing/BSH2_data-clean.R*
seamap_update2 <- seamap_update %>%
  mutate(
    QUAD=ifelse(SEASON=='Summer','MJJA',
                ifelse(SEASON=='Fall','SOND','9999')),
    AREA_ASSESS=ifelse((STATZONE<22 & STATZONE>=18),'18-21',
                       ifelse((STATZONE<18 & STATZONE>=11),'11-17',
                              ifelse((STATZONE<11 & STATZONE>=1),'1-10',
                                     '999')))
  ) %>%
  filter(STATZONE>10 & STATZONE<=21)

##**from ./ModelDevelopment/EDM/01_bsh_data-aggregate.R*
seamap_update3 <- seamap_update2 %>% 
  filter(STATZONE>10 & STATZONE<=21) %>%
  mutate(
    FALL=ifelse(SEASON=='Fall',1,0)
  ) %>% 
  rename(SIZE = CATEGORY)

##stratum G. BROWN SHRIMP SEASONAL (SUMMER, FALL+WINTER) ; SIZE BINS (>67, 67-31, <=30) ; AREA AGG (11:21)
seamap_G_update <- seamap_update3 %>%
  group_by(YEAR, SEASON, SIZE) %>%
  summarize(CPUE=mean(NHR)) %>%
  mutate(YEAR2 = ifelse(SEASON=='Fall', YEAR + 0.5, YEAR),
         GULF='GULFWIDE')

# ##delete years where abundance index is missing any time block for avg (see: SEAMAP summer 2020)
# full_C_update <- max(ldwf_C1_update$nt) #define full average of B time steps needed for a rep index
# ldwf_C_update <- ldwf_C1_update%>%
#   filter(nt>=full_C_update) %>% subset(select = -nt) 

seamap_G_2324<- seamap_G_update %>%
                  filter(YEAR2>=2023) %>%
                  rename(timestep=YEAR2,
                         pop=SIZE,
                         update=CPUE) %>%
                  select(timestep,pop,update)



##merge in full data w old provision and export
seamap_G_update_merge <- seamap_G_update %>% rename (CPUE_newprovision=CPUE)
cpue_land_G_compare <- full_join(seamap_G_update_merge, cpue_land_G, by=c("YEAR","YEAR2","SEASON","SIZE"))
#write.csv(cpue_land_G_compare, file=paste0('./ModelDevelopment/EDM/output_BSH/BSH_SurM_SEAMAPT_compare2024provision.csv'),row.names=F)

topmodfit$spp.run
topmodfit_run <- as.data.frame(cbind(spp.run=topmodfit$spp.run,top='Y'))
#topmod_nocov <- topmodfit %>% filter(z1=='none')
#ctrl_nocov <- ctrl_all %>% filter(is.na(z1))

msyreflist <- list()

#C244 -- high R2pop_out for large no ytrans
#C728 -- high R2pop_out for large gr1
#C992 -- low R2pop_out for large g2
##**now define in x as spp.run*
# i=21016 #10435#10890 #99 #10043 #10435#11806 #21031 #10043#topmod_nocov$run[1]       21031
# j='C' #'G'   #'A'  #'C'  #'G' #'C'#substr(topmod_nocov$stratum[1],1,1)  'G'

ctrl_top <- left_join(topmodfit_run,ctrl_all,by='spp.run')
x='BSH_G20023' #'PSH_C20039'##'BSH_E20061' ##'PSH_C20039' # # # 'WSH_C4182' #'BSH_G20055' #'BSH_C10043' #'BSH_A99' #'BSH_C21016'
#for (x in unique(ctrl_top$spp.run)){
  j=ctrl_all$stratum[ctrl_all$spp.run==x]
  i=ctrl_all$run[ctrl_all$spp.run==x]
#for (j in c('A','C','E')){ #strata$stratum){ ##**Update this once full strata defined**
##update below without "sm" or "ml" -- was not very thoughtful start to finish w this aggregation..
#for (j in c('C','G')){

rm(list=ls()[! ls() %in% c('export_fig','x','i','j','ctrl_all',"ctrl_top","strata", "Tseas","ytrans",
                           "cpue_land_A","cpue_land_B","cpue_land_C","cpue_land_Cml","cpue_land_Gml",
                           "cpue_land_D","cpue_land_E", "cpue_land_F","cpue_land_G","cpue_land_H",
                           "cpue_land_Csm","cpue_land_Dsm","cpue_land_Gsm","cpue_land_Hsm","land_U",
                           "hratevec","tsave","nfore","land.unit","msyreflist","topmodfit",
                           "BSH_Fstatus","BSH_Fstatus_pop","Uproj","seamap_G_update","seamap_G_2324")]) 



  ctrl <- ctrl_all %>% filter(stratum==j) 
#for(i in ctrl$run){
    this.run <- ctrl[ctrl$run==i,]
    spp <- this.run$assessment
    species <- this.run$species
    spp.run <- paste0(this.run$assessment,"_",j,i)
    data.run1 <- eval(parse(text=this.run$data)) 
    data.run <- data.run1 %>% filter(YEAR >= this.run$startyr)
    name <- paste0(this.run$stratum,this.run$run,"_",this.run$assessment,this.run$startyr,"_",this.run$y1,this.run$y2,
                   "_bshare",substring(this.run$bshared,1,1),"_",
                   this.run$pop,this.run$time,"_E",this.run$E,"_",this.run$scaling,"_ytrans",
                   this.run$ytrans,"_",
                   this.run$predictmethod,"_",
                   ifelse(!is.na(this.run$z2), paste0(this.run$z1,this.run$z2),
                          ifelse(!is.na(this.run$z1), this.run$z1,"nocov")),
                   ifelse(this.run$linprior==T,paste0("_ricker"),""))
    
    
    best.mod.fit <- readRDS(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name,'/EDMfit_',spp.run))

    head(best.mod.fit$insampresults)
    
    ####**read in best mod fit with price from previous example**
    ##replace landings with estimated removals using price in units of index
    this.run$y2 = 'removals_est_price_fuel'
    name.price.fuel <- paste0(this.run$stratum,this.run$run,"_",this.run$assessment,this.run$startyr,"_",this.run$y1,this.run$y2,
                   "_bshare",substring(this.run$bshared,1,1),"_",
                   this.run$pop,this.run$time,"_E",this.run$E,"_",this.run$scaling,"_ytrans",
                   this.run$ytrans,"_",
                   this.run$predictmethod,"_",
                   ifelse(!is.na(this.run$z2), paste0(this.run$z1,this.run$z2),
                          ifelse(!is.na(this.run$z1), this.run$z1,"nocov")),
                   ifelse(this.run$linprior==T,paste0("_ricker"),""))    
    
    price.fuel.mod.fit <- readRDS(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name.price.fuel,'/EDMfit_',spp.run))
    
    
    ##**pulling from GP-EDM package to transform sd back to real space**
    #transform
    ytransfun=function(y, m1, e1, ytrans) {
      if(ytrans=="none") return(y)
      if(ytrans=="log") return(log(y))
      if(ytrans=="gr1") return(log(y/m1))
      if(ytrans=="gr2") return(log(y/e1))
    }
    
    #inverse transform
    ytransfuninv=function(y, m1, e1, ytrans) {
      if(ytrans=="none") return(y)
      if(ytrans=="log") return(exp(y))
      if(ytrans=="gr1") return(exp(y)*m1)
      if(ytrans=="gr2") return(exp(y)*e1)
    }
    
    
    ###**Can I loop through these from model type?**
    ###**For now, just run through this for best model then copy/paste and replace for price fit**
    
    if (this.run$pop!='GULF' ){ #& (j=='E' & time!='YEAR2')
      this.run.fore <- makelags_iter(nfore=nfore, data=data.run, pop=this.run$pop,
                                     y=c("CPUE",land.unit), 
                                     time=this.run$time, tau=1, E=this.run$E)
    }
    
    best.mod.proj = predict_iter(best.mod.fit, newdata=this.run.fore, hrate=Uproj$U_20_22)
    
    #plot(this.run.fit$outsampresults)
    plot(best.mod.proj)
    ##NOTE: YEAR HERE IS WRONG ; SHOULD BE SEASONAL TIME STEP BY 0.5 YEAR
    ##extract data, correct timestep (year) label
    best.mod.proj.outsamp <- best.mod.proj$outsampresults
    head(best.mod.proj.outsamp)
    
    ##**APPLY TO PROJECTIONS**
    ###add sd, etc for proj
    ypred_test = ytransfuninv(y=best.mod.proj.outsamp$predmean_trans, e1=best.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypred_uci  = ytransfuninv(y=(best.mod.proj.outsamp$predmean_trans+2*best.mod.proj.outsamp$predsd_trans), e1=best.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypred_lci  = ytransfuninv(y=(best.mod.proj.outsamp$predmean_trans-2*best.mod.proj.outsamp$predsd_trans), e1=best.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypredf_uci = ytransfuninv(y=(best.mod.proj.outsamp$predmean_trans+2*best.mod.proj.outsamp$predfsd_trans), e1=best.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypredf_lci = ytransfuninv(y=(best.mod.proj.outsamp$predmean_trans-2*best.mod.proj.outsamp$predfsd_trans), e1=best.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    
    best.mod.proj.outsamp2 <- cbind(best.mod.proj.outsamp,ypred_test, ypred_uci, ypred_lci, ypredf_uci, ypredf_lci)
    
    ##hard coded for nfore=10, can make more dynamic later
    ##creates a new variable that accurately matches the BSH time step of 0.5
    timestep2 <- sort(rep(seq(from=2023, to=2027.5 , by=0.5),3))
    
    ##replace projection timestep with correct label
    best.mod.proj.outsamp3 <- best.mod.proj.outsamp2 %>% mutate(timestep=timestep2)
    head(best.mod.proj.outsamp3)
    ##keeping tailmp here and calculating landings from price estimated removals below
    best.mod.proj.outsamp4 <- best.mod.proj.outsamp3 %>% select(-c('tailmp_1','tailmp_2','tailmp_3','tailmp_4','tailmp_5'))
    head(best.mod.proj.outsamp4)
    
    ## set projection data with input data, then plot
    best.mod.in.samp <- as.data.frame(best.mod.fit$insampresults) 
    head(best.mod.in.samp) ##landings are missing here -- add them back in
    best.mod.in.samp.pred <- best.mod.in.samp %>% select(-c('obs_trans','obs'))
    head(best.mod.in.samp.pred)
    best.mod.in.samp.obs <- best.mod.in.samp %>% select(c('timestep','pop','obs_trans','obs'))
    head(best.mod.in.samp.obs)
    
    ###add sd, etc for insamp
    ypred_test = ytransfuninv(y=best.mod.in.samp.pred$predmean_trans, e1=best.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypred_uci  = ytransfuninv(y=(best.mod.in.samp.pred$predmean_trans+2*best.mod.in.samp.pred$predsd_trans), e1=best.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypred_lci  = ytransfuninv(y=(best.mod.in.samp.pred$predmean_trans-2*best.mod.in.samp.pred$predsd_trans), e1=best.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypredf_uci = ytransfuninv(y=(best.mod.in.samp.pred$predmean_trans+2*best.mod.in.samp.pred$predfsd_trans), e1=best.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypredf_lci = ytransfuninv(y=(best.mod.in.samp.pred$predmean_trans-2*best.mod.in.samp.pred$predfsd_trans), e1=best.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    
    best.mod.in.samp.pred2 <- cbind(best.mod.in.samp.pred,ypred_test, ypred_uci, ypred_lci, ypredf_uci, ypredf_lci)
    
    best.mod.all.samp <- rbind(best.mod.in.samp.pred2, best.mod.proj.outsamp4)
    best.mod.all.samp2 <- full_join(best.mod.all.samp, best.mod.in.samp.obs, by=c('timestep','pop')) %>%
                                mutate(data='best.mod')
    
    
    
    ###pull and set landings
    head(best.mod.proj.outsamp3)
    landings <- cpue_land_G %>% select(
      timestep=YEAR2,
      pop=SIZE,
      tailmp
    )
    
    landings_project <- best.mod.proj.outsamp3 %>% select(
      timestep,
      pop,
      tailmp_1
    )
    ##lagged landings in 2023 are the landings from 2022.5
    ##need to chop them off and set back together from projections
    
    ##pull timestep,pop and chop off last 3 vars (last time step)
    landings_proj_yr <- head(landings_project %>% select(timestep,pop),-3)
    ##pull value, drop first 3 values (replicated in 2022.5 bc lagged var here, rename var to reflect lag removal)
    landings_proj_val <- tail(landings_project %>% select(tailmp=tailmp_1),-3)
    ##bind together
    landings_project2 <- cbind(landings_proj_yr,landings_proj_val)
    ##bind landings project with landings
    best.mod.land.project <-rbind(landings,landings_project2)
    
    ###***APPLY TO PRICE PRED MODEL**
    
    ##data going into the model is different, exported data with predicted harvest rate from the previous script and reading in here
    cpue_land_Gprice <- readRDS(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name.price.fuel,'/cpue_land_Gprice'))
    data.price.fuel.run <- cpue_land_Gprice
    
    if (this.run$pop!='GULF' ){ #& (j=='E' & time!='YEAR2')
      price.fuel.mod.fore <- makelags_iter(nfore=nfore, data=data.price.fuel.run, pop=this.run$pop,
                                     y=c("CPUE",this.run$y2), 
                                     time=this.run$time, tau=1, E=this.run$E)
    }
    
    ##****NEED TO SWAP OUT UPROJ$U_20_22 FOR PRICE PROJECTION SCENARIOS***
    ##**CREATE IN ANOTHER FILE THEN REPLACE HERE OR GENERATE LOOP FROM HERE DOWN**
    
    head(price.fuel.mod.fit$insampresults)
    
    ##checked that in cpueland file, escapement_1 is equal to CPUE-removals_est_price
    ##confirms that removals_est_price/CPUE is the relative harvest rate for each step
    cpue_land_Gprice <- cpue_land_Gprice %>% mutate (
      Urate = removals_est_price_fuel/CPUE
    )
    
    
    ##calculate an annual harvest rate based on total removals over total index
    ##would it make more sense to use harvest rate applied to 'large' for these simulations?
    ##make sure that 1988 large is included here (accidentally dropped for entire dataset when fitting price function)
    cpue_land_price <- cpue_land_Gprice  %>%
      #mutate(hrate*tailmp )  <--add weighting factor here  ; sum below
      group_by(YEAR) %>%
      summarize(landmp = sum(tailmp),
                removals_est_price_fuel_all = sum(removals_est_price_fuel), ##removals estimated from price
                pop.tot = sum(CPUE)) %>%
      mutate(
        Urate = removals_est_price_fuel_all / pop.tot#,
        # Frate = -log(1-Urate),
        # UUmsy= Urate/Umsy_annual,
        # FFmsy = Frate/Fmsy_annual,
        # BBmsy = Best/Bmsy,
        # Fmsy=Fmsy_annual,
        # Umsy=Umsy_annual,
        # Bmsy=Bmsy
      )
    
    
    ###**PLOT REMOVALS (TOTAL AND BY SIZE) USING PRICE MODEL AND CATCHABILITY x LANDINGS**
    
    ##sum across population and removals to get overall rates
    ##original code below
    # data.b3 <- data.b2  %>%
    #   group_by(spp.run,YEAR) %>%
    #   summarize(landmp = sum(tailmp),
    #             catch.bunit = sum(catch.b), ##landings in terms of cpue units
    #             pop.tot = sum(CPUE),
    #             Best = sum(CPUE),
    #             Best_mp = sum(CPUE_mp)) %>%
    #   mutate(
    #     Urate = catch.bunit / pop.tot,
    #     Frate = -log(1-Urate),
    #     UUmsy= Urate/Umsy_annual,
    #     FFmsy = Frate/Fmsy_annual,
    #     BBmsy = Best/Bmsy,
    #     Fmsy=Fmsy_annual,
    #     Umsy=Umsy_annual,
    #     Bmsy=Bmsy
    #   )
    Uproj_prices_fuel_size <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        price=mean(Price_GOM),
        fuel=mean(fuel_price), ## this is the mean across all years/seasons (grouping by size doesn't matter)
        Urate = mean(Urate)
      ) %>%
      mutate(scenario='2020_22_avg')
    write.csv(Uproj_prices_fuel_size, file=paste0('./ModelDevelopment/EDM/pubs/figs/prices_fuel_2020-22.csv'),row.names=F)
    
    ##input harvest rates here
    ##***Change this to calculate size-specific prices, adjust under various scenarios, then calculate U**
    q=0.401774546
    cpue_land_G_nooutlier <- cpue_land_Gprice %>% mutate(hrate= q*tailmp/CPUE)  %>% filter(hrate<0.95)
    pricemod_G  <- lm( hrate ~  log(Price_GOM) * SIZE + log(fuel_price), data=cpue_land_G_nooutlier)
    summary(pricemod_G)
    
    coef <- pricemod_G$coefficients

    cpue_land_Gprice$hrate_est_price_fuel = ifelse(cpue_land_Gprice$SIZE=='Large', coef[1]+ coef[2]*log(cpue_land_G$Price_GOM) + coef[5]*log(cpue_land_Gprice$fuel_price) ,
                                              ifelse(cpue_land_Gprice$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(cpue_land_Gprice$Price_GOM) + coef[5]*log(cpue_land_Gprice$fuel_price),
                                                     ifelse(cpue_land_Gprice$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(cpue_land_Gprice$Price_GOM) + coef[5]*log(cpue_land_Gprice$fuel_price),
                                                            -1)))
    
    
    ##pull recent harvest rates
    Uproj_price1 <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate),
        z1 = mean(Price_GOM),
        z2 = mean(fuel_price)
      ) 
    Uproj_price1 <-  Uproj_price1 %>%
      mutate(
        U = ifelse(Uproj_price1$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price1$z1)+ coef[5]*log(Uproj_price1$z2) ,
                             ifelse(Uproj_price1$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price1$z1)+ coef[5]*log(Uproj_price1$z2),
                                    ifelse(Uproj_price1$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price1$z1)+ coef[5]*log(Uproj_price1$z2),
                                           -1))),
        scenario='status.quo',
        fscenario='status.quo')
    
    
    Uproj_price2 <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*0.75,
        z1 = mean(Price_GOM)*0.75,
        z2 = mean(fuel_price)
      )
    Uproj_price2<- Uproj_price2%>%
      mutate(
        U = ifelse(Uproj_price2$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price2$z1)+ coef[5]*log(Uproj_price2$z2) ,
                   ifelse(Uproj_price2$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price2$z1)+ coef[5]*log(Uproj_price2$z2),
                          ifelse(Uproj_price2$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price2$z1)+ coef[5]*log(Uproj_price2$z2),
                                 -1))),
        scenario='farmed',
        fscenario='status.quo')
    
    
    Uproj_price3 <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*1.25,
        z1 = mean(Price_GOM)*1.25,
        z2 = mean(fuel_price)
      )
    Uproj_price3<- Uproj_price3%>%
      mutate( U = ifelse(Uproj_price3$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price3$z1)+ coef[5]*log(Uproj_price3$z2) ,
                         ifelse(Uproj_price3$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price3$z1)+ coef[5]*log(Uproj_price3$z2),
                                ifelse(Uproj_price3$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price3$z1)+ coef[5]*log(Uproj_price3$z2),
                                       -1))),
              scenario='tariffs',
              fscenario='status.quo')
    
    Uproj_price4 <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*1.5,
        z1 = mean(Price_GOM)*1.5,
        z2 = mean(fuel_price)
      )
    Uproj_price4<-Uproj_price4%>%
      mutate(U = ifelse(Uproj_price4$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price4$z1)+ coef[5]*log(Uproj_price4$z2) ,
                        ifelse(Uproj_price4$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price4$z1)+ coef[5]*log(Uproj_price4$z2),
                               ifelse(Uproj_price4$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price4$z1)+ coef[5]*log(Uproj_price4$z2),
                                      -1))),
             scenario='label.premium',
             fscenario='status.quo')
    
    
    
    ##################################################################################
    ##**DEFINE FUEL HERE**
    ##################################################################################
    maxfuel <- 4#max(cpue_land_Gprice$fuel_price)
    minfuel <- 1.5#min(cpue_land_Gprice$fuel_price)
    
    ### LOW PRICE SCENARIO
    Uproj_price1a <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate),
        z1 = mean(Price_GOM),
        z2 = minfuel
      ) 
    Uproj_price1a <-  Uproj_price1a %>%
      mutate(
        U = ifelse(Uproj_price1a$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price1a$z1)+ coef[5]*log(Uproj_price1a$z2) ,
                   ifelse(Uproj_price1a$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price1a$z1)+ coef[5]*log(Uproj_price1a$z2),
                          ifelse(Uproj_price1a$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price1a$z1)+ coef[5]*log(Uproj_price1a$z2),
                                 -1))),
        scenario='status.quo',
        fscenario='min')
    
    
    Uproj_price2a <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*0.75,
        z1 = mean(Price_GOM)*0.75,
        z2 = minfuel
      )
    Uproj_price2a<- Uproj_price2a%>%
      mutate(
        U = ifelse(Uproj_price2a$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price2a$z1)+ coef[5]*log(Uproj_price2a$z2) ,
                   ifelse(Uproj_price2a$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price2a$z1)+ coef[5]*log(Uproj_price2a$z2),
                          ifelse(Uproj_price2a$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price2a$z1)+ coef[5]*log(Uproj_price2a$z2),
                                 -1))),
        scenario='farmed',
        fscenario='min')
    
    
    Uproj_price3a <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*1.25,
        z1 = mean(Price_GOM)*1.25,
        z2 = minfuel
      )
    Uproj_price3a<- Uproj_price3a%>%
      mutate( U = ifelse(Uproj_price3a$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price3a$z1)+ coef[5]*log(Uproj_price3a$z2) ,
                         ifelse(Uproj_price3a$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price3a$z1)+ coef[5]*log(Uproj_price3a$z2),
                                ifelse(Uproj_price3a$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price3a$z1)+ coef[5]*log(Uproj_price3a$z2),
                                       -1))),
              scenario='tariffs',
              fscenario='min')
    
    Uproj_price4a <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*1.5,
        z1 = mean(Price_GOM)*1.5,
        z2 = minfuel
      )
    Uproj_price4a<-Uproj_price4a%>%
      mutate(U = ifelse(Uproj_price4a$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price4a$z1)+ coef[5]*log(Uproj_price4a$z2) ,
                        ifelse(Uproj_price4a$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price4a$z1)+ coef[5]*log(Uproj_price4a$z2),
                               ifelse(Uproj_price4a$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price4a$z1)+ coef[5]*log(Uproj_price4a$z2),
                                      -1))),
             scenario='label.premium',
             fscenario='min')
    
    
    
    
    
    ### HIGH PRICE SCENARIO
    Uproj_price1b <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate),
        z1 = mean(Price_GOM),
        z2 = maxfuel
      ) 
    Uproj_price1b <-  Uproj_price1b %>%
      mutate(
        U = ifelse(Uproj_price1b$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price1b$z1)+ coef[5]*log(Uproj_price1b$z2) ,
                   ifelse(Uproj_price1b$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price1b$z1)+ coef[5]*log(Uproj_price1b$z2),
                          ifelse(Uproj_price1b$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price1b$z1)+ coef[5]*log(Uproj_price1b$z2),
                                 -1))),
        scenario='status.quo',
        fscenario='max')
    
    
    Uproj_price2b <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*0.75,
        z1 = mean(Price_GOM)*0.75,
        z2 = maxfuel
      )
    Uproj_price2b<- Uproj_price2b%>%
      mutate(
        U = ifelse(Uproj_price2b$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price2b$z1)+ coef[5]*log(Uproj_price2b$z2) ,
                   ifelse(Uproj_price2b$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price2b$z1)+ coef[5]*log(Uproj_price2b$z2),
                          ifelse(Uproj_price2b$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price2b$z1)+ coef[5]*log(Uproj_price2b$z2),
                                 -1))),
        scenario='farmed',
        fscenario='max')
    
    
    Uproj_price3b <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*1.25,
        z1 = mean(Price_GOM)*1.25,
        z2 = maxfuel
      )
    Uproj_price3b<- Uproj_price3b%>%
      mutate( U = ifelse(Uproj_price3b$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price3b$z1)+ coef[5]*log(Uproj_price3b$z2) ,
                         ifelse(Uproj_price3b$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price3b$z1)+ coef[5]*log(Uproj_price3b$z2),
                                ifelse(Uproj_price3b$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price3b$z1)+ coef[5]*log(Uproj_price3b$z2),
                                       -1))),
              scenario='tariffs',
              fscenario='max')
    
    Uproj_price4b <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        Uavg = mean(Urate)*1.5,
        z1 = mean(Price_GOM)*1.5,
        z2 = maxfuel
      )
    Uproj_price4b<-Uproj_price4b%>%
      mutate(U = ifelse(Uproj_price4b$SIZE=='Large', coef[1]+ coef[2]*log(Uproj_price4b$z1)+ coef[5]*log(Uproj_price4b$z2) ,
                        ifelse(Uproj_price4b$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(Uproj_price4b$z1)+ coef[5]*log(Uproj_price4b$z2),
                               ifelse(Uproj_price4b$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(Uproj_price4b$z1)+ coef[5]*log(Uproj_price4b$z2),
                                      -1))),
             scenario='label.premium',
             fscenario='max')
    
    
    Uproj_price5 <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      group_by(SIZE) %>%
      summarize(
        z1 = mean(Price_GOM),
        z2 = mean(fuel_price)
      )
    Uproj_price5<-Uproj_price5%>%
      mutate(U = c(0.265 ,0.265001, 0.265002 ),
             Uavg=U,
        scenario='msy',
        fscenario='status.quo')  
    
    
    
    
    
    
    Uproj_scenario <- rbind(Uproj_price1, Uproj_price2, Uproj_price3, Uproj_price4,
                            Uproj_price1a, Uproj_price2a, Uproj_price3a, Uproj_price4a,
                            Uproj_price1b, Uproj_price2b, Uproj_price3b, Uproj_price4b, Uproj_price5) %>%
                      mutate(scenario_run = paste0('price',scenario,'_fuel',fscenario))
    # Uproj_scenario <- Uproj_scenario %>%
    #                     mutate(
    #                       full_scenario = paste0(scenario,'_',fscenario)
    #                     )
    # 
      ##pull size specific harvest rates
      # Uproj_Gprice <- cpue_land_Gprice %>% filter(YEAR>=2020) %>%
      #   group_by(SIZE) %>%
      #   summarize(
      #     U_20_22 = mean(Urate)
      #   )
    
    
    
    ###***LOOP**
    ##fillers to run line by line without loop
    # s='farmed'
    # p='Large'
    # f='min'
    # U=Uproj_scenario[(Uproj_scenario$scenario==s & Uproj_scenario$fscenario==f & Uproj_scenario$SIZE==p),]$U
    # for(s in unique(Uproj_scenario$scenario)){
    #   Uproj_scenario_run = Uproj_scenario%>%filter(Uproj_scenario$scenario==s)
    #   for(f in unique(Uproj_scenario$fscenario)){
    #     Uproj_scenario_runf = Uproj_scenario_run%>%filter(Uproj_scenario_run$fscenario==f)
    #   ##^^pull each scenario, then run each harvest rate specific to each size, extract and set together
    # for(U in unique(Uproj_scenario_runf$U)){
    #   for(p in unique(Uproj_scenario_runf$SIZE)) {
    # price.fuel.mod.proj = predict_iter(price.fuel.mod.fit, newdata=price.fuel.mod.fore, hrate=U)
    
    # p='Large'
    # s='pricefarmed_fuelmin'
    # U=Uproj_scenario[(Uproj_scenario$scenario_run==s & Uproj_scenario$SIZE==p),]$U
    for(s in unique(Uproj_scenario$scenario_run)){
      Uproj_scenario_run = Uproj_scenario%>%filter(Uproj_scenario$scenario_run==s)
      ##^^pull each scenario, then run each harvest rate specific to each size, extract and set together
      for(U in unique(Uproj_scenario_run$U)){
        for(p in unique(Uproj_scenario_run$SIZE)) {
          Uproj_scenario_size <- Uproj_scenario_run[Uproj_scenario_run$SIZE==p,]
          price.fuel.mod.proj = predict_iter(price.fuel.mod.fit, newdata=price.fuel.mod.fore, hrate=U)
    
          
          ##notes: need to add "if" statement to create output only if U matches the size p 
    
    
    #plot(this.run.fit$outsampresults)
    #plot(price.fuel.mod.proj)
    ##NOTE: YEAR HERE IS WRONG ; SHOULD BE SEASONAL TIME STEP BY 0.5 YEAR
    ##extract data, correct timestep (year) label
    price.fuel.mod.proj.outsamp <- price.fuel.mod.proj$outsampresults
    head(price.fuel.mod.proj.outsamp)
    
    ##**APPLY TO PROJECTIONS**
    ###add sd, etc for proj
    ypred_test = ytransfuninv(y=price.fuel.mod.proj.outsamp$predmean_trans, e1=price.fuel.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypred_uci  = ytransfuninv(y=(price.fuel.mod.proj.outsamp$predmean_trans+2*price.fuel.mod.proj.outsamp$predsd_trans), e1=price.fuel.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypred_lci  = ytransfuninv(y=(price.fuel.mod.proj.outsamp$predmean_trans-2*price.fuel.mod.proj.outsamp$predsd_trans), e1=price.fuel.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypredf_uci = ytransfuninv(y=(price.fuel.mod.proj.outsamp$predmean_trans+2*price.fuel.mod.proj.outsamp$predfsd_trans), e1=price.fuel.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    ypredf_lci = ytransfuninv(y=(price.fuel.mod.proj.outsamp$predmean_trans-2*price.fuel.mod.proj.outsamp$predfsd_trans), e1=price.fuel.mod.proj.outsamp$escapement_1, ytrans=ytrans)
    
    price.fuel.mod.proj.outsamp2 <- cbind(price.fuel.mod.proj.outsamp,ypred_test, ypred_uci, ypred_lci, ypredf_uci, ypredf_lci)
    
    ##hard coded for nfore=10, can make more dynamic later
    ##creates a new variable that accurately matches the BSH time step of 0.5
    timestep2 <- sort(rep(seq(from=2023, to=2027.5 , by=0.5),3))
    
    ##replace projection timestep with correct label
    price.fuel.mod.proj.outsamp3 <- price.fuel.mod.proj.outsamp2 %>% mutate(timestep=timestep2)
    head(price.fuel.mod.proj.outsamp3)
    ##calculate landings from the removals estimated from price
    price.fuel.mod.proj.outsamp3 <- price.fuel.mod.proj.outsamp3 %>% 
                                          mutate(tailmp_1=removals_est_price_fuel_1/q,
                                                 tailmp_2=removals_est_price_fuel_2/q,
                                                 tailmp_3=removals_est_price_fuel_3/q,
                                                 tailmp_4=removals_est_price_fuel_4/q,
                                                 tailmp_5=removals_est_price_fuel_5/q) #%>%
                               #select(-c('removals_est_price_1','removals_est_price_2','removals_est_price_3','removals_est_price_4','removals_est_price_5'))
      head(price.fuel.mod.proj.outsamp3)
    price.fuel.mod.proj.outsamp4 <- price.fuel.mod.proj.outsamp3 %>% 
                                select(-c('tailmp_1','tailmp_2','tailmp_3', 'tailmp_4', 'tailmp_5',
                                          'removals_est_price_fuel_1','removals_est_price_fuel_2','removals_est_price_fuel_3','removals_est_price_fuel_4','removals_est_price_fuel_5')  )
    head(price.fuel.mod.proj.outsamp4)
    
    ## set projection data with input data, then plot
    price.fuel.mod.in.samp <- as.data.frame(price.fuel.mod.fit$insampresults) 
    head(price.fuel.mod.in.samp)
    price.fuel.mod.in.samp.pred <- price.fuel.mod.in.samp %>% select(-c('obs_trans','obs'))
    head(price.fuel.mod.in.samp.pred)
    price.fuel.mod.in.samp.obs <- price.fuel.mod.in.samp %>% select(c('timestep','pop','obs_trans','obs'))
    head(price.fuel.mod.in.samp.obs)
    
    ###add sd, etc for insamp
    ypred_test = ytransfuninv(y=price.fuel.mod.in.samp.pred$predmean_trans, e1=price.fuel.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypred_uci  = ytransfuninv(y=(price.fuel.mod.in.samp.pred$predmean_trans+2*price.fuel.mod.in.samp.pred$predsd_trans), e1=price.fuel.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypred_lci  = ytransfuninv(y=(price.fuel.mod.in.samp.pred$predmean_trans-2*price.fuel.mod.in.samp.pred$predsd_trans), e1=price.fuel.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypredf_uci = ytransfuninv(y=(price.fuel.mod.in.samp.pred$predmean_trans+2*price.fuel.mod.in.samp.pred$predfsd_trans), e1=price.fuel.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    ypredf_lci = ytransfuninv(y=(price.fuel.mod.in.samp.pred$predmean_trans-2*price.fuel.mod.in.samp.pred$predfsd_trans), e1=price.fuel.mod.in.samp.pred$escapement_1, ytrans=ytrans)
    
    price.fuel.mod.in.samp.pred2 <- cbind(price.fuel.mod.in.samp.pred,ypred_test, ypred_uci, ypred_lci, ypredf_uci, ypredf_lci)
    
    price.fuel.mod.all.samp <- rbind(price.fuel.mod.in.samp.pred2, price.fuel.mod.proj.outsamp4)
    
    # price.mod.all.samp2 <- full_join(price.mod.all.samp, price.mod.in.samp.obs, by=c('timestep','pop')) %>%
    #   mutate(data='price.mod')
    #Uproj_scenario_run
    ##If the Urate is equal to the original optimal run for the given size (i.e. reason running whole model), export that size data
#    current_target <- Uproj_scenario_run$U[Uproj_scenario_run$SIZE == p]
    if(U==Uproj_scenario_size$U){
      assign(paste0('price.fuel.mod.all.samp.',Uproj_scenario_run[U==Uproj_scenario_run$U,]$scenario_run,'.',Uproj_scenario_run[p==Uproj_scenario_run$SIZE,]$SIZE),
           full_join(price.fuel.mod.all.samp, price.fuel.mod.in.samp.obs, by=c('timestep','pop')) %>%
      mutate(data='price.fuel.mod',
             scenario=Uproj_scenario_run[U==Uproj_scenario_run$U,]$scenario,
             fscenario=Uproj_scenario_run[U==Uproj_scenario_run$U,]$fscenario,
             scenario_run=Uproj_scenario_run[U==Uproj_scenario_run$U,]$scenario_run,
             U=U))#%>%
      #filter(pop==p))##filter by size class only for each specific harvest rate -- set back together below
      }
    
    ###**pull and set landings**
    head(price.fuel.mod.proj.outsamp3)
    landings_price_fuel <- cpue_land_Gprice %>% select(
      timestep=YEAR2,
      pop=SIZE,
      tailmp=tailmp_est_price_fuel
    )
    
    landings_price_fuel_project <- price.fuel.mod.proj.outsamp3 %>% select(
      timestep,
      pop,
      tailmp_1
    )
    ##lagged landings in 2023 are the landings from 2022.5
    ##need to chop them off and set back together from projections
    
    ##pull timestep,pop and chop off last 3 vars (last time step)
    landings_price_fuel_proj_yr <- head(landings_price_fuel_project %>% select(timestep,pop),-3)
    ##pull value, drop first 3 values (replicated in 2022.5 bc lagged var here, rename var to reflect lag removal)
    landings_price_fuel_proj_val <- tail(landings_price_fuel_project %>% select(tailmp=tailmp_1),-3)
    ##bind together
    landings_price_fuel_project2 <- cbind(landings_price_fuel_proj_yr,landings_price_fuel_proj_val)
    
    ##bind landings project with landings
    price.fuel.mod.land.project <-rbind(landings_price_fuel,landings_price_fuel_project2)
    #tail(price.fuel.mod.land.project)
    if(U==Uproj_scenario_size$U){
    assign(paste0('price.fuel.mod.land.',Uproj_scenario_run[U==Uproj_scenario_run$U,]$scenario_run,'.',Uproj_scenario_run[p==Uproj_scenario_run$SIZE,]$SIZE),
             price.fuel.mod.land.project %>%
             mutate(data='price.fuel.mod',
                    scenario=Uproj_scenario_run[U==Uproj_scenario_run$U,]$scenario,
                    fscenario=Uproj_scenario_run[U==Uproj_scenario_run$U,]$fscenario,
                    scenario_run=Uproj_scenario_run[U==Uproj_scenario_run$U,]$scenario_run,
                    U=U))#%>%
            # filter(pop==p))##filter by size class only for each specific harvest rate -- set back together below
    }
      }}
      }
    
    

    best.mod.all.samp2 <- best.mod.all.samp2 %>% mutate(scenario='edm.nopricefuel',
                                                        fscenario='edm.nopricefuel',
                                                        scenario_run='edm.nopricefuel',
                                                        U=999) ##filler to match rows below ; this U is from PROJECTIONS ONLY to troubleshoot/verify outputs; DROP LATER
    
    #########*****SET THE DATA TOGETHER AND CREATE ALL.SAMP PLOTS WITH LINETYPE=DATA********** 
    all.samp2 <- best.mod.all.samp2 ##initiate file
    #all.samp <- rbind(best.mod.all.samp, price.mod.all.samp)
    
    ##**MSY scenario doesn't have fuel scenario runs ; extract then reset together after loop**
    # msy_scenario <- Uproj_scenario[Uproj_scenario$scenario=='msy',]
    # Uproj_scenario <- Uproj_scenario[Uproj_scenario$scenario!='msy',]
    
    for(s in unique(Uproj_scenario$scenario_run)){
    for(p in unique(Uproj_scenario_run$SIZE)) {
    all.samp2 <- rbind(all.samp2, get(paste0('price.fuel.mod.all.samp.',s,'.',p)))
    }}
    
    # ##set back together with msy scenario
    # for(s in 'msy'){
    # for(f in 'status.quo'){
    # for(p in unique(Uproj_scenario_run$SIZE)) {
    #   all.samp2 <- rbind(all.samp2, get(paste0('price.fuel.mod.all.samp.',s,'.fuel',f,'.',p)))
    # }}}
    
    table(all.samp2$scenario,all.samp2$fscenario, useNA='always')
    3*(2027-1987+1)*2 *3
    
    table(all.samp2$pop, useNA='always')
    
    ## test shows we're pulling all 3 sizes scenarios ; leftjoin and drop 
    test <- all.samp2 %>% filter(timestep==2025) %>% select(timestep, pop, predmean, data, scenario, fscenario, U, scenario_run)
    
    ##pull data to left_join
    Uproj_select <- Uproj_scenario %>% 
                       select(pop = SIZE,
                              U,
                              scenario_run)
    
    all.samp2.out <- left_join(Uproj_select, all.samp2, by=c('pop','U','scenario_run'))
    table(all.samp2.out$scenario,all.samp2.out$fscenario, useNA='always')
    3*(2027-1987+1)*2
    
    all.samp2.out <- rbind(all.samp2.out, best.mod.all.samp2)
    
    write.csv(all.samp2.out, file=paste0('./ModelDevelopment/EDM/pubs/figs/BSH_price_fuel_mod_allsamp.csv'),row.names=F)
    
    
    ###set all price together
    best.mod.land.project <- best.mod.land.project %>% mutate(data='best.mod',scenario='edm.nopricefuel',
                                                                              fscenario='edm.nopricefuel',
                                                              scenario_run='edm.nopricefuel',
                                                              U=999) ##filler to match rows below ; this U is from PROJECTIONS ONLY to troubleshoot/verify outputs; DROP LATER
    
    head(best.mod.land.project)
    
    #########*****SET THE DATA TOGETHER AND CREATE ALL.LAND PLOTS WITH LINETYPE=DATA********** 
    all.land <- best.mod.land.project ##initiate file
    #all.samp <- rbind(best.mod.all.samp, price.mod.all.samp)
    for(s in unique(Uproj_scenario$scenario_run)){
      for(p in unique(Uproj_scenario_run$SIZE)) {
        all.land <- rbind(all.land, get(paste0('price.fuel.mod.land.',s,'.',p)))
      }}
    
    # ##set back together with msy scenario
    # for(s in 'msy'){
    #   for(f in 'status.quo'){
    #     for(p in unique(Uproj_scenario_run$SIZE)) {
    #       all.land <- rbind(all.land, get(paste0('price.fuel.mod.land.',s,'.fuel',f,'.',p)))
    #     }}}
    
    table(all.land$scenario,all.land$fscenario, useNA='always')
    3*(2027-1987+1)*2 #all.land is one time step shorter: 243
    
    ##filter out incorrect harvest rate / size combos, set with original model again (not efficient...)
    all.land.out <- left_join(Uproj_select, all.land, by=c('pop','U','scenario_run'))
    table(all.land.out$scenario,all.land.out$fscenario, useNA='always')
    3*(2027-1987+1)*2
    all.land.out <- rbind(all.land.out, best.mod.land.project)
    
    
    ##define best.mod.land landings as observations (bc they are)
    best.mod.land.merge <- best.mod.land.project %>% select(timestep, pop, obs=tailmp) %>%
                               filter(timestep<2023)
    
    all.land <- full_join(all.land.out, best.mod.land.merge, by=c('timestep','pop'))
    
    ##redefine original msy scenario within uproj file
    #Uproj_scenario <- rbind(Uproj_scenario,msy_scenario)
    
    ##merge in price scenarios and multiply out for revenue
    Uproj_scenario_merge <- Uproj_scenario %>% bind_rows(
      Uproj_scenario %>% 
        filter(scenario == "status.quo" & fscenario=="status.quo") %>% 
        mutate(scenario = "edm.nopricefuel", fscenario='edm.nopricefuel', scenario_run='edm.nopricefuel_edm.nopricefuel')
    )
    write.csv(Uproj_scenario_merge, file=paste0('./ModelDevelopment/EDM/pubs/figs/Uproj_scenario.csv'),row.names=F)
  
    ##merge in observed prices
    all.land <- full_join(all.land, cpue_land_Gprice%>%select(timestep=YEAR2, pop=SIZE, Price_GOM, fuel_price), by=c('timestep','pop'))
    
    ##merge in projection prices where price is not recorded already
    all.land <- all.land %>%
                  rows_patch(Uproj_scenario_merge%>%select(pop=SIZE,scenario,fscenario,Price_GOM=z1,fuel_price=z2), by=c('scenario','fscenario','pop'))
    ##calculate revenue
    all.land <- all.land %>% mutate(
                   revenue=tailmp*Price_GOM
    )
    
    write.csv(all.land, file=paste0('./ModelDevelopment/EDM/pubs/figs/BSH_price_fuel_mod_land.csv'),row.names=F)
    
    
    ###PLOTS IN NEXT SCRIPT NOW
    
   #  BSH_price_fuel_proj_scenario_size_func <- ggplot(all.samp2 %>% filter(scenario!='edm.noprice')) +
   #    geom_line(aes(y=predmean, x=timestep, linetype=scenario, color=fscenario)) +
   #    facet_wrap(~pop,ncol=1) +
   #    labs(title ="Brown Shrimp Population Projections", 
   #         x = "Year", 
   #         y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 18))
   #  BSH_price_fuel_proj_scenario_size_func
   #  export_fig(BSH_price_fuel_proj_scenario_size_func, size='big',  folder='EDM_pubs')
   #  
   #  
   #  BSH_price_fuel_proj_scenario_size_func_scale <- ggplot(all.samp2 %>% filter(scenario!='edm.noprice')) +
   #    geom_line(aes(y=predmean, x=timestep, linetype=scenario, color=fscenario)) +
   #    facet_wrap(~pop,ncol=1, scales = "free_y") +
   #    labs(title ="Brown Shrimp Population Projections (U by Price)", 
   #         x = "Year", 
   #         y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 18))
   #  BSH_price_fuel_proj_scenario_size_func_scale
   #  export_fig(BSH_price_fuel_proj_scenario_size_func_scale, size='big',  folder='EDM_pubs')
   #  
   #  #export_fig(msy_pop, filename=paste0(spp.run,"_msy_pop.png"), folder='EDM_final')
   #  # export_fig(msy_pop,
   #  #            folder=paste0("EDMout",spp,"_loop"), exportname=name)
   #  
   #  ###**CREATE COMPLEMENTARY LANDINGS FIGURES--SHOW THAT THE POPULATION IS FINE WITH UP TO X REMOVALS**
   #  ###**ALL GAIN NO PAIN!*
   #  
   #  
   #  
   #  BSH_price_fuel_proj_scenario_size_func_LAND_msy <- ggplot(all.land %>% filter(scenario!='edm.noprice', timestep>2021)) +
   #    geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=fscenario)) +
   #    facet_wrap(~pop,ncol=1) +
   #    labs(title ="Brown Shrimp Landings Projections", 
   #         x = "Year", 
   #         y =paste0("Landings (million lbs tails")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 18))
   #  BSH_price_fuel_proj_scenario_size_func_LAND_msy
   #  export_fig(BSH_price_fuel_proj_scenario_size_func_LAND_msy, size='big',  folder='EDM_pubs')
   #  
   #  
   #  ##Facet grid and be intentional / make these better
   # 
   # # BSH_price_fuel_edm_land_comparison <-ggplot(all.land %>% filter(scenario=='edm.noprice'| scenario=='status.quo')) +
   # #    geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=fscenario)) +
   # #    facet_wrap(~pop,ncol=1) +
   # #    labs(title ="Brown Shrimp Landings Projections",
   # #         x = "Year",
   # #         y =paste0("Landings (million lbs tails)")) +                                                      # Labels
   # #    theme_minimal(base_size = 20) +                                                 # Large font
   # #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   # #          axis.title = element_text(size = 22),                                     # Larger axis titles
   # #          axis.text = element_text(size = 18))
   # # BSH_price_fuel_edm_land_comparison
   # # export_fig(BSH_price_edm_land_comparison, size='big',  folder='EDM_pubs')
   # 
   # BSH_price_fuel_proj_scenario_size_func_LAND <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=fscenario)) +
   #   facet_wrap(~pop,ncol=1) +
   #   labs(title ="Brown Shrimp Landings Projections",
   #        x = "Year",
   #        y =paste0("Landings (million lbs tails)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_fuel_proj_scenario_size_func_LAND
   # export_fig(BSH_price_fuel_proj_scenario_size_func_LAND, size='big',  folder='EDM_pubs')
   # 
   # BSH_price_fuel_proj_scenario_size_func_LAND_color <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=fscenario)) +
   #   facet_wrap(~pop,ncol=1) +
   #   labs(title ="Brown Shrimp Landings Projections",
   #        x = "Year",
   #        y =paste0("Landings (million lbs tails)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_fuel_proj_scenario_size_func_LAND_color
   # export_fig(BSH_price_fuel_proj_scenario_size_func_LAND_color, size='big',  folder='EDM_pubs')
   # 
   # 
   # ####**ADD A REVENUE BY SCENARIO PLOT**
   # BSH_price_fuel_proj_scenario_size_func_REVENUE <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=revenue, x=timestep, linetype=scenario, color=fscenario)) +
   #   facet_wrap(~pop,ncol=1) +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_fuel_proj_scenario_size_func_REVENUE
   # export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE, size='big',  folder='EDM_pubs')
   # 
   # 
   # ####**FACET_GRID**
   # BSH_price_fuel_proj_scenario_size_func_REVENUE_grid <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=revenue, x=timestep, linetype=scenario, color=scenario)) +
   #   facet_grid(fscenario~pop) +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_fuel_proj_scenario_size_func_REVENUE_grid
   # export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE_grid, size='big',  folder='EDM_pubs')
   # 
   # 
   # ####**FACET_GRID2**
   # BSH_price_fuel_proj_scenario_size_func_REVENUE_grid2 <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=revenue, x=timestep, linetype=pop, color=pop)) +
   #   facet_grid(fscenario~scenario) +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_fuel_proj_scenario_size_func_REVENUE_grid2
   # export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE_grid2, size='big',  folder='EDM_pubs')
   # 
   # 
   # ############################################################################
   # ############################################################################
   # ############################################################################
   # ############################################################################
   # ############################################################################
   # ############################################################################
   # source("./Functions/fig_format_export.R")
   # 
   # BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3 <- all.land %>% 
   #   # 1. Filter the data
   #   filter(scenario != 'edm.noprice' & scenario != 'msy' & timestep > 2021) %>%
   #   
   #   # 2. Reorder and rename the variables using factors
   #   mutate(
   #     fscenario = factor(fscenario, 
   #                        levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
   #                        labels = c("Fuel $1.50", "Fuel $2.42", "Fuel $4.00")),   # The new names you want displayed on the plot
   #     
   #     scenario = factor(scenario, 
   #                       levels = c("farmed","status.quo", "tariffs" ,"label.premium"), 
   #                       labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling"))
   #   ) %>%
   #   
   #   # 3. Pass to ggplot
   #   ggplot() +
   #   geom_line(aes(y=revenue, x=timestep, linetype=pop, color=pop)) +
   #   facet_grid(fscenario ~ scenario) +
   #   labs(title = "Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y = paste0("Revenue ($2022)"),
   #        color = "Size Class",       # Updates the legend title for fscenario
   #        linetype = "Size Class") +# Updates the legend title for scenario
   #   # ADDS COLORBLIND-FRIENDLY & GRAYSCALE-SAFE COLORS:
   #   #scale_color_grey(start = 0, end = 0.75)+
   #   scale_color_manual(values=size_color)+
   #   theme_minimal(base_size = 20) +
   #   theme(plot.title = element_text(hjust = 0.5),
   #         axis.title = element_text(size = 22),
   #         axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
   #         axis.text.y = element_text(size = 18),
   #         panel.spacing = unit(1, "lines"))
   # BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3
   # export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3, size='bigw',  folder='EDM_pubs')
   # 
   # 
   # 
   # 
   # 
   # 
   # 
   # 
   # 
   # 
   # BSH_price_fuel_proj_scenario_size_func_POP <- all.samp2 %>% 
   #   # 1. Filter the data
   #   filter(scenario != 'edm.noprice' & scenario != 'msy' & timestep > 2021) %>%
   #   
   #   # 2. Reorder and rename the variables using factors
   #   mutate(
   #     fscenario = factor(fscenario, 
   #                        levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
   #                        labels = c("Fuel $1.50", "Fuel $2.42", "Fuel $4.00")),   # The new names you want displayed on the plot
   #     
   #     scenario = factor(scenario, 
   #                       levels = c("farmed","status.quo", "tariffs" ,"label.premium"), 
   #                       labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling"))
   #   ) %>%
   #   
   #   # 3. Pass to ggplot
   #   ggplot() +
   #   geom_line(aes(y=predmean, x=timestep, linetype=pop, color=pop)) +
   #   facet_grid(fscenario ~ scenario) +
   #   labs(title = "Brown Shrimp Population Projections",
   #        x = "Year",
   #        y = paste0("CPUE"),
   #        color = "Size Class",       # Updates the legend title for fscenario
   #        linetype = "Size Class") +# Updates the legend title for scenario
   #   # ADDS COLORBLIND-FRIENDLY & GRAYSCALE-SAFE COLORS:
   #   #scale_color_grey(start = 0, end = 0.75)+
   #   scale_color_manual(values=size_color)+
   #   theme_minimal(base_size = 20) +
   #   theme(plot.title = element_text(hjust = 0.5),
   #         axis.title = element_text(size = 22),
   #         axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
   #         axis.text.y = element_text(size = 18),
   #         panel.spacing = unit(1, "lines"))
   # BSH_price_fuel_proj_scenario_size_func_POP
   # export_fig(BSH_price_fuel_proj_scenario_size_func_POP, size='bigw',  folder='EDM_pubs')
   # 
   # 
   # 
   # 
   ############################################################################
   ############################################################################
   ############################################################################
   ############################################################################
   ############################################################################
   ############################################################################
   # 
   # ####**ADD A REVENUE BY SCENARIO PLOT**
   # BSH_price_fuel_proj_scenario_size_func_REVENUE2 <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=revenue, x=timestep, linetype=fscenario, color=fscenario)) +
   #   facet_grid(rows='pop', cols='fscenario') +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_fuel_proj_scenario_size_func_REVENUE2
   # export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE, size='big',  folder='EDM_pubs')
   # 
   # BSH_price_proj_scenario_size_func_REVENUE_color <- ggplot(all.land %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=revenue, x=timestep, linetype=scenario, color=scenario)) +
   #   facet_wrap(~pop,ncol=1) +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_proj_scenario_size_func_REVENUE_color
   # export_fig(BSH_price_proj_scenario_size_func_REVENUE_color, size='big',  folder='EDM_pubs')
   # 
   # all.land.agg <- all.land %>%group_by(scenario,timestep) %>%
   #                  summarize(rev_total=sum(revenue))
   # 
   # ####**ADD A REVENUE BY SCENARIO PLOT**
   # BSH_price_proj_scenario_REVENUE <- ggplot(all.land.agg %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=rev_total, x=timestep, linetype=scenario)) +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_proj_scenario_REVENUE
   # export_fig(BSH_price_proj_scenario_REVENUE, size='big',  folder='EDM_pubs')
   # 
   # BSH_price_proj_scenario_REVENUE_color <- ggplot(all.land.agg %>% filter(scenario!='edm.noprice'& scenario!='msy' & timestep>2021)) +
   #   geom_line(aes(y=rev_total, x=timestep, linetype=scenario, color=scenario)) +
   #   labs(title ="Brown Shrimp Revenue Projections",
   #        x = "Year",
   #        y =paste0("Revenue ($2022)")) +                                                      # Labels
   #   theme_minimal(base_size = 20) +                                                 # Large font
   #   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #         axis.title = element_text(size = 22),                                     # Larger axis titles
   #         axis.text = element_text(size = 18))
   # BSH_price_proj_scenario_REVENUE_color
   # export_fig(BSH_price_proj_scenario_REVENUE_color, size='big',  folder='EDM_pubs')
   # 
   #  ###***MOVE THE CODE BELOW ELSEWHERE, OR REPLICATE MEANINGFUL SCENARIOS WITH ERROR (KEEP BY MODEL TYPE BELOW IN TACT)**
   # 
   #  ggplot(all.samp2 %>% filter(timestep<=2025, (scenario=='edm.noprice' | scenario=='status.quo' ))) +
   #    geom_line(aes(y=predmean, x=timestep, linetype=data)) +
   #    geom_point(aes(y=obs, x=timestep), size=1) +
   #    facet_wrap(~pop,ncol=1, scales="free") +
   #    labs(title ="Brown Shrimp Population Projections",
   #         x = "Year",
   #         y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 18))
   # 
   # 
   #  ##filter out all runs except comparison of status quo models for price and no price (respective averages for 2020-2022)
   #  all.samp3 <-  full_join((all.samp2 %>% filter((scenario=='edm.noprice' | scenario=='status.quo' ))),
   #                          seamap_G_2324, by=c('timestep','pop'))
   #  
   #  
   #  BSH_price_fuel_proj <- ggplot(all.samp3 %>% filter(timestep<=2024.5)) +
   #    geom_line(aes(y=predmean, x=timestep, linetype=data)) +
   #    geom_point(aes(y=obs, x=timestep), size=1) +
   #    geom_point(aes(y=update, x=timestep), shape=1, size=1) +
   #    geom_vline(xintercept=2022.5, linetype='dashed')+
   #    facet_wrap(~pop,ncol=1, scales='free') +
   #    labs(title ="Brown Shrimp Population Projections", 
   #         x = "Year", 
   #         y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 16))
   #  export_fig(BSH_price_fuel_proj, size='big',  folder='EDM_pubs')
   #  
   #  BSH_price_fuel_proj_err <- ggplot(all.samp3 %>% filter(timestep<=2024.5)) +
   #    geom_line(aes(y=predmean, x=timestep, linetype=data)) +
   #    geom_point(aes(y=obs, x=timestep), size=1) +
   #    geom_point(aes(y=update, x=timestep), shape=1, size=1) +
   #    geom_ribbon(aes(x = timestep, ymin = ypred_lci, ymax = ypred_uci, fill=data), alpha = 0.2) + # Example for a ribbon-style error bar
   #    geom_vline(xintercept=2022.5, linetype='dashed')+
   #    facet_wrap(~pop,ncol=1, scales='free') +
   #    labs(title ="Brown Shrimp Population Projections", 
   #         x = "Year", 
   #         y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 16))
   #  BSH_price_fuel_proj_err
   #  export_fig(BSH_price_fuel_proj_err, size='big', folder='EDM_pubs')
   #  
   #  
   #  BSH_price_fuel_proj_ferr <- ggplot(all.samp3 %>% filter(timestep<=2024.5)) +
   #    geom_line(aes(y=predmean, x=timestep, linetype=data)) +
   #    geom_point(aes(y=obs, x=timestep),size=1) +
   #    geom_point(aes(y=update, x=timestep), shape=1, size=1) +
   #    geom_ribbon(aes(x = timestep, ymin = ypredf_lci, ymax = ypredf_uci, fill=data), alpha = 0.2) + # Example for a ribbon-style error bar
   #    geom_vline(xintercept=2022.5, linetype='dashed')+
   #    facet_wrap(~pop,ncol=1, scales='free') +
   #    labs(title ="Brown Shrimp Population Projections", 
   #         x = "Year", 
   #         y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 16))
   #  export_fig(BSH_price_fuel_proj_ferr, size='big',  folder='EDM_pubs')
   #  
   #  
    