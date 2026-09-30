##Load libraries
source("./ModelDevelopment/EDM/00_load-libraries.R")


##**Define combinations of z1, z2 to cycle through in advance, then integrate below**
##Reasonable combinations of covariates
#z1_opt <- c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
#             'tempbotm_std_1','salbotm_std_1','Price_GOM_1','imp_100mp_1')
#z_opt <-

# y2.vec <- c('tailmp','tail10mp')
# z1.vec <- c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
#             'Price_GOM_1','imp_100mp_1','tempbotm_std_1','salbotm_std_1',
#             'tempbotm_std_2','salbotm_std_2')
# E.vec <- 3:4
# scaling.vec <- c('local')
# ytrans.vec <- c('none','log','gr1','gr2')
# predictmethod.vec <- c('sequential')


##**A: BROWN SHRIMP ANNUAL ; SIZE BINS AGG ; AREA AGG (11:21)**
ctrlA1 <- expand.grid(startyr = c(1987),
            assessment='BSH',
            species='Brown Shrimp',
            index_source='SEAMAP',
            y1='CPUE',
            y2='tailmp',
            z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                 'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
            z2=NA,
            pop='GULF',
            time='YEAR',
            E=3:4, 
            tau=1, 
            forecast='FALSE',
            nfore=NA,
            vtimestep='FALSE',
            append='TRUE',
            scaling=c('global'), ##single pop, scaling doesn't matter
            rhofixed=NA,
            rhomatrix=NA,
            augdata=NA,
            predictmethod = c("lto","sequential"), #single pop, lto = loo
            newdata=NA,
            xname=NA,
            ytrans=c('log','gr1','gr2','none'), 
            bshared=c(TRUE,FALSE),
            linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
            ) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlA2 <- ctrlA1 %>%
           mutate(spp.run=paste0(ctrlA1$assessment,'_A',1:length(ctrlA1$startyr)),
                  stratum='A',
                  run=1:length(ctrlA1$startyr),
                  data='cpue_land_A',
                  assessment=as.character(assessment),
                  species=as.character(species),
                  index_source=as.character(index_source),
                  y1=as.character(y1),
                  y2=as.character(y2),
                  z1=as.character(z1),
                  z2=as.character(z2),
                  zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                              ifelse(grepl("salbotm_std",z1),"salbotm_std",
                                     ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                            ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                                   NA)))),
                  zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                               ifelse(grepl("salbotm_std",z2),"salbotm_std",
                                      ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                             ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                                    NA)))), 
                  pop=as.character(pop),
                  time=as.character(time),
                  forecast=as.character(forecast),
                  vtimestep=as.character(vtimestep),
                  append=as.character(append),
                  scaling=as.character(scaling),
                  predictmethod=as.character(predictmethod),
                  ytrans=as.character(ytrans)) %>%
           relocate(c("spp.run","stratum","run","data"))


##**add ctrl with max E, no cov*
ctrlA1maxE <- expand.grid(startyr = c(1987),
                          assessment='BSH',
                          species='Brown Shrimp',
                          index_source='SEAMAP',
                          y1='CPUE',
                          y2='tailmp',
                          z1=NA, ##could add up to _E per run
                          z2=NA,
                          pop='GULF',
                          time='YEAR',
                          E=5, 
                          tau=1, 
                          forecast='FALSE',
                          nfore=NA,
                          vtimestep='FALSE',
                          append='TRUE',
                          scaling=c('global'), ##single pop, scaling doesn't matter
                          rhofixed=NA,
                          rhomatrix=NA,
                          augdata=NA,
                          predictmethod = c("lto","sequential"), #single pop, lto = loo
                          newdata=NA,
                          xname=NA,
                          ytrans=c('log','gr1','gr2','none'),
                          bshared=c(TRUE,FALSE),
                          linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlA2maxE <- ctrlA1maxE %>%
  mutate(spp.run=paste0(ctrlA1maxE$assessment,'_A',1:length(ctrlA1maxE$startyr)+20000),
         stratum='A',
         run=1:length(ctrlA1maxE$startyr)+20000,
         data='cpue_land_A',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_LA",z1),"tempbotm_LA",
                      ifelse(grepl("salbotm_LA",z1),"salbotm_LA",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_LA",z2),"tempbotm_LA",
                      ifelse(grepl("salbotm_LA",z2),"salbotm_LA",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

ctrlA <- rbind(ctrlA2, ctrlA2maxE)



###*B: BROWN SHRIMP ANNUAL ; SIZE BINS AGG ; AREA (11:17, 18:21)**

ctrlB1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop='AREA_ASSESS',
                     time='YEAR',
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'), 
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"), 
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlB2 <- ctrlB1 %>%
  mutate(spp.run=paste0(ctrlB1$assessment,'_B',1:length(ctrlB1$startyr)),
         stratum='B',
         run=1:length(ctrlB1$startyr),
         data='cpue_land_B',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


ctrlB1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA,
                      z2=NA,
                      pop='AREA_ASSESS',
                      time='YEAR',
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'), 
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"), 
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlB2maxE <- ctrlB1maxE %>%
  mutate(spp.run=paste0(ctrlB1maxE$assessment,'_B',1:length(ctrlB1maxE$startyr)+20000),
         stratum='B',
         run=1:length(ctrlB1maxE$startyr)+20000,
         data='cpue_land_B',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

ctrlB <- rbind(ctrlB2, ctrlB2maxE)


###**C: BROWN SHRIMP ANNUAL ; SIZE BINS (>67, 67-31, <=30) ; AREA AGG (11:21)**
ctrlC1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop='SIZE',
                     time='YEAR',
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'), 
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"),
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlC2 <- ctrlC1 %>%
  mutate(spp.run=paste0(ctrlC1$assessment,'_C',1:length(ctrlC1$startyr)),
         stratum='C',
         run=1:length(ctrlC1$startyr),
         data='cpue_land_C',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


###**Csm: BROWN SHRIMP ANNUAL ; SIZE BINS (>31, <=30) ; AREA AGG (11:21)**
ctrlC2sm <- ctrlC1 %>%
  mutate(spp.run=paste0(ctrlC1$assessment,'_C',(1:length(ctrlC1$startyr)+10000)),
         stratum='C',
         run=(1:length(ctrlC1$startyr)+10000),
         data='cpue_land_Csm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


ctrlC1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA, ##could add up to _E per run
                      z2=NA,
                      pop='SIZE',
                      time='YEAR',
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'), 
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"),
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlC2maxE <- ctrlC1maxE %>%
  mutate(spp.run=paste0(ctrlC1maxE$assessment,'_C',1:length(ctrlC1maxE$startyr)+20000),
         stratum='C',
         run=1:length(ctrlC1maxE$startyr)+20000,
         data='cpue_land_C',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


###**Csm: BROWN SHRIMP ANNUAL ; SIZE BINS (>31, <=30) ; AREA AGG (11:21)**
ctrlC2maxEsm <- ctrlC1maxE %>%
  mutate(spp.run=paste0(ctrlC1maxE$assessment,'_C',(1:length(ctrlC1maxE$startyr)+21000)),
         stratum='C',
         run=(1:length(ctrlC1maxE$startyr)+21000),
         data='cpue_land_Csm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


ctrlC <- rbind(ctrlC2, ctrlC2sm, ctrlC2maxE, ctrlC2maxEsm)
  

###**D: BROWN SHRIMP ANNUAL ; SIZE BINS (>67, 67-31, <=30) ; AREA (11:17, 18:21)**
ctrlD1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop='SIZE_AREA',
                     time='YEAR',
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'), 
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"), 
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlD2 <- ctrlD1 %>%
  mutate(spp.run=paste0(ctrlD1$assessment,'_D',1:length(ctrlD1$startyr)),
         stratum='D',
         run=1:length(ctrlD1$startyr),
         data='cpue_land_D',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlD2sm <- ctrlD1 %>%
  mutate(spp.run=paste0(ctrlD1$assessment,'_D',(1:length(ctrlD1$startyr))+10000),
         stratum='D',
         run=(1:length(ctrlD1$startyr)+10000),
         data='cpue_land_Dsm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


ctrlD1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA, ##could add up to _E per run
                      z2=NA,
                      pop='SIZE_AREA',
                      time='YEAR',
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'), 
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"), 
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlD2maxE <- ctrlD1maxE %>%
  mutate(spp.run=paste0(ctrlD1maxE$assessment,'_D',1:length(ctrlD1maxE$startyr)+20000),
         stratum='D',
         run=1:length(ctrlD1maxE$startyr)+20000,
         data='cpue_land_D',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

##zlag vars are fed into the makelags() function to create covariates up to E lags
ctrlD2maxEsm <- ctrlD1maxE %>%
  mutate(spp.run=paste0(ctrlD1maxE$assessment,'_D',(1:length(ctrlD1maxE$startyr))+21000),
         stratum='D',
         run=(1:length(ctrlD1maxE$startyr)+21000),
         data='cpue_land_Dsm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))), 
         pop=as.character(pop),
         time=as.character(time),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

ctrlD <- rbind(ctrlD2,ctrlD2sm, ctrlD2maxE, ctrlD2maxEsm)


###**E: BROWN SHRIMP SEASONAL (SUMMER, FALL+WINTER) ; SIZE BINS AGG ; AREA AGG (11:21)**
ctrlE1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop=c('GULF','SEASON'),
                     time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'),
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"),
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 



ctrlE2 <- ctrlE1 %>%
  mutate(spp.run=paste0(ctrlE1$assessment,'_E',1:length(ctrlE1$startyr)),
         stratum='E',
         run=1:length(ctrlE1$startyr),
         data='cpue_land_E',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                     ifelse(grepl("salbotm_std",z1),"salbotm_std",
                            ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                   ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                          NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEASON','YEAR',
                     ifelse(pop=='GULF','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))
#table(ctrlE2$pop, ctrlE2$time, useNA='always')

ctrlE1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA, ##could add up to _E per run
                      z2=NA,
                      pop=c('GULF','SEASON'),
                      time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'),
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"),
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 



ctrlE2maxE <- ctrlE1maxE %>%
  mutate(spp.run=paste0(ctrlE1maxE$assessment,'_E',1:length(ctrlE1maxE$startyr)+20000),
         stratum='E',
         run=1:length(ctrlE1maxE$startyr)+20000,
         data='cpue_land_E',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEASON','YEAR',
                     ifelse(pop=='GULF','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

ctrlE <- rbind(ctrlE2, ctrlE2maxE)


##*F: BROWN SHRIMP SEASONAL (SUMMER, FALL+WINTER) ; SIZE BINS AGG ; AREA (11:17, 18:21)**
ctrlF1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop=c('AREA_ASSESS','SEAS_AREA'),
                     time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'),
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"),
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

ctrlF2 <- ctrlF1 %>%
  mutate(spp.run=paste0(ctrlF1$assessment,'_F',1:length(ctrlF1$startyr)),
         stratum='F',
         run=1:length(ctrlF1$startyr),
         data='cpue_land_F',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_AREA','YEAR',
                     ifelse(pop=='AREA_ASSESS','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))
table(ctrlF2$pop, ctrlF2$time, useNA='always')


ctrlF1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA, ##could add up to _E per run
                      z2=NA,
                      pop=c('AREA_ASSESS','SEAS_AREA'),
                      time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'),
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"),
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

ctrlF2maxE <- ctrlF1maxE %>%
  mutate(spp.run=paste0(ctrlF1maxE$assessment,'_F',1:length(ctrlF1maxE$startyr)+20000),
         stratum='F',
         run=1:length(ctrlF1maxE$startyr)+20000,
         data='cpue_land_F',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_AREA','YEAR',
                     ifelse(pop=='AREA_ASSESS','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

ctrlF <- rbind(ctrlF2, ctrlF2maxE)

##**G: BROWN SHRIMP SEASONAL (SUMMER, FALL+WINTER) ; SIZE BINS (>67, 67-31, <=30) ; AREA AGG (11:21)**
ctrlG1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop=c('SIZE','SEAS_SIZE'),
                     time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'),
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"),
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

ctrlG2 <- ctrlG1 %>%
  mutate(spp.run=paste0(ctrlG1$assessment,'_G',1:length(ctrlG1$startyr)),
         stratum='G',
         run=1:length(ctrlG1$startyr),
         data='cpue_land_G',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE','YEAR',
                     ifelse(pop=='SIZE','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))
table(ctrlG2$pop, ctrlG2$time, useNA='always')

ctrlG2sm <- ctrlG1 %>%
  mutate(spp.run=paste0(ctrlG1$assessment,'_G',(1:length(ctrlG1$startyr)+10000)),
         stratum='G',
         run=(1:length(ctrlG1$startyr)+10000),
         data='cpue_land_Gsm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE','YEAR',
                     ifelse(pop=='SIZE','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


ctrlG1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA, ##could add up to _E per run
                      z2=NA,
                      pop=c('SIZE','SEAS_SIZE'),
                      time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'),
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"),
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

ctrlG2maxE <- ctrlG1maxE %>%
  mutate(spp.run=paste0(ctrlG1maxE$assessment,'_G',1:length(ctrlG1maxE$startyr)+20000),
         stratum='G',
         run=1:length(ctrlG1maxE$startyr)+20000,
         data='cpue_land_G',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE','YEAR',
                     ifelse(pop=='SIZE','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))
#table(ctrlG2maxE$pop, ctrlG2maxE$time, useNA='always')

ctrlG2maxEsm <- ctrlG1maxE %>%
  mutate(spp.run=paste0(ctrlG1maxE$assessment,'_G',(1:length(ctrlG1maxE$startyr)+21000)),
         stratum='G',
         run=(1:length(ctrlG1maxE$startyr)+21000),
         data='cpue_land_Gsm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE','YEAR',
                     ifelse(pop=='SIZE','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))

ctrlG <- rbind(ctrlG2,ctrlG2sm, ctrlG2maxE ,ctrlG2maxEsm)


##*H: BROWN SHRIMP SEASONAL (SUMMER, FALL+WINTER) ; SIZE BINS (>67, 67-31, <=30) ; AREA (11:17, 18:21)**
ctrlH1 <- expand.grid(startyr = c(1987),
                     assessment='BSH',
                     species='Brown Shrimp',
                     index_source='SEAMAP',
                     y1='CPUE',
                     y2='tailmp',
                     z1=c(NA,'tempbotm_std','salbotm_std','Price_GOM','imp_100mp',
                          'Price_GOM_1','imp_100mp_1'), ##could add up to _E per run
                     z2=NA,
                     pop=c('SIZE_AREA','SEAS_SIZE_AREA'),
                     time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                     E=3:4, 
                     tau=1, 
                     forecast='FALSE',
                     nfore=NA,
                     vtimestep='FALSE',
                     append='TRUE',
                     scaling=c('global','local'),
                     rhofixed=NA,
                     rhomatrix=NA,
                     augdata=NA,
                     predictmethod = c("lto","sequential"),
                     newdata=NA,
                     xname=NA,
                     ytrans=c('log','gr1','gr2','none'),
                     bshared=c(TRUE,FALSE),
                     linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

ctrlH2 <- ctrlH1 %>%
  mutate(spp.run=paste0(ctrlH1$assessment,'_H',1:length(ctrlH1$startyr)),
         stratum='H',
         run=1:length(ctrlH1$startyr),
         data='cpue_land_H',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE_AREA','YEAR',
                     ifelse(pop=='SIZE_AREA','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))
#table(ctrlH2$pop, ctrlH2$time, useNA='always')

ctrlH2sm <- ctrlH1 %>%
  mutate(spp.run=paste0(ctrlH1$assessment,'_H',(1:length(ctrlH1$startyr)+10000)),
         stratum='H',
         run=(1:length(ctrlH1$startyr)+10000),
         data='cpue_land_Hsm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE_AREA','YEAR',
                     ifelse(pop=='SIZE_AREA','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))


ctrlH1maxE <- expand.grid(startyr = c(1987),
                      assessment='BSH',
                      species='Brown Shrimp',
                      index_source='SEAMAP',
                      y1='CPUE',
                      y2='tailmp',
                      z1=NA, ##could add up to _E per run
                      z2=NA,
                      pop=c('SIZE_AREA','SEAS_SIZE_AREA'),
                      time=NA,  ##only want YEAR with SEASON ; YEAR2 with GULF
                      E=5, 
                      tau=1, 
                      forecast='FALSE',
                      nfore=NA,
                      vtimestep='FALSE',
                      append='TRUE',
                      scaling=c('global','local'),
                      rhofixed=NA,
                      rhomatrix=NA,
                      augdata=NA,
                      predictmethod = c("lto","sequential"),
                      newdata=NA,
                      xname=NA,
                      ytrans=c('log','gr1','gr2','none'),
                      bshared=c(TRUE,FALSE),
                      linprior=c(FALSE,TRUE) ##rigging this to keep current runs (false), then use true to run and match what is in scaling (e.g. local or global)
) 

ctrlH2maxE <- ctrlH1maxE %>%
  mutate(spp.run=paste0(ctrlH1maxE$assessment,'_H',1:length(ctrlH1maxE$startyr)+20000),
         stratum='H',
         run=1:length(ctrlH1maxE$startyr)+20000,
         data='cpue_land_H',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE_AREA','YEAR',
                     ifelse(pop=='SIZE_AREA','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))
#table(ctrlH2maxE$pop, ctrlH2maxE$time, useNA='always')

ctrlH2maxEsm <- ctrlH1maxE %>%
  mutate(spp.run=paste0(ctrlH1maxE$assessment,'_H',(1:length(ctrlH1maxE$startyr)+21000)),
         stratum='H',
         run=(1:length(ctrlH1maxE$startyr)+21000),
         data='cpue_land_Hsm',
         assessment=as.character(assessment),
         species=as.character(species),
         index_source=as.character(index_source),
         y1=as.character(y1),
         y2=as.character(y2),
         z1=as.character(z1),
         z2=as.character(z2),
         zlag1=ifelse(grepl("tempbotm_std",z1),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z1),"salbotm_std",
                             ifelse(grepl("Price_GOM",z1),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z1),"imp_100mp",
                                           NA)))),   
         zlag2=ifelse(grepl("tempbotm_std",z2),"tempbotm_std",
                      ifelse(grepl("salbotm_std",z2),"salbotm_std",
                             ifelse(grepl("Price_GOM",z2),"Price_GOM",
                                    ifelse(grepl("imp_100mp",z2),"imp_100mp",
                                           NA)))),   
         pop=as.character(pop),
         time=ifelse(pop=='SEAS_SIZE_AREA','YEAR',
                     ifelse(pop=='SIZE_AREA','YEAR2',NA)),
         forecast=as.character(forecast),
         vtimestep=as.character(vtimestep),
         append=as.character(append),
         scaling=as.character(scaling),
         predictmethod=as.character(predictmethod),
         ytrans=as.character(ytrans)) %>%
  relocate(c("spp.run","stratum","run","data"))



ctrlH <- rbind(ctrlH2, ctrlH2sm, ctrlH2maxE, ctrlH2maxEsm)


##****Custom definitions?**



ctrl_all <- rbind(ctrlA, ctrlB, ctrlC, ctrlD, ctrlE, ctrlF, ctrlG, ctrlH )#, ctrlX, ctrlY)


##**Turn off certain model runs**

ctrl_all <- ctrl_all %>% 
                 mutate(
                   eval=ifelse(predictmethod=='lto','N',
                              ifelse(linprior==TRUE,'N',  ##testing linprior true --breaks for ind size fits 
                                     ifelse(pop=='GULF' & bshared==TRUE,'N', ##errored out
                                     #ifelse((pop!='GULF'&scaling=='global'),'N', ##add global for figs
                                     ifelse(stratum=='H','N',  ##stratum H can take 20min per run
                        ifelse(stratum=='B' & (run>800 & run <850),'N', 
                               ifelse(spp.run=='BSH_B20059'|spp.run=='BSH_B20060'|
                                        (stratum =='C' & (run >=491 & run <533))|  ##these runs and below broke
                                        (stratum =='C' & (run >=771 & run <785)),'N',
                                            'Y'))))))
                 )



rm(list=ls()[! ls() %in% c('ctrl_all','ctrlA', 'ctrlB', 'ctrlC', 
                           'ctrlD', 'ctrlE', 'ctrlF', 'ctrlG','ctrlH' )])
