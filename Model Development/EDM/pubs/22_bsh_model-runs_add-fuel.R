####################################################################################################
source("./ModelDevelopment/EDM/03_bsh_ctrl_expansion.R") ##this clears files so needs to be ran early
source("./ModelDevelopment/EDM/00_load-libraries.R")
source("./Functions/fig_format_export.R")
####################################################################################################
load('./ModelDevelopment/EDM/Data/bsh_agg.RData') 
## Create directories
#if(!dir.exists(paste0('./ModelDevelopment/EDM/output_BSH/'))){ dir.create(paste0('./ModelDevelopment/EDM/output_BSH/'))} 
#if(!dir.exists(paste0('./ModelDevelopment/EDM/output_BSH/allruns/'))){ dir.create(paste0('./ModelDevelopment/EDM/output_BSH/allruns/'))} 

fuel_price <- read.csv(file='./ModelDevelopment/EDM/Data/Fuelprice_UPLOAD_8722.csv')
fuel_price2 <- fuel_price %>%
  filter(SEASON !='JFMA') %>%
  rename(QUAD=SEASON) %>%
  mutate(
    YEAR2 = ifelse(QUAD=='SOND', YEAR + 0.5, YEAR),
    SEASON = ifelse(QUAD=='SOND','Fall',
                    ifelse(QUAD=='MJJA','Summer',
                           ifelse(QUAD=='JFMA','Winter','Missing')))
  ) 

####**DEFINE THE CATCHABILITY FROM THE PREVIOUS MODEL TO ESTIMATE HARVEST RATE**
####**REFIT THE MODEL, COMPARE WITH THE BEST MODEL IN A PLOT**
####**NEXT SCRIPT WILL READ IN THE DATA AND ADD PROJECTIONS + ERROR BARS**
q=0.401774546

cpue_land_Gfuel <- left_join(cpue_land_G, fuel_price2, by=c('YEAR','SEASON','YEAR2')) %>%
                     rename(fuel_price=P_Fuel_Real)
###working off G_20023 (best run from SEDAR87 work)
cpue_land_G <- cpue_land_Gfuel %>% select(-c('GULF','logcpue','tail10mp','LBS_Imports','imp_10mp','imp_100mp',
                                         'SEAS_SIZE','tempbotm','tempbotm_std','salbotm', 'salbotm_std','QUAD'))%>% 
                   mutate(hrate= q*tailmp/CPUE)
cpue_land_G_nooutlier <- cpue_land_G   %>% filter(hrate<0.95)
pricemod_G  <- lm( hrate ~  log(Price_GOM) * SIZE + log(fuel_price), data=cpue_land_G_nooutlier)
summary(pricemod_G)

coef <- pricemod_G$coefficients


cpue_land_G$hrate_est_price_fuel = ifelse(cpue_land_G$SIZE=='Large', coef[1]+ coef[2]*log(cpue_land_G$Price_GOM) + coef[5]*log(cpue_land_G$fuel_price) ,
                                    ifelse(cpue_land_G$SIZE=='Medium', (coef[1]+coef[3]) + (coef[2]+coef[6])*log(cpue_land_G$Price_GOM) + coef[5]*log(cpue_land_G$fuel_price),
                                      ifelse(cpue_land_G$SIZE=='Small', (coef[1]+coef[4]) + (coef[2]+coef[7])*log(cpue_land_G$Price_GOM) + coef[5]*log(cpue_land_G$fuel_price),
                                             -1)))

cpue_land_G <- cpue_land_G %>%
                  mutate( removals_est_price_fuel = hrate_est_price_fuel * CPUE ,
                          tailmp_est_price_fuel = removals_est_price_fuel/q,
                          #talmp_est = hrate*CPUE/q,
                          error_tailmp_est_price_fuel = tailmp-tailmp_est_price_fuel)

cpue_land_Gprice <- cpue_land_G


  
#ctrl <- ctrlA
#ctrl <- ctrlE
# ctrl_list <- list()
# for (j in strata$stratum){#c('A','C','E')){ #strata$stratum){ ##**Update this once full strata defined**
#   this.ctrl <- left_join(get(paste0('ctrl',j)),strata[strata$stratum==j,])
#   ctrl_list[[j]] <- this.ctrl
# }
# write_xlsx(ctrl_list,paste0('./ModelDevelopment/EDM/output_BSH/BSH_ctrl.xlsx'))
#species='Brown Shrimp'

i=20023 #10239 #10027 #orig placeholder A181_BSH1987_CPUEtailmp_GULFYEAR_E4_global_ytransgr1_lto_nocov
j='G'

ctrl_all <- ctrl_all %>% filter(eval=='Y') #%>% filter(stratum=='C' & run >476)
#filter(stratum==c('A','B'))
#%>% filter(run >= 10000, stratum !=c('G','H'))
#table(ctrl_all$stratum)

ctrl_all <- ctrl_all %>% mutate(uniquerun=seq(1:length(ctrl_all$spp.run)))

#edm_wrapper_function <- function(w){
  #for (j in c('A')){ #strata$stratum){ ##c('E','F','G','H')){
  #load('./ModelDevelopment/EDM/Data/bsh01.RData') 
  
  rm(list=ls()[! ls() %in% c('w','i','j','ctrl_all', "strata", 
                             "cpue_land_A","cpue_land_B","cpue_land_C",
                             "cpue_land_D","cpue_land_E", "cpue_land_F","cpue_land_G","cpue_land_H","cpue_land_Gprice",
                             "cpue_land_Csm","cpue_land_Dsm","cpue_land_Gsm","cpue_land_Hsm","export_fig")])
  
  #w=75
  #ctrl <- ctrl_all %>% filter(uniquerun==w)  
  ctrl <- ctrl_all %>%filter(stratum==j)
  this.run <- ctrl[ctrl$run==i,]
  
  j <- ctrl$stratum[1] ##pull stratum and make sure that code runs properly
  spp <- this.run$assessment
  species <- this.run$species
  spp.run <- paste0(this.run$assessment,"_",j,i)
  
  #for(i in ctrl$run){
   
    
    ##**read in best model for comparisons**
    best.mod.name <- paste0(this.run$stratum,this.run$run,"_",this.run$assessment,this.run$startyr,"_",this.run$y1,this.run$y2,
                            "_bshare",substring(this.run$bshared,1,1),"_",
                            this.run$pop,this.run$time,"_E",this.run$E,"_",this.run$scaling,"_ytrans",
                            this.run$ytrans,"_",
                            this.run$predictmethod,"_",
                            ifelse(!is.na(this.run$z2), paste0(this.run$z1,this.run$z2),
                                   ifelse(!is.na(this.run$z1), this.run$z1,"nocov")),
                            ifelse(this.run$linprior==T,paste0("_ricker"),""))
    best.mod.fit <- readRDS(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',best.mod.name,'/EDMfit_',spp.run))
    summary(best.mod.fit)
    
    
    ##**read in price (sans fuel) model for comparisons**
    price.mod.name <- paste0(this.run$stratum,this.run$run,"_",this.run$assessment,this.run$startyr,"_",this.run$y1,'removals_est_price',
                            "_bshare",substring(this.run$bshared,1,1),"_",
                            this.run$pop,this.run$time,"_E",this.run$E,"_",this.run$scaling,"_ytrans",
                            this.run$ytrans,"_",
                            this.run$predictmethod,"_",
                            ifelse(!is.na(this.run$z2), paste0(this.run$z1,this.run$z2),
                                   ifelse(!is.na(this.run$z1), this.run$z1,"nocov")),
                            ifelse(this.run$linprior==T,paste0("_ricker"),""))
    price.mod.fit <- readRDS(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',price.mod.name,'/EDMfit_',spp.run))
    summary(price.mod.fit)
    
    
    
    ##replace landings with estimated removals using price in units of index
    this.run$y2 = 'removals_est_price_fuel'
    

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
    
    if(!dir.exists(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name))){ dir.create(paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name))} 
    
    
    ##**Make lags, run fitGP_fish**
    this.run.lags=makelags(data=eval(data.run), #y=c(this.run$y1,this.run$y2,this.run$z1),
                           y=if (!is.na(this.run$z2)) {
                             # If z2 is not NA, use y1, y2, z1, and z2
                             c(this.run$y1, this.run$y2, this.run$zlag1, this.run$zlag2)
                           } else if (!is.na(this.run$zlag1)) {
                             # If z2 is NA but z1 is not NA, use y1, y2, and z1
                             c(this.run$y1, this.run$y2, this.run$zlag1)
                           } else {
                             # If both z2 and z1 are NA, use y1 and y2
                             c(this.run$y1, this.run$y2)
                           },
                           pop=ifelse(is.na(this.run$pop),1,this.run$pop),
                           vtimestep= this.run$vtimestep, 
                           time=this.run$time, 
                           tau=this.run$tau, E=this.run$E, 
                           append=this.run$append)
    
    
    ##test apply for scaling differences
    # y=this.run$y1
    # m=c(paste0(this.run$y1,"_",1:this.run$E))
    # h=c(paste0(this.run$y2,"_",1:this.run$E))
    # if(!is.na(this.run$z2)) z = c(this.run$z1,this.run$z2) else 
    #   if(!is.na(this.run$z1)) z = this.run$z1 else z = NULL
    # xd <- this.run.lags[,c(m,z)]
    # xds=apply(xd,2,scale)
    
    
    
    this.run.fit.price.fuel=fitGP_fish(data=this.run.lags, y=this.run$y1, 
                            m=c(paste0(this.run$y1,"_",1:this.run$E)), 
                            h=c(paste0(this.run$y2,"_",1:this.run$E)), 
                            #z= NULL,
                            ##test adding variable time steps as z
                            #z=c(paste0("Tdiff_",1:this.run$E)), ##ADD IF VTIMESTEP=T CAVEAT
                            if(!is.na(this.run$z2)) z = c(this.run$z1,this.run$z2) else 
                              if(!is.na(this.run$z1)) z = this.run$z1 else z = NULL,
                            time=this.run$time,
                            pop=this.run$pop,
                            scaling=this.run$scaling,
                            predictmethod=this.run$predictmethod,
                            ytrans=this.run$ytrans,
                            bfixed=1,
                            linprior=ifelse(this.run$linprior==FALSE,"none",
                                            ifelse(this.run$linprior==TRUE,this.run$scaling,
                                                   'error'))
    )
    summary(this.run.fit.price.fuel)
  ##most fitstats without f(z) are slightly better
    
    this.run.fit.price.fuel$outsampfitstats
  ##BUT R2scaled_out is better (0.364 w/out, 0.420 w/) due to the better fit for Large shrimp (0.345 w/out, 0.669 w/)
    
    
    summary(best.mod.fit)
    best.mod.fit$outsampfitstats
    
    summary(price.mod.fit)
    price.mod.fit$outsampfitstats
    

    this.run.fit.price.fuel=fitGP_fish(data=this.run.lags, y=this.run$y1, 
                                  m=c(paste0(this.run$y1,"_",1:this.run$E)), 
                                  h=c(paste0(this.run$y2,"_",1:this.run$E)), 
                                  z= NULL,#'fuel_price',  ##**ADD FUEL VAR HERE**
                                  ##test adding variable time steps as z
                                  #z=c(paste0("Tdiff_",1:this.run$E)), ##ADD IF VTIMESTEP=T CAVEAT
                                  if(!is.na(this.run$z2)) z = c(this.run$z1,this.run$z2) else 
                                    if(!is.na(this.run$z1)) z = this.run$z1 else z = NULL,
                                  time=this.run$time,
                                  pop=this.run$pop,
                                  scaling=this.run$scaling,
                                  predictmethod=this.run$predictmethod,
                                  ytrans=this.run$ytrans,
                                  bfixed=1,
                                  linprior=ifelse(this.run$linprior==FALSE,"none",
                                                  ifelse(this.run$linprior==TRUE,this.run$scaling,
                                                         'error'))
    )
    summary(this.run.fit.price.fuel)
    ##most fitstats without f(z) are slightly better
    
    this.run.fit.price.fuel$outsampfitstats
    ##BUT R2scaled_out is better (0.364 w/out, 0.420 w/) due to the better fit for Large shrimp (0.345 w/out, 0.669 w/)
    
    
    
        
saveRDS(this.run.fit.price.fuel, file = paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name,'/EDMfit_',spp.run))
saveRDS(cpue_land_Gprice, file = paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',name,'/cpue_land_Gprice'))

    
####**PLOT best.mod.fit and best.mod.fit.price together with observed data**
####**+export best.mod.fit.price stats similar to loop used in sedar87*
    
best.mod.plot <- best.mod.fit$insampresults %>% mutate(data='best.mod')

price.mod.plot <- price.mod.fit$insampresults %>% mutate(data='price.mod')
    
this.run.price.fuel.plot <- this.run.fit.price.fuel$insampresults %>% mutate(data='price.fuel.mod')


mod.plot <- rbind(best.mod.plot, this.run.price.fuel.plot)


pub.price.fuel.mod.compare <- ggplot(mod.plot %>% filter(timestep<=2024.5)) +
  geom_line(aes(y=predmean, x=timestep, linetype=data)) +
  geom_point(aes(y=obs, x=timestep)) +
  #geom_point(aes(y=update, x=timestep), shape=1) +
  #geom_vline(xintercept=2022.5, linetype='dashed')+
  facet_wrap(~pop,ncol=1, scales='free') +
  labs(title ="Brown Shrimp Population Projections", 
       x = "Year", 
       y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 16))
  
pub.price.fuel.mod.compare
export_fig(pub.price.fuel.mod.compare, size='big', folder='EDM_final')


####***EXPORT PRICE MODEL STATS & PARAMETERS**

##**parameter estimates and gradients**
pars <- as.data.frame(cbind(
  est=this.run.fit.price.fuel$pars,
  estt=this.run.fit.price.fuel$parst,
  grad=this.run.fit.price.fuel$grad)) %>%
  rownames_to_column("par") %>%
  mutate(spp.run='BSH_G20023_price_fuel',
         fullrun=name,
         ind=0) %>%
  relocate(c("spp.run","fullrun"))
b <- as.data.frame(this.run.fit.price.fuel$b,rownames=NULL) %>%
  mutate(
    spp.run='BSH_G20023_price_fuel',
    fullrun=name,
    par=paste0('b',names(this.run.fit.price.fuel$b)),
    est=this.run.fit.price.fuel$b,
    estt=NA,
    grad=NA,
    ind=0
  ) %>%
  select(-c(1)) ##drop first weird naming for b
#b <- c(spp.run, name, 'b',this.run.fit$b,NA,NA,0)
pars <- rbind(pars,b)

write.csv(pars, file=paste0('./ModelDevelopment/EDM/output_BSH/BSH_pars_price_fuel.csv'),row.names=F)


##**fit statistics**
##rename out of sample statistics
outsamp <- as.data.frame(cbind(est=this.run.fit.price.fuel$outsampfitstats))%>%
  rownames_to_column("stats") %>%
  mutate(stats = ifelse(stats=='R2','R2_out',
                        ifelse(stats=='R2centered','R2centered_out',
                               ifelse(stats=='R2scaled','R2scaled_out',
                                      ifelse(stats=='rmse', 'rmse_out','NA')))))
##insample statistics
insamp <- as.data.frame(cbind(est=this.run.fit.price.fuel$insampfitstats)) %>%
  rownames_to_column("stats")
##merge for export
fitstats <- rbind(insamp,outsamp) %>%
  mutate(spp.run='BSH_G20023_price_fuel',
         fullrun=name,
         ind=0) %>%
  relocate(c("spp.run","fullrun"))


  inpopfit <- as.data.frame(this.run.fit.price.fuel$insampfitstatspop)
  outpopfit<- as.data.frame(this.run.fit.price.fuel$outsampfitstatspop) %>%
    rename(R2pop_out = R2pop,
           rmsepop_out= rmsepop)
  popfit1 <- cbind(inpopfit, outpopfit) %>%
    rownames_to_column("pop")
  popfit2 <- popfit1 %>%
    pivot_longer(c("R2pop","rmsepop","R2pop_out","rmsepop_out"), values_to="est", names_to="stats1") %>%
    mutate(stats=paste0(stats1,"_",pop))
  fitstatspop <- as.data.frame(popfit2)%>%
    mutate(spp.run=spp.run,
           fullrun=name,
           ind=0) %>%
    relocate(c("spp.run","fullrun","stats")) %>%
    subset(select= -c(pop,stats1))
  fitstats <- rbind(fitstats, fitstatspop)

  
write.csv(fitstats, file=paste0('./ModelDevelopment/EDM/output_BSH/BSH_fitstats_price_fuel.csv'),row.names=F)


