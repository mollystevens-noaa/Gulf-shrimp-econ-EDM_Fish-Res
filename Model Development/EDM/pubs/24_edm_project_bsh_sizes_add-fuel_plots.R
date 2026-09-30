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

Uproj_scenario <- read.csv(file=paste0('./ModelDevelopment/EDM/pubs/figs/Uproj_scenario.csv'))

all.land <- read.csv(file=paste0('./ModelDevelopment/EDM/pubs/figs/BSH_price_fuel_mod_land.csv'))
all.samp2 <- read.csv(file=paste0('./ModelDevelopment/EDM/pubs/figs/BSH_price_fuel_mod_allsamp.csv'))

##last year of real data should be identical among model types (e.g. )
test.land.same <- all.land %>% filter(timestep==2022.5)
test.samp.same <- all.samp2 %>% filter(timestep==2022.5)

##years of projections should be different
test.land <- all.land %>% filter(timestep==2023.5)
test.samp <- all.samp2 %>% filter(timestep==2023.5)
#Uproj_scenario

all.land.sum1 <- all.land %>% mutate(
  year=substr(timestep,1,4) )
all.land.sum2 <- all.land.sum1 %>%
    group_by(year, scenario, fscenario)%>%
    summarize(totrev = sum(revenue)) %>%
  filter(year>2022, fscenario=='status.quo')
all.land.sum3 <- all.land.sum2 %>%
  group_by(scenario) %>%
  summarize(rev_avg = mean(totrev))



BSH_price_fuel_proj_scenario_size_pub <- ggplot(all.samp2 %>% filter(scenario!='edm.nopricefuel')) +
  geom_line(aes(y=predmean, x=timestep, linetype=scenario, color=fscenario)) +
  facet_wrap(~pop,ncol=1, scales = "free_y") +
  labs(title ="Brown Shrimp Population Projections (U by Price)", 
       x = "Year", 
       y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))
BSH_price_fuel_proj_scenario_size_pub
export_fig(BSH_price_fuel_proj_scenario_size_func_scale, size='big',  folder='EDM_pubs')



BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3a <- all.land %>% 
  # 1. Filter the data
  filter(scenario != 'edm.nopricefuel' & scenario != 'msy' & timestep > 2021) %>%
  # 2. Reorder and rename the variables using factors
  mutate(
    fscenario = factor(fscenario,
                       levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
                       labels = c("Fuel $1.50", "Fuel $2.42", "Fuel $4.00")),   # The new names you want displayed on the plot

    scenario = factor(scenario,
                      levels = c("farmed","status.quo", "tariffs" ,"label.premium"),
                      labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling"))
  ) %>%
  # 3. Pass to ggplot
  ggplot() +
  geom_line(aes(y=revenue, x=timestep, linetype=pop, color=pop)) +
  facet_grid(fscenario ~ scenario) +
  labs(title = "Brown Shrimp Revenue Projections",
       x = "Year",
       y = paste0("Revenue ($2022)"),
       color = "Size Class",       # Updates the legend title for fscenario
       linetype = "Size Class") +# Updates the legend title for scenario
  # ADDS COLORBLIND-FRIENDLY & GRAYSCALE-SAFE COLORS:
  #scale_color_grey(start = 0, end = 0.75)+
  scale_color_manual(values=size_color)+
  geom_vline(xintercept=2022.5, linetype='dashed')+
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        panel.spacing = unit(1, "lines"))
BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3a
export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3a, size='bigw',  folder='EDM_pubs')




BSH_price_fuel_proj_scenario_size_func_LAND_grid3 <- all.land %>% 
  # 1. Filter the data
  filter(scenario != 'edm.nopricefuel' & scenario != 'msy' & timestep > 2021) %>%
  # 2. Reorder and rename the variables using factors
  mutate(
    fscenario = factor(fscenario,
                       levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
                       labels = c("Fuel $1.50", "Baseline Fuel ($2.42)", "Fuel $4.00")),   # The new names you want displayed on the plot

    scenario = factor(scenario,
                      levels = c("farmed","status.quo", "tariffs" ,"label.premium"),
                      labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling"))
  ) %>%
  # 3. Pass to ggplot
  ggplot() +
  geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=scenario)) +
  facet_grid( pop ~fscenario) +
  labs(title = "Brown Shrimp Landings Projections",
       x = "Year",
       y = paste0("Landings (million lbs tails)"),
       color = "Scenario",       # Updates the legend title for fscenario
       linetype = "Scenario") +# Updates the legend title for scenario
  # ADDS COLORBLIND-FRIENDLY & GRAYSCALE-SAFE COLORS:
  #scale_color_grey(start = 0, end = 0.75)+
 # scale_color_manual(values=size_color)+
  scale_color_viridis_d(option = "H") + # Try options "A" through "H" for different vibes
  geom_vline(xintercept=2022.5, linetype='dashed')+
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        panel.spacing = unit(1, "lines"))
BSH_price_fuel_proj_scenario_size_func_LAND_grid3
export_fig(BSH_price_fuel_proj_scenario_size_func_LAND_grid3, size='bigw',  folder='EDM_pubs')



BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3 <- all.land %>% 
  # 1. Filter the data
  filter(scenario != 'edm.nopricefuel' & scenario != 'msy' & timestep > 2021) %>%
  # 2. Reorder and rename the variables using factors
  mutate(
    fscenario = factor(fscenario,
                       levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
                       labels = c("Fuel $1.50", "Baseline Fuel ($2.42)", "Fuel $4.00")),   # The new names you want displayed on the plot
    
    scenario = factor(scenario,
                      levels = c("farmed","status.quo", "tariffs" ,"label.premium"),
                      labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling"))
  ) %>%
  # 3. Pass to ggplot
  ggplot() +
  geom_line(aes(y=revenue, x=timestep, linetype=scenario, color=scenario)) +
  facet_grid( pop ~fscenario) +
  labs(title = "Brown Shrimp Revenue Projections",
       x = "Year",
       y = paste0("Revenue ($2022)"),
       color = "Scenario",       # Updates the legend title for fscenario
       linetype = "Scenario") +# Updates the legend title for scenario
  # ADDS COLORBLIND-FRIENDLY & GRAYSCALE-SAFE COLORS:
  #scale_color_grey(start = 0, end = 0.75)+
  # scale_color_manual(values=size_color)+
  scale_color_viridis_d(option = "H") + # Try options "A" through "H" for different vibes
  geom_vline(xintercept=2022.5, linetype='dashed')+
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        panel.spacing = unit(1, "lines"))
BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3
export_fig(BSH_price_fuel_proj_scenario_size_func_REVENUE_grid3, size='bigw',  folder='EDM_pubs')





# 
# BSH_price_fuel_proj_scenario_size_func <- ggplot(all.samp2 %>% filter(scenario!='edm.nopricefuel')) +
#       geom_line(aes(y=predmean, x=timestep, linetype=scenario, color=fscenario)) +
#       facet_wrap(~pop,ncol=1) +
#       labs(title ="Brown Shrimp Population Projections", 
#            x = "Year", 
#            y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
#       theme_minimal(base_size = 20) +                                                 # Large font
#       theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
#             axis.title = element_text(size = 22),                                     # Larger axis titles
#             axis.text = element_text(size = 18))
#     BSH_price_fuel_proj_scenario_size_func
#     export_fig(BSH_price_fuel_proj_scenario_size_func, size='big',  folder='EDM_pubs')
#     
#     
#     BSH_price_fuel_proj_scenario_size_func_scale <- ggplot(all.samp2 %>% filter(scenario!='edm.noprice')) +
#       geom_line(aes(y=predmean, x=timestep, linetype=scenario, color=fscenario)) +
#       facet_wrap(~pop,ncol=1, scales = "free_y") +
#       labs(title ="Brown Shrimp Population Projections (U by Price)", 
#            x = "Year", 
#            y =paste0("CPUE (shrimp per hour)")) +                                                      # Labels
#       theme_minimal(base_size = 20) +                                                 # Large font
#       theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
#             axis.title = element_text(size = 22),                                     # Larger axis titles
#             axis.text = element_text(size = 18))
#     BSH_price_fuel_proj_scenario_size_func_scale
#     export_fig(BSH_price_fuel_proj_scenario_size_func_scale, size='big',  folder='EDM_pubs')
#     
    #export_fig(msy_pop, filename=paste0(spp.run,"_msy_pop.png"), folder='EDM_final')
    # export_fig(msy_pop,
    #            folder=paste0("EDMout",spp,"_loop"), exportname=name)
    
    ###**CREATE COMPLEMENTARY LANDINGS FIGURES--SHOW THAT THE POPULATION IS FINE WITH UP TO X REMOVALS**
    ###**ALL GAIN NO PAIN!*
    
    
    
    BSH_price_fuel_proj_scenario_size_func_LAND_msy <- ggplot(all.land %>% filter(scenario!='edm.nopricefuel', timestep>2021)%>%
                                                                # 2. Reorder and rename the variables using factors
                                                                mutate(
                                                                  fscenario = factor(fscenario,
                                                                                     levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
                                                                                     labels = c("Fuel $1.50", "Baseline Fuel ($2.42)", "Fuel $4.00")),   # The new names you want displayed on the plot
                                                                  
                                                                  scenario = factor(scenario,
                                                                                    levels = c("farmed","status.quo", "tariffs" ,"label.premium", "msy"),
                                                                                    labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling", "MSY"))
                                                                )) +
      geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=scenario)) +
      facet_grid(pop ~ fscenario) +
      labs(title ="Brown Shrimp Landings Projections", 
           x = "Year", 
           y =paste0("Landings (million lbs tails)"),
           color = "Scenario",       # Updates the legend title for fscenario
           linetype = "Scenario") +                                                      # Labels
      theme_minimal(base_size = 20) +                    # Large font
      scale_color_viridis_d(option = "H") + # Try options "A" through "H" for different vibes
      geom_vline(xintercept=2022.5, linetype='dashed')+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 22),
            axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 18),
            panel.spacing = unit(1, "lines"))
    BSH_price_fuel_proj_scenario_size_func_LAND_msy
    export_fig(BSH_price_fuel_proj_scenario_size_func_LAND_msy, size='bigw',  folder='EDM_pubs')
    
    
    ###aggregate landings
    
    all.land.agg <- all.land %>%group_by(scenario,fscenario,timestep) %>%
                        summarize(rev_total=sum(revenue))
    
    BSH_price_proj_scenario_REVENUE_total <- ggplot(all.land.agg %>% filter(scenario!='edm.nopricefuel'& scenario!='msy' & timestep>2021) %>%mutate(
      fscenario = factor(fscenario,
                         levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
                         labels = c("Fuel $1.50", "Baseline Fuel ($2.42)", "Fuel $4.00")),   # The new names you want displayed on the plot
      
      scenario = factor(scenario,
                        levels = c("farmed","status.quo", "tariffs" ,"label.premium"),
                        labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling"))
    )) +
      geom_line(aes(y=rev_total, x=timestep, linetype=fscenario, color=scenario)) +
      labs(title ="Brown Shrimp Revenue Projections",
           x = "Year",
           y =paste0("Revenue ($2022)")) + # Labels
      scale_color_viridis_d(option = "H") + # Try options "A" through "H" for different vibes
      geom_vline(xintercept=2022.5, linetype='dashed')+
      theme_minimal(base_size = 20) +                                                 # Large font
      theme(plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 22),
            axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 18),
            panel.spacing = unit(1, "lines"))
    BSH_price_proj_scenario_REVENUE_total
    export_fig(BSH_price_proj_scenario_REVENUE_total, size='bigw',  folder='EDM_pubs')

    
    
    
    
    
    BSH_price_fuel_proj_scenario_size_func_POP <- all.samp2 %>% 
      # 1. Filter the data
      filter(scenario != 'edm.nopricefuel'  & timestep > 2021) %>%
      
      # 2. Reorder and rename the variables using factors
      mutate(
        fscenario = factor(fscenario, 
                           levels = c("min", "status.quo", "max"),                   # The exact original data values in the order you want
                           labels = c("Fuel $1.50", "Baseline Fuel ($2.42)", "Fuel $4.00")),   # The new names you want displayed on the plot
        
        scenario = factor(scenario, 
                          levels = c("farmed","status.quo", "tariffs" ,"label.premium","msy"), 
                          labels = c("Farmed Competition","Baseline Ex-Vessel Price", "Import Tariffs",  "Premium Labeling","MSY"))
      ) %>%
      
      # 3. Pass to ggplot
      ggplot() +
      geom_line(aes(y=predmean, x=timestep, linetype=scenario, color=scenario)) +
      facet_grid(pop~fscenario, scales="free") +
      labs(title = "Brown Shrimp Population Projections",
           x = "Year",
           y = paste0("CPUE"),
           color = "Scenario",       # Updates the legend title for fscenario
           linetype = "Scenario") +# Updates the legend title for scenario
      # ADDS COLORBLIND-FRIENDLY & GRAYSCALE-SAFE COLORS:
      #scale_color_grey(start = 0, end = 0.75)+
      #scale_color_manual(values=size_color)+
      scale_color_viridis_d(option = "H") + # Try options "A" through "H" for different vibes
      geom_vline(xintercept=2022.5, linetype='dashed')+
      theme_minimal(base_size = 20) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 22),
            axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 18),
            panel.spacing = unit(1, "lines"))
    BSH_price_fuel_proj_scenario_size_func_POP
    export_fig(BSH_price_fuel_proj_scenario_size_func_POP, size='bigw',  folder='EDM_pubs')
    
    
    
    
    
    ##Facet grid and be intentional / make these better

   # BSH_price_fuel_edm_land_comparison <-ggplot(all.land %>% filter(scenario=='edm.noprice'| scenario=='status.quo')) +
   #    geom_line(aes(y=tailmp, x=timestep, linetype=scenario, color=fscenario)) +
   #    facet_wrap(~pop,ncol=1) +
   #    labs(title ="Brown Shrimp Landings Projections",
   #         x = "Year",
   #         y =paste0("Landings (million lbs tails)")) +                                                      # Labels
   #    theme_minimal(base_size = 20) +                                                 # Large font
   #    theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
   #          axis.title = element_text(size = 22),                                     # Larger axis titles
   #          axis.text = element_text(size = 18))
   # BSH_price_fuel_edm_land_comparison
   # export_fig(BSH_price_edm_land_comparison, size='big',  folder='EDM_pubs')

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
   
   ############################################################################
   ############################################################################
   ############################################################################
   ############################################################################
   ############################################################################
   ############################################################################
  
   
   
   
   
   
   
   
   
   
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
    