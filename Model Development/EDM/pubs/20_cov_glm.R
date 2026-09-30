####################################################################################################
source("./ModelDevelopment/EDM/00_load-libraries.R")
source("./ModelDevelopment/EDM/03_bsh_ctrl_expansion.R")
source("./Functions/fig_format_export.R")
####################################################################################################
## Load data and functions
load('./ModelDevelopment/EDM/Data/bsh01.RData')
spp <- 'BSH'
species <- 'Brown Shrimp'
land.unit <- 'tailmp'
ctrl_all <- ctrl_all %>% filter(eval=='Y')
term_year = 2022
##########################################

##**for projecting constant harvest rates (0:1) for both MSY and projection scenarios*

###**CALCULATE HRATE HERE AND ADD GLM FOR PRICE ~ HRATE x SIZE**
###**started below*

##**multiply by q--read in outputs and call here--NEED TO FIX**
cpue_land_A <- cpue_land_A %>% mutate(hrate = 0.401774546*tailmp/CPUE)
cpue_land_C <- cpue_land_C %>% mutate(hrate = 0.401774546*tailmp/CPUE)
cpue_land_G <- cpue_land_G %>% mutate(hrate = 0.401774546*tailmp/CPUE)


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

##annual
pricemod_A <- lm(hrate ~ Price_GOM , data=cpue_land_A)
summary(pricemod_A)


##A
ggplot(cpue_land_A, aes(x = Price_GOM , y = hrate)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", se = TRUE) +  # Linear model fit
  labs(title = "Harvest Rate predicted with Ex-Vessel Price",
       x = "Ex-Vessel Price",
       y = "Harvest Rate") +
  theme_minimal()

##size
pricemod_C <- lm( hrate ~  Price_GOM * SIZE, data=cpue_land_C)
summary(pricemod_C)


##C
ggplot(cpue_land_C, aes(x = Price_GOM , y = hrate , color = SIZE)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE) +  # Linear model fit
  labs(title = "Harvest Rate predicted with Ex-Vessel Price and Size",
       x = "Ex-Vessel Price",
       y = "Harvest Rate",
       color = "Size") +
  theme_minimal()


##size / season
pricemod_G <- lm( hrate ~  Price_GOM * SIZE, data=cpue_land_G)
summary(pricemod_G)


##G
ggplot(cpue_land_G, aes(x = Price_GOM , y = hrate , color = SIZE)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE) +  # Linear model fit
  labs(title = "Harvest Rate predicted with Ex-Vessel Price and Size",
       x = "Ex-Vessel Price",
       y = "Harvest Rate",
       color = "Size") +
  theme_minimal()



###remove 1988 large w hrate~1
cpue_land_Gb <- cpue_land_G %>% filter(hrate<0.95)
pricemod_Gb <- lm( hrate ~  Price_GOM * SIZE, data=cpue_land_Gb)
summary(pricemod_Gb)



##G
ggplot(cpue_land_Gb, aes(x = Price_GOM , y = hrate , color = SIZE)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE) +  # Linear model fit
  labs(title = "Harvest Rate predicted with Ex-Vessel Price and Size",
       x = "Ex-Vessel Price",
       y = "Harvest Rate",
       color = "Size") +
  theme_minimal()


##plots for pub 
##look at estimated harvest rates in 2014 (nearly hit red snapper exclusion zone)
BSH_harv_RS <- ggplot(cpue_land_A %>% filter(YEAR>=2008), aes(x = YEAR , y = hrate )) +
  geom_point() +  # Scatter plot of price vs. landings
  #geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE, level=0.95) +  # Linear model fit
 # scale_color_manual(values=size_color)+
  labs(title = "Harvest Rate with Red Snapper Threshold",
       x = "Year",
       y = "Harvest Rate") +  
  geom_vline(xintercept=2014)+
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))

BSH_harv_RS
export_fig(BSH_harv_RS, size='big',  folder='EDM_pubs')

BSH_harv_RS_size <- ggplot(cpue_land_C %>% filter(YEAR>=2008), aes(x = YEAR , y = hrate, color= SIZE )) +
  geom_point() +  # Scatter plot of price vs. landings
  #geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE, level=0.95) +  # Linear model fit
  scale_color_manual(values=size_color)+
  labs(title = "Harvest Rate with Red Snapper Threshold",
       x = "Year",
       y = "Harvest Rate") +  
  geom_vline(xintercept=2014)+
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))

BSH_harv_RS_size
export_fig(BSH_harv_RS_size, size='big',  folder='EDM_pubs')

BSH_harv_RS_size_summer <- ggplot(cpue_land_G %>% filter(YEAR>=2008, SEASON=='Summer'), aes(x = YEAR , y = hrate, color= SIZE )) +
  geom_point() +  # Scatter plot of price vs. landings
  #geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE, level=0.95) +  # Linear model fit
  scale_color_manual(values=size_color)+
  labs(title = "Summer Harvest Rate with Red Snapper Threshold",
       x = "Year",
       y = "Harvest Rate") +  
  geom_vline(xintercept=2014)+
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))
BSH_harv_RS_size_summer
export_fig(BSH_harv_RS_size_summer, size='big',  folder='EDM_pubs')


BSH_harv_price <- ggplot(cpue_land_Gb, aes(x = Price_GOM , y = hrate , color = SIZE)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE, level=0.95) +  # Linear model fit
  scale_color_manual(values=size_color)+
  labs(title = "Harvest Rate predicted with Ex-Vessel Price and Size",
       x = "Ex-Vessel Price",
       y = "Harvest Rate",
       color = "Size") +  
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))

BSH_harv_price
export_fig(BSH_harv_price, size='big',  folder='EDM_pubs')


###**Where do these pass the y intercept for hrate=0? at what point do they tie up and not trawl?**

# ##size / season
# pricemod_G2 <- lm( hrate ~  Price_GOM * SIZE * SEASON, data=cpue_land_G)
# summary(pricemod_G2)


##**Add Fuel Price**
cpue_land_Gfuel <- left_join(cpue_land_Gb, fuel_price2, by=c('YEAR','SEASON','YEAR2'))
#write.csv(cpue_land_Gfuel, file=paste0('./ModelDevelopment/EDM/pubs/figs/cpue_land_Gfuel.csv'),row.names=F)


pricemod_Gfuel <- lm( hrate ~  Price_GOM * SIZE + P_Fuel_Real, data=cpue_land_Gfuel)
summary(pricemod_Gfuel)

BSH_harv_pfuel_price <- 
ggplot(cpue_land_Gfuel, aes(x = Price_GOM , y = hrate , color = SIZE)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE, level=0.95) +  # Linear model fit
  scale_color_manual(values=size_color)+
  labs(title = "Harvest Rate predicted with Ex-Vessel Price by Size",
       x = "Ex-Vessel Price",
       y = "Harvest Rate",
       color = "Size") +  
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))
BSH_harv_pfuel_price
export_fig(BSH_harv_pfuel_price, size='big',  folder='EDM_pubs')


BSH_harv_pfuel_fuel<- ggplot(cpue_land_Gfuel, aes(x = P_Fuel_Real , y = hrate , color = SIZE)) +
  geom_point() +  # Scatter plot of price vs. landings
  geom_smooth(method = "lm", aes(group=SIZE, color=SIZE), se = TRUE, level=0.95) +  # Linear model fit
  scale_color_manual(values=size_color)+
  labs(title = "Harvest Rate predicted with Ex-Vessel Price by Fuel Price",
       x = "Fuel Price",
       y = "Harvest Rate",
       color = "Size") +  
  theme_minimal(base_size = 20) +                                                 # Large font
  theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
        axis.title = element_text(size = 22),                                     # Larger axis titles
        axis.text = element_text(size = 18))
BSH_harv_pfuel_fuel
export_fig(BSH_harv_pfuel_fuel, size='big',  folder='EDM_pubs')



##**LOG TRANSFORM Fuel Price**
pricemod_Gfuel_log <- lm( hrate ~  log(Price_GOM) * SIZE + log(P_Fuel_Real), data=cpue_land_Gfuel)
summary(pricemod_Gfuel_log)


# Fit the model keeping your exact formula
pricemod_Gfuel_glm <- glm(
  hrate ~ log(Price_GOM) * SIZE + log(P_Fuel_Real), 
  data = cpue_land_Gfuel)
summary(pricemod_Gfuel_glm)

library(marginaleffects)

# Predict hrate across a range of Prices for different SIZEs, 
# while holding P_Fuel_Real at its mean (default behavior of datagrid)
preds <- predictions(
  pricemod_Gfuel_glm,
  newdata = datagrid(
    # Create a sequence from the minimum to maximum observed prices
    Price_GOM = seq(min(cpue_land_Gfuel$Price_GOM), 
                    max(cpue_land_Gfuel$Price_GOM), 
                    length.out = 50),
    # If SIZE is categorical or has a few specific sizes, specify them
    SIZE = unique(cpue_land_Gfuel$SIZE) 
  )
)

# View the predictions (the 'estimate' column will strictly be between 0 and 1)
# head(preds)
# 
# 
# library(ggplot2)
# 
# plot_predictions(
#   pricemod_Gfuel_glm, 
#   condition = c("Price_GOM", "SIZE")
# ) +
#   plot_predictions(
#     pricemod_Gfuel_glm, 
#     condition = c("P_Fuel_Real", "SIZE"),
#     newdata = datagrid(grid_type = "observed")
#   ) + 
#   # Force the Y-axis to show the 0 to 1 limits
#   scale_y_continuous(limits = c(0, 0.3)) + 
#   labs(
#     title = "Predicted Harvest Rate (hrate)",
#     x = "Price (GOM)",
#     y = "Harvest Rate"
#   ) +
#   scale_color_manual(values=size_color)+
#   labs(title = "Harvest Rate predicted with Ex-Vessel Price by Size and Fuel Price",
#        x = "Fuel Price",
#        y = "Harvest Rate",
#        color = "Size") +  
#   theme_minimal(base_size = 20) +                                                 # Large font
#   theme(plot.title = element_text(hjust = 0.5),                                   # Centered title
#         axis.title = element_text(size = 22),                                     # Larger axis titles
#         axis.text = element_text(size = 18))
#   #theme_minimal()



# Use this alternative if you don't want lines drawn where a specific SIZE lacks data

  # ... add the same geom_point and formatting layers as above ...
  

  
###################################****FINAL PLOT TO ADAPT FOR PAPER

library(ggplot2)
library(marginaleffects)
library(dplyr)

# 1. Extract the prediction data instead of plotting it immediately
pred_data <- plot_predictions(
  pricemod_Gfuel_glm, 
  condition = c("Price_GOM", "SIZE"),
  draw = FALSE # This tells marginaleffects to return a dataframe
)

# 2. Find the observed min and max Price_GOM for each SIZE category
observed_ranges <- cpue_land_Gfuel %>%
  group_by(SIZE) %>%
  summarize(
    min_price = min(Price_GOM, na.rm = TRUE),
    max_price = max(Price_GOM, na.rm = TRUE),
    .groups = "drop"
  )

# 3. Filter the prediction data to only keep values within the observed ranges
pred_data_filtered <- pred_data %>%
  left_join(observed_ranges, by = "SIZE") %>%
  filter(Price_GOM >= min_price & Price_GOM <= max_price)

# 4. Build the plot manually using the filtered predictions and raw data
p1 <- ggplot(pred_data_filtered, aes(x = Price_GOM, color = SIZE, fill = SIZE)) +
  geom_vline(xintercept=7.643, linetype='dashed', color='grey')+ ##add predition interval up top (background)
  geom_vline(xintercept=1.304, linetype='dashed', color='grey')+ ##add predition interval up top (background)
  # Add the confidence interval ribbon (using the filtered prediction data)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  # Add the prediction lines (using the filtered prediction data)
  geom_line(aes(y = estimate), linewidth = 1) +
  # Add the raw data points from your original dataframe
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = Price_GOM, y = hrate, color = SIZE),
    alpha = 0.75,
    size = 2,
    inherit.aes = FALSE # Prevents geom_point from looking for ribbon aesthetics
  ) +
  scale_color_manual(values = size_color) +
  scale_fill_manual(values = size_color) +
  scale_y_continuous(limits = c(0, 0.5)) + 
  labs(
    x = "Ex-vessel Price",
    y = "",
    color = "Size",
    fill = "Size"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

p1



##fuel by size
BSH_glm_fig_fuelsize <- plot_predictions(
  pricemod_Gfuel_glm, 
  condition = c("P_Fuel_Real", "SIZE")
) +
  # Overlay the raw data points, updating x to match the fuel price variable
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = P_Fuel_Real, y = hrate, color = SIZE),
    alpha = 0.75,       
    size = 2
  ) +
  # Force the Y-axis to show the 0 to 1 limits
  scale_y_continuous(limits = c(0, 0.5)) + 
  
  # Apply your custom colors to lines, points, and shaded regions
  scale_color_manual(values = size_color) +
  scale_fill_manual(values = size_color) + 
  
  # Update titles and axis labels for Fuel Price
  labs(
    title = "Predicted Harvest Rate by Fuel Price and Size",
    x = "Fuel Price",
    y = "",
    color = "Size",    
    fill = "Size"      
  ) +
  theme_minimal(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
BSH_glm_fig_fuelsize
export_fig(BSH_glm_fig_fuelsize, size='big',  folder='EDM_pubs')



##just fuel
p2<-plot_predictions(
  pricemod_Gfuel_glm, 
  condition = "P_Fuel_Real" # Removed SIZE
) +
  geom_vline(xintercept=1.5, linetype='dashed', color='grey')+ ##add predition interval up top (background)
  geom_vline(xintercept=4, linetype='dashed', color='grey')+ ##add predition interval up top (background)
  # You can still plot the raw data colored by size to show the spread
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = P_Fuel_Real, y = hrate),
    alpha = 0.75,       
    size = 2
  ) +
  scale_y_continuous(limits = c(0, 0.5)) + 
  #scale_color_manual(values = size_color) +
  labs(
    #title = "Predicted Harvest Rate",
    x = "Fuel Price",
    y = "Harvest Rate",
    color = "Size"
  ) +
  theme_minimal(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
p2


p2+p1

library(patchwork)

# p2 + p1 automatically places them side-by-side. 
# plot_annotation() adds the overarching title.
BSH_glm_fig <- p2 + p1 + 
  plot_annotation(
    title = "Predicted Harvest Rate",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 24))
  )
BSH_glm_fig
export_fig(BSH_glm_fig, size='bigw',  folder='EDM_pubs')


vline_data <- data.frame(
  SIZE = c("Small", "Small", "Medium", "Medium", "Large", "Large"),
  intercept = c(1.304, 2.679,     # Lines for Small
                2.170, 4.339, # Lines for Medium
                3.821, 7.643)     # Lines for Large
)
##export figure by size class only
BSH_glm_fig_sizeonly<-ggplot(pred_data_filtered, aes(x = Price_GOM, color = SIZE, fill = SIZE)) +
  geom_vline(
    data = vline_data, 
    aes(xintercept = intercept), 
    linetype = 'dashed', 
    color = 'grey'
  ) +
  # Add the confidence interval ribbon (using the filtered prediction data)
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  # Add the prediction lines (using the filtered prediction data)
  geom_line(aes(y = estimate), linewidth = 1) +
  # Add the raw data points from your original dataframe
  facet_wrap(~SIZE,ncol=1, scales='free')+
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = Price_GOM, y = hrate, color = SIZE),
    alpha = 0.75,
    size = 2,
    inherit.aes = FALSE # Prevents geom_point from looking for ribbon aesthetics
  ) +
  scale_color_manual(values = size_color) +
  scale_fill_manual(values = size_color) +
  #scale_y_continuous(limits = c(0, 0.5)) + 
  scale_x_continuous(limits=c(1.25,12.75))+
  labs(
    x = "Ex-vessel Price",
    y = "",
    color = "Size",
    fill = "Size"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 22),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )
BSH_glm_fig_sizeonly
export_fig(BSH_glm_fig_sizeonly, size='big',  folder='EDM_pubs')

BSH_glm_fig3 <- p2 + BSH_glm_fig_sizeonly + 
  plot_annotation(
    title = "Predicted Harvest Rate",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 24))
  )
BSH_glm_fig3
export_fig(BSH_glm_fig3, size='bigw',  folder='EDM_pubs')





##**LOG LOG TRANSFORM Fuel Price**
pricemod_Gfuel_log_log <- lm( log(hrate) ~  log(Price_GOM) * SIZE + log(P_Fuel_Real), data=cpue_land_Gfuel)
summary(pricemod_Gfuel_log_log)


# Fit the model keeping your exact formula
pricemod_Gfuel_glm_log <- glm(
  log(hrate) ~ log(Price_GOM) * SIZE + log(P_Fuel_Real), 
  data = cpue_land_Gfuel)
summary(pricemod_Gfuel_glm_log)

library(marginaleffects)

# Predict hrate across a range of Prices for different SIZEs, 
# while holding P_Fuel_Real at its mean (default behavior of datagrid)
preds <- predictions(
  pricemod_Gfuel_glm_log,
  newdata = datagrid(
    # Create a sequence from the minimum to maximum observed prices
    Price_GOM = seq(min(cpue_land_Gfuel$Price_GOM), 
                    max(cpue_land_Gfuel$Price_GOM), 
                    length.out = 50),
    # If SIZE is categorical or has a few specific sizes, specify them
    SIZE = unique(cpue_land_Gfuel$SIZE) 
  )
)


###################################****FINAL PLOT TO ADAPT FOR PAPER
library(ggplot2)
library(marginaleffects)

p1log<-plot_predictions(
  pricemod_Gfuel_glm_log, 
  condition = c("Price_GOM", "SIZE")
) +
  # 1. Add the raw data points from your original dataframe
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = Price_GOM, y = log(hrate), color = SIZE),
    alpha = 0.75,       # Adds transparency so overlapping points don't hide the lines
    size = 2
  ) +
  scale_color_manual(values=size_color)+
  scale_fill_manual(values=size_color)+
  # Force the Y-axis to show the 0 to 1 limits
  scale_y_continuous(limits = c(-6,0)) + 
  labs(
    # title = "Predicted Harvest Rate",
    x = "Ex-vessel Price",
    y = "",
    color = "Size",    # Unifies the line and point legends
    fill = "Size"      # Unifies the confidence interval ribbon legend
  ) +
  theme_minimal(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
p1log

##fuel by size
BSH_glm_fig_fuelsize_log <- plot_predictions(
  pricemod_Gfuel_glm_log, 
  condition = c("P_Fuel_Real", "SIZE")
) +
  # Overlay the raw data points, updating x to match the fuel price variable
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = P_Fuel_Real, y = log(hrate), color = SIZE),
    alpha = 0.75,       
    size = 2
  ) +
  # Force the Y-axis to show the 0 to 1 limits
  scale_y_continuous(limits = c(-6,0)) + 
  
  # Apply your custom colors to lines, points, and shaded regions
  scale_color_manual(values = size_color) +
  scale_fill_manual(values = size_color) + 
  
  # Update titles and axis labels for Fuel Price
  labs(
    title = "Predicted Harvest Rate by Fuel Price and Size",
    x = "Fuel Price",
    y = "",
    color = "Size",    
    fill = "Size"      
  ) +
  theme_minimal(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
export_fig(BSH_glm_fig_fuelsize_log, size='big',  folder='EDM_pubs')



##just fuel
p2log<-plot_predictions(
  pricemod_Gfuel_glm_log, 
  condition = "P_Fuel_Real" # Removed SIZE
) +
  # You can still plot the raw data colored by size to show the spread
  geom_point(
    data = cpue_land_Gfuel, 
    aes(x = P_Fuel_Real, y = log(hrate)),
    alpha = 0.75,       
    size = 2
  ) +
  scale_y_continuous(limits = c(-6,0)) + 
  #scale_color_manual(values = size_color) +
  labs(
    #title = "Predicted Harvest Rate",
    x = "Fuel Price",
    y = "Harvest Rate",
    color = "Size"
  ) +
  theme_minimal(base_size = 20)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))
p2log


p2log+p1log

library(patchwork)

# p2 + p1 automatically places them side-by-side. 
# plot_annotation() adds the overarching title.
BSH_glm_fig_log <- p2log + p1log + 
  plot_annotation(
    title = "Predicted Harvest Rate",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 24))
  )

export_fig(BSH_glm_fig_log, size='bigw',  folder='EDM_pubs')



##test alternate formulations
summary(pricemod_Gfuel_log_log)

pricemod_Gfuel_log_log_nosize <- lm( log(hrate) ~  log(Price_GOM) + log(P_Fuel_Real) , data=cpue_land_Gfuel)
pricemod_Gfuel_log_log_interact <- lm( log(hrate) ~  log(Price_GOM)*SIZE + log(P_Fuel_Real)*SIZE , data=cpue_land_Gfuel)
summary(pricemod_Gfuel_log_log_interact)


summary(pricemod_Gfuel_log_log_nosize)

# Compare AIC and BIC (Lower is better)
AIC(pricemod_Gfuel_log, pricemod_Gfuel_log_log,pricemod_Gfuel_log_log_nosize)
BIC(pricemod_Gfuel_log, pricemod_Gfuel_log_log,pricemod_Gfuel_log_log_nosize)

# Likelihood Ratio Test / F-Test for nested models
# Tests if the more complex model (mod2) is significantly better
#anova(pricemod_Gfuel, pricemod_Gfuel_log, pricemod_Gfuel_log_log, pricemod_Gfuel_log_log_nosize)

# Set up a 3x4 grid to plot Model 1 on top and Model 2 on the bottom
par(mfrow = c(3, 4), mar = c(4, 4, 2, 1))

# ask = FALSE prevents R from prompting you to hit 'Enter' between plots
plot(pricemod_Gfuel_log, ask = FALSE)
plot(pricemod_Gfuel_log_log, ask = FALSE)
plot(pricemod_Gfuel_log_log_nosize, ask = FALSE)

# Reset plotting window to default
par(mfrow = c(1, 1))

# install.packages("performance")
library(performance)

# Generates a clean table comparing R2, RMSE, AIC, BIC, and Bayes Factors
compare_performance( pricemod_Gfuel_log, pricemod_Gfuel_log_log, pricemod_Gfuel_log_log_nosize, rank = TRUE)


