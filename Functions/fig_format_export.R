

##Define color schemes for ALL plots 
#wes_palette("GrandBudapest1")
size_color <- c(Small = wes_palette("GrandBudapest1")[1],
                Medium = wes_palette("GrandBudapest1")[4],
                Large = wes_palette("GrandBudapest1")[3],
                Smedium = wes_palette("GrandBudapest1")[2],
                Marge = wes_palette("GrandBudapest1")[2])

#wes_palette("Rushmore1")
area_color <- c('1-10' = wes_palette("Rushmore1")[5],
                '11-17' = wes_palette("Rushmore1")[3],
                '18-21'= wes_palette("Rushmore1")[4])

#wes_palette("Zissou1")
season_color <- c(Summer = wes_palette("Zissou1")[3],
                  Fall = wes_palette("Zissou1")[5],
                  Winter = wes_palette("Zissou1")[1])


species_color <- c(Brown = "tan4",
                   Pink = "hotpink3",
                   White = "grey70")

#wes_palette("Darjeeling1")
source_color <- c(EDM_Biomass =  wes_palette("Darjeeling1")[4],
                  EDM_CPUE =  wes_palette("Darjeeling1")[3],
                  VAST_Index =  wes_palette("Darjeeling1")[2],
                  JABBA_Biomass =  wes_palette("Darjeeling1")[1])

##**fix output folder in edm_loop**

##Create Figure subfolders for function below
if(!dir.exists('./Figures/VAST')){ dir.create('./Figures/VAST') } 
if(!dir.exists('./Figures/EDM')){ dir.create('./Figures/EDM') } 
if(!dir.exists('./Figures/SPM')){ dir.create('./Figures/SPM') } 
if(!dir.exists('./Figures/EDM_final')){ dir.create('./Figures/EDM_final') } 


export_fig <- function(fig, size="medium", exportname=name, folder="main", filename = (paste0(deparse(substitute(fig)),".png"))){
  if(size=="small"){w=5;h=4}else if(size=="medium"){w=8;h=5.8}else if(size=="page"){w=8;h=9} else if (size=="wide"){w=11;h=5} else if (size=="big"){w=11;h=8} else if (size=="bigw"){w=14;h=8}
  if(folder=="main"){p='./Figures'}
    else if(folder=="VAST"){p='./Figures/VAST'}
      else if(folder=="EDM"){p='./Figures/EDM'}
        else if(folder=="SPM"){p='./Figures/SPM'}
        else if(folder=="EDM_final"){p='./Figures/EDM_final'}
        else if(folder=="EDM_pubs"){p='./ModelDevelopment/EDM/pubs/figs'} 
        else if(folder=="RW"){p='./Figures/RW_requests'}
        else if(folder=="EDM_loop"){p=paste0('./ModelDevelopment/EDM/output_',spp,'/allruns/',exportname)} #'name' defined in 02_model-runs
    else if(folder=="EDMout_loop"){p=paste0('./ModelDevelopment/EDM/output/allruns/',exportname)} #first built for BSH then adapted below
    else if(folder=="EDMoutWSH_loop"){p=paste0('./ModelDevelopment/EDM/output_WSH/allruns/',exportname)} #'name' defined in 02_model-runs
    else if(folder=="EDMoutBSH_loop"){p=paste0('./ModelDevelopment/EDM/output_BSH/allruns/',exportname)} #'name' defined in 02_model-runs
    else if(folder=="EDMoutPSH_loop"){p=paste0('./ModelDevelopment/EDM/output_PSH/allruns/',exportname)} #'name' defined in 02_model-runs
    else if(folder=="JABBA_loop"){p=paste0('./ModelDevelopment/JABBA/output/',exportname)} #'name' defined in 01_jabbarun
      ggsave(filename = filename,
         plot = fig,
         device = "png",
         #path =  "./figures",
         path = p,
         width = w,
         height = h)
}


##Define functions for figures
# myTheme <- function(fig){
#   fig + 
#     #scale_y_continuous(breaks = scales::pretty_breaks(2), limits = c(0, NA)) +
#     #scale_x_continuous(limits = c(0,35), breaks = seq(0,35,10))+
#     theme(axis.ticks = element_line(colour = "black"),
#           title = element_text(family = "Times",
#                                size = 14),
#           axis.title = element_text(family = "Times",
#                                     size = 12),
#           axis.text = element_text(family = "Times",
#                                    size = 12,
#                                    colour = "black"),
#           axis.text.x = element_text(family = "Times"),
#           legend.position="bottom",
#           legend.text = element_text(size = 12,
#                                      family = "Times"),
#           legend.title = element_blank())
# }