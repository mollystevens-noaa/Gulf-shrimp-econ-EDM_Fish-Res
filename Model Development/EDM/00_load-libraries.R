##Load libraries
#devtools::install_github("tanyalrogers/GPEDM", force=TRUE)# ref="dev")
# librarian::shelf(ggplot2, rEDM, tidyverse, readxl, writexl, trackdown, 
#                    snowfall, flextable, devtools, wesanderson, plotly)

edm_packages = c(as.character(expression(ggplot2, rEDM, tidyverse, readxl, snowfall,flextable,
                                         writexl, trackdown, GPEDM, wesanderson, plotly, marginaleffects)))

lapply(edm_packages, library, character.only=TRUE)


# Not in
'%nin%' <- Negate('%in%')

#wes_palette("FantasticFox1")
# season_color <- c(Summer = wes_palette("FantasticFox1")[3],
#                   Fall = wes_palette("FantasticFox1")[4],
#                   Winter = wes_palette("FantasticFox1")[2])
