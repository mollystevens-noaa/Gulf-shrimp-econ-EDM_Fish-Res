####**Load packages**
# install.packages("remotes")
# remotes::install_github("claudiozandonella/trackdown",
#                          build_vignettes = TRUE)
rep_packages = c(as.character(expression(ggplot2, tidyverse, readxl, flextable,
                                         writexl, trackdown, wesanderson, trackdown,
                                         #magick, cowplot, patchwork,
                                         knitr, scales, invgamma,
                                         GPEDM, wesanderson, plotly,officer)))

lapply(rep_packages, library, character.only=TRUE)

