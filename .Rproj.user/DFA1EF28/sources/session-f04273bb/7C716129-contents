# all inputs needed for shinyapp
library(shiny); library(ggplot2); library(plotly); library(tidyverse)
# setwd("/home/mkoltai/Desktop/mas/soc/valasztasok/terkepek/shinyapp_HU_pop_distr/")

# message("LIBRARIES LOADED")

l_telep_meret <- readRDS(file="l_telep_meret.RDS") # shinyapp_HU_pop_distr/

# message("l_plotly has been created:")

# Source the UI and server files
source("ui.R") # shinyapp_HU_pop_distr/
source("server.R") # shinyapp_HU_pop_distr/

# ShinyApp
shinyApp(
  ui = ui,
  server)

#  = function(input, output, session) {
#    server(input, output, session, plotly_list = l_plotly) }
