# Description :


# Libraries ---------------------------------------------------------------
### ///////////////////////////////////////////////////////////////////////
# Script name : app.R
# Script status : WIP
# Script description : Build the shiny app to be able to play mastermind and to
#                      test algorithm that solves master mind. 
#                      The app has two panels, a game panel which code the 
#                      classical mastermind. A test panel that enable the user 
#                      to load a script that solve the the game.
### ///////////////////////////////////////////////////////////////////////


# Libraries ---------------------------------------------------------------

library(shiny)
library(reactlog)
library(tidyverse)
library(waiter)

# Options -----------------------------------------------------------------

options(shiny.reactlog = TRUE)

# Load functions ----------------------------------------------------------

source("fct4app.r")

# Load global variables ---------------------------------------------------

source("globalVar.R")

# UI ----------------------------------------------------------------------

source("ui.R")

# Server ------------------------------------------------------------------

source("server.R")

# Run the app -------------------------------------------------------------

shinyApp(ui, server)

# END