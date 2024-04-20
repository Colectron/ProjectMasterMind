### ///////////////////////////////////////////////////////////////////////
# Script name : globalVar.R
# Script status : 
# Script description : Build all the global variables used in the app.R or in
#                      the solver algorithm.  
### ///////////////////////////////////////////////////////////////////////

# colour ------------------------------------------------------------------
# State : Done
# Description : List of the selected colour of the buttons.
### ///////////////////////////////////////////////////////////////////////

colour <-
  c(
    "#43caa7",
    "#000bdf",
    "#df0000",
    "#701abf",
    "#dfbe00",
    "#ff7e31",
    "#ffffff",
    "#df5abe"
  )

secret_combination <- 
  # rep(0,4)
  sample(0:(length(colour)-1),4,replace = TRUE)

# END