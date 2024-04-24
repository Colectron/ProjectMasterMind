# solver

# Dumbest solver ever -----------------------------------------------------
solver_dumbest <- function(history){
  output <- sample(0:(length(colour)-1),4,replace = TRUE)
  return(output)
}

# Very dumb solver --------------------------------------------------------
comb_to_test <- crossing(sqr1 = 1:8, sqr2 = 1:8, sqr3 = 1:8, sqr4 = 1:8)
iter = 1

solver_very_dumb <- function(history){
  output = comb_to_test[iter,]
  iter <<- iter + 1
  return(output)
}

# A bit smarter one -------------------------------------------------------


# Write the name of your function here
solver <- solver_very_dumb
# end