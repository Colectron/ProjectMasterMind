# solver

# Dumbest solver ever -----------------------------------------------------
solver_dumbest <- function(history){
  output <- sample(0:(length(colour)-1),4,replace = TRUE)
  return(output)
}

# Very dumb solver --------------------------------------------------------
comb_to_test <- crossing(sqr1 = 1:8, sqr2 = 1:8, sqr3 = 1:8, sqr4 = 1:8)
iter <-  1
solver_very_dumb <- function(history){
  output = comb_to_test[iter,]
  iter <<- iter + 1
  return(output)
}

# A bit smarter one -------------------------------------------------------

# Very dumb solver --------------------------------------------------------
all_comb <- crossing(pawn1 = 1:8, pawn2 = 1:8, pawn3 = 1:8, pawn4 = 1:8) %>% 
  mutate(
    seq = paste0(pawn1,pawn2,pawn3,pawn4),
    weight = 1,
    nb_red = NA,
    nb_white = NA,
    id = row_number(),
    tried = FALSE
  )


iter <-  1
solver_a_bit_smarter <- function(history){
  last_try <- history[iter,c("pawn1","pawn2","pawn3","pawn4")]
  combn_n_red = combn(1:4, max(history[iter,"nb_red"],0) )
  combn_n_red <- combn_n_red %>% t %>% 
    tibble() %>% 
    mutate(id = row_number()) %>% 
    group_nest(id) %>%
    mutate(data = map(data,unlist))
  
  combn_n_white = combn(1:4, max(history[iter,"nb_white"],0))
  combn_n_white <- combn_n_white %>% t %>% 
    tibble() %>% 
    mutate(id = row_number()) %>% 
    group_nest(id) %>%
    mutate(data = map(data,unlist))
  
  # Update tried status in all comb according to the last element of history
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "tried"] = TRUE
  
  if(iter>1){ borwser()}
  
  # For all combs compute if it may matches white or matches red
  all_comb %>%
    mutate(
      white_hint_mathes = 
    )

  
  output <- all_comb %>% 
    filter(weight == max(.$weight),!tried) %>% 
    select(dplyr::contains("pawn")) %>% 
    .[sample(1:dim(.)[1],1),]
  
  iter <<- iter + 1
  return(output)
}



# Write the name of your function here
solver <- solver_a_bit_smarter
# end