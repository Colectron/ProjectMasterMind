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
# Initiate objects
all_comb <- crossing(pawn1 = 1:8, pawn2 = 1:8, pawn3 = 1:8, pawn4 = 1:8) %>% 
  mutate(
    seq = paste0(pawn1,pawn2,pawn3,pawn4),
    weight = 1,
    nb_red = NA,
    nb_white = NA,
    id = row_number(),
    tried = FALSE,
    matches_white_hint = NA,
    matches_red_hint = NA
  )
iter <-  1

# utility function
replace_at_pos <- function(string, pos, value) {
  stopifnot(length(pos) == length(value))
  chars <- str_split(string, pattern = "", simplify = TRUE)
  chars[pos] <- value
  str_c(chars, collapse = "")
}
all_permutations <- function(x) {
  asplit(permutations(length(x), length(x), x), 1)
}

solver_a_bit_smarter <- function(history){
  
  # Initialization
  last_try <- history[iter,c("pawn1","pawn2","pawn3","pawn4")]
  nb_white <- as.numeric(history[iter,"nb_white"])
  nb_red <- as.numeric(history[iter,"nb_red"])
  
  #
  if( !(iter>1 & (nb_white + nb_red)>0) ){
  }else{
    # Initialisation algorithm
    .tmp_perm <- permutations(4,nb_white,c(1,2,3,4))
    .tmp_comb <- combinations(4,nb_white,c(1,2,3,4))
    perm_values <- matrix(last_try[.tmp_perm], nrow = nrow(.tmp_perm))
    
    # Red hints
    combn_n_red <- combn(1:4, max(nb_red,0) )
    combn_n_red <- combn_n_red %>% t %>% 
      tibble() %>% 
      mutate(id = row_number()) %>% 
      group_nest(id) %>%
      mutate(data = map(data,unlist))
    
    # White hints
    combn_n_white <- tibble(
      position = asplit(.tmp_comb,1),
      values = rep(list(perm_values),nrow(.tmp_comb))
      ) |> 
      mutate(
        position = map(position, all_permutations)
      ) |>
      unnest(position) |> 
      mutate(
        patterns = map2(
          .x = position, 
          .y = values, 
          .f = function(pos,val){map(asplit(val,1),~replace_at_pos("....",pos,.x))} 
          )
      ) |>
      pull(patterns) |>
      unlist() |> unique()
  }

  
  # Update all comb according to the last element of history
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "tried"] = TRUE
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "nb_white"] = history[iter,"nb_white"]
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "nb_red"] = history[iter,"nb_red"]
  
  # For all combs compute if it may matches white or matches red
  all_comb <- all_comb |> 
    rowwise() |> 
    mutate(
      matches_white_hint = case_when(
        is.na(matches_white_hint) | matches_white_hint ~ any(str_detect(seq,combn_n_white)),
        matches_white_hint
      )
    )

  output <- all_comb %>% 
    filter(weight == max(.$weight),!tried,matches_white_hint) %>% 
    select(dplyr::contains("pawn")) %>% 
    .[sample(1:dim(.)[1],1),]
  
  iter <<- iter + 1
  return(output)
}



# Write the name of your function here
solver <- solver_a_bit_smarter
# end