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
    tried = FALSE,
    matches_white_hint = T, # indicating if the sequence matches white hints, if TRUE it is a possible solution
    matches_red_hint = T # same
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
  last_try_seq <- paste(last_try,collapse="")
  nb_white <- as.numeric(history[iter,"nb_white"])
  nb_red <- as.numeric(history[iter,"nb_red"])
  
  # Update all comb according to the last element of history
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "tried"] = TRUE
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "nb_white"] = history[iter,"nb_white"]
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "nb_red"] = history[iter,"nb_red"]
  
  # Algorithm 
  if( !(iter>1 & (nb_white + nb_red)>0) ){
  }else{

    # Red hints
    if(nb_red>0){
      # Initialization
      .tmp_comb <- combinations(4,nb_red,c(1,2,3,4))
      
      red_hints_patterns <- asplit(.tmp_comb,1) |>
        map_chr(
          function(pos) {
            pattern <- rep(".", 4)
            pattern[pos] <- last_try[pos]
            str_c(pattern, collapse = "")
          }
        )
      
      # For all sequences, computes if it matches red hints
      all_comb <- all_comb |> 
        rowwise() |> 
        mutate(
          matches_red_hint = case_when(
            matches_red_hint ~ any(str_detect(seq,red_hints_patterns)),
            !matches_red_hint ~ matches_red_hint,
            .default = matches_red_hint
          )
        ) 
    }
    
    # White hints
    if(nb_white>0){
      # Initialization
      .tmp_perm <- permutations(4,nb_white,c(1,2,3,4))
      .tmp_comb <- combinations(4,nb_white,c(1,2,3,4))
      perm_values <- matrix(last_try[as.numeric(.tmp_perm)], nrow = nrow(.tmp_perm))
      
      # Compute all possible patterns according to white hints
      white_hints_patterns <- tibble(
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
      
      # Remove patterns matches the last try, ie impossible solutions
      white_hints_patterns <- white_hints_patterns[str_detect(last_try_seq,white_hints_patterns,negate = T)]
      
      # For all sequences, computes if it matches white hints
      all_comb <- all_comb |> 
        rowwise() |> 
        mutate(
          matches_white_hint = case_when(
            matches_white_hint ~ any(str_detect(seq,white_hints_patterns)),
            matches_white_hint ~ matches_white_hint,
            .default = matches_white_hint
          )
        )  
    }
    
  }

  output <- all_comb %>% 
    filter(
      weight == max(.$weight),
      !tried,
      matches_white_hint,
      matches_red_hint
      ) %>% 
    select(dplyr::contains("pawn")) %>% 
    .[sample(1:dim(.)[1],1),]
  
  print("------------------------------------------------------")  
  message <- sprintf(
    "Current try : %s (R : %s; B %s) || nb séq restantes : %s", 
    paste(output,collapse=""),
    nb_red,
    nb_white,
    all_comb %>% 
      filter(
        weight == max(.$weight),
        !tried,
        matches_white_hint,
        matches_red_hint
        ) |> 
      nrow()
    )
  print(message)
  
  a <- readline("Debug? (y/n): ")
  if(a=="y"){
    browser()
  }
  
  all_comb <<- all_comb
  iter <<- iter + 1
  return(output)
}



# Write the name of your function here
solver <- solver_a_bit_smarter
# end