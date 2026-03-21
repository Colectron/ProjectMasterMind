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
  # Initialization
  last_try <- history[iter,c("pawn1","pawn2","pawn3","pawn4")]
  last_try_seq <- paste(last_try,collapse="")
  nb_white <- as.numeric(history[iter,"nb_white"])
  nb_red <- as.numeric(history[iter,"nb_red"])
  
  # Algorithm
  output <- comb_to_test[iter,]
  
  # Debugging message
  # print("------------------------------------------------------")  
  # message <- sprintf(
  #   "Current try no: %s, try : %s (R : %s; B %s) ", 
  #   iter,
  #   paste(output,collapse=""),
  #   nb_red,
  #   nb_white
  #   )
  # print(message)
  
  # Update and exit function
  iter <<- iter + 1
  return(output)
}

# A bit smarter one -------------------------------------------------------
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
count_digits <- function(seq, digits = 1:8) {
  
  chars <- strsplit(seq, "")[[1]]
  
  counts <- tabulate(match(chars, as.character(digits)), nbins = length(digits))
  
  paste0(counts, collapse = "")
}

# Initiate objects
all_comb <- crossing(pawn1 = 1:8, pawn2 = 1:8, pawn3 = 1:8, pawn4 = 1:8) %>% 
  rowwise() |> 
  mutate(
    seq = paste0(pawn1,pawn2,pawn3,pawn4),
    trace_seq = count_digits(seq), 
    weight = 1,
    nb_red = NA,
    nb_white = NA,
    tried = FALSE,
    pool = NA,
    matches_pool = T,
    matches_trace = T,
    matches_white_hint = T, # indicating if the sequence matches white hints, if TRUE it is a possible solution
    matches_red_hint = T # same
  )

pool <-1:8

iter <- 0

solver_a_bit_smarter <- function(history,debug_solver=F){

  if (exists("debug_solver") && isTRUE(debug_solver)) {
    browser()
  }
  
  # Initialization
  last_try <- history[iter+1,c("pawn1","pawn2","pawn3","pawn4")]
  last_try_seq <- paste(last_try,collapse="")
  nb_white <- as.numeric(history[iter+1,"nb_white"])
  nb_red <- as.numeric(history[iter+1,"nb_red"])
  
  # Update all comb according to the last element of history
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "tried"] = TRUE
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "nb_white"] = history[iter+1,"nb_white"]
  all_comb[ all_comb$seq == paste(last_try,collapse = ""), "nb_red"] = history[iter+1,"nb_red"]
  
  # Algorithm 
  if( iter<1 || (nb_white + nb_red)==0 ){
    # Pool approach
    pool <<- setdiff(pool,as.numeric(last_try))
    pool_seq <- crossing(pawn1 = pool, pawn2 = pool, pawn3 = pool, pawn4 = pool) |> 
      mutate(seq = paste0(pawn1,pawn2,pawn3,pawn4)) |>
      pull(seq)
    
    # Sequences with pawns not in the pool are excluded from the possible solutions
    all_comb <- all_comb |> 
      rowwise() |> 
      mutate(
        matches_pool = case_when(
          matches_pool ~ seq %in% pool_seq,
          !matches_trace ~ matches_pool,
          .default = matches_pool
        )
      ) 
    
  }else{
    
    # Trace approach
    nb_hint <- nb_white + nb_red
    .tmp_perm <- permutations(4,nb_hint,c(1,2,3,4))
    .tmp_comb <- combinations(4,nb_hint,c(1,2,3,4))
    perm_values <- matrix(last_try[as.numeric(.tmp_perm)], nrow = nrow(.tmp_perm))
    
    # Compute all possible patterns according to the maximum number of hints
    patterns <- tibble(
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
    
    # Compute trace of all possible patterns
    trace_patterns <- patterns |> 
      map_chr(count_digits) |> 
      unique() |> 
      str_replace_all("0",".") |> 
      str_replace_all(c("1"="[1-4]","2"="[2-4]","3"="[3-4]")) # Handdle duplicates

    # Sequences with non-matching trace patterns are not valid solutions
    all_comb <- all_comb |> 
      rowwise() |> 
      mutate(
        matches_trace = case_when(
          matches_trace ~ any(str_detect(trace_seq,trace_patterns)),
          !matches_trace ~ matches_trace,
          .default = matches_trace
        )
      ) 
    
    # Red hints filter
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
            matches_red_hint & matches_trace ~ any(str_detect(seq,red_hints_patterns)),
            !matches_red_hint ~ matches_red_hint,
            .default = matches_red_hint
          )
        ) 
    }
    
    # White hints filter
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
            matches_white_hint & matches_trace ~ any(str_detect(seq,white_hints_patterns)),
            matches_white_hint ~ matches_white_hint,
            .default = matches_white_hint
          )
        )  
    }
    
  }

  all_comb <- all_comb |> 
    filter(
      weight == max(weight),
      !tried,
      matches_pool,
      matches_trace,
      matches_white_hint,
      matches_red_hint
    ) |> 
    ungroup()
  
  output <- all_comb |>
    select(dplyr::contains("pawn")) |>  
    slice_sample(n = 1)
  
  # Update and exit function
  all_comb <<- all_comb
  iter <<- iter + 1
  return(output)
}

# Distance based solver ---------------------------------------------------
# Initialization
all_comb2 <- expand.grid(1:8, 1:8, 1:8, 1:8) |>
  as.matrix()

# Functions
score_matrix <- function(candidates, guess) {
  
  guess <- as.integer(guess)
  
  # red
  reds <- rowSums(candidates == matrix(guess, nrow(candidates), 4, byrow = TRUE))
  
  # counts (0-8)
  counts_cand <- t(apply(candidates, 1, tabulate, nbins = 8))
  counts_guess <- tabulate(guess, nbins = 8)
  
  commons <- rowSums(pmin(counts_cand, matrix(counts_guess, nrow(candidates), 8, byrow = TRUE)))
  
  whites <- commons - reds
  
  output <- list(reds = reds, whites = whites)
  return(output)
}

iter2 <- 0
solver_distance_based <- function(history){
  
  # Initialization
  last_try <- as.integer(history[iter2+1,1:4])
  nb_white <- history[iter2+1,"nb_white"] |> as.numeric()
  nb_red <- history[iter2+1,"nb_red"] |> as.numeric()
  
  # Algorithm
  if(iter2 > 0){
    scores <- score_matrix(all_comb2, last_try)
    
    keep <- scores$reds == nb_red & scores$whites == nb_white
    
    all_comb2 <<- all_comb2[keep, , drop = FALSE]
  }
  
  # Build output
  output <- all_comb2[sample(nrow(all_comb2), 1), ]
  names(output) = paste0("pawn",1:4)
  
  if (F) {
    print("------------------")
    print(
      sprintf(
        "Try n°%s : %s (R : %s; B : %s) || nb remaining comb %s",
        iter2+1,
        paste(last_try,collapse=""),
        nb_red,
        nb_white,
        all_comb2 |> nrow()
      )
    )
    # a <- readline("Debug? (y/n): ")
    # if(a=="y"){
    #   browser()
    # }
  }
  
  # Update and exit function
  iter2 <<- iter2 + 1
  return(output)
}

# Write the name of your function here
solver <- solver_distance_based

# END ---------------------------------------------------------------------

