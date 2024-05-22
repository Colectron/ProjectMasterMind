### ///////////////////////////////////////////////////////////////////////
# Script name : fct4app.R
# Script status : WIP
# Script description : Build the functions used in the app.R. The objective is
#                      to have a short and clean app script.
### ///////////////////////////////////////////////////////////////////////

# btnClickNDisplay --------------------------------------------------------
# State : Done
# Description : Button used to submit a combination. Round button, each time you
#               click on a button it change its colour from the colour defined
#               in the global variable "colour".
# Input :
#         input : input object of the server
#         output : output object of the server
#         btnName : Name of the button (char string). To be used in the uiOutput.
# Output :
#         clickCount : Number of time this button has been clicked. Use
#                      colour[clickCount() %% n + 1] to get the actual colour of
#                      the button, where n is the number of possible colour from
#                      the global variable colour. The class of the output is a
#                      reactive value.
### ///////////////////////////////////////////////////////////////////////

btnCLickNDisplay <- function(input, output, btnName) {
  # Initialize button variables
  clickCount <- reactiveVal(0)
  
  # Update click variables
  observeEvent(input[[btnName]], {
    clickCount( clickCount() + 1 ) 
  })
  
  # Build the button parameters
  button_colour <- reactive( clickCount2Colour(clickCount) ) 
  button_style <- reactive(
    paste0(
      "color: white;
          background-color: ",
      button_colour(),
      ";
          position: relative;
          left: 3%;
          height: 35px;
          width: 35px;
          text-align:center;
          text-indent: -2px;
          border-radius: 50%;
          border-width: 2px"
    )
  )
  
  # Display the buttons
  output[[btnName]] <-
    renderUI({
      actionButton(inputId = btnName,
                   label = "",
                   style = button_style())
    })
  
  # Build output
  return(clickCount)
}


# clickCount2Colour -------------------------------------------------------
# State : WIP -> add a description to the function
# Description : Transform a number to a colour according to its position in the
#               global variable colour.
# Input :
#        clickCount : reactive variable, must be numeric
# Output :
#         .colour : character string
### ///////////////////////////////////////////////////////////////////////

clickCount2Colour <- function(clickCount){
  .colour = colour[clickCount() %% length(colour) + 1]
  return(.colour)
}

# generateButton ----------------------------------------------------------
# State : WIP -> add a description to the function
# Description : Function to generate a new button with the specified label
# Input :
#        label : character, label of the button
# Output :
### ///////////////////////////////////////////////////////////////////////
generateButton <- function(btn_name,clr) {
  button_style <- paste0(
      "color: white;
          background-color: ",
      colour[unlist(clr) + 1],
      ";
          position: relative;
          left: 3%;
          height: 25px;
          width: 25px;
          text-align:center;
          text-indent: -2px;
          border-radius: 50%;
          border-width: 2px"
    )
  
  output = actionButton(
    inputId = paste0("dynamic_button_", btn_name),
    label = "",
    style = button_style
    )
  return(output)
}


# redNWhite ---------------------------------------------------------------
# State : DONE
# Description : Function to compute the number of Red & white pawns to return 
#               giving a try and a secret sequence.
# Input :
#        current_try : numerical vector
#        to_find : numerical vector of the same length as current_try
# Output :
### ///////////////////////////////////////////////////////////////////////
redNWhite <- function(current_try,to_find){
  # browser()
  nb_red = 0
  nb_white = 0
  
  for (i in unique(to_find) ) {
    nb_red_found = sum(( which(to_find == i) %in% which(current_try == i) ))
    nb_red = nb_red + nb_red_found
    if ( length(which(to_find == i)) > nb_red_found ) {
      nb_white = nb_white + 
        (
          min( length(which(current_try == i)) , length(which(to_find == i)) ) - 
            nb_red_found
        )
    }
  }
  
  output = tibble(nb_red = nb_red , nb_white = nb_white )
  return(output)
}


# hintButton --------------------------------------------------------------
# State : WIP -> add a description to the function
# Description : Function to compute the number of Red & white pawns to return 
#               giving a try and a secret sequence.
# Input :
# Output :
### ///////////////////////////////////////////////////////////////////////
hintButton <- function(hints_info) {
  # Initiate variables
  btn_fct <- function(x,clr){
    btn_style = paste0(
      "color: black;
          background-color:", clr,";
          position: relative;
          left: 3%;
          height: 1px;
          width: 1px;
          text-align:center;
          text-indent: 0px;
          border-width: 2px"
    )
    
    output = actionButton(
        inputId = paste0("dynamic_button_", "hint_red", counter),
        label = "",
        style = btn_style
      )
    
    counter <<- counter + 1
    
    return(output)
  }
  
  counter = 1
  # Build lists
  red_buttons <-
    lapply(
      vector(mode = "list", length = hints_info$nb_red),
      function(x){btn_fct(x,"red")}
           )     
  white_buttons <-
    lapply(
      vector(mode = "list", length = hints_info$nb_white),
      function(x){btn_fct(x,"white")}
    )  
      
  # Merge lists in output
  output = c(red_buttons,white_buttons)
  
  if (length(output) < 4) {
    for (i in (length(output) + 1):4) {
      output[[i]] = list()
    }
  }
  return(output)
}

# oneRun ------------------------------------------------------------------
# State : WIP
# Description : Run one simulation of a game giving a secret combination and 
#               a solver function. 
#               TO DO : describe the specifications of the solver function
# Input :
#        scrt_cmb : secret combination to find numerical vector of length 4
#        slvr : solver R function object. It has as input a game history and
#               gives as an output a new combination.
# Output :
### ///////////////////////////////////////////////////////////////////////

oneRun <- function(scrt_cmb,slvr_f_p){
  browser()
  scrt_cmb = unlist(scrt_cmb) # force the type of scrt_cmb to be a simple vector
  
  # Source the function
  source(slvr_f_p)
  
  # initiate game
  game <- list(
    n_clr = length(colour), # total number of colour
    history = tibble(
      pawn1 = -1,
      pawn2 = -1,
      pawn3 = -1,
      pawn4 = -1,
      nb_red = -1, # win if = 4
      nb_white = -1
    )
  )
  
  # Loop till we find the correct combination
  runing_time <- 0
  nb_try <- 1
  while (game$history$nb_red[nb_try] != 4) {
    # Compute solution
    start_time <- Sys.time()  
    current_try = as.vector(solver(game$history))
    end_time <- Sys.time()
    runing_time <- runing_time + end_time - start_time
    
    current_line = c(current_try,
                     redNWhite(
                       current_try = current_try,
                       to_find = scrt_cmb)
    ) %>% unlist
    game$history = rbind(game$history,current_line)
    nb_try = nb_try + 1
    
    ### DEBUG
    if (FALSE) {
      print("------------------")
      print(sprintf("Secret combination %s", paste(scrt_cmb,collapse = "")))
      print(sprintf("N° try %s", nb_try))
      print(sprintf("Comb tried %s", paste(current_try,collapse = "")))
    }
  }
  
  return(tibble(`Nb try` = nb_try,runing_time = runing_time))
}


# histPerfSolver ----------------------------------------------------------
# State : WIP
# Description : 
# Input :
# Output :
### ///////////////////////////////////////////////////////////////////////

histPerfSolver <- function(data, column, x_lab = column, title_precision = NULL, facet = FALSE){
  
  num_2_chr <- function(x){
    output <- format(x,digits = 2,nsmall = 1) %>% 
    return(output)
  }
  
  p <- data %>% 
    ggplot(aes(x = .data[[column]])) +
    geom_histogram(color = "#000000", fill = "#0099F8") +
    theme_minimal() + 
    theme(
      plot.title = element_text(color = "#0099F8", size = 16, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "bold"),
      plot.caption = element_text(face = "italic")
    ) +
    labs(
      title = paste0("Histogram of",title_precision),
      subtitle = "WIP : here is the name of the solver function and the number of games",
      x = x_lab,
      y = "Count"
      ) 
  
    if (facet) {
      # Build title facets
      title_facets <-  paste("Number of differents colour :",1:4)
      names(title_facets) <- as.character(1:4)
    
      # Build summary statistics
      stats_facets <- data %>% group_by(scrt_cmb_nb_clr) %>%
        summarize(
          min = num_2_chr(min(get(column))),
          max = num_2_chr(max(get(column))),
          mean = num_2_chr(mean(get(column))),
          median = num_2_chr(median(get(column))),
          ) %>% 
        mutate(stats = sprintf("min : %s\nmax : %s\nmean : %s\nmedian : %s\n",min,max,mean,median))
      
      p <- p + facet_wrap(
        ~ scrt_cmb_nb_clr,
        labeller = labeller(scrt_cmb_nb_clr = title_facets)
      ) +
        geom_text( data = stats_facets,
                   hjust = 1, vjust = 1,size = 3,
          aes(x = Inf,
              y = Inf,
              label = stats,
              color = NULL,
              group = NULL)
        )
        
    } else {
      stats <- data %>%
        summarize(
          min = num_2_chr(min(get(column))),
          max = num_2_chr(max(get(column))),
          mean = num_2_chr(mean(get(column))),
          median = num_2_chr(median(get(column))),
        ) %>% 
        mutate(stats = sprintf("min : %s\nmax : %s\nmean : %s\nmedian : %s\n",min,max,mean,median))
      
      p <- p + 
        geom_text( data = stats,
                   hjust = 1, vjust = 1,size = 5,
                   aes(x = Inf,
                       y = Inf,
                       label = stats,
                       color = NULL,
                       group = NULL)
        )
      
    }
  
  return(p)
}


# END

