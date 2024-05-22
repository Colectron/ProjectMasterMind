### ///////////////////////////////////////////////////////////////////////
# Script name : server.R
# Script status : WIP
# Script description : 
### ///////////////////////////////////////////////////////////////////////

server <- function(input, output, session) {
  
  # Play tab ----------------------------------------------------------------
  # Secret combination buttons
  btn5 <- generateButton(btn_name = "s_comb_1",clr = secret_combination[1])
  btn6 <- generateButton(btn_name = "s_comb_2",clr = secret_combination[2])
  btn7 <- generateButton(btn_name = "s_comb_3",clr = secret_combination[3])
  btn8 <- generateButton(btn_name = "s_comb_4",clr = secret_combination[4])
  
  output$secret_combination <- renderUI({
    tagList(btn5,btn6,btn7,btn8)
  })
  
  # Playable buttons to submit
  btn1 <- btnCLickNDisplay(input, output, btnName = "button1")
  btn2 <- btnCLickNDisplay(input, output, btnName = "button2")
  btn3 <- btnCLickNDisplay(input, output, btnName = "button3")
  btn4 <- btnCLickNDisplay(input, output, btnName = "button4")
  
  # Show the possible colours and the order
  output$show_paragraph <- renderUI({
    paragraph <- "List and order of all possible colour"
    tags$p(paragraph)
  })
  output$show_colour <- renderUI({
    # Create the list of buttons
    btn_show_colour <- lapply(
      c(0:(length(colour) - 1)),
      function(x){
        btn_tmp = generateButton(btn_name = paste0("possible_colour",x),clr = x)
        fluidRow(column(width = 1,offset = 6, btn_tmp))
      } 
    )
    do.call(tagList, btn_show_colour)
  })
  
  # Reactive data frame
  # df_data <-
  #   tibble(
  #     btn1 = 0,
  #     btn2 = 0,
  #     btn3 = 0,
  #     btn4 = 0,
  #     `Try n°` = 1,
  #     nb_red = 0,
  #     nb_white = 0
  #   )
  submit_clicks <- reactiveVal(0)
  buttons <- reactiveVal(list())
  
  # What happened when you click on submit
  observeEvent(input[["Submit_button"]],
               {
                 # Update number of clicks of the submit button
                 submit_clicks(submit_clicks() + 1)
                 
                 # Update the data frame 
                 #        Create the new line
                 new_line = tibble(btn1 = btn1() %% 8, 
                                   btn2 = btn2() %% 8, 
                                   btn3 = btn3() %% 8, 
                                   btn4 = btn4() %% 8, 
                                   `Try n°` = submit_clicks()) %>% 
                   mutate(
                     redNWhite(
                       current_try = c(btn1,btn2,btn3,btn4),
                       to_find = secret_combination
                     )
                   )
                 
                 #        Add the new line to the current data frame
                 # df_data <<- rbind(df_data, new_line)
                 # df_data <- df_data
                 
                 # Generate played buttons
                 played_buttons <- lapply(1:4, function(i) {
                   generateButton(
                     btn_name = (submit_clicks() - 1) * 4 + i , 
                     clr = new_line[,i] ) 
                 })
                 
                 # Generate hint buttons
                 hint_buttons <- hintButton(
                   hints_info = new_line[,c("nb_red","nb_white")]
                 )
                 
                 # Add the new buttons to the list of buttons we just created
                 buttons_list <- buttons()
                 buttons_list[[submit_clicks()]] <- list(
                   played = played_buttons,
                   hint = hint_buttons
                 )
                 buttons(buttons_list)
                 
               })
  
  # Render the dynamic_rows to show combination played
  output$dynamic_rows <- renderUI({
    # Get the list of buttons
    buttons_list <- buttons()
    
    # Create a list to store button elements
    button_elements <- lapply(buttons_list, function(buttons_row) {
      fluidRow(
        # Combination just played
        column(width = 1, buttons_row$played[[1]]),
        column(width = 1, buttons_row$played[[2]]),
        column(width = 1, buttons_row$played[[3]]),
        column(width = 1, buttons_row$played[[4]]),
        # All the hints
        column(
          width = 1, 
          buttons_row$hint[[1]],
          buttons_row$hint[[2]]
        ),
        column(
          width = 1,
          buttons_row$hint[[3]],
          buttons_row$hint[[4]]
        )
      )
    })
    
    # Convert the list of button elements to a tagList
    do.call(tagList, button_elements)
  })
  
  # Try your solver tab -----------------------------------------------------
  
  # Read the solver function ----
  # Read the content of the uploaded file and store it as a reactive value
  observeEvent(input$source_btn, {
    solver_file_path <<- input$file$datapath
    source_result <- tryCatch({
      source(solver_file_path)
      "File sourced successfully."
      solver_file_status <- TRUE
    }, error = function(e) {
      paste("Error:", e$message)
    })
    output$source_status <- renderText({
      source_result
    })
  })
  
  # Parameter of the simulation ----
  # Number of replicates if the simulation is random
  output$value <- renderPrint({ input$radio })
  output$nb_rep <- renderUI({
    req(input$random_simu)
    if (input$random_simu == "Yes") {
      sliderInput("nb_rep", label = "Number of replicates", min = 1, 
                  max = 10, value = 5)
    } else if (input$random_simu == "No") {
      NULL
    }
  })
  
  # Total number of simulation to run
  tot_number_simu <- reactive({
    # get the number of replicates 
    req(input$random_simu)
    if (input$random_simu == "Yes") {
      n_rep <- input$nb_rep
    } else if (input$random_simu == "No") {
      n_rep <- 1
    }
    n_simu <<- n_rep*input$nb_games
    output <- sprintf("Total number of simulation ready to run : %s", n_simu)
    return(output)
    })
  
  output$tot_simu <- renderText({
    tot_number_simu()
    })
  
  # Runing the simulation ----
  w <- Waiter$new(id = "run_simu",html = span("Initialising"))
  # Clickin the run simulation button ----
  observeEvent(input[["run_simu"]], {
    # browser()
    w$show()
    
    # Initialize simu data set
    n <- n_simu
    simu <- crossing(sqr1 = 1:8, sqr2 = 1:8, sqr3 = 1:8, sqr4 = 1:8) %>%
      mutate(`Try N°` = row_number()) %>% 
      group_nest(`Try N°`)

    simu <- simu[ sample(1:dim(simu)[1],n,replace=FALSE) ,] %>% # Uncomment for full code
      mutate(`Try N°` = row_number())
    
    # Add a progression bar to the simulation :
    pb <- progress_estimated(nrow(simu), 0)
    
    # Compute simulation data set
    simu <- simu %>%
      mutate(index = row_number()) %>% # used in the waiter shiny element
      mutate(
        resu = map2(
          .x = data,
          .y = index,
          .f = function(x , y ,slvr_f_p = solver_file_path, .pb = NULL, n) {
            w$update(html = sprintf("Runing : %s / %s", y, n) ) # used in the waiter shiny element
            if (.pb$i < .pb$n) .pb$tick()$print() # progress bar of the R console 
            output = oneRun(scrt_cmb = x, slvr_f_p = slvr_f_p) 
            return(output)
          },
          # function(x){ 
          # oneRun(scrt_cmb = x, slvr_f_p = solver_file_path)
          # },
          .pb = pb,
          n = n()
          # .progress = TRUE
        ),
        scrt_cmb_nb_clr = map(data,function(x){ length(unique(unlist(x))) })
      ) %>% 
      select(-index) %>% 
      unnest(c(resu,scrt_cmb_nb_clr)) %>% 
      mutate(runing_time = as.numeric(runing_time))
    
    w$update(html = "Generating report")   
    
    # Gives statistics to the user ----
    output$hist_nb_try <- renderPlot({
      histPerfSolver(simu,
                     column = "Nb try",
                     title_precision = "number of tries per game")
    })
    output$hist_nb_try_facet <- renderPlot({
      histPerfSolver(simu,column = "Nb try",
                     facet = TRUE,
                     title_precision = "number of tries per game \nper number of different colour in the secret combination")
    })
    output$hist_time <- renderPlot({
      histPerfSolver(simu,
                     column = "runing_time",
                     title_precision = "number of runing time per game")
    })
    output$hist_time_facet <- renderPlot({
      histPerfSolver(simu,
                     column = "runing_time",
                     facet = TRUE,
                     title_precision = "number of runing time per game \nper number of different colour in the secret combination")
    })
    
    w$hide()
  }
  )
}

# end
