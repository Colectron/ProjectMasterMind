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

ui <- fluidPage(
  titlePanel("MasterMind Algorithm testing"),
  tabsetPanel(
    
# Play panel --------------------------------------------------------------
    tabPanel("Play",
                sidebarLayout(
                  sidebarPanel(
                    titlePanel("Combination played"),
                    uiOutput("dynamic_rows")
                  ),
                  mainPanel(
                    titlePanel("Your next move"),
                    # Four button to play with
                    uiOutput(style = "display: inline-block", "button1"),
                    uiOutput(style = "display: inline-block", "button2"),
                    uiOutput(style = "display: inline-block", "button3"),
                    uiOutput(style = "display: inline-block", "button4"),
                    # uiOutput("secret_combination"), # uncomment to show 
                    actionButton("Submit_button", "submit"),
                    # Recap of all the possible colours
                    column(3, offset = 5, align = "center", uiOutput("show_paragraph")),
                    uiOutput("show_colour")
                  )
                )
             ),

# Solver Panel ------------------------------------------------------------
    tabPanel("Try your solver",
             useWaiter(),
             fluidRow(
               column(width = 3,
                      wellPanel(
                        p("WIP : Describe the solver function spec"),
                        fileInput("file", "Choose a file"),
                        actionButton("source_btn", "Source File"),
                        textOutput("source_status")
                      ),
                      wellPanel(
                        p("WIP : Describe the parameters function"),
                        # Copy the chunk below to make a group of checkboxes
                        checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                           selected = 1),
                        hr(),
                        fluidRow(column(3, verbatimTextOutput("value")))
                      )                        
                      ),
               column( width = 9,
                       actionButton("runSimu","Launch Simulation"),
                       p("WIP : Number of try summary"),
                       plotOutput("hist_nb_try"),
                       plotOutput("hist_nb_try_facet"),
                       p("WIP : Runing time summary"),
                       plotOutput("hist_time"),
                       plotOutput("hist_time_facet")
                      )
               )
             )
    )
  )

# Server ------------------------------------------------------------------

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
    }, error = function(e) {
      paste("Error:", e$message)
    })
    output$source_status <- renderText({
      source_result
    })
  })

  # Parameter of the simulation ----
  
  
  w <- Waiter$new(id = "runSimu",html = span("Initialising"))
  # Clickin the run simulation button ----
  observeEvent(input[["runSimu"]], {
    # browser()
    w$show()
    
    # Initialize simu data set
    n <- 100
    simu <- crossing(sqr1 = 1:8, sqr2 = 1:8, sqr3 = 1:8, sqr4 = 1:8) %>%
      mutate(`Try N°` = row_number()) %>% 
      group_nest(`Try N°`)
    
    # simu <- simu[ sample(1:dim(simu)[1],n,replace=FALSE) ,] %>% # Uncomment for full code
    #   mutate(`Try N°` = row_number())
    
    # DEBUG
    simu <- simu[ 1:n,] %>%
      mutate(`Try N°` = row_number())
    
    # END DEBUG
    
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

shinyApp(ui, server)

# END