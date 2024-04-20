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

# Options -----------------------------------------------------------------

options(shiny.reactlog = TRUE)

# Load functions ----------------------------------------------------------

source("fct4app.r")

# Load global variables ---------------------------------------------------

source("globalVar.R")

# UI ----------------------------------------------------------------------

ui <- fluidPage(titlePanel("My great mastermind !!!"),
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
                ))
#button_panel
# Server ------------------------------------------------------------------

server <- function(input, output) {
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
    paragraph <- "List and order of all the possible colour"
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
  #     nbRed = 0,
  #     nbWhite = 0
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
                       currentTry = c(btn1,btn2,btn3,btn4),
                       toFind = secret_combination
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
                   hints_info = new_line[,c("nbRed","nbWhite")]
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
}

shinyApp(ui, server)

# END