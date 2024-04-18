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
                  mainPanel(titlePanel("Combination played"),
                            uiOutput("button_panel")
                            ),
                  sidebarPanel(
                    titlePanel("Your next move"),
                    uiOutput(style = "display: inline-block", "button1"),
                    uiOutput(style = "display: inline-block", "button2"),
                    uiOutput(style = "display: inline-block", "button3"),
                    uiOutput(style = "display: inline-block", "button4"),
                    uiOutput("secret_combination"),
                    actionButton("Submit_button", "submit")
                  )
                ))

# Server ------------------------------------------------------------------

server <- function(input, output) {
  # Show the secret combination
  btn5 <- generateButton(btn_name = "s_comb_1",clr = secret_combination[1])
  btn6 <- generateButton(btn_name = "s_comb_2",clr = secret_combination[2])
  btn7 <- generateButton(btn_name = "s_comb_3",clr = secret_combination[3])
  btn8 <- generateButton(btn_name = "s_comb_4",clr = secret_combination[4])

  print(secret_combination)
  output$secret_combination <- renderUI({
    tagList(btn5,btn6,btn7,btn8)
  })
  
  # Build the button
  btn1 <- btnCLickNDisplay(input, output, btnName = "button1")
  btn2 <- btnCLickNDisplay(input, output, btnName = "button2")
  btn3 <- btnCLickNDisplay(input, output, btnName = "button3")
  btn4 <- btnCLickNDisplay(input, output, btnName = "button4")
  
  # Reactive data frame
  df <-
    reactiveVal(tibble(
      btn1 = 0,
      btn2 = 0,
      btn3 = 0,
      btn4 = 0,
      `Try n°` = 0
    ))
  submit_clicks <- reactiveVal(0)
  buttons <- reactiveVal(list())
  
  # What happened when you click on submit
  observeEvent(input[["Submit_button"]],
               {
                 submit_clicks(submit_clicks() + 1)
                 # Update the data frame
                 df_data <-
                   rbind(df(), c(btn1(), btn2(), btn3(), btn4(), -1 )) %>% mutate(`Try n°` = row_number())
                 df_data <- df_data
                 df(df_data)
                 
                 # Generate four new buttons with sequential labels
                 new_buttons <- lapply(1:4, function(i) {
                   generateButton(btn_name = (submit_clicks() - 1 * 4 + i) , clr = df_data[submit_clicks() + 1,i] )
                 })
                 
                 # Add the new buttons to the list of buttons
                 buttons_list <- buttons()
                 buttons_list[[submit_clicks()]] <- new_buttons
                 buttons(buttons_list)
                 
                 # Print the data frame storing all the try
                 # if (TRUE) { # TRUE : for debugging, FALSE : to have a clean app.
                 #   print(
                 #     df_data %>%
                 #       pivot_longer(1:4, names_to = "btnID", values_to = "colour") %>%
                 #       mutate(btnID = str_replace(btnID, "btn", "")),
                 #     colourDF = colour[colourDF %% length(colour) + 1]
                 #   )
                 # }
               })
  
  # Render the dynamic buttons
  output$button_panel <- renderUI({
    # Get the list of buttons
    buttons_list <- buttons()
    
    # Create a list to store button elements
    button_elements <- lapply(buttons_list, function(buttons_row) {
      fluidRow(
        column(width = 1, buttons_row[[1]]),
        column(width = 1, buttons_row[[2]]),
        column(width = 1, buttons_row[[3]]),
        column(width = 1, buttons_row[[4]])
      )
    })
    
    # Convert the list of button elements to a tagList
    do.call(tagList, button_elements)
  })
  
  # Plot the game
  # output$my_plot <- renderPlot({
  # df() %>%
  #     pivot_longer(1:4,names_to = "btnID",values_to = "colourDF") %>%
  #     mutate(
  #       btnID = str_replace(btnID, "btn", ""),
  #       colourDF = colour[colourDF %% 8 +1]
  #       ) %>%
  #     ggplot(aes(btnID,`Try n°`,colour = colourDF)) +
  #     geom_point(size = 20) +  # /(nrow(df())^1)
  #     scale_colour_identity()
  # })
  
}

shinyApp(ui, server)

# END