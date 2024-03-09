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
                  mainPanel(titlePanel("toto"),
                            plotOutput("my_plot")),
                  sidebarPanel(
                    uiOutput(style = "display: inline-block", "button1"),
                    uiOutput(style = "display: inline-block", "button2"),
                    uiOutput(style = "display: inline-block", "button3"),
                    uiOutput(style = "display: inline-block", "button4"),
                    actionButton("reset_button", "Submit")
                  )
                ))

# Server ------------------------------------------------------------------

server <- function(input, output) {
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
  observeEvent(input[["reset_button"]],
               {
                 # Update the data frame
                 df_data <-
                   rbind(df(), c(btn1(), btn2(), btn3(), btn4())) %>% mutate(`Try n°` = row_number())
                 df_data <- df_data
                 df(df_data)
                 
                 # Add Pageantry button on the main panel to show the combinations played
                 
                 # Print the data frame storing all the try
                 if (TRUE) { # TRUE : for debugging, FALSE : to have a clean app.
                   print(
                     df_data %>%
                       pivot_longer(1:4, names_to = "btnID", values_to = "colour") %>%
                       mutate(btnID = str_replace(btnID, "btn", "")),
                     colourDF = colour[colourDF %% length(colour) + 1]
                   )
                 }
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