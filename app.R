# Description : 
# 
# mastermind colour :
# green : "#43caa7"
# blue : "#000bdf"
# red : "#df0000"
# violet : "#701abf"
# yellow :"#dfbe00"
# orange : "#ff7e31"
# white : "#ffffff"
# pink : "#df5abe"



# Libraries ---------------------------------------------------------------


library(shiny)
library(reactlog)
library(tidyverse)

# Options -----------------------------------------------------------------

options(shiny.reactlog = TRUE)

# Load functions ----------------------------------------------------------
btnCLickNDisplay <- function(input, output, colour, btnName){
  
  # Initialize button variables
  clickCount <- reactiveVal(0)
  
  # Update click variables
  observeEvent(input[[btnName]], {
    clickCount(clickCount() + 1)
  })
  
  # Build the button parameters
  button_colour <- reactive(colour[clickCount() %% 8 + 1])
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
  
  # Extract colour
  btnColourName = paste0(btnName,"Colour")
  output[[btnColourName]] = clickCount
  
  # Build output
  return( clickCount )
}

# buttons = reactiveVar(c(btn1(),btn2(),btn3(),btn4()))

btnPageantry <- function(input,output,buttons){
  
  for(btn in buttons){
    button_style <- reactive(
      paste0(
        "color: white;
          background-color: ",
        btn(),
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
    
    insertUI(
      selector = "#my_buttons",
    )
    
    output[[btnName]] <-
      renderUI({
        actionButton(inputId = btnName,
                     label = "",
                     style = button_style())
      })
    
  }
  
  
  
  return(NULL)
}

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("My great mastermind !!!"),
  sidebarLayout(
    mainPanel(
      titlePanel("toto"),
      plotOutput("my_plot")
    ),
    sidebarPanel(
      uiOutput(style = "display: inline-block", "button1"),
      uiOutput(style = "display: inline-block", "button2"),
      uiOutput(style = "display: inline-block", "button3"),
      uiOutput(style = "display: inline-block", "button4"),
      actionButton("reset_button", "Submit")
    )
  )
)

# Server ------------------------------------------------------------------


server <- function(input, output) {
  # Initialize global variables
  colour <-
    c(
      "#43caa7",
      "#000bdf",
      "#df0000",
      "#701abf",
      "#dfbe00",
      "#ff7e31",
      "#ffffff",
      "#df5abe"
    )
  
  # Build the button 
  btn1 <- btnCLickNDisplay(input, output, colour, btnName = "button1" )
  btn2 <- btnCLickNDisplay(input, output, colour, btnName = "button2" )
  btn3 <- btnCLickNDisplay(input, output, colour, btnName = "button3" )
  btn4 <- btnCLickNDisplay(input, output, colour, btnName = "button4" )
  
  
  # Reactive data frame
  df <- reactiveVal(tibble(btn1 = 0, btn2 = 0, btn3 = 0, btn4 = 0, `Try n°` = 0))
  observeEvent(input[["reset_button"]],
               {
                 # Update the data frame
                 df_data <- rbind(df(), c(btn1(), btn2(), btn3(), btn4())) %>% mutate(`Try n°` = row_number())
                 df_data <- df_data
                 df(df_data)
                 
                 # Add Pageantry button on the main pannel to show the combinaisons played
                 
                 # Print the data frame storing all the try
                 print( df_data %>%
                 pivot_longer(1:4,names_to = "btnID",values_to = "colour") %>%
                 mutate(
                   btnID = str_replace(btnID, "btn", "")
                   ),
                 colourDF = colour[colourDF %% 8 +1]
                 )
               }
               )
  
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