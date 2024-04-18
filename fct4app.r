### ///////////////////////////////////////////////////////////////////////
# Script name : fct4app.R
# Script status :
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
    clickCount(clickCount() + 1)
  })
  
  # Build the button parameters
  button_colour <- reactive( clickCount2Colour(clickCount) ) # The warning is normal
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
# State : WIP
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

# btnPageantry ------------------------------------------------------------
# State : WIP
# Description :
# Input :
# Output :
### ///////////////////////////////////////////////////////////////////////

# buttons = reactiveVar(c(btn1(),btn2(),btn3(),btn4()))
#
# btnPageantry <- function(input,output,buttons){
#
#   for(btn in buttons){
#     button_style <- reactive(
#       paste0(
#         "color: white;
#           background-color: ",
#         btn(),
#         ";
#           position: relative;
#           left: 3%;
#           height: 35px;
#           width: 35px;
#           text-align:center;
#           text-indent: -2px;
#           border-radius: 50%;
#           border-width: 2px"
#       )
#     )
#
#     insertUI(
#       selector = "#my_buttons",
#     )
#
#     output[[btnName]] <-
#       renderUI({
#         actionButton(inputId = btnName,
#                      label = "",
#                      style = button_style())
#       })
#
#   }
#
#
#
#   return(NULL)
# }


# generateButton ----------------------------------------------------------
# State : WIP
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
          height: 35px;
          width: 35px;
          text-align:center;
          text-indent: -2px;
          border-radius: 50%;
          border-width: 2px"
    )
  
  actionButton(
    inputId = paste0("dynamic_button_", btn_name),
    label = "",
    style = button_style
    )
}

# END
