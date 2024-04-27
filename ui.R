### ///////////////////////////////////////////////////////////////////////
# Script name : ui.R
# Script status : WIP
# Script description : Build the functions used in the app.R. The objective is
#                      to have a short and clean app script.
### ///////////////////////////////////////////////////////////////////////

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
             # Simulation parameters
             fluidRow(
               column(width = 3,
                      wellPanel(
                        p("WIP : Describe the solver function spec"),
                        fileInput("file", "Choose a file"),
                        actionButton("source_btn", "Source File"),
                        textOutput("source_status")
                      ),
                      wellPanel(
                        radioButtons("random_simu", label = "Is your solver random?",
                                     choices = list("Yes", "No"), 
                                     selected = 1),
                        uiOutput("nb_rep")
                      ),
                      wellPanel(
                        sliderInput("nb_games", label = "Number of games to simulate", min = 1, 
                                    max = 8^4, value = 50)
                      )
               ),
               # Render results of the simulation
               column( width = 9,
                       actionButton("run_simu","Launch Simulation"),
                       textOutput("tot_simu"),
                       h1("Number of try summary"),
                       plotOutput("hist_nb_try"),
                       plotOutput("hist_nb_try_farcet"),
                       h2("Runing time summary"),
                       plotOutput("hist_time"),
                       plotOutput("hist_time_facet")
               )
             )
    )
  )
)

# end