
library('shiny')
library('shinydashboard')

################################################################################
##############################  User Interface   ###############################
################################################################################

# Create an empty header
header <- dashboardHeader(title = "Stochastic Processes")
# Create an empty sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'Dashboard',
             tabName = 'dashboard'
    )
  ),
  sliderInput(
    inputId = "nsteps",
    label = "n-steps",
    min = 1,
    max = 10000,
    value = 100,
    animate=animationOptions(10)),
  
  sliderInput(
    inputId = "nrep",
    label = "n-rep",
    min = 2,
    max = 1000,
    value = 100),
  
  sliderInput(
    inputId = "a",
    label = "Probability (right)",
    min = 0,
    max = 1,
    value = 0.5),
  
  # action buttom
  #actionButton("click", "Update")
)
# Create an empty body
body <- dashboardBody(
  fluidRow(
    #column 3
    valueBox(
      width = 12,
      value = "Simple Random Walk",
      subtitle = "UANL", 
      icon = icon("fire")
    ), 
  ),
  
  fluidRow(
    # Row 2
    box(title = "Simple Random Walk",status = "primary", solidHeader = TRUE,
        plotOutput('RandomWalk_trayectories',height = 350),width=7,height = "10%"),
    
    box(title = "Distribution at time t",status = "primary", solidHeader = TRUE,
        plotOutput('RandomWalk_distribution',height = 350),width=5,height = "10%"),
    
  ),
  fluidRow(
    box(title = "2 Dimensional Random Walk",status = "primary",solidHeader= TRUE,
        plotOutput("RandomWalk_2d",height = 350), width=7),
    box(title = "3 Dimensional Random Walk",status = "primary",solidHeader= TRUE,
        plotlyOutput("RandomWalk_3d",height = 350), width=5),
  ),
  
)


ui <- dashboardPage(skin = "blue",header,sidebar,body)
