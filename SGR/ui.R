#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(
  
  # Application title
  titlePanel("Fault Seal Analysis  Webapp"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      helpText( h4("The Monte Carlo Simulation of Fault Seal Analysis Methods")),
      br(),
      wellPanel(
        
        selectInput("select", label = h3("Select The Proper Method"), 
                    choices = list("Shale Gouge Ratio (SGR) ", "Shale Smear Factor (SSF)"), 
                    selected = 1),
        
        sliderInput("Simu", "Numbers of Simulations:",  
                    min = 0, max = 10000, value = 5000)),
      
      verbatimTextOutput("SGRtxt"),tags$head(tags$style("#SGRtxt{color: blue;font-size: 15px;font-style: italic;}"))
      #,dataTableOutput("SGRtxt")
      
      ,helpText( h4("Histogram of Fault Seal Analysis Method"))
      ,tags$h3(textOutput("histext")),plotOutput("SGRpanel")
      
 
    ,br()
      
      ,downloadButton('export', "Download Report")
      
     
      ), #sidebarPanel
    
    ########### Show a plot of the generated distribution
    mainPanel(
      
   tabsetPanel(id = 'datainput',
     
               tabPanel( "Input data",
      wellPanel(style = "background-color: #ffffff;",
                
                #Rendering the Fault throw
                fluidRow(
        helpText( h4("Enter the Estimated Fault throw in meters below: ")),
        column(1,textInput("Ft90", label = h4("P90",tags$h6("(is small)")), value = "900")),
        
        column(1,textInput("Ft10", label = h4("P10",tags$h6("(is large)")), value = "1000"))
        ,column(7,plotOutput("Ftpanel", height = "200px"))
        ,column(3,selectInput("FtDist",
                              label = "Distribution Type:",
                              choices = c("Normal Distribution", "Lognormal Distribution"),
                              selected = "Normal Distribution"))
      )
            #Rendering the Layer thickness
      ,fluidRow(
        helpText( h4("Enter the Estimated Layer thickness in meters below: ")),
        column(1,textInput("Lt90", label = h4("P90",tags$h6("(is small)")), value = "100")),
        
        column(1,textInput("Lt10", label = h4("P10",tags$h6("(is large)")), value = "200"))
        ,column(7,plotOutput("Ltpanel", height = "200px"))
        ,column(3,selectInput("LtDist",
                              label = "Distribution Type:",
                              choices = c("Normal Distribution", "Lognormal Distribution"),
                              selected = "Normal Distribution"))
      ),
      
      
      fluidRow(
        helpText( h4("Enter the Net to Gross ratio below: ")),
        column(2,sliderInput("nG", "Net to Gross :",  
                             min = 1, max = 99, value = c(40, 55 )))
        ,column(7,plotOutput("NGpanel", height = "200px"))
        ,column(3,selectInput("NgDist",
                              label = "Distribution Type:",
                              choices = c("Normal Distribution", "Lognormal Distribution"),
                              selected = "Normal Distribution"))
          )#,
      
      
      # fluidRow(
      #   wellPanel(style = "background-color: #ffffff;",
      #   column(4, dataTableOutput("sgrTable"))
      #   ,column(2,"")
      #   ,column(4,dataTableOutput("ssfTable"), options = list(pageLength = 5, dom = 'tip'))
        
        )
 ),
 tabPanel( "Interpretation",
           
           column(6,plotOutput("SGRpanel1") )
           ,column(6,plotOutput("SSFpanel2") )
           ,fluidRow(
             dataTableOutput("sgrTable")),
           fluidRow(dataTableOutput("ssfTable")),
           
           verbatimTextOutput("result"),tags$head(tags$style("#result{color: blue;font-size: 35px;font-style: italic;}")))
 
)
    ) # MainPanel 
   
  ) #sidebarLayout
) #fluidPage
)#shinyUI
