#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
  
  fluidPage(shinythemes::themeSelector(),
  # Application title
  titlePanel("Geological Risk Analysis  Webapp"),

  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      helpText( h4("The Monte Carlo Simulation of Geological Risk Analysis Methods")),
      br(),
      wellPanel(
        
        selectInput("select", label = h3("Select The Proper Method"), 
                    choices = list("Five Points Risking ", "Four Points Risking"), 
                    selected = 1),
        
        sliderInput("Simu", "Numbers of Simulations:",  
                    min = 0, max = 10000, value = 5000)),
      
      verbatimTextOutput("GCOS"),tags$head(tags$style("#GCOS{color: blue;font-size: 15px;font-style: italic;}"))
      
      ,helpText( h4("Histogram of Risking Analysis Method"))
      ,tags$h3(textOutput("histext")),plotOutput("SGRpanel")
      
 
    ,br()
      
      ,downloadButton('export', "Download Report")
      
     
      ), #sidebarPanel
    
    ########### Show a plot of the generated distribution
    mainPanel(
      wellPanel(
                
                #Rendering the Fault throw
                fluidRow(
                  helpText( h4("Please Rate The Closure below: ")),
                  column(4,sliderInput("Cl", "Closure Efficiency :",  
                                       min = 1, max = 99, value = c(40, 55 )))
                  ,column(5,plotOutput("Clpanel", height = "200px"))
                  ,column(3,selectInput("ClDist",
                                        label = "Distribution Type:",
                                        choices = c("Normal Distribution", "Lognormal Distribution"),
                                        selected = "Normal Distribution"))
                ),
                      fluidRow(
                  helpText( h4("Please Rate The Sealing Capacity below: ")),
                  column(4,sliderInput("Sel", "Sealing Capacity :",  
                                       min = 1, max = 99, value = c(40, 55 )))
                  ,column(5,plotOutput("Selpanel", height = "200px"))
                  ,column(3,selectInput("SelDist",
                                        label = "Distribution Type:",
                                        choices = c("Normal Distribution", "Lognormal Distribution"),
                                        selected = "Normal Distribution"))
                ),
                     fluidRow(
                  helpText( h4("Please Rate The Reservoir Presence and Quality below: ")),
                  column(4,sliderInput("Res", "Reservoir :",  
                                       min = 1, max = 99, value = c(40, 55 )))
                  ,column(5,plotOutput("Respanel", height = "200px"))
                  ,column(3,selectInput("ResDist",
                                        label = "Distribution Type:",
                                        choices = c("Normal Distribution", "Lognormal Distribution"),
                                        selected = "Normal Distribution"))
                ),
          fluidRow(
        helpText( h4("Please Rate The Source Maturation below: ")),
        column(4,sliderInput("SRC", "Source Maturation :",  
                             min = 1, max = 99, value = c(40, 55 )))
        ,column(5,plotOutput("SRCpanel", height = "200px"))
        ,column(3,selectInput("SRCDist",
                              label = "Distribution Type:",
                              choices = c("Normal Distribution", "Lognormal Distribution"),
                              selected = "Normal Distribution"))
      )
      ,fluidRow(
        helpText( h4("Please Rate The Migration Efficiency below: ")),
        column(4,sliderInput("MG", "Migration Efficiency :",  
                             min = 1, max = 99, value = c(40, 55 )))
        ,column(5,plotOutput("MGpanel", height = "200px"))
        ,column(3,selectInput("MGDist",
                              label = "Distribution Type:",
                              choices = c("Normal Distribution", "Lognormal Distribution"),
                              selected = "Normal Distribution"))
          )
 
 
    )  #well Panel
    ) # MainPanel 
   
  ) #sidebarLayout
) #fluidPage
)#shinyUI
