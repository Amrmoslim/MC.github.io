
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(" Simple Production Profile Estimator"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Pi",
                     "Initial Production:",
                     min = 1,
                     max = 50000,
                     value = 1000),
         sliderInput("Ti",
                     "Time to produce:",
                     min = 1,
                     max = 50,
                     value = 10),
         sliderInput("Dr",
                     "Decline rate:",
                     min = 0.011,
                     max = 1,
                     value = 0.20)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         tableOutput("text"),
         plotOutput("cumPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  results <- reactive({
    
    A1 <- input$Pi
    B1 <- input$Ti-1
    df <- list()
  for (i in 1:input$Ti) {
    repeat{
    A1 <- A1 - (A1 * input$Dr)
    #df <- rbind(df,data.frame(i, round(A1,0)))
    df <- c(df,A1)
    results<- data.frame(df)
    break
  }
  }
    # B2 <- list(seq(1:input$Ti-1))
    # C1<- list(t(results()))
    # C2 <- data.frame(B2,C1)
    # names(C2)[] <- "Time"
    # names(C2)[2] <- "Production"
    # C2
    results
    #cum=results*365
  })
  
   output$distPlot <- renderPlot({
     A1 <- input$Pi
     B2 <- list(seq(1:input$Ti-1))
     C1 <- list(t(results()))
     C2 <- list (A1, 1)
     C2 <- data.frame(B2,C1)
     names(C2)[] <- "Time"
     names(C2)[2] <- "Production"

    plot(C2, type = "b")
     #results[2,] <- seq(1:input$Ti)
     #ggplot(results, aes(Ti, A1))+geom_line()
     })
   
   output$text <- renderTable({
     B2 <- list(seq(1:input$Ti-1))
     C1<- list(t(results()))
            C2 <- data.frame(B2,C1)
            names(C2)[] <- "Time"
            names(C2)[2] <- "Production"
            C2
            })
   
   # output$cumPlot <- renderPlot({
   #   A1 <- input$Pi
   #   B2 <- list(seq(1:input$Ti-1))
   #   C1 <- list(t(results()))
   #   C2 <- list (A1, 1)
   #   C2 <- data.frame(B2,C1*365)
   #   names(C2)[] <- "Time"
   #   names(C2)[2] <- "CumProduction"
   #   
   #   plot(Cum(), type = "b")
   #   #results[2,] <- seq(1:input$Ti)
   #   #ggplot(results, aes(Ti, A1))+geom_line()
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

