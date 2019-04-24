library(shiny)
library(mc2d)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ###################### Inputs #####################################################
  seed = 999
  Ftp90 <- reactive({as.numeric(input$Ft90)})
  Ftp10 <- reactive({as.numeric(input$Ft10)})
  
  Ltp90 <- reactive({as.numeric(input$Lt90)})
  Ltp10 <- reactive({as.numeric(input$Lt10)})
  
  Ngp90 <- reactive({input$nG[1]})
  Ngp10 <- reactive({input$nG[2]})
  
  n = reactive({input$Simu})
  
  ###################### Calculate the SD ###########################################
  Ftsd <- reactive({sd(Ftp90():Ftp10())})
  Ltsd <- reactive({sd(Ltp90():Ltp10())})
  ngsd <- reactive({sd(Ngp90():Ngp10())})
  
  ###################### Calculate the Mean ##########################################
  
  Ftmean <- reactive({mean(Ftp90():Ftp10())})
  Ltmean <- reactive({mean(Ltp90():Ltp10())})
  ngmean <- reactive({mean(Ngp90():Ngp10())})
  
  ###################### Generate the Distributions of the aspects ###################
  #Fault throw thaickness Distribution
  Ft <- reactive({if (input$FtDist =="Normal Distribution" ) 
  {
    
    mcstoc(rnorm, mean=Ftmean(), sd=Ftsd(), rtrunc=TRUE, linf=Ftp90(), lsup=Ftp10(), seed = seed, nsv= n() )}
    
    else if (input$FtDist == "Lognormal Distribution")
    {
      
      mcstoc(rlnorm, mean=Ftmean(), sd=Ftsd(), rtrunc=TRUE, linf=Ftp90(), lsup=Ftp10(), seed = seed, nsv= n())}
    
  })
  #Layer thaickness Distribution
  Lt <- reactive({if (input$LtDist =="Normal Distribution" ) 
  {
    
    mcstoc(rnorm, mean=Ltmean(), sd=Ltsd(), rtrunc=TRUE, linf=Ltp90(), lsup=Ltp10(), seed = seed, nsv= n() )}
    
    else if (input$LtDist == "Lognormal Distribution")
    {
      
      mcstoc(rlnorm, mean=Ltmean(), sd=Ltsd(), rtrunc=TRUE, linf=Ltp90(), lsup=Ltp10(), seed = seed, nsv= n())}
    
  })
  
  #NG Distribution
  #
  NG <- reactive({if (input$NgDist =="Normal Distribution" ) {
    
    mcstoc(rnorm, mean=ngmean(), sd=ngsd(), rtrunc=TRUE, linf=Ngp90(), lsup=Ngp10(), seed = seed, nsv= n())}
    
    else if (input$NgDist == "Lognormal Distribution") {
      mcstoc(rlnorm, mean=ngmean(), sd=ngsd(), rtrunc=TRUE, linf=Ngp90(), lsup=Ngp10(), seed = seed, nsv= n())}
    
  })
  ###########################################################################################
  SGR = reactive({ 
          (((100-NG())*Lt())/Ft())
  })
  #PSGRdata<- reactive({ data.frame(unmc(summary(round(SGR(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))})
  
  
  SSF = reactive({ 
    (Ft()/((100-NG())*Lt()))*100
  })
  #PSSFdata<- reactive({ data.frame(unmc(summary(round(SSF(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
  #})  
  ######################  Rendering Histograms  .###########################################
  
  ### Fault Throw panel----
  output$Ftpanel <- renderPlot({
    
    hist(Ft(), xlab="Fault throw (meters)", breaks=50, col="green", border = NA)
    
  })
  ### Layer thickness panel----
  output$Ltpanel <- renderPlot({
    
    hist(Lt(), xlab="Layer thickness (meters)", breaks=50, col="blue", border = NA)
    
  })
  ### NG panel----
  output$NGpanel <- renderPlot({
    
    hist(NG(), xlab="Net to Gross (%) ", breaks=50, col="purple", border = NA)})
  
  ### Method panel----
  output$SGRpanel <- renderPlot({
    if (input$select == "Shale Gouge Ratio (SGR) ") {
        
    hist(SGR(), xlab="SGR (%)", breaks=100, col="seagreen1") }
    else {hist(SSF(), xlab="SSF (%)", breaks=100, col="red") }
  })
  
  ### Method Interpretation tab panel----
  output$SGRpanel1 <- renderPlot({
      hist(SGR(), xlab="SGR (%)", breaks=100, col="seagreen1") 
  })
  output$SSFpanel2 <- renderPlot({
    hist(SSF(), xlab="SSF (%)", breaks=100, col="red")  
  })
  

  ### Method Results Print----
  output$SGRtxt <- renderPrint({ 
    PSGRdata<- data.frame(unmc(summary(round(SGR(), digits = 0), probs = c(0.01,0.1,0.50,0.9,0.99))))
    colnames(PSGRdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
    rownames(PSGRdata)[1] <- "SGR"
    
     if (input$select == "Shale Gouge Ratio (SGR) ") { PSGRdata[c(1:7)]
    #summary(round(SGR(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))
     }
    else {
    summary(round(SSF(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99)) }
    
    })
  ### Method Results Table----
  output$sgrTable <- renderDataTable({
    PSGRdata<- data.frame(unmc(summary(round(SGR(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
    colnames(PSGRdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
    rownames(PSGRdata)[1] <- "SGR"
    datatable(PSGRdata[3:7], options = list(pageLength = 1, dom = 'tip'))
  })
  
   output$ssfTable <- renderDataTable({
    PSSFdata<- data.frame(unmc(summary(round(SSF(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
    colnames(PSSFdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
    rownames(PSSFdata)[1] <- "SSF"
    datatable(PSSFdata[3:7], options = list(pageLength = 1, dom = 'tip' ))
   })
   
   output$result <- renderPrint({ 
     PSSFdata<- data.frame(unmc(summary(round(SSF(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
     colnames(PSSFdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
     rownames(PSSFdata)[1] <- "SSF"
     
     PSGRdata<- data.frame(unmc(summary(round(SGR(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
     colnames(PSGRdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
     rownames(PSGRdata)[1] <- "SGR"
     
     if (PSGRdata[3] > 20 && PSSFdata[3]<8){
       
       "based on the universal data correlation the fault is seal completely "
     } else {"fault is Leak"}
     
     })

})
