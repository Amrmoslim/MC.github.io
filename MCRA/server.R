library(shiny)
library(mc2d)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ###################### Inputs #####################################################
  seed = 999

  Clp90 <- reactive({input$Cl[1]})
  Clp10 <- reactive({input$Cl[2]})
  
  Selp90 <- reactive({input$Sel[1]})
  Selp10 <- reactive({input$Sel[2]})
  
  Resp90 <- reactive({input$Res[1]})
  Resp10 <- reactive({input$Res[2]})
  
  SRCp90 <- reactive({input$SRC[1]})
  SRCp10 <- reactive({input$SRC[2]})
  
  MGp90 <- reactive({input$MG[1]})
  MGp10 <- reactive({input$MG[2]})
  
  n = reactive({input$Simu})
  
  ###################### Calculate the SD ###########################################
  Clsd <- reactive({sd(Clp90():Clp10())})
  Selsd <- reactive({sd(Selp90():Selp10())})
  Ressd <- reactive({sd(Resp90():Resp10())})
  SRCsd <- reactive({sd(SRCp90():SRCp10())})
  MGsd <- reactive({sd(MGp90():MGp10())})
  
  ###################### Calculate the Mean ##########################################
  
  Clmean <- reactive({mean(Clp90():Clp10())})
  Selmean <- reactive({mean(Selp90():Selp10())})
  Resmean <- reactive({mean(Resp90():Resp10())})
  SRCmean <- reactive({mean(SRCp90():SRCp10())})
  MGmean <- reactive({mean(MGp90():MGp10())})
  
  ###################### Generate the Distributions of the aspects ###################
  #Closure Distribution
  Cl <- reactive({if (input$ClDist =="Normal Distribution" ) 
  {
    
    mcstoc(rnorm, mean=Clmean(), sd=Clsd(), rtrunc=TRUE, linf=Clp90(), lsup=Clp10(), seed = seed, nsv= n() )}
    
    else if (input$ClDist == "Lognormal Distribution")
    {
      
      mcstoc(rlnorm, mean=Clmean(), sd=Clsd(), rtrunc=TRUE, linf=Clp90(), lsup=Clp10(), seed = seed, nsv= n())}
    
  })
  #Sealing Capacity Distribution
  Sel <- reactive({if (input$SelDist =="Normal Distribution" ) 
  {
    
    mcstoc(rnorm, mean=Selmean(), sd=Selsd(), rtrunc=TRUE, linf=Selp90(), lsup=Selp10(), seed = seed, nsv= n() )}
    
    else if (input$SelDist == "Lognormal Distribution")
    {
      
      mcstoc(rlnorm, mean=Selmean(), sd=Selsd(), rtrunc=TRUE, linf=Selp90(), lsup=Selp10(), seed = seed, nsv= n())}
    
  })
  
  #Reservoir Distribution
  #
  Res <- reactive({if (input$ResDist =="Normal Distribution" ) {
    
    mcstoc(rnorm, mean=Resmean(), sd=Ressd(), rtrunc=TRUE, linf=Resp90(), lsup=Resp10(), seed = seed, nsv= n())}
    
    else if (input$ResDist == "Lognormal Distribution") {
      mcstoc(rlnorm, mean=Resmean(), sd=Ressd(), rtrunc=TRUE, linf=Resp90(), lsup=Resp10(), seed = seed, nsv= n())}
    
  })
  #Source Distribution
  #
  SRC <- reactive({if (input$SRCDist =="Normal Distribution" ) {
    
    mcstoc(rnorm, mean=SRCmean(), sd=SRCsd(), rtrunc=TRUE, linf=SRCp90(), lsup=SRCp10(), seed = seed, nsv= n())}
    
    else if (input$SRCDist == "Lognormal Distribution") {
      mcstoc(rlnorm, mean=SRCmean(), sd=SRCsd(), rtrunc=TRUE, linf=SRCp90(), lsup=SRCp10(), seed = seed, nsv= n())}
    
  })
  #Migration Distribution
  #
  MG <- reactive({if (input$MGDist =="Normal Distribution" ) {
    
    mcstoc(rnorm, mean=MGmean(), sd=MGsd(), rtrunc=TRUE, linf=MGp90(), lsup=MGp10(), seed = seed, nsv= n())}
    
    else if (input$MGDist == "Lognormal Distribution") {
      mcstoc(rlnorm, mean=MGmean(), sd=MGsd(), rtrunc=TRUE, linf=MGp90(), lsup=MGp10(), seed = seed, nsv= n())}
    
  })
  ###########################################################################################
  RA = reactive({ 
         (Cl()/100*Sel()/100*Res()/100*SRC()/100*MG()/100)*100
  })
  #PSGRdata<- reactive({ data.frame(unmc(summary(round(SGR(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))})
  
  
  RA4 = reactive({
             (Cl()/100*Res()/100*SRC()/100*MG()/100)*100
  })
  PSSFdata<- reactive({ data.frame(unmc(summary(round(SSF(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
  })
  ######################  Rendering Histograms  .###########################################
  
  ### Closure panel----
  output$Clpanel <- renderPlot({
    
    hist(Cl(), xlab="Closure %", breaks=50, col="orange", border = NA)
    
  })
  ### Seal Capacity panel----
  output$Selpanel <- renderPlot({
    
    hist(Sel(), xlab="Sealing %", breaks=50, col="blue", border = NA)
    
  })
  ### Reservoir panel----
  output$Respanel <- renderPlot({
    
    hist(Res(), xlab="Reservoir % ", breaks=50, col="purple", border = NA)})
 
   ### Source panel----
  output$SRCpanel <- renderPlot({
    
    hist(SRC(), xlab="Source % ", breaks=50, col="green", border = NA)})
  
  ### Migration panel----
    output$MGpanel <- renderPlot({
    
    hist(MG(), xlab="Source % ", breaks=50, col="dark green", border = NA)})
  
  ### Method panel----
  output$SGRpanel <- renderPlot({
    if (input$select == "Five Points Risking ") {
        
    hist(RA(), xlab="5 points Risking (%)", breaks=100, col="seagreen1") }
    else {hist(RA4(), xlab="4 points Risking (%)", breaks=100, col="red") }
  })
  
  

  ### Method Results Print----
  output$GCOS <- renderPrint({ 
    GCOSdata<- data.frame(unmc(summary(round(RA(), digits = 0), probs = c(0.01,0.1,0.50,0.9,0.99))))
    colnames(GCOSdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
    rownames(GCOSdata)[1] <- "GCOS"
    
     if (input$select == "Five Points Risking ") { GCOSdata[c(1:7)]
     }
    else {
    summary(round(RA4(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99)) }
    
    })
  ### Method Results Table----
  output$RATable <- renderDataTable({
    RAdata<- data.frame(unmc(summary(round(RA(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
    colnames(PSGRdata)<- c("Mean","SD","1%","10%","50%","90%","99%")
    rownames(PSGRdata)[1] <- "GCOS"
    datatable(PSGRdata[3:7], options = list(pageLength = 1, dom = 'tip'))
  })
  
   output$RA4Table <- renderDataTable({
    RA4data<- data.frame(unmc(summary(round(RA4(), digits = 1), probs = c(0.01,0.1,0.50,0.9,0.99))))
    colnames(RA4data)<- c("Mean","SD","1%","10%","50%","90%","99%")
    rownames(RA4data)[1] <- "GCOS"
    datatable(RA4data[3:7], options = list(pageLength = 1, dom = 'tip' ))
   })
   
})
