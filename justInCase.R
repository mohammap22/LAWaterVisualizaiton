library(shiny)
library(dplyr)
library(tidyr)
library(intrval)
library(readxl)
library(plotrix)
library(assertive, warn.conflicts = FALSE)


LA20_2 <- read.csv("data/XC_LA20_2.csv")
colnames(LA20_2) <- c("Station", "Elevation")
hydro2020 <- read.csv("data/hydraulic_ts_LA20_2.csv")

LA11<- read.csv("data/XC_LA11.csv")
colnames(LA11) <- c("Station", "Elevation")
hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA11101<- read.csv("data/XC_11101250.csv")
colnames(LA11101) <- c("Station", "Elevation") 
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF34D<- read.csv("data/XC_F34D.csv")
colnames(LAF34D) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF37B_High<- read.csv("data/XC_F37B_High.csv")
colnames(LAF37B_High) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF3_Low<- read.csv("data/XC_F37B_Low.csv")
colnames(LAF3_Low) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF45B<- read.csv("data/XC_F45B.csv")
colnames(LAF45B) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF57C<- read.csv("data/XC_F57C.csv")
colnames(LAF57C) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF300<- read.csv("data/XC_F300.csv")
colnames(LAF300) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAF319<- read.csv("data/XC_F319.csv")
colnames(LAF319) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LAGLEN<- read.csv("data/XC_GLEN.csv")
colnames(LAGLEN) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA1<- read.csv("data/XC_LA1.csv")
colnames(LA1) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA2<- read.csv("data/XC_LA2.csv")
colnames(LA2) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA3<- read.csv("data/XC_LA3.csv")
colnames(LA3) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA8<- read.csv("data/XC_LA8.csv")
colnames(LA8) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA13<- read.csv("data/XC_LA13.csv")
colnames(LA13) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA14<- read.csv("data/XC_LA14.csv")
colnames(LA14) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA20<- read.csv("data/XC_LA20.csv")
colnames(LA20) <- c("Station", "Elevation")
#hydro11 <- read.csv("data/hydraulic_ts_LA11.csv")

LA11Depth <- read.csv("data/equationBetter.csv")

.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "img",
    directoryPath = system.file(
      "data/www",
      package = "imageissue"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("img")
}


draw.stick <- function(x,y,scale=1,arms="down",  #Francis Smart made this stick figure code 
                       gender="male",lwd=3, clcol="white",
                       linecol="black",
                       hat=NA) {
  s <- scale/100
  
  require("plotrix")
  
  # Draw Head
  draw.ellipse(x+50*s,y+75*s,10*s+2,15*s,lwd=lwd, border=linecol)
  
  # Draw torso
  lines(c(x+50*s,x+50*s), c(y+35*s,y+60*s),lwd=lwd, col=linecol)
  
  if (arms=="up"){
    lines(c(x+50*s,x-5*s), c(y+50*s,y+25*s),lwd=lwd, col=linecol) # Left
    lines(c(x+50*s,x+108*s), c(y+50*s,y+65*s),lwd=lwd, col=linecol) # Right
  }
  
  # Draw male legs
  if (gender=="male") {
    lines(c(x+50*s,x+30*s), c(y+35*s,y+9*s),lwd=lwd, col=linecol)
    lines(c(x+50*s,x+70*s), c(y+35*s,y+9*s),lwd=lwd, col=linecol)
  }
}
tabPlan <- function(name, image, threshhold){
  plotName <- paste("plot", name, sep = "")
  flowName <- paste("y", name, sep = "")
  obs_n <- paste("obs_numeric", name, sep = "")
  tabPanel(name, plotOutput(plotName, width = "150%", height = "450px"), 
           sidebarPanel(sliderInput(flowName, "cfs", min=0, max= threshhold, value=0, width = "100%"),
                        numericInput(obs_n, "Flow", min = 0, max = threshhold, value = 0)),
           mainPanel(img(src = image, align = "right", height = "85%", width = "89%")))
}
ui <- fluidPage(
  # Application title
  titlePanel("LA River Water Data"),
  
  mainPanel(
    style = "height:850px;", style = "width:50%", tabsetPanel(type = "tabs",
                                                              tabPlan("LA11", "LA11.jpg", 65000), tabPlan("LA20_2", "LA20_2.jpg", 28000), tabPlan("LA111", "LA111.jpg", 50),
                                                              tabPlan("LAF34D", "F34D.jpg", 150000), tabPlan("LAH", "F37B_High.jpg", 13250), tabPlan("L", "F37B_Low.jpg", 6000),
                                                              tabPlan("5B", "F45B.jpg", 50000), tabPlan("7C", "F57C.jpg", 96000), tabPlan("LA300", "F300.jpg", 40500), 
                                                              tabPlan("LA319", "F319.jpg", 150000), tabPlan("LAGLEN", "GLEN.jpg", 40000), tabPlan("LA1", "LA1.jpg", 138000),
                                                              tabPlan("LA2", "LA2.jpg", 150000), tabPlan("LA3", "LA3.jpg", 150000), tabPlan("LA8", "LA8.jpg", 150000), 
                                                              tabPlan("LA13", "LA13.jpg", 84000), tabPlan("LA14", "LA14.jpg", 46500), tabPlan("LA20", "LA20_2.jpg", 29000),
                                                              tabPanel("Source", p("This is a visualization of preliminary simulation results from the Los Angeles River Environmental 
                                            Flows Study. Aerial imagery is from Google Earth. Created by P. Mohammadi at University of Portland. 
                                            For more information contact Jordy Wolfand at Wolfand@up.edu.")))),
)

server <- function(input, output, session) {
  graphPlan <- function(name, fix){
    better1 <- paste("plot", name, sep = "")
    better2 <- paste("y", name, sep = "")
    output[[better1]] <- renderPlot({
      if(name == "LA20_2"){
        if(input[[better2]] < 100){
          yx <- 0.0982723426180356 * input[[better2]]^(1/2) - (0.137860687788668)*(input[[better2]])^(1/3) + 0.106779733730162*(input[[better2]])^(1/4)
        }
        else{
          yx <- 0.113178199994571*input[[better2]]^(1/2) + 0.862028504219065*input[[better2]]^(1/3) - 1.47065979396366*input[[better2]]^(1/4)
        }
      }
      if(name == "LA11"){
          yx <- -0.000161384274632234 * input$yLA11 + (.00000000134144192304252)*(input$yLA11)^2 + 
            0.101098565120723*(input$yLA11)^(1/2)
      }
      if(name == "LA111"){
          yx <- 0.122502137160693*input$yLA111^(1/2) -1.3526968455542*input$yLA111^(1/3) + 3.19768748641264*input$yLA111^(1/4)
        }
      if(name == "LAF34D"){
        if(input$yLAF34D < 100){
          yx <- 0.338742303977195  * input$yLAF34D^(1/2) - (0.993921932339524)*input$yLAF34D^(1/3) + 0.762674656477336*(input$yLAF34D)^(1/4)
        }
        else {
          yx <- 0.114815849451872 * input$yLAF34D^(1/2) - 1.05754157903341*input$yLAF34D^(1/3) + 1.51371890971113*input$yLAF34D^(1/4)
        }
      }
      if(name == "LAH"){
        yx <- 0.000153956 * input$yLAH - (0.00000000274269712402202)*(input$yLAH)^2 + 0.108855716970931*(input$yLAH)^(1/2)
      }
      if(name == "L"){
        if(input$yL >= 300){
          yx <- -0.00475403538968897*input$yL + .0000000932877355374253*input$yL^(2) + 0.583779664755916*input$yL^(1/2)
        }
        else {
          yx <- -0.0455962556515382 * input$yL + (0.000170730527700887)*(input$yL)^2 + 0.444915029731235*(input$yL)^(1/2)
        }
      }
      if(name == "5B"){
        if(input$y5B >= 1000){
          yx<- -0.000116590092483093*input$y5B - .00000000124817808745318*input$y5B^(2) + 0.114059260112597*input$y5B^(1/2)
        }
        else {
          yx <- 0.00104783901470246 * input$y5B - (.000000285933334630127)*(input$y5B)^2 + 0.020822868112729*(input$y5B)^(1/2)
        }
      }
      if(name == "7C"){
        if(input$y7C < 1000){
          yx <- 0.00345355152493176 * input$y7C^(1/2) + (0.25013423692204)*(input$y7C)^(1/3) + 0.0727947851233417*(input$y7C)^(1/4)
        }
        else {
          yx <- 0.295771586061073 * input$y7C^(1/2) - 3.25835313087246*input$y7C^(1/3) + 4.80564060476122*input$y7C^(1/4)
        }
      }
      if(name == "LA300"){
        if(input$yLA300 < 100){
          yx <- 0.724401864709473 * input$yLA300^(1/2) - (2.1078836800482)*(input$yLA300)^(1/3) + 1.47163652313834*(input$yLA300)^(1/4)
        }
        else {
          yx <- 0.297280322182544*input$yLA300^(1/2) - 3.17458081314686*input$yLA300^(1/3) + 4.74259509467312*input$yLA300^(1/4)
        }
      }
      if(name == "LA319"){
        if(input$yLA319 < 25){
          yx <- 0.191366890824538 * (input$yLA319)^(1/2) - (0.215254033736986)*(input$yLA319)^(1/3) + 
            0.110562267574707*(input$yLA319)^(1/4)
        }
        else {
          yx <- 0.127645856879227 * input$yLA319^(1/2) - (1.03783606707263)*input$yLA319^(1/3) + 1.48617498783991*input$yLA319^(1/4)
        }
      }
      if(name == "LAGLEN"){
        if(input$yLAGLEN < 200){
          yx <- -0.0188614944947775 * input$yLAGLEN + (0.0000466264447541684)*(input$yLAGLEN)^2 + 0.243004019570674*(input$yLAGLEN)^(1/2)
        }
        else{
          yx <- 0.000060982874293727 * input$yLAGLEN - (.000000000633606207512683)*(input$yLAGLEN)^2 + 0.0933625812038356*(input$yLAGLEN)^(1/2)
        }
      }
      if(name == "LA1"){
        yx <- 0.0902297974755006 * (input$yLA1)^(1/2) - (0.384476744)*(input$yLA1)^(1/3) + 0.598485325*(input$yLA1)^(1/4)
      }
      if(name == "LA2"){
        if(input$yLA2 < 1000){
          yx <- 0.842405600848801 * (input$yLA2)^(1/2) - (7.72881363460028)*(input$yLA2)^(1/3) + 10.1128360899612*(input$yLA2)^(1/4)
        }
        else {
          yx <- 0.131262954287157 * (input$yLA2)^(1/2) - (1.48390524035558)*(input$yLA2)^(1/3) + 3.00047747608061*(input$yLA2)^(1/4)
        }
      }
      if(name == "LA3"){
        if(input$yLA3 < 100){
          yx <- -0.180865210443433 * (input$yLA3)^(1/2) + (1.574682418851)*(input$yLA3)^(1/3) - 1.38091222115991*(input$yLA3)^(1/4)
        }
        else{
          yx <- 0.124045295674031 * (input$yLA3)^(1/2) - (1.01500690858326)*(input$yLA3)^(1/3) + 1.43714949784877*(input$yLA3)^(1/4)
        }
      }
      if(name == "LA8"){
        if(input$yLA8 < 100){
          yx <- 0.0791836803731445 * input$yLA8^(1/2) + (0.161313732112881)*(input$yLA8)^(1/3) -0.182404828000544*(input$yLA8)^(1/4)
        }
        else{
          yx <- 0.165685529581623 * input$yLA8^(1/2) - (1.29646146348071)*(input$yLA8)^(1/3) + 1.68822670353883*(input$yLA8)^(1/4)
        }
      }
      if(name == "LA13"){
        if(input$yLA13 < 1000){
          yx <- 0.049502462787842  * input$yLA13^(1/2) - (0.156671523698658)*(input$yLA13)^(1/3) + 0.129636936682007*(input$yLA13)^(1/4)
        }
        else{
          yx <- 0.384020330151552  * input$yLA13^(1/2) - (3.86362084890618)*(input$yLA13)^(1/3) + 4.8639476996109*(input$yLA13)^(1/4)
        }
      }
      if(name == "LA14"){
        if(input$yLA14 < 100){
          yx <- -0.0099795552468806 * input$yLA14 + (0.0000166205602657921)*(input$yLA14)^2 + 0.175542073026732*(input$yLA14)^(1/2)
        }
        else {
          yx <- 0.0000398298556143352 * input$yLA14 - (.0000000002957975111101)*(input$yLA14)^2 + 0.0858394439338323*(input$yLA14)^(1/2)
        }
      }
      if(name == "LA20"){
        if(input$yLA20 < 1000){
          yx <- -0.262227369672964 * input$yLA20^(1/2) + (2.36100297686463)*(input$yLA20)^(1/3) - 2.07120517866165*(input$yLA20)^(1/4)
        }
        else {
          yx <- 0.229912137184949 * input$yLA20^(1/2) - (1.02170238666808)*(input$yLA20)^(1/3) + 1.17328441885261*(input$yLA20)^(1/4)
        }
      }
      y <- fix$Elevation - min(fix$Elevation)
      n <- length(y)
      x <- fix$Station 
      s = smooth.spline(x, y, spar=0.5)
      xy <- predict(s, seq(min(x), max(x), by=1)) # Some vertices on the curve
      m <- length(xy$x)                         
      x.poly <- c(xy$x, xy$x[m], xy$x[1])         # Adjoin two x-coordinates
      y.poly <- c(xy$y, 0, 0)   # .. and the corresponding y-coordinates
      plot(x, y, type='n', xlab="Station (ft)", ylab="Depth (ft)", 
           main = name, cex.main = 3, col.main = '#2E8B57',
           cex.lab = 1.7, cex.sub = 3, col.lab = "#228B22", font.lab = 4, font.main = 4)
      polygon(c(min(x),min(x), max(x)-2, max(x)-2 ),c(0,yx, yx, 0), col = "#81b7d2")
      polygon(x.poly, y.poly, col= "#097770", border=NA) 
      if(name == "LAH" || name == "LA300"){
        draw.stick(min(x)-6,-1, scale = 8.2, arms="up", lwd = 3)
      }
      else {
        draw.stick(min(x)-9,-1, scale = 8.2, arms="up", lwd = 3)
      }
    })
  }
  graphPlan("LA11", LA11)
  graphPlan("LA20_2", LA20_2)
  graphPlan("LA111", LA11101)
  graphPlan("LAF34D", LAF34D)
  graphPlan("LAH", LAF37B_High)
  graphPlan("L", LAF3_Low)
  graphPlan("5B", LAF45B)
  graphPlan("7C", LAF57C)
  graphPlan("LA300", LAF300)
  graphPlan("LA319", LAF319)
  graphPlan("LAGLEN", LAGLEN)
  graphPlan("LA1", LA1)
  graphPlan("LA2", LA2)
  graphPlan("LA3", LA3)
  graphPlan("LA8", LA8)
  graphPlan("LA13", LA13)
  graphPlan("LA14", LA14)
  graphPlan("LA20", LA20)

  #-----------------------------------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------------------------------#
  sliderPlan <- function(name, Yname, Oname){
    better1 <- paste("y", name, sep = "")
    better2 <- paste("obs_numeric", name, sep = "")
    observe({
      updateSliderInput(session, Yname,
                        value = input[[better2]])
    })
    observe({
      updateSliderInput(session, Oname,
                        value = input[[better1]])
    })
  }
    sliderPlan("LA11", "yLA11", "obs_numericLA11") 
    sliderPlan("LA20_2", "yLA20_2", "obs_numericLA20_2")
    sliderPlan("LA111", "yLA111", "obs_numericLA111")
    sliderPlan("LAF34D", "yLAF34D", "obs_numericLAF34D")  
    sliderPlan("LAH", "yLAH", "obs_numericLAH") 
    sliderPlan("L", "yL", "obs_numericL")
    sliderPlan("5B", "y5B", "obs_numeric5B")
    sliderPlan("7C", "y7C", "obs_numeric7C")
    sliderPlan("LA300", "yLA300", "obs_numericLA300") 
    sliderPlan("LA319", "yLA319", "obs_numericLA319")
    sliderPlan("LAGLEN", "yLAGLEN", "obs_numericLAGLEN")
    sliderPlan("LA1", "yLA1", "obs_numericLA1")
    sliderPlan("LA2", "yLA2", "obs_numericLA2") 
    sliderPlan("LA3", "yLA3", "obs_numericLA3")
    sliderPlan("LA8", "yLA8", "obs_numericLA8")
    sliderPlan("LA13", "yLA13", "obs_numericLA13")
    sliderPlan("LA14", "yLA14", "obs_numericLA14") 
    sliderPlan("LA20", "yLA20", "obs_numericLA20")
}
getDepth5 <- LA11Depth$Depth
names(getDepth1) <-LA11Depth$Flow
getDepth6 <- function(state, lookupvector = getDepth1){
  yep <- unname(lookupvector[state])
  return(yep)
}
getDepth(65224)
getFlow5 <- LA11Depth$Flow
names(getFlow1) <- LA11Depth$Depth
getFlo6 <- function(state, lookupvector = getFlow1){
  yep <- round(unname(lookupvector[state]))
  return(yep)
}
getFlow(1)


library(shiny)

server <- function(input, output, session) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$region, "and", input$prodCat)
  })
  
}

#Add 2 Drop Down Selection for Region and Product Category
ui <-   basicPage(
  h1("R Shiny selectInput & display text from selectInput"),
  
  selectInput("region",
              label = "Region",
              choices = c("Region 1", "Region 2"),
              selected = "Region 1"),
  
  selectInput("prodCat",
              label = "Product Category",
              choices = c("Category 1", "Category 2"),
              selected = "Category 1"), 
  
  textOutput("selected_var")
  
)

shinyApp(ui = ui, server = server)


shinyApp(ui=ui, server = server)