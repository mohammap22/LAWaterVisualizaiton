library(shiny)
library(dplyr)
library(tidyr)
library(intrval)
library(readxl)
library(plotrix)
library(plotly)
library(forcats)
library(leaflet)
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

GLEN<- read.csv("data/XC_GLEN.csv")
colnames(GLEN) <- c("Station", "Elevation")
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




which(LA11Depth$Filler2 == 3.5)

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
sampleNum = seq(1,4650000,1)
mydata = data.frame(LA11Depth$obs_numericLAF34D, LA11Depth$yLAF34D)
mydata
names(mydata)

r = 3
mydata[r, which(colnames(mydata) =="LA11Depth.obs_numericLAF34D")]
mydata[r, which(colnames(mydata) =="LA11Depth.yLAF34D")]


getDepth <- function(name, flow){
  dName <- paste("obs_numeric", name, sep = "")
  fName <-paste("y", name, sep = "")
  mydata = data.frame(LA11Depth[dName], LA11Depth[fName])
  names(mydata)
  here <- mydata[flow, which(colnames(mydata) ==dName)]
  return(here)
}
getDepth("F34D", 38000)


getFlow <- function(depth, name){
  dName <- paste("obs_numeric", name, sep = "")
  fName <-paste("y", name, sep = "")
  mydata = data.frame(round(LA11Depth[dName]), LA11Depth[fName])
  names(mydata)
  rownum = which(mydata[ ,which(colnames(mydata)== dName) ] == depth, arr.ind=TRUE)
  return(round(median(rownum)))
}
getFlow(8, "F34D")

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
    lines(c(x+50*s,x-2*s), c(y+50*s,y+25*s),lwd=lwd, col=linecol) # Left
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
  obs_depth <- paste("obs_depth", name, sep = "")
  obs_n <- paste("obs_numeric", name, sep = "")
  tabPanel(name, value = name, plotOutput(plotName, width = "130%", height = "450px"),
           mainPanel(img(src = image, align = "right", height = "90%", width = "90%")),
           sidebarPanel(sliderInput(flowName, "Flow (cfs)", min=1, max= threshhold, value=1, width = "140%"),
                        numericInput(obs_n, "Flow (cfs)", min = 1, max = threshhold, value = 1),
                        #numericInput(obs_depth, "Depth (feet)", min = 1, max = 100, value = 1),
                        strong("Depth (ft)", align = "right"),
                        textOutput(name)
                        ))
}
ui <- fluidPage(
  # Application title
  titlePanel("Los Angeles River Depth Visualization"),
  sidebarPanel(leafletOutput("LosAngeles", width = "100%", height = 550)),
  mainPanel(
    style = "height:850px;", style = "width:50%",  tabsetPanel(type = "tabs", id = "inTabset",
                                                               tabPanel("Home", p(tags$div("This interactive dashboard provides a visualization of river discharge and its relationship to channel depth in the 
                                                                                    Los Angeles River. The dashboard is based on the hydrologic and hydraulic model of the river created for the Los 
                                                                                    Angeles River Environmental Flows Project, led by the Southern California Coastal Water Research Project (SCCWRP) with support 
                                                                                    from Colorado School of Mines and the University of Portland. The technical team is working with the State Water Resources Control 
                                                                                    Board and the Los Angeles Regional Water Quality Control Board, in cooperation with local municipalities (including City of LA 
                                                                                    Bureau of Sanitation, City of LA Department of Water and Power, LA County Department of Public Works, and LA County Sanitation 
                                                                                    Districts), to develop a process for establishing flow criteria, apply the process to provide recommendations for flow criteria in 
                                                                                    the LA River, and produce tools and approaches to evaluate management scenarios necessary to achieve recommended flow criteria. 
                                                                                             Click",
                                                                                    tags$a(href = "https://www.sccwrp.org/about/research-areas/ecohydrology/los-angeles-river-flows-project/",
                                                                                    "here"),"for more information about the project.")),
                                                                        p(tags$div("For details on how the hydrologic and hydraulic models were created, see", tags$a(
                                                                        href = "https://ftp.sccwrp.org/pub/download/DOCUMENTS/TechnicalReports/1154_LARiverAquaticLifeUses.pdf", 
                                                                        "Stein et al. Assessment of Aquatic Life Use Needs for 
                                                                          the Los Angeles River."), "(2021).")), 
                                                                        p(tags$div("Aerial imagery is from Google Earth. Created by",tags$a(href="mailto:mohammap22@up.edu", 
                                                                        "Peter Mohammadi"),"at University of Portland. Contact", tags$a(href="mailto:%20erics@sccwrp.org",
                                                                         "Eric Stein"), "for technical 
                                                                          questions about the project and", tags$a(href="mailto:%20wolfand@up.edu",
                                                                          "Jordy Wolfand"), "for questions about the dashboard."))),

                                                               tabPlan("LA20", "LA20_2.jpg", 29000), tabPlan("LA20_2", "LA20_2.jpg", 28000), tabPlan("F300", "F300.jpg", 32655), 
                                                               tabPlan("LA14", "LA14.jpg", 46500), tabPlan("LA13", "LA13.jpg", 84000), tabPlan("GLEN", "GLEN.jpg", 33948), 
                                                               tabPlan("LA11", "LA11.jpg", 60436), tabPlan("F57C", "F57C.jpg", 96000), tabPlan("LA8", "LA8.jpg", 150000), 
                                                               tabPlan("11101250", "LA111.jpg", 103), tabPlan("F34D", "F34D.jpg", 200000), tabPlan("F45B", "F45B.jpg", 50000),
                                                               tabPlan("F37B_High", "F37B_High.jpg", 13250), tabPlan("LA3", "LA3.jpg", 150000), tabPlan("F37B_Low", "F37B_Low.jpg", 5089),
                                                               tabPlan("F319", "F319.jpg", 150000)
                                                               
                                                               
                                                               
    )))



popCon <- function(name){
  link <- paste("?url=inTabpanel_", name, sep = "")
  name <- paste(sep = "<br/>",
                actionLink(link, name),
                "This one would also update to another tab")
}
server <- function(input, output, session) {
  
 
  
  PopcontentLA20 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA20", onclick = 'Shiny.onInputChange(\"link_clickLA20\",  Math.random())'),
                          "Los Angeles River within Sepulveda Basin")
  PopcontentLA11 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA11", onclick = 'Shiny.onInputChange(\"link_clickLA11\",  Math.random())'),
                          "Los Angeles River at Glendale Narrows")
  PopcontentLA20_2 <- paste(sep = "<br/>",
                            ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                            actionLink("?url=inTabset/Home", "Learn about LA20_2", onclick = 'Shiny.onInputChange(\"link_clickLA20_2\",  Math.random())'),
                            "Los Angeles River above Sepulveda Basin")
  Popcontent11101250 <- paste(sep = "<br/>",
                           ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                           actionLink("?url=inTabset/Home", "Learn about 11101250", onclick = 'Shiny.onInputChange(\"link_click11101250\",  Math.random())'),
                           "Rio Hondo above Whittier Narrows Dam")
  PopcontentF34D <- paste(sep = "<br/>",
                            ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                            actionLink("?url=inTabset/Home", "Learn about F34D", onclick = 'Shiny.onInputChange(\"link_clickF34D\",  Math.random())'),
                            "Los Angeles River above confluence with Rio Hondo")
  PopcontentF37B_High<- paste(sep = "<br/>",
                         ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                         actionLink("?url=inTabset/Home", "Learn about F37B_High", onclick = 'Shiny.onInputChange(\"link_clickF37B_High\",  Math.random())'),
                         "Upper Compton Creek")
  PopcontentF37B_Low <- paste(sep = "<br/>",
                       ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                       actionLink("?url=inTabset/Home", "Learn about F37B_Low", onclick = 'Shiny.onInputChange(\"link_clickF37B_Low\",  Math.random())'),
                       "Lower Compton Creek")
  PopcontentF45B <- paste(sep = "<br/>",
                        ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                        actionLink("?url=inTabset/Home", "Learn about F45B", onclick = 'Shiny.onInputChange(\"link_clickF45B\",  Math.random())'),
                        "Rio Hondo below spreading grounds")
  PopcontentF57C <- paste(sep = "<br/>",
                        ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                        actionLink("?url=inTabset/Home", "Learn about F57C", onclick = 'Shiny.onInputChange(\"link_clickF57C\",  Math.random())'),
                        "Los Angeles River above confluence with Arroyo Seco")
  PopcontentF300 <- paste(sep = "<br/>",
                           ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                           actionLink("?url=inTabset/Home", "Learn about F300", onclick = 'Shiny.onInputChange(\"link_clickF300\",  Math.random())'),
                           "Los Angeles River above confluence with Burbank Channel")
  PopcontentF319 <- paste(sep = "<br/>",
                           ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                           actionLink("?url=inTabset/Home", "Learn about F319", onclick = 'Shiny.onInputChange(\"link_clickF319\",  Math.random())'),
                           "Los Angeles River at Wardlow Gage")
  PopcontentGLEN <- paste(sep = "<br/>",
                            ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                            actionLink("?url=inTabset/Home", "Learn about GLEN", onclick = 'Shiny.onInputChange(\"link_clickGLEN\",  Math.random())'),
                            "Los Angeles River below Glendale WRP")
  PopcontentLA3 <- paste(sep = "<br/>",
                         ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                         actionLink("?url=inTabset/Home", "Learn about LA3", onclick = 'Shiny.onInputChange(\"link_clickLA3\",  Math.random())'),
                         "Los Angeles River below confluence with Rio Hondo")
  PopcontentLA8 <- paste(sep = "<br/>",
                         ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                         actionLink("?url=inTabset/Home", "Learn about LA8", onclick = 'Shiny.onInputChange(\"link_clickLA8\",  Math.random())'),
                         "Los Angeles River above confluence with Rio Hondo")
  PopcontentLA13 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA13", onclick = 'Shiny.onInputChange(\"link_clickLA13\",  Math.random())'),
                          "Los Angeles River above Glendale WRP")
  PopcontentLA14 <- paste(sep = "<br/>",
                          ##Here I have added and event which needs to be updated on clicking the link called "link_click"
                          actionLink("?url=inTabset/Home", "Learn about LA14", onclick = 'Shiny.onInputChange(\"link_clickLA14\",  Math.random())'),
                          "Los Angeles River below confluence with Burbank Channel")
  
  output$LosAngeles <-  renderLeaflet({
    LosAngeles <- leaflet(options = leafletOptions( 
                                               minZoom = 8,     maxZoom = 16)) %>%
      setView(lng = -118.2437, lat =34.0522, zoom = 10)%>%
      addTiles(options = providerTileOptions(opacity = .9)) %>%
      addCircleMarkers(lng=-118.247706, lat=34.108175, radius = 11,
                       opacity = 0.8, color = "#006400", 
                       popup=PopcontentLA11)%>% #MarkerHome
      addCircleMarkers(lng=-118.514642, lat=34.184708, radius = 11, 
                       color =  "#006400",popup=PopcontentLA20) %>%
      addCircleMarkers(lng=-118.484394, lat=34.173175, radius = 11, 
                       color =  "#006400",popup=PopcontentLA20_2)%>%
      addCircleMarkers(lng=-118.173592, lat=33.948297, radius = 11, 
                       color =  "#006400",popup=PopcontentF34D) %>%
      addCircleMarkers(lng=-118.228233, lat=34.085803, radius = 11, 
                       color =  "#006400",popup=PopcontentF57C) %>%
      addCircleMarkers(lng=-118.222186, lat=33.878144, radius = 11, 
                       color =  "#006400",popup=PopcontentF37B_High) %>%
      addCircleMarkers(lng=-118.275339, lat=34.136858, radius = 11, 
                       color =  "#006400",popup=PopcontentGLEN) %>%
      addCircleMarkers(lng=-118.196333, lat=33.8639, radius = 11, 
                       color =  "#006400",popup=PopcontentLA3) %>%
      addCircleMarkers(lng=-118.228603, lat=34.040528, radius = 11, 
                       color =  "#006400",popup=PopcontentLA8) %>%
      addCircleMarkers(lng=-118.299758, lat=34.156839, radius = 11, 
                       color =  "#006400",popup=PopcontentLA14) %>%
      addCircleMarkers(lng=-118.205728, lat=33.817733, radius = 11, 
                       color =  "#006400",popup=PopcontentF319) %>%
      addCircleMarkers(lng=-118.378453, lat=34.1411, radius = 11, 
                       color =  "#006400",popup=PopcontentF300) %>%
      addCircleMarkers(lng=-118.164831, lat=33.944528, radius = 11, 
                       color =  "#006400",popup=PopcontentF45B) %>%
      addCircleMarkers(lng=-118.211672, lat=33.851269, radius = 11, 
                       color =  "#006400",popup=PopcontentF37B_Low) %>%
      addCircleMarkers(lng=-118.072069, lat=34.049108, radius = 11, 
                       color =  "#006400",popup=Popcontent11101250) %>%
      addCircleMarkers(lng=-118.278844, lat=34.150964, radius = 11, 
                       color =  "#006400",popup=PopcontentLA13)
  })  
  
  
  #Here I have the observEvent for link_click which updates the tab
  links <- function(name){
    link <- paste("link_click", name, sep = "")
    observeEvent(input[[link]],{
      updateTabsetPanel(session, "inTabset", name)
      
    })
  }
  links("LA11")
  links("LA20_2")
  links("LA20")
  links("F34D")
  links("11101250")
  links("F57C")
  links("F37B_High")
  links("GLEN")
  links("LA3")
  links("LA8")
  links("LA13")
  links("LA14")
  links("F319")
  links("F300")
  links("F45B")
  links("F37B_Low")
  
  textPlan <- function(name){
    flow <-paste("y", name, sep = "")
    output[[name]] <- renderText({ round(getDepth(name, (input[[flow]])),2)})
  }
  textPlan("LA11")
  textPlan("LA20_2")
  textPlan("LA20")
  textPlan("F34D")
  textPlan("11101250")
  textPlan("F57C")
  textPlan("F37B_High")
  textPlan("GLEN")
  textPlan("LA3")
  textPlan("LA8")
  textPlan("LA13")
  textPlan("LA14")
  textPlan("F319")
  textPlan("F300")
  textPlan("F45B")
  textPlan("F37B_Low")
  
  graphPlan <- function(name, fix){
    better1 <- paste("plot", name, sep = "")
    better2 <- paste("y", name, sep = "")
    output[[better1]] <- renderPlot({
      if(name == "LA20_2"){
        yx <- getDepth("LA20_2", input$yLA20_2)
      }
      if(name == "LA11"){
        yx <- getDepth("LA11", input$yLA11)
      }
      if(name == "11101250"){
        yx <- getDepth("11101250", input$y11101250)
      }
      if(name == "F34D"){
        yx <- getDepth("F34D", input$yF34D)
      }
      if(name == "F37B_High"){
        yx <- getDepth("F37B_High", input$yF37B_High)
      }
      if(name == "F37B_Low"){
        yx <- getDepth("F37B_Low", input$yF37B_Low)
      }
      if(name == "F45B"){
        yx <- getDepth("F45B", input$yF45B)
      }
      if(name == "F57C"){
        yx <- getDepth("F57C", input$yF57C)
      }
      if(name == "F300"){
        yx <- getDepth("F300", input$yF300)
      }
      if(name == "F319"){
        yx <- getDepth("F319", input$yF319)
      }
      if(name == "GLEN"){
        yx <- getDepth("GLEN", input$yGLEN)
      }
      #if(name == "LA1"){
      # yx <- getDepth("LA1", input$yLA1)
      #}
      #if(name == "LA2"){
      # yx <- getDepth("LA2", input$yLA2)
      #}
      if(name == "LA3"){
        yx <- getDepth("LA3", input$yLA3)
      }
      if(name == "LA8"){
        yx <- getDepth("LA8", input$yLA8)
      }
      if(name == "LA13"){
        yx <- getDepth("LA13", input$yLA13)
      }
      if(name == "LA14"){
        yx <- getDepth("LA14", input$yLA14)
      }
      if(name == "LA20"){
        yx <- getDepth("LA20", input$yLA20)
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
           cex.lab = 1.5, cex.sub = 3, col.lab = "#228B22", font.lab = 4, font.main = 4)
      polygon(c(min(x),min(x), max(x)-2, max(x)-2 ),c(0,yx, yx, 0), col = "#81b7d2")
      polygon(x.poly, y.poly, col= "#097770", border=NA) 
      if(name == "F37B_High" || name == "F300"){
        draw.stick(min(x)-6,-1, scale = 7.5, arms="up", lwd = 3)
      }
      else {
        draw.stick(min(x)-9,-1, scale = 7.5, arms="up", lwd = 3)
      }
    })
  }
  
  graphPlan("LA11", LA11)
  graphPlan("LA20_2", LA20_2)
  graphPlan("11101250", LA11101)
  graphPlan("F34D", LAF34D)
  graphPlan("F37B_High", LAF37B_High)
  graphPlan("F37B_Low", LAF3_Low)
  graphPlan("F45B", LAF45B)
  graphPlan("F57C", LAF57C)
  graphPlan("F300", LAF300)
  graphPlan("F319", LAF319)
  graphPlan("GLEN", GLEN)
  #graphPlan("LA1", LA1)
  #graphPlan("LA2", LA2)
  graphPlan("LA3", LA3)
  graphPlan("LA8", LA8)
  graphPlan("LA13", LA13)
  graphPlan("LA14", LA14)
  graphPlan("LA20", LA20)
  
  #-----------------------------------------------------------------------------------------------------#
  #-----------------------------------------------------------------------------------------------------#
  
  
  sliderPlan <- function(name, Yname, Oname){
    obs_depth <- paste("obs_depth", name, sep = "")
    better1 <- paste("y", name, sep = "")
    better2 <- paste("obs_numeric", name, sep = "")
    observe({
      updateSliderInput(session, Yname,
                        value = input[[Oname]])
    })
    observe({
      updateSliderInput(session, Oname,
                        value = input[[Yname]])
    })
    observe({
      updateSliderInput(session, Yname,
                        value = input[[Yname]])
    })
    observe({
      updateSliderInput(session, Oname,
                        value = input[[Oname]])
    })
    observe({
      updateSliderInput(session, obs_depth,
                        value = getDepth(name, input[[Oname]]))
    })
    observe({
      updateSliderInput(session, obs_depth,
                        value = getDepth(name, input[[Yname]]))
    })
  }
  
  sliderPlan("LA11", "yLA11", "obs_numericLA11") 
  sliderPlan("LA20_2", "yLA20_2", "obs_numericLA20_2")
  sliderPlan("11101250", "y11101250", "obs_numeric11101250")
  sliderPlan("F34D", "yF34D", "obs_numericF34D")  
  sliderPlan("F37B_High", "yF37B_High", "obs_numericF37B_High") 
  sliderPlan("F37B_Low", "yF37B_Low", "obs_numericF37B_Low")
  sliderPlan("F45B", "yF45B", "obs_numericF45B")
  sliderPlan("F57C", "yF57C", "obs_numericF57C")
  sliderPlan("F300", "yF300", "obs_numericF300") 
  sliderPlan("F319", "yF319", "obs_numericF319")
  sliderPlan("GLEN", "yGLEN", "obs_numericGLEN")
  #sliderPlan("LA1", "yLA1", "obs_numericLA1")
  #sliderPlan("LA2", "yLA2", "obs_numericLA2") 
  sliderPlan("LA3", "yLA3", "obs_numericLA3")
  sliderPlan("LA8", "yLA8", "obs_numericLA8")
  sliderPlan("LA13", "yLA13", "obs_numericLA13")
  sliderPlan("LA14", "yLA14", "obs_numericLA14") 
  sliderPlan("LA20", "yLA20", "obs_numericLA20")
  
  #output$depthValue <- renderText({input$})
}


shinyApp(ui=ui, server = server)