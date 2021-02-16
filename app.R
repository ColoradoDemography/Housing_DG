#' SDO Internal Housing unit Dashboard
#' @author  Adam Bickford, Colorado State Demography Office
#' Release Version 1.0 01/21/2021

rm(list = ls())
library(tidyverse, quietly=TRUE)
library(plyr)
library(readxl, quietly=TRUE)
library(scales, quietly=TRUE)
library(RPostgreSQL)
library(shiny, quietly=TRUE)
library(shinydashboard, quietly=TRUE)
library(shinyjs, quietly=TRUE)
library(plotly)



# Additions for Database pool
library('pool') 
library('DBI')
library('config')

source("R/GenDG.R")
source("R/chkID.R")
source("R/listTofips.R")
source("R/percent.R")
source("R/popPlace.R")
source("R/simpleCap.R")
source("R/NumFmt.R")
source("R/rangeVal.R")

# Set up database pool 1/23/19

config <- get("database")
DOLAPool <-  dbPool(
  drv <- dbDriver(config$Driver),
  dbname = config$Database,
  host = config$Server,
  port = config$Port,
  user = config$UID,
  password = config$PWD
)

dbGetInfo(DOLAPool)


onStop(function(){
  poolClose(DOLAPool)
})


# Structure of user Interface
ui <-
  dashboardPage( skin="green", 
                 title= "Housing and Das Gupta Estimates",
                 dashboardHeader(title = span(img(src="co_dola__NoText-dept.png", height = 45, align = "top"),"Housing and Das Gupta Estimates"), titleWidth=550), #dashboardHeader
                 dashboardSidebar( width = 300,  useShinyjs(),
                                   # data level Drop down
                                   selectInput("level", "Select Data Level" ,
                                               choices=c("Select a Data Level","Counties","Municipalities")  #Enabled in V1
                                   ),
                                   
                                   # profile Unit dropdown
                                   selectInput("unit", "Select Location" ,choices=""),
                                   selectInput("comp", "Select Comparison" ,choices=c("Select Comparison","Total Housing Units", "Population", "Persons Per Household")),
                                  #Output Content Checkboxes   
                                   checkboxGroupInput("outChk", label=NULL,
                                                      choices = NULL
                                   ),
                                   
                                   #Action Button
                                   actionButton("profile","View Chart")
                                   
                                   
                                   
                 ), #dashboardSidebar
                 dashboardBody(  tags$head( 
                   tags$meta(name="keywords", content="Colorado, demographic, county, community, municiplaity, city, population, housing, household, age, median income, jobs, wages"),
                   tags$link(rel = "stylesheet", type = "text/css", href = "dashboard.css"),  #Link to CSS...
                   tags$title("Housing and Das Gupta Estimates")
                 ),
                 tags$body(includeHTML("www/tag_body.js")),  # for non-JS instances
                 tags$style(HTML("
                                 .box.box-solid.box-primary>.box-header {
                                 color:#fffff;
                                 background:#C9C6C5
                                 }
                                 .box.box-solid.box-primary{
                                 color: #ffffff;
                                 border-bottom-color:#C9C6C5;
                                 border-left-color:#C9C6C5;
                                 border-right-color:#C9C6C5;
                                 border-top-color:#C9C6C5;
                                 }   ")),
                 box(title = "", width = 12, height = "500px",
                     plotlyOutput("outPlot",width = "1200px", height = "400px"))
                 
                 ) #dashboardBody
                 
  )

# Server Management Function
server <- function(input, output, session) {
  

   # updates Dropdown boxes and selects data level and unit
  LocList <- popPlace(DOLAPool,2019)
  CountyList <- LocList$Counties
  PlaceList <- LocList$Munis
  
 
  
  observeEvent(input$level, ({
 
    #clears the comp2 dropdown on change

    if(input$level == "Select a Data Level") { #the initial state of the dropdowns
      outUnit <- ""
      outComp <- ""
    }
    
    if(input$level == "Counties") {
      outUnit <- unique(as.list(CountyList[,3]))
    }
    if(input$level == "Municipalities") {  
      outUnit <- unique(as.list(PlaceList[,3]))
    }
    
    updateSelectInput(session, "unit", choices = outUnit)
  }))  #observeEvent input$level
  
observeEvent(input$comp, ({
  if(input$comp == "Select Comparison") {
    updateCheckboxGroupInput(session, "outChk", label = NULL, choices = NULL)
  }
  if(input$comp == "Total Housing Units") {
    updateCheckboxGroupInput(session,"outChk",label="Select Comparison Estimate(s)",
                             choices = c("Das Gupta 1" = "DG_1",
                                         "Das Gupta 2" = "DG_2",
                                         "Das Gupta 3" = "DG_3",
                                         "Das Gupta 4" = "DG_4",
                                         "Das Gupta 6" = "DG_6",
                                         "Census Intercensal Estimates 2000-2010" = "CensHu00",
                                         "Data from SDO County Profile" = "SDO"))               
  }
  if(input$comp == "Population") {
    updateCheckboxGroupInput(session,"outChk",label="Select Comparison Estimate(s)",
                             choices = c("Das Gupta 1" = "DG_1",
                                         "Das Gupta 2" = "DG_2",
                                         "Das Gupta 3" = "DG_3",
                                         "Das Gupta 4" = "DG_4",
                                         "Das Gupta 6" = "DG_6",
                                         "Census Intercensal Population Estimates 1990-2000" = "CensPop90",
                                         "Census Intercensal Estimates 2000-2010" = "CensHu00",
                                         "Data from SDO County Profile" = "SDO"))               
  }
  if(input$comp == "Persons Per Household") {
    updateCheckboxGroupInput(session,"outChk",label="Select Comparison Estimate(s)",
                             choices = c("Das Gupta 1" = "DG_1",
                                         "Das Gupta 2" = "DG_2",
                                         "Das Gupta 3" = "DG_3",
                                         "Das Gupta 4" = "DG_4",
                                         "Das Gupta 6" = "DG_6"))               
  }
}))
  
  # Event for click on profile button
  observeEvent(input$profile, {

    
    #creating the input FIPS list to generate data
    if(input$unit == "") {
      lnError <- tags$h2("Please specify a Data Level and a Profile to display")
      output$outPlot <- htmlOutput(lnError)
    }  else {
        #Building fipslist  and read datafiles
        if(input$level == "Counties") {
          fipslist <- listTofips(CountyList,input$level,input$unit)
          placeName <- simpleCap(input$unit) 
        } 
        if(input$level == "Municipalities") { 
          fipslist <- listTofips(PlaceList,input$level,input$unit)
          placeName <- simpleCap(input$unit)
        }
        
        #creating ids and output flags for multiple counties and small places
        idList <- chkID(lvl=input$level,fipslist= fipslist,plName=placeName,ctyList=CountyList, plList=PlaceList)

        DG_Out <- GenDG(DOLAPool, idList, input$comp, input$outChk)
   
    output$outPlot <- renderPlotly(DG_Out$PLOT)

  } #  input$unit
  
  }) # input$Profile

  }  #server



shinyApp(ui = ui, server = server)

