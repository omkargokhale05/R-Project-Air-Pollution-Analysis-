#load the library
library(shiny)
library(RSQLite)
library(ggplot2)
library(shinythemes)
#define shiny ui
shinyUI( 
  fluidPage(
    titlePanel(title = h2("AIR POLUTION IN SUMMER AND WINTER!!",align="center")),
    sidebarLayout(
      ####################################### SideBar Panel ##############################################################################    
      sidebarPanel(h4("SELECT THE FOLLOWING:"),
        helpText("The Following selectInput drop down choices are dynamically populated based on dataset selected by User"),
        br(),
        #select input with list of datasets
        selectInput("season","SELECT THE SEASON",choices = c("summer_data","winter_data")),
        br(),
        uiOutput("v"), # v is coming from renderUI in server.r 
        br(),
        radioButtons("plot","SELECT THE TYPE OF GRAPH:",choices = c("Line","Points","Both_L_&_P","Stairs")),
        #radioButtons("color","Select the color of histograph",choices = c("Green","Red","Blue"),selected = "Green")
        # radioButtons("var","select the format:",choices = list("png","pdf"),inline=T)  
        shinythemes::themeSelector()
        ),
      ######################################## Main Panel ##############################################
      mainPanel(
                tabsetPanel(type="tab",
                            tabPanel("GRAPH",plotOutput("mygraph")),
                            tabPanel("CALCULATION","SEASON:",verbatimTextOutput("sname") ,br(),
                                     "MIN:",verbatimTextOutput("minm") ,br(),
                                     "MAX:",verbatimTextOutput("maxm"),br(),
                                     "MEAN:",verbatimTextOutput("mean") ,br(),
                                     "VARIANCE:",verbatimTextOutput("variance") ,br(),br(),
                                    h3( "Select the Pollutants :"),br(),
                                     uiOutput("c1"),br(),
                                     uiOutput("c2"),br(),
                                    "correlation coefficient:",br(),
                                     verbatimTextOutput("corr"),br(),br(),
                                    "Significance Level(p<0.001)",br(),
                                    verbatimTextOutput("sig"),br(),br()
                                    
                                    
                                     ),
                            tabPanel("T-Test Analysis",
                                     uiOutput("t1"),br(),
                                     uiOutput("t2"),br(),
                                     "t value:",br(),
                                     verbatimTextOutput("tvalue"),br(),br(),
                                     "Difference Between Means:",br(),
                                     verbatimTextOutput("dfmean"),br()
                                     
                                      ),
                            tabPanel("Query's AND Abstract View",
                                     "Q.WHAT IS CORRELATION COEFFICIENT DOES?",br(),
                                     "ANS:. The positive correlation indicates increase in corresponding concentration of one 
                                      pollutant with increase in concentration of other pollutant, while negative correlation 
                                      indicates corresponding decrease in concentration of one pollutant with increase in concentration 
                                      of the other pollutant.  ",br(),br(),
                                     "Q.What is T Test?",br(),
                                     "ANS:t test is used when we wish to compare difference between means or difference between two databases or difference between variance",br(),br(),
                                     h3("Abstract View Of Summer AND Winter Pollutants"),br(),br(),
                                     plotOutput("abview")
                                     )
                            #,
                           # tabPanel("Data",tableOutput("data")),
                            #tabPanel("Summary",tableOutput("summary"))
                           )))
      ################################## Main Panel End ##################################################
    ))
