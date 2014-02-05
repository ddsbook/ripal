#' ui.R
#' 
#' ripal Shiny client-side renderer 
#' 

shinyUI(pageWithSidebar(

  headerPanel("ripal - password dump analysis"),
  
  sidebarPanel(
    tags$head( 
      tags$link(rel="stylesheet", 
                type="text/css", 
                href="ripal.css"),
      
      tags$link(rel="stylesheet", 
                type="text/css", 
                href="http://openfontlibrary.org/face/fantasque-sans-mono"),
      
      tags$link(rel="stylesheet", 
                type="text/css", 
                href="http://fonts.googleapis.com/css?family=Lato:400,700,400italic")        
    ), 
    
    helpText("Upload a cracked password dump (ASCII/UTF-8, pls) ",
             "and get some spiffy stats in return! Large password ",
             "dumps will take a while, so pls be kind to the server."),
      
    helpText("For really large cracked password dumps, you're better off ",
             "running this locally in RStudio or on your own instance. ",
             "We will severely limit the functionality if abuse is detected."),
    
    div(HTML("If you need a test file, grab the '<a href='https://raw.github.com/ddsbook/ripal/master/singles.org.txt'>singles.org</a>' cracked password dump from our github repository or from the original dump at <a href='https://wiki.skullsecurity.org/Passwords'>SkullSecurity</a>.")),
    
    tags$hr(),
    
    fileInput('dumpfile', 
              'Choose password dump to analyze:', 
              accept=c('text/plain')),
    
    numericInput("topN", 
                 "Limit most 'Top N' lists to this many items:", 
                 10, 5, 30, step=1),
    
    tags$hr(),
    
    div(HTML("Source at: <a href='https://github.com/ddsbook/ripal'>github</a>")),
    br(),
    div(HTML("Another app brought to you by <a href='http://datadrivensecurity.info/'>Data Driven Security</a>"))
        
  ),
  
  mainPanel(
    
    tabsetPanel(
      
      tabPanel("Overview",
               htmlOutput("overview1"),
               br(),
               div(class="topContainer",
                   div(class="topDiv", 
                       strong("Top Passwords"), 
                       tableOutput("top1")),
                   div(class="topDiv", 
                       strong("Top Basewords"), 
                       tableOutput("topBasewords")))
      ), 
      
      tabPanel("Length/Composition Analyses",
               div(class="topContainer",
                   div(class="topDiv", 
                       strong("Top By Length"), 
                       tableOutput("topLen")),
                   div(class="topDiv", 
                       strong("Top By Freq"), 
                       tableOutput("topFreq"))),
               br(),
               plotOutput("pwLenFreq"),
               br(),
               htmlOutput("pwCompStats"),
               br()
      ),
      
      tabPanel("Word List Analyses",
               div(class="topDiv", 
                   strong("25 'Worst' Internet Passwords Corpus Counts"), 
                   tableOutput("worst25")),
               br(),
               div(class="topContainer",
                   div(class="topDiv", 
                       strong("Weekdays (Full) Corpus Counts"), 
                       tableOutput("weekdaysFull")),
                   div(class="topDiv",
                       strong("Weekdays (Abbrev) Corpus Counts"), 
                       tableOutput("weekdaysAbbrev"))),
               br(),
               div(class="topContainer",
                   div(class="topDiv", 
                       strong("Months (Full) Corpus Counts"), 
                       tableOutput("monthsFull")),
                   div(class="topDiv", 
                       strong("Months (Abbrev) Corpus Counts"),
                       tableOutput("monthsAbbrev"))),
               br(),
               div(class="topDiv", 
                   strong("Years (1975-2030) Corpus Counts"), 
                   tableOutput("yearsTab")),
               br()
      ),
      
      tabPanel("Last Digit(s) Analyses",
               plotOutput("pwLastDigit"),               
               br(),
               div(class="topContainer",
                   div(class="topDiv", tableOutput("last2")),
                   div(class="topDiv", tableOutput("last3"))),
               br(),
               div(class="topContainer",
                   div(class="topDiv", tableOutput("last4")),
                   div(class="topDiv", tableOutput("last5"))),
               br()
      ),      
      
      id="tabs"
      
    )
    
  )
  
))