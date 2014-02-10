#' ui.R
#' 
#' ripal Shiny client-side renderer 
#' 

shinyUI(pageWithSidebar(

  headerPanel("ripal - password dump analysis in R"),
  
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
    
    helpText("Select your own cracked password dump (ASCII/UTF-8, pls) ",
             "or choose from an existing password dump in the list ",
             "and get some spiffy stats in return! Large password ",
             "dumps will take a while, so pls be kind to the server."),
    
    selectInput("localDumpFile",
                "Choose from existing lists:",
                c("hak5.txt", "hotmail.txt", "myspace.txt", 
                  "phpbb.txt", "singles.org.txt"),
                selected="hak5.txt"),
    
    div(HTML("<b>OR</b>")),
    
    fileInput('dumpfile', 
              'Choose a password dump to analyze:', 
              accept=c('text/plain')),
    
    numericInput("topN", 
                 "'Top #' lists max items:", 
                 10, 5, 30, step=1),
    
    sliderInput("dateRange","Date Range (for 3rd tab)", min=1975 , max=2050, value=c(1990,2020), step=1),
            
    div(HTML("You can find many password dumps at <a href='https://wiki.skullsecurity.org/Passwords'>SkullSecurity</a>.<hr/>")),
    
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
                       tableOutput("topBasewords"))),
               plotOutput("top1Chart"),
               plotOutput("topBasewordsChart")
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
      
      tabPanel("Word List/Dates Analyses",
               h4("25 'Worst' Internet Passwords Corpus Counts"), 
               dataTableOutput("worst25"),
               br(),
               h4("Weekdays (Full) Corpus Counts"), 
               dataTableOutput("weekdaysFullDT"),
               br(),
               h4("Weekdays (Abbrev) Corpus Counts"), 
               dataTableOutput("weekdaysAbbrevDT"),
               br(),
               h4("Months (Full) Corpus Counts"), 
               dataTableOutput("monthsFullDT"),
               br(),
               h4("Months (Abbrev) Corpus Counts"), 
               dataTableOutput("monthsAbbrevDT"),
               br(),
               h4(textOutput("yearRangeTitle")), 
               dataTableOutput("yearsDT"),
               br(),
               h4("Colors Corpus Counts"), 
               dataTableOutput("colorsDT"),
               br(),
               h4("Seasons Corpus Counts"), 
               dataTableOutput("seasonssDT"),
               br(),
               h4("Planets Corpus Counts"), 
               dataTableOutput("planetsDT"),
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