#' server.R
#' 
#' ripal Shiny server-side main handler (Does all the heavy lifting) 
#' 
#' this is the initial version and needs some code cleanup and
#' definitely some user interface care & feeding
#' 

shinyServer(function(input, output) {
    
  results <- reactive({
    
    print("results()")
    
    useLocal <- input$useLocal
    
    dumpfile <- NULL 
    
    if (is.null(input$dumpfile)) {
      if (is.null(input$localDumpFile)) {
        return(NULL)
      } else {        
        pw.info <- file.info(sprintf("www/data/%s",input$localDumpFile))
        print(str(pw.info))
        dumpfile <- list(datapath=sprintf("www/data/%s", input$localDumpFile),
                         name=input$localDumpFile, 
                         size=pw.info$size)
        print(str(dumpfile))
      }
    } else {      
      dumpfile <- input$dumpfile      
    }
    
    passwords <- read.delim(dumpfile$datapath,
                            header=FALSE, 
                            col.names=c("orig"), 
                            blank.lines.skip = FALSE,
                            stringsAsFactors=FALSE)
    
    passwords <- data.table(passwords)
    tot <- nrow(passwords) 
    
    passwords$basewords <- gsub("^[^a-z]*", "", passwords$orig, ignore.case=TRUE)
    passwords$basewords <- gsub("[^a-z]*$", "", passwords$basewords, ignore.case=TRUE)
    passwords$basewords <- tolower(passwords$basewords)
    
    passwords$len <- nchar(passwords$orig)
    
    passwords$last.num <- as.numeric(str_extract(passwords$orig, "[0-9]$"))
    
    passwords$last.2 <- str_extract(passwords$orig, "[0-9]{2}$")
    passwords$last.3 <- str_extract(passwords$orig, "[0-9]{3}$")
    passwords$last.4 <- str_extract(passwords$orig, "[0-9]{4}$")
    passwords$last.5 <- str_extract(passwords$orig, "[0-9]{5}$")
        
    return(list(filename=dumpfile$name, bytes=dumpfile$size, passwords=passwords, tot=tot))
    
  })
  
  #' helper function to display tabular data
  
  printCounts <- function(ct) {
    p <- results()$passwords
    tmp <- data.frame(Term=names(ct), Count=as.numeric(unlist(ct)))
    tmp$Percent <- sprintf("%3.2f%%", ((tmp$Count / results()$tot) * 100))
    print(tmp[order(-tmp$Count),])
  }
  
  #' each "output" function follows the Shiny pattern, rendering
  #' whatever the element is on the ui.R side. Most of these
  #' key off of changes to the dumpfile, but some reference the
  #' input$topN variable from ui.R and will re-render when that
  #' reactive value changes. 
    
  output$overview1 <- renderText({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return("No file selected") }
    return(sprintf("File: %s (%s lines/%s bytes)",
                   results()$filename, 
                   format(results()$tot, big.mark=",", scientific=FALSE), 
                   format(results()$bytes, big.mark=",", scientific=FALSE)))
  })
  
  output$top1 <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords
    top.n <- as.data.frame(head(sort(table(p$orig), decreasing=TRUE), input$topN))
    top.n$Password <- rownames(top.n)
    rownames(top.n) <- NULL
    top.n <- top.n[,c(2,1)]
    colnames(top.n) <- c("Password","Count")
    top.n$Percent <- sprintf("%3.2f%%", ((top.n$Count / results()$tot) * 100))
    print(top.n)
  }, include.rownames=FALSE)
  
  output$topBasewords <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords
    basewords <- as.data.frame(head(sort(table(p[nchar(p$basewords)>3,]$basewords), decreasing=TRUE), input$topN))
    basewords$Password <- rownames(basewords)
    rownames(basewords) <- NULL
    basewords <- basewords[,c(2,1)]
    colnames(basewords) <- c("Password","Count")
    basewords$Percent <- sprintf("%3.2f%%", ((basewords$Count / results()$tot) * 100))
    print(basewords)
  }, include.rownames=FALSE)
  
  output$topLen <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords
    by.length <- as.data.frame(table(p$len))
    colnames(by.length) <- c("Password","Count")
    by.length$Percent <- sprintf("%3.2f%%", ((by.length$Count / results()$tot) * 100))
    print(by.length)
  }, include.rownames=FALSE)
  
  output$topFreq <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords
    length.tab <- table(p$len)
    by.freq <- as.data.frame(table(factor(p$len, 
                                          levels = names(length.tab[order(length.tab, decreasing = TRUE)]))))
    colnames(by.freq) <- c("Password","Count")
    by.freq$Percent <- sprintf("%3.2f%%", ((by.freq$Count / results()$tot) * 100))
    print(by.freq)
  }, include.rownames=FALSE)
  
  output$pwLenFreq <- renderPlot({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords
    length.tab <- table(p$len)
    plot(length.tab, col="steelblue", main="Password Length Frequency",
         xlab="Password Length", ylab="Count")    
  })

  output$pwCompStats <- renderText({
    
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    
    p <- results()$passwords
    
    one.to.six <- nrow(p[p$len>=1 & p$len<=6,])
    one.to.eight <- nrow(p[p$len>=1 & p$len<=8,])
    nine.plus <- nrow(p[p$len>8,])
    
    only.lower.alpha <- sum(grepl("^[a-z]+$",p$orig))
    only.upper.alpha <- sum(grepl("^[A-Z]+$",p$orig))
    only.alpha <- only.lower.alpha + only.upper.alpha
    
    only.numeric <- sum(grepl("^[0-9]+$",p$orig))
    
    first.cap.last.sym <- sum(grepl("^[A-Z].*[[:punct:]]$",p$orig))
    first.cap.last.num <- sum(grepl("^[A-Z].*[0-9]$",p$orig))
    
    singles.on.end <- sum(grepl("[^0-9]+([0-9]{1})$", p$orig))
    doubles.on.end <- sum(grepl("[^0-9]+([0-9]{2})$", p$orig))
    triples.on.end <- sum(grepl("[^0-9]+([0-9]{3})$", p$orig))
    
    o <- c(sprintf("One to six characters = %d, (%3.3f%%)", one.to.six, 100*(one.to.six/tot)),
    sprintf("One to eight characters = %d, (%3.3f%%)", one.to.eight, 100*(one.to.eight/tot)),
    sprintf("More than eight characters = %d, (%3.3f%%)", nine.plus, 100*(nine.plus/tot)),
    sprintf("Only lowercase alpha = %d, (%3.3f%%)", only.lower.alpha, 100*(only.lower.alpha/tot)),
    sprintf("Only uppercase alpha = %d, (%3.3f%%)", only.upper.alpha, 100*(only.upper.alpha/tot)),
    sprintf("Only alpha = %d, (%3.3f%%)", only.alpha, 100*(only.alpha/tot)),
    sprintf("Only numeric = %d, (%3.3f%%)", only.numeric, 100*(only.numeric/tot)),
    sprintf("First capital last symbol = %d, (%3.3f%%)", first.cap.last.sym, 100*(first.cap.last.sym/tot)),
    sprintf("First capital last number = %d, (%3.3f%%)", first.cap.last.num, 100*(first.cap.last.num/tot)),
    sprintf("Single digit on the end = %d, (%3.3f%%)", singles.on.end, 100*(singles.on.end/tot)),
    sprintf("Two digits on the end = %d, (%3.3f%%)", doubles.on.end, 100*(doubles.on.end/tot)),
    sprintf("Three digits on the end = %d, (%3.3f%%)", doubles.on.end, 100*(doubles.on.end/tot)))
    
    print(sprintf("%s<br/>",o))
    
  })  
  
  output$worst25 <- renderTable({    
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    worst.ct <- sapply(worst.pass, function(x) { return(x=list("count"=sum(grepl(x, results()$p$orig, ignore.case=TRUE))))}, simplify=FALSE)
    printCounts(worst.ct)    
  }, include.rownames=FALSE)
  
  output$weekdaysFull <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    printCounts(sapply(weekdays.full, function(x) { return(x=list("count"=sum(grepl(x, results()$p$orig, ignore.case=TRUE))))}, simplify=FALSE))
  }, include.rownames=FALSE)
  
  output$weekdaysAbbrev <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    printCounts(sapply(weekdays.abbrev, function(x) { return(x=list("count"=sum(grepl(x, results()$p$orig, ignore.case=TRUE))))}, simplify=FALSE))
  }, include.rownames=FALSE)
  
  output$monthsFull <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords        
    printCounts(sapply(months.full, function(x) { return(x=list("count"=sum(grepl(x, results()$p$orig, ignore.case=TRUE))))}, simplify=FALSE))
  }, include.rownames=FALSE)
  
  output$monthsAbbrev <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    printCounts(sapply(months.abbrev, function(x) { return(x=list("count"=sum(grepl(x, results()$p$orig, ignore.case=TRUE))))}, simplify=FALSE))
  }, include.rownames=FALSE)
  
  output$yearsTab <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    yrs <- as.character(1975:2030)
    printCounts(sapply(yrs, function(x) { return(x=list("count"=sum(grepl(x, results()$p$orig, ignore.case=TRUE))))}, simplify=FALSE))
  }, include.rownames=FALSE)  
  
  output$pwLastDigit <- renderPlot({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    last.num.factor <- factor(na.omit(p$last.num))
    plot(last.num.factor, col="steelblue", main="Count By Last digit")    
  })
  
  output$last2 <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    last.df <- as.data.frame(tail(sort(table(na.omit(p$last.2))), input$topN))
    last.df$Digits <- rownames(last.df)
    rownames(last.df) <- NULL
    last.df <- last.df[,c(2,1)]
    colnames(last.df) <- c("Last 2 Digits","Count")
    print(last.df)
  }, include.rownames=FALSE)
  
  output$last3 <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    last.df <- as.data.frame(tail(sort(table(na.omit(p$last.3))), input$topN))
    last.df$Digits <- rownames(last.df)
    rownames(last.df) <- NULL
    last.df <- last.df[,c(2,1)]
    colnames(last.df) <- c("Last 3 Digits","Count")
    print(last.df)
  }, include.rownames=FALSE)
  
  output$last4 <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    last.df <- as.data.frame(tail(sort(table(na.omit(p$last.4))), input$topN))
    last.df$Digits <- rownames(last.df)
    rownames(last.df) <- NULL
    last.df <- last.df[,c(2,1)]
    colnames(last.df) <- c("Last 4 Digits","Count")
    print(last.df)
  }, include.rownames=FALSE)
  
  output$last5 <- renderTable({
    if (is.null(input$dumpfile) & is.null(input$localDumpFile)) { return(NULL) }
    p <- results()$passwords    
    last.df <- as.data.frame(tail(sort(table(na.omit(p$last.5))), input$topN))
    last.df$Digits <- rownames(last.df)
    rownames(last.df) <- NULL
    last.df <- last.df[,c(2,1)]
    colnames(last.df) <- c("Last 5 Digits","Count")
    print(last.df)
  }, include.rownames=FALSE)
  
})

