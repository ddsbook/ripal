library(data.table)
library(stringr)
library(knitr)
library(shiny)

# "25 worst passwords"
worst.pass <- c("password", "123456", "12345678", "qwerty", "abc123", 
                "monkey", "1234567", "letmein", "trustno1", "dragon", 
                "baseball", "111111", "iloveyou", "master", "sunshine", 
                "ashley", "bailey", "passw0rd", "shadow", "123123", 
                "654321", "superman", "qazwsx", "michael", "football")

weekdays.full <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
weekdays.abbrev <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

months.full <- tolower(month.name)
months.abbrev <- tolower(month.abb)

yrs <- as.character(1968:2030)

common.colors <- c("black", "blue", "brown", "gray", "green", "orange", "pink", 
                   "purple", "red", "white", "yellow", "violet", "indigo")

seasons <- c("summer", "fall", "winter", "spring")

planets <- c("mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune", "pluto")

#' Build passwords data frame
#' 
#' Reads in a list of passwords (one per-line) and creates a data frame 
#' from it with basewords, length ... columns
#' 
#' @param file the name of the file which the passwords are to be read from.
#' @return data frame of passwords with columns for original strings, lengths, basewords
#' @export 
#' @examples
#' passwords <- readPass("singles.org.txt")

readPass <- function(file) {

  passwords <- read.delim(file, header=FALSE, col.names=c("orig"), stringsAsFactors=FALSE)
  passwords <- data.table(passwords)
  
  # make a column for lengths of the orig passwords
  passwords$len <- nchar(passwords$orig)
  
  # make a column for the basewords of the original passwords
  # "Basewords" are the remaining text from removing all non-alpha
  # characters from the beginnin and end of a password string
  
  passwords$basewords <- gsub("^[^a-z]*", "", passwords$orig, ignore.case=TRUE)
  passwords$basewords <- gsub("[^a-z]*$", "", passwords$basewords, ignore.case=TRUE)
  passwords$basewords <- tolower(passwords$basewords)
  
  passwords$last.num <- as.numeric(str_extract(passwords$orig, "[0-9]$"))
  
  passwords$last.2 <- str_extract(passwords$orig, "[0-9]{2}$")
  passwords$last.3 <- str_extract(passwords$orig, "[0-9]{3}$")
  passwords$last.4 <- str_extract(passwords$orig, "[0-9]{4}$")
  passwords$last.5 <- str_extract(passwords$orig, "[0-9]{5}$")
  
  return(passwords)
  
}

#' Extract top "n" passwords from a password list
#' 
#' Returns the top "n" (i.e. most prevalent) paasswords in the passwords
#' data frame
#' 
#' @param passwords a data frame of passwords
#' @param n how many most prevalent passwords to include in the resultant data frame (default: 10)
#' @return data frame of count & ratio of top 'n' passwords
#' @export 
#' @examples
#' passwords <- readPass("singles.org.txt")
#' top.20 <- topPasswords(passwords, 20)

topPasswords <- function(passwords, n=10) {
  
  top <- as.data.frame(head(sort(table(passwords$orig), decreasing=TRUE), n))
  top$Password <- rownames(top)
  rownames(top) <- NULL
  top <- top[,c(2,1)]
  colnames(top) <- c("Password", "Count")
  top$Percent <- ((top$Count / nrow(passwords)) * 100)
  
  return(top)

}

#' Extract top 'n' basewords from a password list
#' 
#' Returns a data frame of the count and ratio of the top "N" "basewords"
#' in the passwords data frame. 
#' 
#' @param passwords a data frame of passwords with basewords already computed
#' @param n how many most prevalent basewords to include in the resultant data frame (default: 10)
#' @return data frame of count & ratio of top "n" basewords
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' basewords <- topBasewords(passwords, 20)
#' 

topBasewords <- function(passwords, n=10) {
  
  basewords <- factor(passwords[nchar(passwords$basewords)>3,]$basewords)
  
  basewords <- as.data.frame(head(sort(table(passwords[nchar(passwords$basewords)>3,]$basewords), decreasing=TRUE), n))
  basewords$Password <- rownames(basewords)
  rownames(basewords) <- NULL
  basewords <- basewords[,c(2,1)]
  colnames(basewords) <- c("Baseword", "Count")
  basewords$Percent <- ((basewords$Count / nrow(passwords)) * 100)
  
  return(basewords)
  
}

#' Get password length statistics from a passwords data frame
#' 
#' Returns a data frame of the count and ratio of passwords by length
#' in the passwords data frame (ordered by length)
#' 
#' @param passwords data frame of passwords with basewords already computed
#' @return data frame of count & ratio of passwords by length, ordered by length
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' countByLen <- byLength(passwords)

byLength <- function(passwords) {
  
  by.length <- as.data.frame(table(passwords$len))
  colnames(by.length) <- c("Length", "Count")
  by.length$Percent <- ((by.length$Count / nrow(passwords)) * 100)
  
  return(by.length)

}

#' Return password length statistics (by frequency) from passwords in a passwords data frame
#' 
#' Returns a data frame of the count and ratio of passwords by length
#' in the passwords data frame (ordered by frequency)
#' 
#' @param passwords a data frame of passwords with basewords already computed
#' @return data frame of count & ratio of passwords by length, ordered by freq
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' countByFreq <- lengthFreq(passwords)

lengthFreq <- function(passwords) {

  length.tab <- table(passwords$len)
  
  by.freq <- as.data.frame(table(factor(passwords$len, 
                                        levels = names(length.tab[order(length.tab, decreasing = TRUE)]))))
  colnames(by.freq) <- c("Length", "Count")
  by.freq$Percent <- ((by.freq$Count / nrow(passwords)) * 100)
  
  return(by.freq)
  
}

#' Compute various "character" statistics from passwords in a passwords data frame
#' 
#' Returns a list of character stats (raw and percentage) for the passwords
#' in the passwords data frame
#' 
#' @param passwords a data frame of passwords with basewords already computed
#' @return list of character stats (row & percentage) 
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' char.stats <- charStats(passwords)
#' char.stats$oneToSix
#' [1] 5083

charStats <- function(passwords) {
  
  one.to.six <- nrow(passwords[passwords$len>=1 & passwords$len<=6,])
  one.to.eight <- nrow(passwords[passwords$len>=1 & passwords$len<=8,])
  nine.plus <- nrow(passwords[passwords$len>8,])
  
  only.numeric <- sum(grepl("^[0-9]+$",passwords$orig))
  
  only.lower.alpha <- sum(grepl("^[a-z]+$",passwords$orig))
  only.upper.alpha <- sum(grepl("^[A-Z]+$",passwords$orig))
  only.alpha <- only.lower.alpha + only.upper.alpha
  
  first.cap.last.sym <- sum(grepl("^[A-Z].*[[:punct:]]$",passwords$orig))
  first.cap.last.num <- sum(grepl("^[A-Z].*[0-9]$",passwords$orig))
  
  singles.on.end <- sum(grepl("[^0-9]+([0-9]{1})$", passwords$orig))
  doubles.on.end <- sum(grepl("[^0-9]+([0-9]{2})$", passwords$orig))
  triples.on.end <- sum(grepl("[^0-9]+([0-9]{3})$", passwords$orig))
  
  tot <- nrow(passwords)
  
  return(list(
    oneToSix=one.to.six, oneToSixPct=100*one.to.six/tot,
    oneToEight=one.to.eight, oneToEightPct=100*one.to.eight/tot,
    ninePlus=nine.plus, ninePlusPct=100*nine.plus/tot,
    onlyNumeric=only.numeric, onlyNumericPct=100*only.numeric/tot,
    onlyLower=only.lower.alpha, onlyLowerPct=100*only.lower.alpha/tot,
    onlyUpper=only.upper.alpha, onlyUpperPct=100*only.upper.alpha/tot,
    onlyAlpha=only.alpha, onlyAlphaPct=100*only.alpha/tot,
    firstCapLastSym=first.cap.last.sym, firstCapLastSymPct=100*first.cap.last.sym/tot,
    firstCapLastNum=first.cap.last.num, firstCapLastNumPct=100*first.cap.last.num/tot,
    singlesOnEnd=singles.on.end, singlesOnEndPct=100*singles.on.end/tot,
    doublesOnEnd=doubles.on.end, doublesOnEndPct=100*doubles.on.end/tot,
    triplesOnEnd=triples.on.end, triplesOnEndPct=100*triples.on.end/tot
  ))
  
}

#' Generate frequency counts by substring for passwords in a passwords data frame
#' 
#' Returns a list of the frequency count of string "x" found in the
#' original password string (orig column of 'passwords' variable).
#' The search performed is case insensitive. Intended to be
#' used in a *apply() function.
#' 
#' @param x a chacter vector to grep & count in source password list
#' @param passwords data frame of passwords with basewords already computed
#' @return list of the counts of each character string
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' printCounts(makeWordListCounts(months.abb, passwords), nrow(passwords))

makeWordListCounts <- function(x, passwords) {
  return(sapply(x, function(x, passwords) {
    return(x=list("count"=sum(grepl(x, passwords$orig, ignore.case=TRUE))))      
  }, passwords, simplify=FALSE))
}

#' Print the counts and ratio of found terms via makeCounts(x, passwords)
#' 
#' @param list as a result of call to makeCounts()
#' @param total passwords in password list
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' printCounts(makeWordListCounts(months.abb, passwords), nrow(passwords))

printCounts <- function(ct, tot) {
  tmp <- data.frame(Term=names(ct), Count=as.numeric(unlist(ct)))
  tmp$Percent <- sprintf("%3.2f%%", ((tmp$Count / tot) * 100))
  print(tmp[order(-tmp$Count),], row.names=FALSE)
}

#' Compute and return counts of "common" word lists in passwords dump data frame
#' 
#' Returns a list of computed frequency counts from strings in the following built-in word lists:
#' \itemize{
#' \item Worst 25 passwords on the internet
#' \item Weekday names (full & abbreviated)
#' \item Month names (full & abbreviated)
#' \item Planets (including Pluto! \code{#fightthesystem})
#' \item Common colors
#' \item Seasons
#' }
#' 
#' @param passwords list as a result of call to makeCounts()
#' @export
#' @examples
#' passwords <- readPass("singles.org.txt")
#' counts <- commonCounts(passwords)
#' counts$colors$red$count
#' [1] 66

commonCounts <- function(passwords) {
  return(list(
    worst=makeWordListCounts(worst.pass, passwords),
    weekdays=makeWordListCounts(weekdays.full, passwords),
    weekdaysAbbrev=makeWordListCounts(weekdays.abbrev, passwords),
    months=makeWordListCounts(months.full, passwords),
    monthsAbbrev=makeWordListCounts(months.abbrev, passwords),
    years=makeWordListCounts(yrs, passwords),
    planets=makeWordListCounts(planets, passwords),
    colors=makeWordListCounts(common.colors, passwords),
    seasons=makeWordListCounts(seasons, passwords)
  )) 
}

#' Analyze a password dump file and generate a report
#' 
#' Takes a password dump file as input and generates a \code{pipal}-like report.
#' 
#' @param file the name of the file which the passwords are to be read from.
#' @param output the name of the file to generate the report into (defaults to ~/ripal-output.txt)
#' @export 
#' @examples
#' # from within code
#' analyzePasswordDump("singles.org.txt", "~/singles-report.txt")
#' 
#' # from the commmand line
#' R -q -e 'ripal::analyzePasswordDump("/usr/share/text/singles.org.txt", "~/reports/singles-rpt.txt")'

analyzePasswordDump <- function(file, output="~/ripal-output.txt") {
  
  reportFile <- system.file(package="ripal", "Rmd", "ripal.Rmd")
  knitrenv <- new.env()
  assign("dumpfile", "~/Desktop/r/ripal/data/singles.org.txt", knitrenv)
  knit(reportFile, output, envir=knitrenv, quiet=TRUE)

}
#' Analyze a password dump file (GUI)
#' 
#' Presents a shiny app for password dump analysis
#' 
#' @export 
#' @examples
#' # from within code
#' ripalApp()
#' 
#' # from the commmand line
#' R -q -e 'ripal::ripalApp()'

ripalApp <- function() {
  runApp(system.file(package="ripal", "shiny"), launch.browser=TRUE)
}
