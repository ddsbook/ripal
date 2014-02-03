library(data.table)
library(stringr)

passwords <- fread("phpbb.txt")
setnames(passwords ,"V1", "orig") # decent label for the column
tot <- nrow(passwords) # we compute many ratios with this

top.10 <- as.data.frame(head(sort(table(passwords$orig), decreasing=TRUE),10))
top.10$Password <- rownames(top.10)
rownames(top.10) <- NULL
top.10 <- top.10[,c(2,1)]
colnames(top.10) <- c("Password","Count")
top.10$Percent <- sprintf("%3.2f%%", ((top.10$Count / tot) * 100))
print(top.10, row.names=FALSE)

passwords$basewords <- gsub("^[^a-z]*", "", passwords$orig, ignore.case=TRUE)
passwords$basewords <- gsub("[^a-z]*$", "", passwords$basewords, ignore.case=TRUE)
passwords$basewords <- tolower(passwords$basewords)

basewords <- factor(passwords[nchar(passwords$basewords)>3,]$basewords)

basewords <- as.data.frame(head(sort(table(passwords[nchar(passwords$basewords)>3,]$basewords), decreasing=TRUE),10))
basewords$Password <- rownames(basewords)
rownames(basewords) <- NULL
basewords <- basewords[,c(2,1)]
colnames(basewords) <- c("Password","Count")
basewords$Percent <- sprintf("%3.2f%%", ((basewords$Count / tot) * 100))
print(basewords, row.names=FALSE)

passwords$len <- nchar(passwords$orig)

by.length <- as.data.frame(table(passwords$len))
colnames(by.length) <- c("Password","Count")
by.length$Percent <- sprintf("%3.2f%%", ((by.length$Count / tot) * 100))
print(by.length, row.names=FALSE)

length.tab <- table(passwords$len)

by.freq <- as.data.frame(table(factor(passwords$len, 
                                      levels = names(length.tab[order(length.tab, decreasing = TRUE)]))))
colnames(by.freq) <- c("Password","Count")
by.freq$Percent <- sprintf("%3.2f%%", ((by.freq$Count / tot) * 100))
print(by.freq, row.names=FALSE)

plot(length.tab, col="steelblue", main="Password Length Frequency",
     xlab="Password Length", ylab="Count")

one.to.six <- nrow(passwords[passwords$len>=1 & passwords$len<=6,])
one.to.eight <- nrow(passwords[passwords$len>=1 & passwords$len<=8,])
nine.plus <- nrow(passwords[passwords$len>8,])

only.lower.alpha <- sum(grepl("^[a-z]+$",passwords$orig))
only.upper.alpha <- sum(grepl("^[A-Z]+$",passwords$orig))
only.alpha <- only.lower.alpha + only.upper.alpha

first.cap.last.sym <- sum(grepl("^[A-Z].*[[:punct:]]$",passwords$orig))
first.cap.last.num <- sum(grepl("^[A-Z].*[0-9]$",passwords$orig))

print(sprintf("One to six characters = %d, (%3.3f%%)", one.to.six, 100*(one.to.six/tot)))
print(sprintf("One to eight characters = %d, (%3.3f%%)", one.to.eight, 100*(one.to.eight/tot)))
print(sprintf("More than eight characters = %d, (%3.3f%%)", nine.plus, 100*(nine.plus/tot)))
print(sprintf("Only lowercase alpha = %d, (%3.3f%%)", only.lower.alpha, 100*(only.lower.alpha/tot)))
print(sprintf("Only uppercase alpha = %d, (%3.3f%%)", only.upper.alpha, 100*(only.upper.alpha/tot)))
print(sprintf("Only alpha = %d, (%3.3f%%)", only.alpha, 100*(only.alpha/tot)))
print(sprintf("Only numeric = %d, (%3.3f%%)", only.numeric, 100*(only.numeric/tot)))
print(sprintf("First capital last symbol = %d, (%3.3f%%)", first.cap.last.sym, 100*(first.cap.last.sym/tot)))
print(sprintf("First capital last number = %d, (%3.3f%%)", first.cap.last.num, 100*(first.cap.last.num/tot)))

makeCounts <- function(x) {
  return(x=list("count"=sum(grepl(x, passwords$orig, ignore.case=TRUE))))  
}

printCounts <- function(ct) {
  tmp <- data.frame(Term=names(ct), Count=as.numeric(unlist(ct)))
  tmp$Percent <- sprintf("%3.2f%%", ((tmp$Count / tot) * 100))
  print(tmp[order(-tmp$Count),], row.names=FALSE)
}

worst.pass <- c("password", "123456", "12345678", "qwerty", "abc123", 
                "monkey", "1234567", "letmein", "trustno1", "dragon", 
                "baseball", "111111", "iloveyou", "master", "sunshine", 
                "ashley", "bailey", "passw0rd", "shadow", "123123", 
                "654321", "superman", "qazwsx", "michael", "football")

worst.ct <- sapply(worst.pass, makeCounts, simplify=FALSE)
printCounts(worst.ct)

weekdays.full <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
weekdays.abbrev <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")

months.full <- tolower(month.name)
months.abbrev <- tolower(month.abb)

yrs <- as.character(1975:2030)

printCounts(sapply(weekdays.full, makeCounts, simplify=FALSE))
printCounts(sapply(weekdays.abbrev, makeCounts, simplify=FALSE))
printCounts(sapply(months.full, makeCounts, simplify=FALSE))
printCounts(sapply(months.abbrev, makeCounts, simplify=FALSE))
printCounts(sapply(yrs, makeCounts, simplify=FALSE))

singles.on.end <- sum(grepl("[^0-9]+([0-9]{1})$", passwords$orig))
doubles.on.end <- sum(grepl("[^0-9]+([0-9]{2})$", passwords$orig))
triples.on.end <- sum(grepl("[^0-9]+([0-9]{3})$", passwords$orig))

print(sprintf("Single digit on the end = %d, (%3.3f%%)", singles.on.end, 100*(singles.on.end/tot)))
print(sprintf("Two digits on the end = %d, (%3.3f%%)", doubles.on.end, 100*(doubles.on.end/tot)))
print(sprintf("Three digits on the end = %d, (%3.3f%%)", doubles.on.end, 100*(doubles.on.end/tot)))

passwords$last.num <- as.numeric(str_extract(passwords$orig, "[0-9]$"))
last.num.factor <- factor(na.omit(passwords$last.num))
plot(last.num.factor, col="steelblue", main="Count By Last digit")
summary(last.num.factor)
last.num <- as.data.frame(table(last.num.factor))
colnames(last.num) <- c("Digit","Count")
last.num$Percent <- sprintf("%3.2f%%", ((last.num$Count / tot) * 100))
print(last.num, row.names=FALSE)

passwords$last.2 <- str_extract(passwords$orig, "[0-9]{2}$")
passwords$last.3 <- str_extract(passwords$orig, "[0-9]{3}$")
passwords$last.4 <- str_extract(passwords$orig, "[0-9]{4}$")
passwords$last.5 <- str_extract(passwords$orig, "[0-9]{5}$")

print(tail(sort(table(na.omit(passwords$last.2))),10))
print(tail(sort(table(na.omit(passwords$last.3))),10))
print(tail(sort(table(na.omit(passwords$last.4))),10))
print(tail(sort(table(na.omit(passwords$last.5))),10))

