\name{charStats}
\alias{charStats}
\title{Compute various "character" statistics from passwords in a passwords data frame}
\usage{
charStats(passwords)
}
\arguments{
  \item{passwords}{a data frame of passwords with basewords
  already computed}
}
\value{
list of character stats (row & percentage)
}
\description{
Returns a list of character stats (raw and percentage) for
the passwords in the passwords data frame
}
\examples{
passwords <- readPass("singles.org.txt")
char.stats <- charStats(passwords)
char.stats$oneToSix
[1] 5083
}

