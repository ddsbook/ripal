\name{makeWordListCounts}
\alias{makeWordListCounts}
\title{Generate frequency counts by substring for passwords in a passwords data frame}
\usage{
makeWordListCounts(x, passwords)
}
\arguments{
  \item{x}{a chacter vector to grep & count in source
  password list}

  \item{passwords}{data frame of passwords with basewords
  already computed}
}
\value{
list of the counts of each character string
}
\description{
Returns a list of the frequency count of string "x" found
in the original password string (orig column of 'passwords'
variable). The search performed is case insensitive.
Intended to be used in a *apply() function.
}
\examples{
passwords <- readPass("singles.org.txt")
printCounts(makeWordListCounts(months.abb, passwords), nrow(passwords))
}

