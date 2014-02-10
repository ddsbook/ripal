\name{printCounts}
\alias{printCounts}
\title{Print the counts and ratio of found terms via makeCounts(x, passwords)}
\usage{
printCounts(ct, tot)
}
\arguments{
  \item{list}{as a result of call to makeCounts()}

  \item{total}{passwords in password list}
}
\description{
Print the counts and ratio of found terms via makeCounts(x,
passwords)
}
\examples{
passwords <- readPass("singles.org.txt")
printCounts(makeWordListCounts(months.abb, passwords), nrow(passwords))
}

