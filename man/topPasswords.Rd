\name{topPasswords}
\alias{topPasswords}
\title{Extract top "n" passwords from a password list}
\usage{
topPasswords(passwords, n = 10)
}
\arguments{
  \item{passwords}{a data frame of passwords}

  \item{n}{how many most prevalent passwords to include in
  the resultant data frame (default: 10)}
}
\value{
data frame of count & ratio of top 'n' passwords
}
\description{
Returns the top "n" (i.e. most prevalent) paasswords in the
passwords data frame
}
\examples{
passwords <- readPass("singles.org.txt")
top.20 <- topPasswords(passwords, 20)
}

