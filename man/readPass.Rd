\name{readPass}
\alias{readPass}
\title{Build passwords data frame}
\usage{
readPass(file)
}
\arguments{
  \item{file}{the name of the file which the passwords are
  to be read from.}
}
\value{
data frame of passwords with columns for original strings,
lengths, basewords
}
\description{
Reads in a list of passwords (one per-line) and creates a
data frame from it with basewords, length ... columns
}
\examples{
passwords <- readPass("singles.org.txt")
}

