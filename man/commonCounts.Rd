\name{commonCounts}
\alias{commonCounts}
\title{Compute and return counts of "common" word lists in passwords dump data frame}
\usage{
commonCounts(passwords)
}
\arguments{
  \item{passwords}{list as a result of call to
  makeCounts()}
}
\description{
Returns a list of computed frequency counts from strings in
the following built-in word lists: \itemize{ \item Worst 25
passwords on the internet \item Weekday names (full &
abbreviated) \item Month names (full & abbreviated) \item
Planets (including Pluto! \code{#fightthesystem}) \item
Common colors \item Seasons }
}
\examples{
passwords <- readPass("singles.org.txt")
counts <- commonCounts(passwords)
counts$colors$red$count
[1] 66
}

