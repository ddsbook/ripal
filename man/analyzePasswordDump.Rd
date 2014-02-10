\name{analyzePasswordDump}
\alias{analyzePasswordDump}
\title{Analyze a password dump file and generate a report}
\usage{
analyzePasswordDump(file, output = "~/ripal-output.txt")
}
\arguments{
  \item{file}{the name of the file which the passwords are
  to be read from.}

  \item{output}{the name of the file to generate the report
  into (defaults to ~/ripal-output.txt)}
}
\description{
Takes a password dump file as input and generates a
\code{pipal}-like report.
}
\examples{
# from within code
analyzePasswordDump("singles.org.txt", "~/singles-report.txt")

# from the commmand line
R -q -e 'ripal::analyzePasswordDump("/usr/share/text/singles.org.txt", "~/reports/singles-rpt.txt")'
}

