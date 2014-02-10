ripal
=====

Password Dump Analysis in R, now with *extra crunchy package-y goodness*!

*Heavily* based on [pipal](https://github.com/digininja/pipal) by @digininja.

To run the command-line or GUI (shiny app) locally, you'll obviously need R and the `devtools` package. NOTE: If you're new to R, a good way to get started (especially if you're in infosec) is the book [Data Driven Security](http://amzn.to/ddsec) and the [sister blog](http://dds.ec/blog).

Here's how to install the package:

  library(devtools)
	install_github(username="ddsbook", repo="ripal")

We'll be making the usage a bit more straightforward over time, but you can do command-line analyses with rudimentary `pipal`-like report output by invoking R and calling the `analyzePasswordDump()` function like this:

	 R -q -e 'ripal::analyzePasswordDump("/path/to/singles.org.txt", "/path/to/singles-report.txt")'

(It should be pretty straightforwad to wrap that in an `alias` or shell script).

We also have version 0.1 of a GUI that is also pretty straightforward to run:

	R -q -e 'ripal::ripalApp()'

Again, that's simple enough to wrap in an `alias` or shell script.

![alt text](https://raw.github.com/ddsbook/ripal/master/ripal-shiny-screenshot.png "ripal-shiny screenshot")

Find out more over at [Data Driven Security](http://datadrivensecurity.info/blog/posts/2014/Feb/ripal/).
