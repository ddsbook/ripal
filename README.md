ripal
=====

Password Dump Analysis in R

*Heavily* based on [pipal](https://github.com/digininja/pipal) by @digininja.

**To run the GUI shiny app (`ripal-shiny`) locally**

- clone the repository: `git clone https://github.com/ddsbook/ripal.git`
- grab a free copy of [RStudio](http://www.rstudio.com/) & start it
- ensure the `shiny` package is installed: `install.packages('shiny')` and loaded: `library(shiny)`
- ensure the other library dependencies are installed: `install.packages(c("data.table","stringr"))`
- use `Session->Set Working Directory...` to select the `ripal-shiny` folder
- run the application (`runApp()`)

![alt text](https://raw.github.com/ddsbook/ripal/master/ripal-shiny-screenshot.png "ripal-shiny screenshot")

Find out more over at [Data Driven Security](http://datadrivensecurity.info/blog/posts/2014/Feb/ripal/).
