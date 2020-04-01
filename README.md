# Entries into the Final 3rd

This repository contains the code for the RShiny application that I build to support my article (https://npranav10.github.io/blog/entries-into-final-third.html) focussing on quantifying a team's attacking prowess in their attacking third.

```R
# Load the shiny package
library(shiny)

# Use runGitHub to soft download the app and run it in your local computer.
shiny::runGitHub("entries-into-final-third", "npranav10")
```
### Why isn't the app served online ?  
1. Initially I planned on hosting this app on shinyapps.io or running a shiny server in an AWS EC2 instacne, but ended up running out of memory in the former and running out of free-tier privileges (P.S : Void of free-tier status, any AWS service consumes a lot of my AWS Educate credits.) 
2. That's when I found that one can host the app.R code on GitHub can call the app from local computer's R Studio.
