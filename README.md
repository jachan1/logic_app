## Synopsis

App for specifying node to node logic and calculating logic needed for form creation. 

## App

App is available online. The sample_logic.csv file shows an example algorithm. https://jcbch.shinyapps.io/logicEP. 

You can also run this app locally in RStudio by running the line shiny::runGitHub("logic_app", "jachan1").

To see the output upload the sample_logic.csv and then search "rua_xray" to see all of the paths to rua_xray.

## Input

The input csv file should have three columns in the order: Open, Start, Logic. The logic should define when you should go from the #Start# node to the specified #Open# node.
