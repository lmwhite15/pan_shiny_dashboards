# https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
# I got the code below through the link above.

library(rsconnect)

rsconnect::setAccountInfo(name='bonnies-sandbox', 
                          token='017075910852C7BE39DB8265BA87CA26', 
                          secret='K7RQYuCbi+aNB0tIAvxzZhuTx6uiODL+L2FTTFYU')

# Make sure custom.css is in workspace
deployApp(account = 'bonnies-sandbox',
          appName = "pan_recruitment_new",
          appTitle = "PAN Recruitment Dashboard")

# Last deployed: 25-May-23

