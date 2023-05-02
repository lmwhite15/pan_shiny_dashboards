
library(rsconnect)

rsconnect::setAccountInfo(name='bonnies-sandbox', 
                          token='017075910852C7BE39DB8265BA87CA26', 
                          secret='K7RQYuCbi+aNB0tIAvxzZhuTx6uiODL+L2FTTFYU')

# Make sure custom.css is in workspace
deployApp(account = 'bonnies-sandbox',
          appName = "pan_survey_dashboard",
          appTitle = "Healthy Minds For Life Survey Tracker")

# Last deployed: 01-May-23

