
# library(rsconnect)

# rsconnect::setAccountInfo(name='bonnies-sandbox',
#                           token='017075910852C7BE39DB8265BA87CA26',
#                           secret='K7RQYuCbi+aNB0tIAvxzZhuTx6uiODL+L2FTTFYU')
# 
# # Make sure custom.css is in workspace
# rsconnect::deployApp(account = 'bonnies-sandbox',
#           appName = "pan_game_dashboard",
#           appTitle = "Healthy Minds For Life Games Tracker")

rsconnect::connectApiUser(account='lisamwhite',                     
                          apiKey='FzriDTvWO4eZ3BpdnOSt6qSn0B2rKiIl')

deployApp(account = 'lisamwhite',       
          server = "viz.datascience.arizona.edu",
          appName = "pan_game_dashboards",          
          appTitle = "Healthy Minds For Life Games Tracker")

# Last deployed: 20-Jul-23
