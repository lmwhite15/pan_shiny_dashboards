# https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
# I got the code below through the link above.

# library(rsconnect)
# 
# rsconnect::setAccountInfo(name='bonnies-sandbox', 
#                           token='017075910852C7BE39DB8265BA87CA26', 
#                           secret='K7RQYuCbi+aNB0tIAvxzZhuTx6uiODL+L2FTTFYU')
# 
# # Make sure custom.css is in workspace
# deployApp(account = 'bonnies-sandbox',
#           appName = "hml_id_assignment",
#           appTitle = "PAN HML ID Assignment")

rsconnect::connectApiUser(account='tyuhas',                     
                          apiKey='REV0hmc9aLXey926qoRBUo6gNJr7nGfx')

rsconnect::deployApp(account = 'tyuhas',          
          appName = 'hml_id_assignment',          
          appTitle = "PAN HML ID Assignment")

# Last deployed: 27-Jul-23
