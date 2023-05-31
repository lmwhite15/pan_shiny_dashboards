# https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
# I got the code below through the link above.

library(rsconnect)

rsconnect::setAccountInfo(name='bonnies-sandbox', 
                          token='017075910852C7BE39DB8265BA87CA26', 
                          secret='K7RQYuCbi+aNB0tIAvxzZhuTx6uiODL+L2FTTFYU')

# Make sure custom.css is in workspace
deployApp(account = 'bonnies-sandbox',
          appName = "hml_id_assignment",
          appTitle = "PAN HML ID Assignment")

# Last deployed: 30-May-23


# Get files from shinyapps.io bundle download
untar(tarfile = "C:/Users/Lisa/Downloads/1f32e16ef42647f58e8fa929c7245d20 (1).tar", 
      exdir = "C:/Users/Lisa/Downloads/")
