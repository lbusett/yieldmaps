
# To run this package in shinyServer,
# copy this file in /srv/shiny-server/yieldmaps/app.R
# https://deanattali.com/2015/04/21/r-package-shiny-app/

dir <- system.file("app", package = "yieldmaps")
setwd(dir)
shiny::shinyAppDir(".")
