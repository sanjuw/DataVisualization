###########################
########## Lab 5 ##########
## By: Sanjeevni wanchoo ##
###########################
library(shiny)
# Load Data
dir = "C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\"
file = "04cars data.csv"
data = read.csv(paste0(dir,file))

setwd("C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\Labs\\Lab 5 apps")
runApp("lab5")
