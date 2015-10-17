###########################
########## Lab 4 ##########
## By: Sanjeevni wanchoo ##
###########################

# Load Data
dir = "C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\"
file = "04cars data.csv"
data = read.csv(paste0(dir,file))

# Problem 1
# Using qplot() create a scatter plot of "HP" against "Retail.Price." Fit a 
# second degree polynomial through the data, i.e. Retail.Price = HP2. 
# Make sure that the datapoints and the fitted line are shown on the same figure.

qplot(HP, Retail.Price, data=data, geom=c("point","smooth"), 
      xlab="Horsepower", ylab="Retail Price", main="Retail Price vs Horsepower",
      formula = y ~ poly(x, 2), method="lm")

# Select all the cars with 4, 6 and 8 cylinders. For each cylinder category regress 
# (using "lm") "HP" on "Retail.Price." In total you need to have three regression
# lines. Remove the 95% confidence interval around the lines
data_sub = data[data$Cyl %in% c(4,6,8),]
data_sub_cyl = data_sub[data_sub$Cyl==i,]
qplot(HP, Retail.Price, data=data_sub, method="lm", formula = y~x,
      main = "Retail Price vs Horsepower", xlab="Horsepower",
      geom = "smooth", facets = .~Cyl, se=FALSE)  

# For the same subset of the data as in part (b) (cars with 4, 6 and 8 cylinders), 
# use ggplot() to a create boxplot for "City.MPG" for all three categories. 
# (The final figure should have 3 boxplots and "City.MPG" on the y-axis.) 
# Give a different fill color to each boxplot.

data_sub$City.MPG = as.numeric(data_sub$City.MPG)
data_sub$Cyl = as.factor(data_sub$Cyl)
ggplot(data_sub, aes(x=Cyl, y=City.MPG, fill=Cyl)) + geom_boxplot() + 
  ylab("Miles Per Gallon") + ggtitle("Boxplot of City Miles per Gallon")

# On the final figure from part (c) add another layer of boxplots in the following way:
# for every category of cylinders create two boxplots, one corresponding to sports 
# cars ("Sports.Car" = 1), and the other corresponding to non-sports cars 
# ("Sports.Car" = 0). Your resulting figure should have 6 boxplots. Use two fill 
# colors; one color for sports cars boxplots and another for non-sports
# cars boxplots. You must use ggplot().

data_sub$Sports.Car <- factor(data_sub$Sports.Car,levels=c(0,1),
                              labels=c("nonsports","sports")) 

ggplot() +   facet_wrap(Cyl~Sports.Car) +
geom_boxplot(data=data_sub, mapping=aes(x=Cyl, y=City.MPG, color=Sports.Car)) + 
  ylab("Miles Per Gallon") + ggtitle("Boxplot of City Miles per Gallon per Cyl type")
  
# Using ggplot() create a histogram for "City.MPG" conditional on whether a car 
# is a sports car or not (so you have to end up with 2 histograms). 
# Both histograms must be shown on the same figure on top of each other. 
# Use two different colors and set the transparency control alpha = 0.5.

ggplot(data_sub, aes(x=City.MPG, fill=Sports.Car)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity") + 
  ggtitle("Histograph for City MPG")


# Using ggplot() create a density plot for "Hwy.MPG" and draw a dashed line showing 
# the mean of "Hwy.MPG" and a straight line showing the mean of "City.MPG".

mean.Hwy <- mean(as.numeric(data_sub$Hwy.MPG))
mean.City <- mean(as.numeric(data_sub$City.MPG))
ggplot(data_sub, aes(x=Hwy.MPG)) + geom_density(alpha=.3) +
  geom_vline(data=data_sub, aes(xintercept=mean.Hwy), linetype="dashed", color = "red", size=1) +
  geom_vline(data=data_sub, aes(xintercept=mean.City), color = "red", size=1) + 
  ggtitle("Density plot for Highway Miles Per Gallon")
           

# Problem 2
#
library(plyr)
library(rgeos)
library(maptools)
library(sp)
library(rgdal)
dir = "C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\Labs"
np_dist <- readShapeSpatial(paste0(dir,"\\NPL_adm3.shp"))
plot(np_dist)
np_dist <- fortify(np_dist, region="NAME_3")
nepal<-read.csv(paste0(dir,"\\nepal.csv"),header=TRUE,na.strings=c("","*","NA"))
str(nepal)
np_dist$id <- toupper(np_dist$id)  #change ids to uppercase
distrpassave <- ddply(nepal, .(District), summarize, PassMean = mean(PASS.PERCENT))

# Part a
ggplot() + geom_map(data = distrpassave, aes(map_id = District, fill = PassMean), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat) 

# Part b
ggplot() + geom_map(data = distrpassave, aes(map_id = District, fill = PassMean), 
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat) +
  scale_fill_gradient2(low="red", mid = "white", midpoint = 50, high = "blue",limits=c(0,100))

# Part c
distlabels <- ddply(np_dist, .(id), summarize, long = mean(long), lat = mean(lat))

ggplot() + geom_map(data = distrpassave, aes(map_id = District, fill = PassMean),
                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)  + 
  scale_fill_gradient2(low="red",mid="white",high="blue",midpoint=50) +
  ggtitle("Nepal School Districts by Average Pass Percentages") + 
  geom_text(data = distlabels,aes(long,lat,label=id),size=2)

