##############################
######### Exercise 1 #########
### By: Sanjeevni Wanchoo ####
##############################

## Problem 1 ## 
library(corrgram)

x_a = seq(-4,4,0.1)
x_b = seq(-2,2,0.1)
x_c = seq(-3,3,0.1)

y_a = sin(x_a)
y_b = x_b^2
y_c = (x_c>=0)*1

plot(x_a, y_a, xlim = range(c(x_a,x_b,x_c)), ylim = range(c(y_a,y_b,y_c)), col = "red", type="l", xlab = "x", ylab = "y", main="Line Charts y vs x")
par(new=TRUE)
plot(x_b,y_b, xlim = range(c(x_a,x_b,x_c)), ylim = range(c(y_a,y_b,y_c)), col = "green", type = "p", axes=F,xlab="",ylab="",)
par(new=TRUE)
plot(x_c, y_c, xlim = range(c(x_a,x_b,x_c)), ylim = range(c(y_a,y_b,y_c)), col = "blue", type = "o", axes=F,xlab="",ylab="",)
legend("topright", c("sin(x)", "x^2", "step function"), fill=c("red","green","blue"), cex=0.8)

## Problem 2 ##
dir = "C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\"
file = "04cars data.csv"
data = read.csv(paste0(dir,file))

#a
ind = grep("toyota", data$Vehicle.Name, ignore.case = TRUE)
toyota = data[ind,]
d = density(toyota$Engine.Size..l.)
plot(d, xlab="Engine Size", ylab="Density", main="Kernel Density plot of Engine Size for all Toyota Models")

#b
toyota.sorted = toyota[order(toyota$Engine.Size..l.),]
toyota.sorted$Sports.Car = factor(toyota.sorted$Sports.Car)

toyota.sorted$color[toyota.sorted$Sports.Car=="0"] = "red"
toyota.sorted$color[toyota.sorted$Sports.Car=="1"] = "blue"
dotchart(as.numeric(toyota$Engine.Size..l.),labels=toyota.sorted$Vehicle.Name,cex=.8,groups=toyota.sorted$Sports.Car,color=toyota.sorted$color, main="Engine Size for different Toyota Models")

#c
ind_f = grep("ford", data$Vehicle.Name, ignore.case = TRUE)
ford = data[ind_f,]

toyota$cartype = "Toyota"
ford$cartype = "Ford"
cars = rbind(toyota,ford)

sp.car = table(cars$Sports.Car, cars$cartype)
barplot(sp.car, col=c("red","blue"),xlab="0: Not a Sports Car, 1: Sports Car", ylab="Frequency", main = "Sports Car vs Non-Sports Car Distribution", legend = rownames(sp.car), beside=TRUE)

sp_or_sedan.car = table(cars$Small.Sporty..Compact.Large.Sedan, cars$cartype)
barplot(sp_or_sedan.car, col=c("red","blue"),xlab="0: Small Sporty, 1: Compact Large Sedan", , ylab="Frequency", main = "Small Sporty or Compact Large Sedan Distribution", legend=rownames(sp_or_sedan.car), beside=TRUE)






