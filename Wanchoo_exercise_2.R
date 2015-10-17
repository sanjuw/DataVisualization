###############
#### Lab 2 ####
###############

## 1
dir = "C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\"
file = "04cars data.csv"
data = read.csv(paste0(dir,file))
library(corrgram)

#A
corrgram(data[,8:19], order = FALSE, upper.panel = NULL, lower.panel = panel.shade, text.panel=panel.txt, main = "Correlogram of Car Data")

#B
corrgram(data[,8:19], order = TRUE, upper.panel = panel.pie, lower.panel = panel.shade, text.panel=panel.txt, main = "Correlogram of Car Data", col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1",
                                                                                                                                             "darkkhaki","darkgreen")))
## 2
attach(data)

#A
x1 <- seq(0,10,length=500)
exp_dens <- dexp(x1, rate=3)
plot(x1,exp_dens, xlim=range(0,10), ylim=range(0,3.5), col="blue", xlab="x", ylab="density", main = "Density Plots")
par(new=TRUE)
chisq_dens <- dchisq(x1,df=1)
plot(x1,chisq_dens,col="red", axes=F, xlab="", ylab="")
legend("topright", c("exponential", "chi-sq"), fill=c("blue","red"), cex=0.8)

#B
x_exp = rexp(1000,rate=3)
y_chisq = rchisq(1000, df=1)

qqplot(y_chisq,x_exp, xlim=(range(x_exp,y_chisq)), ylim=range(x_exp,y_chisq),main="QQ plot", sub = "Exponential Random Variables", col="red")
qqline(x_exp,col='black')

# C
qqplot(y_chisq,x_exp ,main="QQ plot", sub = "Exponential Random Variables", col="red")
qqline(x_exp,col='black')


## 3

#A

data_loretail = data[data$Retail.Price<50000,]

# If we take horsepower range for entire data, then select only those with retail price <50000
# Gives Levels: (72.6,180] (180,286] (286,393] (393,500]
HP_cat = cut(data$HP, 4)
HP_cat = HP_cat[data$Retail.Price<50000]

bwplot(~Retail.Price|HP_cat,data=data_loretail, main="Retail Prices for Cars with Different Horsepower", 
        xlab="Horse Power Range Category", ylab="Retail Price")

# If we first select data with retail price <50000, and find the Horsepower range from this subset
# Gives Levels: (72.7,142] (142,212] (212,281] (281,350] 

HP_cat = cut(data_loretail$HP, 4)
bwplot(~Retail.Price|HP_cat,data=data_loretail, main="Retail Prices for Cars with Different Horsepower", 
        xlab="Horse Power Range Category", ylab="Retail Price")

# B
RP_cat = cut(data$Retail.Price,4)
xyplot(City.MPG~HP|RP_cat, data=data, xlab = "Horsepower", ylab = "City MPG", main = "City MPG vs Horsepower for cars in different Price Range")

