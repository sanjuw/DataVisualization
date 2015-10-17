###########################
########## Lab 3 ##########
## By: Sanjeevni wanchoo ##
###########################

# Load Data
dir = "C:\\Users\\Sanjeevni\\Documents\\1 - Northwestern\\2015 Spring\\Visualization\\"
file = "04cars data.csv"
data = read.csv(paste0(dir,file))

### Problem 1
library(manipulate)

#Create a scatterplot for horsepower ("HP") against retail price ("Retail.Price") - horsepower on
#the x-axis and retail price on the y-axis. Add two sliders for the axis: "x.min" between 0 and 250,
#and "x.max" between 250 and 500.
manipulate(plot(data$HP, data$Retail.Price, main = "Car Price vs Horsepower", xlab="Horsepower", ylab = "Retail Price", xlim=c(x.min,x.max)), x.min=slider(0, 250, label="x-axis min"), x.max=slider(250, 500, label = "x-axis max"))

# For the same scatterplot as in part (a) create one slider for the x-axis between 0 and 500, and one
# slider for the y-axis between 10,000 and 200,000.
manipulate(plot(data$HP, data$Retail.Price,  main = "Car Price vs Horsepower", xlab="Horsepower", ylab = "Retail Price", xlim=c(0,x.max), ylim=c(0,y.max)), x.max=slider(0, 500, label="x-axis"), y.max=slider(10000, 200000, label = "y-axis"))

# Create a barplot for "City.MPG" with a picker for the following car 
# makes/combinations of car
# makes: Mazda, Audi, Toyota, Toyota and Audi.
mazda = grep("Mazda", data$Vehicle.Name, ignore.case=TRUE)
audi = grep("Audi", data$Vehicle.Name, ignore.case=TRUE)
toyota = grep("Toyota", data$Vehicle.Name, ignore.case=TRUE)
toyota_audi = c(toyota,audi)

data$City.MPG = as.numeric(data$City.MPG)
manipulate(hist(data[cartype,"City.MPG"], main="City MPG for Car Models", xlab="City MPG"), cartype=picker("Mazda" = mazda, "Audi" = audi, "Toyota" = toyota, "Toyota and Audi" = toyota_audi))

# Create a scatterplot of "City.MPG" versus "Retail.Price", which will contain the following: 
# slider for the x-axis between 5 and 65, by increments of 5 and initial value 25; 
# picker with points, line, and step (as shown in class); 
# checkbox with "draw labels".

manipulate(plot(data$City.MPG, data$Retail.Price, xlim=c(5, x.max), 
                type=type, ann=label, xlab="City MPG", ylab="Retail Price", 
                main = "City MPG vs Retail Price"), 
                x.max=slider(5,65,step=5,label="x-axis"), 
                type = picker("points" = "p", "line" = "l", "step" = "s"),
                label = checkbox(F, "Draw Labels"))

# For this part you will combine the manipulate package with the lattice package. 
# Select the first 20 rows of the "04cars data" set and, using the lattice package, 
# create density plots for "City.MPG" for cars with 4, 6 and 8 cylinders. 
# You must include a slider for the x-axis.
library(lattice)
data.subset = data[1:20,]
data.subset$Cyl = factor(data.subset$Cyl)
manipulate(densityplot(~data.subset$City.MPG | data.subset[, "Cyl"],
            xlim=c(0,x.max),
            main = "Density Plot", 
            xlab = "City Miles Per Gallon"),
            x.max = slider(1,25,label="x-axis"))


## Problem 2

# Create a histogram for "City.MPG". Create a selection with all 
# Toyota, Mazda and Audi vehicles in the histogram 
# (part of the histogram should turn red after you create the selection). 
# What proportion of the dataset was selected?
library(iplots)
library(JGR)
JGR()
# The below is to be entered in the iPlot GUI
attach(data)
ihist(City.MPG,xlab="Miles Per Gallon")
iset.select(grep(("Toyota|Mazda|Audi"),Vehicle.Name))

## Problem 3
library(googleVis)
library(MASS)
attach(Cairo)

## Part b
# Inspect the Cairo data set and fill in the the gaps ("") in the command above appropriately
# (leave the options list empty for the time being). Create a google calendar plot.
GCal <- gvisCalendar(Cairo, datevar = "Date", numvar = "Temp", options = list())
plot(GCal)

#You will notice that not all of the months are shown in the plot of part (i) 
# (can see up to half of August). Find a way to make all the months show up 
# (including the legend). Hint: you need to adjust the size of the figure.
GCal2 <- gvisCalendar(Cairo, datevar = "Date", numvar = "Temp", options = list(width=1000))
plot(GCal2)
#Add a title to your calendar chart. Also, change the font size of the years on 
# the side of the calendar to 30.
GCal3 <- gvisCalendar(Cairo, datevar = "Date", numvar = "Temp", 
                      options = list(title="Temperatures in Cairo", width=1000,
                      calendar="{yearLabel: { fontSize: 32 }}"))
plot(GCal3)

detach(Cairo)
## Part c
attach(Stock)

#Inspect the the Stock data set, fill in the gaps ("") in the command above appropriately, and
#plot the time line. Leave the options list empty.
GTimeline = gvisAnnotatedTimeLine(Stock, datevar = "Date", numvar = "Value", idvar = "Device", titlevar = "Title",
                      annotationvar = "Annotation", date.format = "%Y-%m-%d", options = list())
plot(GTimeline)

# Find a way to make the annotations visible on the time line.
GTimeline2 = gvisAnnotatedTimeLine(Stock, datevar = "Date", numvar = "Value", idvar = "Device", titlevar = "Title",
                                  annotationvar = "Annotation", date.format = "%Y-%m-%d", 
                                  options = list(displayAnnotations=TRUE))
plot(GTimeline2)

# Notice that there are two types of devices in the data set, namely pencils and pens.
# Since their values differ quite a bit, create two y-axes on the timeline - one for 
# pencils, and one for pens.
GTimeline3 = gvisAnnotatedTimeLine(Stock, datevar = "Date", numvar = "Value", idvar = "Device", titlevar = "Title",
                                   annotationvar = "Annotation", date.format = "%Y-%m-%d", 
                                   options = list(displayAnnotations=TRUE, scaleColumns='[0,1]',scaleType='allmaximized'))
plot(GTimeline3)











