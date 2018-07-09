##################### Reading the Desired datasets into the environment ##################
rm(list=ls(all=T)) # Clear the global environment

##### Train
# Load the Data and drope the variables which will not add any value to the model
data=read.csv("abhi.csv")

str(data)
unique(data$NewID)
new=data

# Dropping the varaibales which will not add value in any model building purpose.
new$request_no=NULL
new$NewID=NULL
new$CostCentre_Code=NULL

# Variable conversion as per the data format which would be most suitable.
new$date_of_joining=as.character(new$date_of_joining)
new$date_of_joining=as.Date(new$date_of_joining,format='%b %d %Y')

new$date_of_birth=as.character(new$date_of_birth)
new$date_of_birth=as.Date(new$date_of_birth,format='%b %d %Y')

new$joinmonth=format(new$date_of_joining, "%m")
new$joinday=format(new$date_of_joining, "%d")
new$joinyear=format(new$date_of_joining, "%Y")

new$birthmonth=format(new$date_of_birth, "%m")
new$birthday=format(new$date_of_birth, "%d")
new$birthyear=format(new$date_of_birth, "%Y")

new$joinmonth=as.factor(new$joinmonth)
new$birthmonth=as.factor(new$birthmonth)

new$joinyear=as.factor(new$joinyear)
new$birthyear=as.factor(new$birthyear)

new$joinday=as.numeric(new$joinday)
new$birthday=as.numeric(new$birthday)

#####################################################


Numeric <- new[, sapply(new, is.numeric)]
Categorical <- as.data.frame(new[,sapply(new, is.factor)])

# Dummify the categorical data.
library(dummies)
Dummies <- dummy.data.frame(Categorical)
NumericScale <- scale(Numeric)

# Scale down the numeric attributes.
library(caret)
Scaled <- data.frame(NumericScale, Dummies)

