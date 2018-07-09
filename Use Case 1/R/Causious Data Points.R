'''{r}
Clustering Based Suspecious Data Point Detection Technique

The k-means clustering technique.

In the k-means based Suspecious detection technique the data are partitioned in to k groups by assigning them to the closest cluster centers.

Once assigned we can compute the distance or dissimilarity between each object and its cluster center, and pick those with largest distances as outliers.

'''

# Load the Data and drope the variables which will not add any value to the model
require(xlsx)
data=read.xlsx("usecase1.xlsx", sheetName = "RapidMiner Data")

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

str(new)
####################################################
################ K Means Clustering #############################3

Numeric <- new[, sapply(new, is.numeric)]
Categorical <- as.data.frame(new[,sapply(new, is.factor)])

# Dummify the categorical data.
library(dummies)
Dummies <- dummy.data.frame(Categorical)
NumericScale <- scale(Numeric)

# Scale down the numeric attributes.
library(caret)
Scaled <- data.frame(NumericScale, Dummies)
newdf <- data.frame(Numeric, Dummies)

# K means clustering
KMeanClusters <- kmeans(Scaled, centers = 8, iter.max = 10)
KMeanClusters
KMeanClusters$withinss
KMeanClusters$betweenss

library(cluster)
clusplot(Scaled, KMeanClusters$cluster, main = '2D representation of the Cluster solution',
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

# Extract the outliers based on the maximum distance from the centroid.
centers <- KMeanClusters$centers[KMeanClusters$cluster, ] # "centers" is a data frame of 5 centers but the length of main dataset so we can canlculate distance difference easily.
distances <- sqrt(rowSums((Scaled - centers)^2))
outliers <- order(distances, decreasing=T)[1:6]
print(outliers) # these rows are 5 top outliers

outlierpoints=Scaled[outliers,]

# Plot the outlier point and which is the main variable adds to the point of suspecious dta point extraction.
plot(newdf[,c("bill_amount", "No..of.Tour.Days")], pch=19, col=KMeanClusters$cluster, cex=1)
points(KMeanClusters$centers[,c("bill_amount", "No..of.Tour.Days")], col=1:3, pch=15, cex=2)
points(newdf[outliers, c("bill_amount", "No..of.Tour.Days")], pch="+", col=4, cex=3)

plot(newdf[,c("bill_amount", "Distance.in.KMs")], pch=19, col=KMeanClusters$cluster, cex=1)
points(KMeanClusters$centers[,c("bill_amount", "Distance.in.KMs")], col=1:3, pch=15, cex=2)
points(newdf[outliers, c("bill_amount", "Distance.in.KMs")], pch="+", col=4, cex=3)

plot(newdf[,c("bill_amount", "joinday")], pch=19, col=KMeanClusters$cluster, cex=1)
points(KMeanClusters$centers[,c("bill_amount", "joinday")], col=1:3, pch=15, cex=2)
points(newdf[outliers, c("bill_amount", "joinday")], pch="+", col=4, cex=3)

plot(newdf[,c("bill_amount", "birthday")], pch=19, col=KMeanClusters$cluster, cex=1)
points(KMeanClusters$centers[,c("bill_amount", "birthday")], col=1:3, pch=15, cex=2)
points(newdf[outliers, c("bill_amount", "birthday")], pch="+", col=4, cex=3)

######################## Pca Based Outlier Data Point#########################
# compute PCs
pca.out = princomp(Scaled)
# princomp(wine.predictors, cor=TRUE) would
# automatically scale
names(pca.out)
summary(pca.out)
pca.out$loadings
plot(pca.out)

library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pca.out, obs.scale = 0.1, var.scale = 0.1
              , ellipse = TRUE, circle = TRUE)
g
