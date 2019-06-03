#Reading the dataset
pr<- read.csv("C:/Users/Yshah/Downloads/Rutgers Sem 2/MVA/Protein_Consumption.csv")
str(pr)
summary(pr)
hist(pr$Red.Meat)
hist(pr$White.Meat)
hist(pr$Egg)
hist(pr$Milk)
hist(pr$Fish)
hist(pr$Cereals)
hist(pr$Starchy.Foods)
hist(pr$Pulses.Nuts.and.Oilseeds)
hist(pr$Fruits.and.Vegetables)
hist(pr$Total)
pairs(pr[-1])
cor(pr[-1])

#Display first 5 line of data
head(pr)
#View(pr)
#install library
library("ggplot2")
library("corrplot")
library("reshape")

#dimensions of data set
dim(pr)

#Correlation matrix
#1 showing strong correaltion, -1 red showing weak correlation
corMatMy <- cor(pr[,2:10])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)


#Get the Correlations between the measurements
cor(pr[-1])
c <- (cor(pr[-1]))
#plot the correlation between white meat and read meat
plot(c)

# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one
pr_pca <- prcomp(pr[,-1],scale=TRUE)
#creation PC using prcomp fucntion
pr_pca
#plotting of PCA
#PCA aim is to reduce dimensionality 
#These new sets of variables, or principal components, are uncorrelated.
plot(pr_pca)
#The summary fucntions provides you the SD, Variance between PC
summary(pr_pca)
#
plot(eigen_pr, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")

# sample scores stored in pr_pca$x
# singular values (square roots of eigenvalues) stored in pr_pca$sdev
# loadings (eigenvectors) are stored in pr_pca$rotation
# variable means stored in pr_pca$center
# variable standard deviations stored in pr_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2


############################################CLUSTERING


protein_scaled=scale(pr[-1])
row.names(protein_scaled)=pr$Country
pr_dist <- dist(protein_scaled, method="euclidean")
c_single=hclust(pr_dist,method = "single")

par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(c_single),ylab="Distance between countries",ylim=c(0,6),
     main="Dendrogram")

c_single
cutree(c_single,h=3)
#4 clusters are formed 

c_complete=hclust(pr_dist)

par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(c_complete),ylab="Distance between countries",ylim=c(0,6),
     main="Dendrogram")
cutree(c_complete,h=5)

#5 clusters 


c_average=hclust(pr_dist,method="average")

par(mar=c(6, 4, 4, 2) + 0.1)
plot(as.dendrogram(c_average),ylab="Distance between countries",ylim=c(0,6),
     main="Dendrogram")

cutree(c_average,h=4)
#4 clusters

#4 clusters seems good , will try both 4 and 5 
k4= kmeans(protein_scaled,4,nstart=10)
plot(protein_scaled,col=(k4$cluster+1), pch=20, cex=2)

perc.var.4<- round(100*(1 - k4$betweenss/k4$totss),1)
perc.var.4

Common Factors are 
#RC1 =  Pulses.Nuts.and.Oilseeds ,White.Meat ,Egg , Cereals
#RC2 = RedMeat and Milk 
#Rc3 = Fish, Fruits and Vegetable , Starchy Foods
4 PC
#PC1 eggs, creals and pulses,nuts and oilseed
#PC2 Fish Starchy.Foods
#PC3 Fruits.and.Vegetables
#PC4 white.Meat

The countries are clustered in 4 segments have similarities
## [[1]]
##      Cluster 1 
## [1,] "Portugal"
## [2,] "Spain"   
## 
## [[2]]
##       Cluster 2       
##  [1,] "Austria"       
##  [2,] "Belgium"       
##  [3,] "Czechoslovakia"
##  [4,] "East Germany"  
##  [5,] "France"        
##  [6,] "Ireland"       
##  [7,] "Netherlands"   
##  [8,] "Poland"        
##  [9,] "Switzerland"   
## [10,] "United Kingdom"
## [11,] "West Germany"  
## 
## [[3]]
##      Cluster 3   
## [1,] "Albania"   
## [2,] "Bulgaria"  
## [3,] "Greece"    
## [4,] "Hungary"   
## [5,] "Italy"     
## [6,] "Romania"   
## [7,] "USSR"      
## [8,] "Yugoslavia"
## 
## [[4]]
##      Cluster 4
## [1,] "Denmark"
## [2,] "Finland"
## [3,] "Norway" 
## [4,] "Sweden"
