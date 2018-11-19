getwd()
setwd("C:/Users/nirvi/Documents/sem3/ABA")

#installing packages
install.packages("cluster", repos = "https://cran.r-project.org")
install.packages("clustertend", repos = "https://cran.r-project.org")
install.packages("dbscan", repos = "https://cran.r-project.org")

#reading data
data <- read.csv("dow_jones_data.csv")
head(data)
colnames(data)

if (TRUE){
  df <- scale(data[-1]) # Standardize the data
} else{
  df <- data[-1] 
}
head(df)

#running k-means
k.means.fit <- kmeans(df, 3) # Perform k-means clustering with 3 clusters
attributes(k.means.fit) # Check the attributes that k-means generates
k.means.fit$centers # The locations of the centroids
k.means.fit$cluster # The cluster to which each observation belongs
k.means.fit$size # Check the size of each cluster

#To check the clustering tendency of the data, we can use library(clustertend)
library(clustertend)
hopkins(df, n = nrow(df)-1)

#determine a good value for the number of clusters
withinssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(df, nc=10) 

#dimentionality reduction and ploting it it 2D
# To create the clusters in 2-dimensional space:
library(cluster)
clusplot(df, k.means.fit$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, labels=2, lines=0)

#add the cluster label to the data
data$kmeans <- k.means.fit$cluster
