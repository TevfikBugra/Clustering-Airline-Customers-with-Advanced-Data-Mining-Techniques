library(readxl)
library(data.table)
library(ggplot2)
library(cluster)
library(dplyr)

data = read_excel("EastWestAirlines.xlsx",sheet = 3) #reading the data


data = data[c(-1)] # removing ID variable from the data
setDT(data) # converting categorical miles data into numerical data based on given ranges and their mean values

data$cc1_miles = ifelse(data$cc1_miles==1,2500,
                        ifelse(data$cc1_miles==2,7500,
                               ifelse(data$cc1_miles==3,17500,
                                      ifelse(data$cc1_miles==4,32500,
                                             ifelse(data$cc1_miles==5,50000,0)))))

data$cc2_miles = ifelse(data$cc2_miles==1,2500,
                        ifelse(data$cc2_miles==2,7500,
                               ifelse(data$cc2_miles==3,17500,
                                      ifelse(data$cc2_miles==4,32500,
                                             ifelse(data$cc2_miles==5,50000,0)))))

data$cc3_miles = ifelse(data$cc3_miles==1,2500,
                        ifelse(data$cc3_miles==2,7500,
                               ifelse(data$cc3_miles==3,17500,
                                      ifelse(data$cc3_miles==4,32500,
                                             ifelse(data$cc3_miles==5,50000,0)))))


summary(data) # checking if there is any problem 

data_sc <- (scale(data)) # scaling the data 
summary(data_sc) # checking if there is any problem

dist_mat <- dist(data_sc, method = 'euclidean') # distance matrix 

#Part A


hc_complete=hclust(dist_mat, method="complete") # Hierarchical Clustering with 
plot(hc_complete,main="Complete Linkage", xlab="", cex=.9) #Euclidean distance and complete linkage


# finding the best k values based on silhoutte index 
silhout=c()
for (k in 2:8){
  clust=cutree(hc_complete,k=k)
  X_sil=silhouette(clust, dist_mat)
  silhout[k-1]=mean(X_sil[,3])
}
data.frame(k=2:8,silhout) # making it a dataframe

Best_k = which.max(silhout) + 1 # obtaining the best k value
cat("Appropriate # of clusters:", Best_k)


# Clusters:
best_set = cutree(hc_complete,k=Best_k)
best_set = table(best_set)
best_set


#Part B

hic_cluster = cutree(hc_complete,k=Best_k)
Clustcentro <- aggregate(data,list(hic_cluster),mean) #Finding the mean of all the values

#Appending the size of clusters with the mean of all other values
dataF <- data.frame(Cluster=Clustcentro[,1],Observations_in_this_cluster=as.vector(table(hic_cluster)),Clustcentro[,-1])

transpose_df <- t(dataF) #transposing

df_round <- function(x, digits) {
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x}

Clustcentros <- df_round(transpose_df, 2)

Clustcentros[2,] = as.numeric(Clustcentros[2,])

Clustcentros


#Part C 

# removing 5% of the data randomly
set.seed(425)
rem_indices <- sample(1:nrow(data), 200) #since 200 is the 5% of the data
data_c <- data[-rem_indices,]
summary(data_c)

# scaling the data 
data_scaled_c <- (scale(data_c))
summary(data_scaled_c)

# distance matrix 
dist_matrix_c <- dist(data_scaled_c, method = 'euclidean')

# Hierarchical Clustering with Euclidean distance and complete linkage for part c
hc_complete_c=hclust(dist_matrix_c, method="complete")
plot(hc_complete,main="Complete Linkage", xlab="", cex=.8)

silhoutc=c()
for (k in 2:8){
  clustc = cutree(hc_complete_c,k=k)
  Sil = silhouette(clustc, dist_matrix_c)
  silhoutc[k-1] = mean(Sil[,3])
}
data.frame(k=2:8,silhoutc)

#finding the best k
best_k = which.max(silhoutc) + 1
cat("Suitable number of clusters for part c:", best_k)


#obtaining the table for the best clustering set
best_clust_set = cutree(hc_complete_c,k=best_k)
best_clust_set = table(best_clust_set)
best_clust_set


hc_cluster_c = cutree(hc_complete_c,k=best_k) #Finding the mean of all the values

Clustcentro <- aggregate(data_c,list(hc_cluster_c),mean) #Appending the size of clusters with the mean of all other values

dataf <- data.frame(Cluster=Clustcentro[,1],Observations_in_this_cluster=as.vector(table(hc_cluster_c)),Clustcentro[,-1])
transpose_df <- t(dataf)
df_round <- function(x, digits) {
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
Clustcentros_c <- df_round(transpose_df, 2)
Clustcentros[2,] = as.numeric(Clustcentros[2,])

#Comparison
Clustcentros_c
Clustcentros


#Part D

best_km = kmeans(x=data_sc,centers=2,nstart=20) 
best_km$size # the cluster distribution


km_centroid = aggregate(data,list(best_km$cluster),mean)
vec = c(sum(best_km$cluster==1), sum(best_km$cluster==2))
hc_cluster = cutree(hc_complete,k=the_best_k_value)

#Finding the mean of all the values
Cluster_centroid <- aggregate(data,list(hc_cluster),mean)

#Appending the size of clusters with the mean of all other values
df = data.frame(Cluster=km_centroid[,1],Observations_in_this_cluster=vec,km_centroid[,-1])
trans_df = t(df)
df_round <- function(x, digits) {
  numeric_columns    <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
km_centroids = df_round(trans_df, 2)
km_centroids
