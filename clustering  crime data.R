#install.packages("kselection")
library(kselection)
input <- read.csv(file.choose())
View(input)
mydata<-input[1:50,c(2:5)]
View(mydata)
k <- kselection(mydata, parallel = TRUE, k_threshold = 0.9, max_centers=12) # Threshold point = Heighest value
k$max_centers
?kselection

#install.packages("doParallel")
library(doParallel)
registerDoParallel(cores=4)
k <- kselection(mydata, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k

input <- read.csv(file.choose())
View(input)
mydata<-input[1:50,c(2:5)]
View(mydata)
normalized_data<-scale(mydata)
d<-dist(normalized_data,method="euclidean")
fit<-hclust(d,method="complete")
?hclust
plot(fit) # Display Dendrogram
plot(fit,hang=-1)
groups<-cutree(fit,k=2)

?cutree
rect.hclust(fit,k=2,border="green")
?rect.hclust

clusters<-as.matrix(groups)

final<-data.frame(mydata,clusters)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)



