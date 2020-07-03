library(readxl)
input<-read_excel(file.choose(),sheet = 2)
View(input)

mydata<-input[1:3999,-c(1)]
View(mydata)

normalized_data<-scale(mydata) 
d<-dist(normalized_data,method="euclidean") 
fit<-hclust(d,method="complete")
?hclust

plot(fit)
plot(fit,hang=-1)
groups<-cutree(fit,k=5)
?cutree

rect.hclust(fit,k=5,border="red")
?rect.hclust

membership<-as.matrix(groups)

final<-data.frame(mydata,membership)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

  #k means algorithim

library(readxl)
input<-read_excel(file.choose(),sheet = 2)
mydata<-input[1:3999,-c(1)]
View(mydata)
normalized_data<-scale(mydata)
View(normalized_data)

# model Building
fit <- kmeans(normalized_data, 3) # 5 cluster solution
fit$cluster
final2<- data.frame(mydata, fit$cluster) # append cluster membership
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit.cluster"))
View(final2)
aggregate(mydata, by=list(fit$cluster), FUN=mean)


# k clustering alternative for large dataset

install.packages("cluster")
library(cluster)
library(readxl)
input<-read_excel(file.choose(),sheet = 2)
mydata<-input[1:3999,-c(1)]
View(mydata)
normalized_data<-scale(mydata)
View(normalized_data)
xcl <- clara(normalized_data,3, sample = 1000)  
clusplot(xcl)
xpm <- pam(normalized_data, 3)
clusplot(xpm)

