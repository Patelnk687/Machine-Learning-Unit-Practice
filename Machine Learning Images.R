flower<-read.csv("flower.csv", header=FALSE)
str(flower)
#changes to matrix
flowerMatrix=as.matrix(flower)
str(flowerMatrix)

#
flowerVector=as.vector(flowerMatrix)
str(flowerVector)


distance= dist(flowerVector, method="euclidean")

#hierarchical clustering /dendrogram
clusterIntensity = hclust(distance, method ="ward.D")
plot(clusterIntensity)
#plot rectangles on clusters
rect.hclust(clusterIntensity,k=3, border="blue")
flowerClusters= cutree(clusterIntensity, k=3)
flowerClusters


#apply the vector to the cluster number
tapply(flowerVector, flowerClusters, mean)

#c rows and columns 50*50 pic to matrix
dim(flowerClusters)= c(50,50)

# make the image
image(flowerClusters, axes=FALSE)


#original
image(flowerMatrix, axes=FALSE)
image(flowerMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))



healthy=read.csv("healthy.csv", header=FALSE)
healthyMatrix= as.matrix(healthy)
str(healthyMatrix)

image(healthyMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))
healthyVector= as.vector(healthyMatrix)
distance=dist(healthyVector, method="euclidean")
str(healthyVector)
n=365636
n*(n-1)/2


#segmnent using kmeans clustering
k=5
set.seed(1)
KMC=kmeans(healthyVector, centers = k, iter.max=1000)
str(KMC)
healthyClusters=KMC$cluster
KMC$centers[2]
dim(healthyClusters)=c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters,axes=FALSE, col=rainbow(k))

# tumor
tumor=read.csv("tumor.csv", header=FALSE)
tumorMatrix= as.matrix(tumor)
tumorVector=as.vector(tumorMatrix)
str(tumorMatrix)
install.packages("flexclust")      

library("flexclust")
KMC.kcca<-as.kcca(KMC,healthyVector)
tumorClusters<-predict(KMC.kcca,newdata=tumorVector)
dim(tumorClusters)=c(nrow(tumorMatrix),ncol(tumorMatrix))
image(tumorClusters, axes=FALSE, col=rainbow(k))
