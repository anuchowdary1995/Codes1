clus=read.csv("C:/Users/Anusha/Desktop/responses.csv")
clus=read.csv(file.choose())
View(clus)
clust=na.omit(clus)
View(clust)
clust1=clust[,-c(150,149,148,147,146,145,74,75,133,108,109)]
View(clust1)
clust1<-clust1[-c(285,771),]
sub=data.frame(scale(clust[,1:19]))
View(sub)
#h-clusters
hcls=hclust(dist(sub))
plot(hcls)
clust1$hcls1=cutree(hcls,4)
plot(clust1$hcls1)
View(clust)

all_var_list=clust[,c(1:19)]

for(var in 1:19){
  print(aov(clust[,var]~ clust$kmn))

  boxplot(clust[,var] ~ clust$kmn)
}

#============kmeans===========
kmn=kmeans(sub,4)
clust$kmnn=kmn$cluster
View(clust1)

plot(clust$kmnn)


#=======factors=====
pd=princomp(sub)
pc
pd$loadings
plot(pd)
pd$scores
clus=cbind(clust,pc$scores)

cor(pd$scores)

cor(sub)

fc=factanal(sub,4)#throws errors
fc=factanal(sub,4,rotation="varimax",scores="regression")#convergs

fc$scores
clust=cbind(factor,fc$scores)
fc$loadings







