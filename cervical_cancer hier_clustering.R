library(dendextend)
library(factoextra)
library(Metrics)
library(caret)



a <- read.csv("/home/tarun/Downloads/sobar-72.csv")
b <- a[,20]

head(a)
str(a)
head(a[,1:19])
a<- a[,1:19]


summary_stats <- data.frame(
  Min = apply(a, 2, min), # minimum
  Med = apply(a, 2, median), # median
  Mean = apply(a, 2, mean), # mean
  SD = apply(a, 2, sd), # standard deviation
  Max = apply(a, 2, max) # maximum
)


summary_stats <- round(summary_stats, 1);summary_stats


boxplot(a)

as.matrix(dist(a[1:3,1:3]))



a.clus <- hclust(dist(a))
a.clus


ggplot_fviz <-fviz_nbclust(a,FUN=hcut,method = "silhouette")
ggplot_fviz


dend <- as.dendrogram(a.clus,hang=0)
dendrogram_cut_2clusters <- color_branches(dend,k=2) #2 clusters


nodePar <- list(lab.cex = 0.5, pch = c(NA, 19), 
                cex = 0.1, col = "blue")

plot(dendrogram_cut_3clusters,ylab="complete linkage",nodePar = nodePar,
     xlab="Euclidean distance",horiz=TRUE)


allocations <- cutree(dend,k=2)


pred <- c(allocations)
pred

predict <- pred -1
pred

accuracy(b, predict)


ce(b,predict)


