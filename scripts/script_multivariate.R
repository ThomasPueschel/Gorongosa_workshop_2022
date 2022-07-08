# load packages
library(tidyverse)
library(corrplot)
library(viridis)
library(ClassDiscovery)
# load foot data
# https://elifesciences.org/articles/44433
# https://datadryad.org/stash/dataset/doi:10.5061/dryad.d112p8r
load('foot_data.Rdata')
# explore data
view(foot_data)
# select metric variables
foot1<-foot_data %>% select(mt1:cub)
# compute correlation matrix
R<-cor(foot1, method = c("pearson"))
R
# plot
corrplot(R, order = "hclust")
# var-cov matrix
cov(foot1)
# compute dist
Q<-dist(foot1[1:10,], method = 'euclidean')
Q
# PCA
PCA <- foot1 %>% prcomp(scale = F) 
#
eig<- (PCA$sdev)^2
eig1 <- (PCA$sdev[1])^2
eig2 <- (PCA$sdev[2])^2
variance1 <- eig1* 100 / sum(eig)
variance2 <- eig2 * 100 / sum(eig)
# color by species
gp <- as.factor(foot_data$taxon)
col.gp <- turbo(length(levels(gp)))
names(col.gp) <- levels(gp)
col.gp <- col.gp[match(gp, names(col.gp))]
# label axes
xlab <- paste("Principal Component 1", "(", round(variance1, digits = 2), "%)")
ylab <- paste("Principal Component 2", "(", round(variance2, digits = 2), "%)")
# plot PC1 vs PC2
plot(PCA$x[, 1], PCA$x[, 2], pch = 21, cex = 2.5, bg = col.gp, xlab = xlab, ylab = ylab, asp = T)
legend(50,20, legend= unique(foot_data$taxon), pch=19,  col=unique(as.factor(foot_data$taxon)),cex=0.3)


# clustering
# distance matrix
d <- dist(x = foot1, method = "euclidean")
# single-linkage or nearest neighbour 
clust <- hclust(d = d, method = 'single')
# plot
plotColoredClusters(clust, cols = col.gp, labs = foot_data$taxon, cex=0.3)

# we can use Pcs as variables
summary(PCA)
# dist using PCs
d2 <- dist(x = PCA$x[,1:2], method = "euclidean")
# single-linkage or nearest neighbour
clust2 <- hclust(d = d2, method = "single")
plotColoredClusters(clust2, cols = col.gp, labs = foot_data$taxon, cex=0.3)
