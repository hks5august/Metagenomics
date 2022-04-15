#Load libraries
library(ggfortify)
library(cluster)

#Load data
ExpressionTable <- read.table("12EBV_Geo_ZAIRE2015_Abundance.txt", sep='\t', header=TRUE, check.names=FALSE, stringsAsFactors=TRUE, row.names=1)

df = t(ExpressionTable)
#Take data without groups
df <- ExpressionTable[2:10001]

#Generate PCA components
pca_res <- prcomp(df, scale.=TRUE)

#Object
head(pca_res$scale^2, n=5)

var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
var_explained[1:5]

barplot(var_explained[1:10])

autoplot(pca_res, label=TRUE, label.size=3)

#Plot PCA plot
plot <- autoplot(pca_res, data=ExpressionTable, frame=F, colour='Group', frame.type='norm')
plot(plot)






pca_res$scale