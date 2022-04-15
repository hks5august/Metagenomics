# Load libraries
library(openxlsx)
library(dplyr)
library(tibble)

OTUtable <- read.xlsx('https://github.com/PineBiotech/bioinformatics/blob/master/17sample-OTU-mat1_phylum1.xlsx?raw=true', sheet = 2)

#sum by phyla
OTUtableGroup <- OTUtable %>% group_by(OTU) %>% summarise_all(funs(sum))
row.names(OTUtableGroup) <- OTUtableGroup$OTU

OTUtableGroup1 <- OTUtableGroup[rowSums(OTUtableGroup[,2:18])>0, rownames=TRUE]

phylum <- factor(OTUtableGroup1$OTU)
phylum1 <- unique(phylum)

df <- as.matrix(OTUtableGroup1[,-1], rownames=TRUE)
barplot(df, las=2, col=rainbow(length(phylum1)))

# Plot with %age

## Compute the %age of each phyla
df2 <- as.matrix(as.data.frame(prop.table(as.matrix(df), 2)* 100))

## Plot a barplot with % age

par(mar = c(4, 3, 3, 6),   xpd = TRUE)                                
barplot(df2, las=2, col = rainbow(length(phylum)), cex.axis=0.5, cex.names=0.5, main = "OTU by Sample")
legend("right", inset = c(- 0.3, 0), legend = phylum1, pch = 7, col = rainbow(length(phylum)), cex = 0.5)

#prepare 2 groups
byGroupRarely <- as.matrix(rowMeans(OTUtableGroup1[,2:8]))
byGroupRegularly <- as.matrix(rowMeans(OTUtableGroup1[,9:18]))

#combine into a single data frame
OTUGroups <- cbind(byGroupRarely,byGroupRegularly)
colnames(OTUGroups) <- c("Rarely", "Regularly")
row.names(OTUGroups) <- phylum1
