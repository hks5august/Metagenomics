#Load Libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(openxlsx)

#Load input data
data <- read.xlsx('Rhizobacteria-table_with_phyla.xlsx', sheet = 1)

#make dataframe 
df=as.data.frame(data)

dim(df)
#Change into required  input, so melt the data ###
df$category <- row.names(df)
melted_df <- melt(df)

#Extract phylum
phylum = melted_df$OTU

#Make Groups (Rare and Regular)
Seleniferous<- as.data.frame(rowMeans(df[,2:28]), rownames=TRUE)
Non_Seleniferous <- as.data.frame(rowMeans(df[,29:59]), rownames=TRUE)

#Combine groups together
group <- cbind(Seleniferous, Non_Seleniferous)
               
#Name the column
colnames(group) <- c("Seleniferous", "Non_Seleniferous")
#Create final input group with phylum names
group1 <-cbind(phylum,group)
               
#make desired input file for plots
group1$category <- row.names(group1)
melted_group <- melt(group1)
Group_type = melted_group$variable
               
#Extract values
               
Counts= melted_group$value

#Create plots with counts
               
ppp <- ggplot(melted_group, aes(x = Group_type, y = Counts)) + geom_col(aes(fill = phylum))
               
ppp + theme(legend.key.size = unit(0.3, "cm"), legend.text = element_text( color="Black", size=5.5), axis.text.x = element_text( color="Black", size=7.5, angle=45), axis.text.y = element_text( color="Black", size=7.5))

#Plot with counts in groups
print (ppp + theme(legend.key.size = unit(0.3, "cm"), legend.text = element_text( color="Black", size=5.5), axis.text.x = element_text( color="Black", size=7.5, angle=45), axis.text.y = element_text( color="Black", size=7.5)))


               
#Convert into  Percentage
Percent= round(Counts / sum(Counts) * 100,2)
               
#Create plots
               
pppp <- ggplot(melted_group, aes(x = Group_type, y = Percent, fill = phylum)) + 
geom_bar(position = "fill",stat = "identity") +
 scale_y_continuous(labels = scales::percent_format())
               
pppp + theme(legend.key.size = unit(0.38, "cm"), legend.text = element_text( color="Black", size=5.8), axis.text.x = element_text( color="Black", size=7.5, angle=90), axis.text.y = element_text( color="Black", size=7.5)) 
               
 # Plot with %age in groups
 print (pppp + theme(legend.key.size = unit(0.38, "cm"), legend.text = element_text( color="Black", size=5.8), axis.text.x = element_text( color="Black", size=7.5, angle=90), axis.text.y = element_text( color="Black", size=7.5)) )
               