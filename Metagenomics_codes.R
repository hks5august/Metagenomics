#Load required Libraries:
library("phyloseq")
library("ggplot2")
library("dplyr")

#Import the data:
otu_mat<- read.table("soil_dada_2__Abundance_table.txt", header = TRUE, sep = "\t")
tax_mat<- read.table("soil_dada_2__Taxonomy_table.txt", header = TRUE, sep = "\t")
samples_df <- read.table("Soil_data_supplementary-table.txt", header = TRUE, sep = "\t")

#Define Row Names for each type of data
row.names(otu_mat) <- otu_mat$OTUs

#Remove the column which is  already used as row from the data
otu_mat <- otu_mat %>% select (-OTUs)
row.names(tax_mat) <- tax_mat$class
tax_mat <- tax_mat %>% select (-class)
row.names(samples_df) <- samples_df$Run
samples_df <- samples_df %>% select (-Run)
sampletype <- unique(row.names(samples_df))

#Transform into matrix
otu_mat <- as.matrix(otu_mat)
tax_mat <- as.matrix(tax_mat)

#Transform matrix data as input for Phyloseq
OTU = otu_table(otu_mat, taxa_are_rows = TRUE)
TAX = tax_table(tax_mat)
samples = sample_data(samples_df)
phyl_obj <- phyloseq(OTU, TAX, samples)

#Visualization: bar chart
p=plot_bar(phyl_obj, fill = "Phylum")
plot1<- p+ theme(legend.key.size = unit(0.3, "cm"), legend.text = element_text( color="Black", size=7.5), axis.text.x = element_text( color="Black", size=7.5))
plot(plot1)

#NMDS - non-metric multidimensional scaling ordination
phyl.ord <- ordinate(phyl_obj, "NMDS", "bray")
#sample_variables(acne)

#Richness plot
pp = plot_richness(phyl_obj, color="Type",measures=c("Chao1", "Shannon"))
plot2<-pp+ theme(legend.key.size = unit(0.3, "cm"), legend.text = element_text( color="Black", size=7.5), axis.text.x = element_text( color="Black", size=6.0))

plot(plot2)
#Draw ordination plot
s1 <- cbind(samples,samples)

#rename column names
colnames(s1) <- c("Type","Type1")
plot3<-plot_ordination(phyl_obj, phyl.ord, type="s1", color="Type")

plot(plot3)










#Split by groups 
plot4<-plot_ordination(phyl_obj, phyl.ord, type="split", color="Type")

#plot all plots


plot(plot4)
