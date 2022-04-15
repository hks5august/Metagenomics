install.packages("vegan")
library("phyloseq")
library("ggplot2")
library("dplyr")

#theme_set(theme_bw())

otu_mat<- read.table("Acne_Amplicon_Abundance_table.txt", header = TRUE, sep = "\t")
tax_mat<- read.table("Acne_Taxonomy_table.txt", header = TRUE, sep = "\t")
samples_df <- read.table("Acne_SupplementaryTable.txt", header = TRUE, sep = "\t")

row.names(otu_mat) <- otu_mat$OTUs
otu_mat <- otu_mat %>% select (-OTUs)

row.names(tax_mat) <- tax_mat$class
tax_mat <- tax_mat %>% select (-class)

row.names(samples_df) <- samples_df$Run
samples_df <- samples_df %>% select (-Run)

sampletype <- unique(row.names(samples_df))

otu_mat <- as.matrix(otu_mat)
tax_mat <- as.matrix(tax_mat)

OTU = otu_table(otu_mat, taxa_are_rows = TRUE)
TAX = tax_table(tax_mat)
samples = sample_data(samples_df)

acne <- phyloseq(OTU, TAX, samples)
acne
####### Barplot #####
#save plot as PDF ###
pdf(file="barplot.pdf")
plot_bar(acne, fill = "Phylum")
dev.off()

#NMDS - non-metric multidimensional scaling ordination
acne.ord <- ordinate(acne, "NMDS", "bray")

sample_variables(acne)
pdf(file="Richness-plot.pdf")
plot_richness(acne, color="Dermatology_Disord", measures=c("Chao1", "Shannon"))
dev.off()

###### this code does work on single data containing single columns; thus, we adding extra column to make data(samples) work

s1 <- cbind(samples,samples)

#### rename column names
colnames(s1) <- c("Dermatology_Disord","copy")

pdf(file="ordination-plot.pdf")
#plot_ordination(acne, acne.ord, type="samples", color="Dermatology_Disord")
plot_ordination(acne, acne.ord, type="s1", color="Dermatology_Disord")
dev.off()

pdf(file="ordination-split-plot.pdf")
plot_ordination(acne, acne.ord, type="split", color="Dermatology_Disord")
dev.off()

