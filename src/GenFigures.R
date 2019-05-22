#!/usr/bin/env Rscript
#
# Generate figures.
#

library(ggplot2)
library(ggdendro)
library(ggpubr)
library(hashmap)
library(stringr)
library(ggsci)
library(ape)

#########################################################################
## Load Data
#########################################################################
print("Load Data")

comparable_fields <- read.csv('../data/screening/screenvars.csv',
                  sep=',', na.strings = c('.'));
rdf <- read.csv('../data/raw/NSDUH_2017_Tab.tsv', 
                  sep='\t', na.strings = c('.'));

a_summary_df <- read.csv('../calc/a_summary_df.csv')
y_summary_df <- read.csv('../calc/y_summary_df.csv')

merged_df <- read.csv('../calc/merged_df.csv')
sig_merged_df <- read.csv('../calc/sig_merged_df.csv')

diff_questions_df <- read.csv('../calc/diff_questions_df.csv')

comparable_fields <- read.csv('../data/screening/screenvars.csv',
                  sep=',', na.strings = c('.'));
screenable <- lapply(as.character(comparable_fields$ScreenableValues), function(x) {
  eval(parse(text=x))
})
comparable_fields$ParsedSceenableVals <- screenable;

boolvars <- vapply(screenable, function(vals) {
  length(vals)
}, FUN.VALUE = integer(1)) == 2

#########################################################################
## Generate Figures
#########################################################################
print("Generate Scatter")

plottable_merged_df <- merged_df;
plottable_merged_df <- subset(plottable_merged_df, 
                              is.finite(OddsRatio.youth) & is.finite(OddsRatio.adult)
                              & OddsRatio.youth != 0 & OddsRatio.adult != 0);
plottable_merged_df$LOD.youth <- log10(plottable_merged_df$OddsRatio.youth);
plottable_merged_df$LOD.adult <- log10(plottable_merged_df$OddsRatio.adult);

ggplot(plottable_merged_df, aes(x=LOD.youth, y=LOD.adult)) +
  geom_point() +
  xlab(bquote(log(OR[youth]))) +
  ylab(bquote(log(OR[adult]))) +
  theme(axis.title = element_text(size=28),
        axis.text = element_text(size=20));

ggsave('../calc/AdultYouthScatter.png', height = 8, width = 8)

print(paste(nrow(plottable_merged_df), "valid variables plotted", sep=" "));
plottable_lm <- lm(LOD.youth ~ LOD.adult, data = plottable_merged_df);
print("linear model of LOD.youth ~ LOD.adult")
summary(plottable_lm)

#########################################################################
## Recode Questions
#########################################################################
print("Recode Called Questions")

invert_or_dir <- function(x) {
  x$OddsRatio.youth <- 1/x$OddsRatio.youth;
  x$ConfLow.youth <- 1/x$ConfLow.youth;
  x$ConfHigh.youth <- 1/x$ConfHigh.youth;
  x$OddsRatio.adult <- 1/x$OddsRatio.adult;
  x$ConfLow.adult <- 1/x$ConfLow.adult;
  x$ConfHigh.adult <- 1/x$ConfHigh.adult;
  return(x);
}

diff_renamed_fig_df <- diff_questions_df;
diff_renamed_fig_df$Name <- as.character(diff_renamed_fig_df$Name)

# ALCMON is inverted
diff_renamed_fig_df[diff_renamed_fig_df$Name == 'ALCMON',] <- 
  invert_or_dir(diff_renamed_fig_df[diff_renamed_fig_df$Name == 'ALCMON',])
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'ALCMON'] <-
  'Alcohol use in the past month';

# ALCYR is inverted
diff_renamed_fig_df[diff_renamed_fig_df$Name == 'ALCYR',] <- 
  invert_or_dir(diff_renamed_fig_df[diff_renamed_fig_df$Name == 'ALCYR',])
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'ALCYR'] <-
  'Alcohol use in the past year';

# CIGEVER
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'CIGEVER'] <-
  'Ever used cigarettes (self-reported)';

# CIGFLAG is inverted
diff_renamed_fig_df[diff_renamed_fig_df$Name == 'CIGFLAG',] <- 
  invert_or_dir(diff_renamed_fig_df[diff_renamed_fig_df$Name == 'CIGFLAG',])
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'CIGFLAG'] <-
  'Ever used cigarettes (imputed)';

# FUALC18
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'FUALC18'] <-
  'First used alcohol prior to age 18';

# FUCIG18
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'FUCIG18'] <-
  'First used cigarettes prior to age 18';

# FUCIG21
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'FUCIG21'] <-
  'First used cigarettes prior to age 21';

# ILLALCMON is inverted
diff_renamed_fig_df[diff_renamed_fig_df$Name == 'ILLALCMON',] <- 
  invert_or_dir(diff_renamed_fig_df[diff_renamed_fig_df$Name == 'ILLALCMON',])
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'ILLALCMON'] <-
  'Illicit drug or alcohol use in past month';

# IRSEX is inverted (since female sex is considered the "exposure")
diff_renamed_fig_df[diff_renamed_fig_df$Name == 'IRSEX',] <- 
  invert_or_dir(diff_renamed_fig_df[diff_renamed_fig_df$Name == 'IRSEX',])
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'IRSEX'] <-
  'Female sex';

# MJONLYFLAG is inverted
diff_renamed_fig_df[diff_renamed_fig_df$Name == 'MJONLYFLAG',] <- 
  invert_or_dir(diff_renamed_fig_df[diff_renamed_fig_df$Name == 'MJONLYFLAG',])
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'MJONLYFLAG'] <-
  'Marijuana is the only (illicit) drug ever used';

# YOWRPROB
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'YOWRPROB'] <-
  'One particular depressive incident was worst ever';

# IRMCDCHP
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'IRMCDCHP'] <-
  'Has Medicaid/CHIP';

# DIFFTHINK
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'DIFFTHINK'] <-
  'Serious difficulty concentrating, remembering, or making decisions due to condition*';

# CAIDCHIP
diff_renamed_fig_df$Name[diff_renamed_fig_df$Name == 'CAIDCHIP'] <-
  'Covered by Medicaid/CHIP';

code2desc <- hashmap(as.character(diff_questions_df$Name), as.character(diff_renamed_fig_df$Name));

## Remove ADWRPROB 
diff_renamed_fig_df <- diff_renamed_fig_df[-(diff_renamed_fig_df$Name == 'ADWRPROB'),];

#########################################################################
## Cluster by Jaccard Distance Index
#########################################################################
print("Run complete-clustering with Jaccard distance")

# Try with Jaccard index
dist_rdf <- rdf[,colnames(rdf) %in% comparable_fields$ColName];
dist_rdf <- dist_rdf[,boolvars];
dist_rdf <- dist_rdf[,colnames(dist_rdf) %in% as.character(diff_questions_df$Name)];
dist_rdf <- dist_rdf[,!(colnames(dist_rdf) %in% 'ADWRPROB')]; # remove ADWRPROB from clustering
dist_rdf <- na.omit(dist_rdf);

# set all columns to 0/1 for correlation
dist_mat <- as.matrix(dist_rdf);
min_by_cols <- apply(dist_mat, 2, FUN=min)
dist_mat <- dist_mat - matrix(min_by_cols, nrow=nrow(dist_mat), ncol=length(min_by_cols),byrow=TRUE);

# Invert flags for which 0/1 directionality is reversed with respect to *MDEYR
dist_mat[,'CIGEVER'] <- -1 * (dist_mat[,'CIGEVER'] - 1);
dist_mat[,'FUALC18'] <- -1 * (dist_mat[,'FUALC18'] - 1);
dist_mat[,'FUCIG18'] <- -1 * (dist_mat[,'FUCIG18'] - 1);
dist_mat[,'FUCIG21'] <- -1 * (dist_mat[,'FUCIG21'] - 1);
dist_mat[,'YOWRPROB'] <- -1 * (dist_mat[,'YOWRPROB'] - 1);
dist_mat[,'IRMCDCHP'] <- -1 * (dist_mat[,'IRMCDCHP'] - 1);
dist_mat[,'DIFFTHINK'] <- -1 * (dist_mat[,'DIFFTHINK'] - 1);
dist_mat[,'CAIDCHIP'] <- -1 * (dist_mat[,'CAIDCHIP'] - 1);

colnames(dist_mat) <- code2desc[[colnames(dist_mat)]];
jaccard_dist <- dist(t(dist_mat), method='binary')
hc <- hclust(as.dist(jaccard_dist));

n_clust <- 7;
clust_colors <- pal_d3()(n_clust)
clust_defs = cutree(hc, n_clust)

png("../calc/clusterdend.png", width = 900, height = 600)
plot(as.phylo(hc), tip.color = clust_colors[clust_defs], 
     cex = 1, label.offset = 0.05)
dev.off();

#########################################################################
## Build Main Plot
#########################################################################
print("Plot different questions")

youth_renamed_pt_df <- data.frame(
  name = as.character(diff_renamed_fig_df$Name),
  lod = log10(diff_renamed_fig_df$OddsRatio.youth),
  min_x = log10(diff_renamed_fig_df$ConfLow.youth),
  max_x = log10(diff_renamed_fig_df$ConfHigh.youth),
  Age = rep('Youth (12-17 y.o.)', length(diff_renamed_fig_df$ConfHigh.youth))
)

adult_renamed_pt_df <- data.frame(
  name = as.character(diff_renamed_fig_df$Name),
  lod = log10(diff_renamed_fig_df$OddsRatio.adult),
  min_x = log10(diff_renamed_fig_df$ConfLow.adult),
  max_x = log10(diff_renamed_fig_df$ConfHigh.adult),
  Age = rep('Adult (18+ y.o)', length(diff_renamed_fig_df$ConfHigh.youth))
)

barbox_renamed_df <- rbind(youth_renamed_pt_df, adult_renamed_pt_df)
barbox_renamed_df$name <- str_wrap(barbox_renamed_df$name, width=50)
barbox_renamed_df$name_reorder <- factor(barbox_renamed_df$name, 
                                         levels = str_wrap(hc$labels[hc$order], width=50))

p2 <- ggplot(barbox_renamed_df, aes(x=name_reorder, y=lod, group=Age, color=Age)) +
  geom_pointrange(aes(ymin=min_x, ymax=max_x)) + coord_flip() +
  ylab(bquote(log(OR))) + xlab('') + geom_hline(yintercept=0)
# Use geom_pointrange
p2

ht <- 6;
aspect_ratio <- 20/12;

ggsave('../calc/diff_questions_renamed.png', height = ht, width = ht*aspect_ratio)

#########################################################################
## Extract Dendrogram Plot Data
#########################################################################
print("Plot different questions")

dhc <- as.dendrogram(hc)
ddata <- dendro_data(dhc, type = "rectangle")

p <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.1, 0)) + theme_dendro()
p

ggsave('../calc/dendro_no_labels.png', height = ht, width = ht*aspect_ratio)

pdendro <- p + theme(plot.margin=unit(c(0.8, 0, 1.5, 1),"cm"))
pcore <- p2 + theme(plot.margin=unit(c(1, 1, 1, 0), "cm"))

ggarrange(pdendro, pcore, ncol = 2, nrow = 1, widths = c(1, 4))

ggsave('../calc/linked_dendro_core.png', height = ht, width = ht*aspect_ratio)

# Remove ggplots issue file for ggsave() bug
# https://github.com/tidyverse/ggplot2/issues/2787
if (file.exists("Rplots.pdf")) {
  file.remove("Rplots.pdf");
}
