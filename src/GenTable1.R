#!/usr/bin/env Rscript
#
# Generate statistics for "Table 1"
#

#########################################################################
## Load Data
#########################################################################
print("Load Data")

rdf <- read.csv('../data/raw/NSDUH_2017_Tab.tsv', 
                sep='\t', na.strings = c('.'));

ydf <- subset(rdf, YMDEYR == 1);
youth_cols <- sapply(seq(1,9), function(x) { paste("YO_MDEA", x, sep="")});
youth_dsm <- sapply(youth_cols, function(x) {sum(ydf[,x] == 1, na.rm = T) } );

print(youth_dsm)

adf <- subset(rdf, AMDEYR == 1);
adult_cols <- sapply(seq(1,9), function(x) { paste("AD_MDEA", x, sep="")});
adult_dsm <- sapply(adult_cols, function(x) {sum(adf[,x] == 1, na.rm = T) } );

print(adult_dsm)
