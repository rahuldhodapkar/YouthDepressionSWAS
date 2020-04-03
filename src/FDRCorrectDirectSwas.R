#!/usr/bin/env Rscript
#
# Run SWAS directly from core data allowing for swappability of multiple
# comparisons correction methods by re-configuring the unit of analysis.
#
# Generated in response to reviewer comments from PLOS ONE submission
#
# @author Rahul Dhodapkar <rahul.m.dhodapkar@gmail.com>
# @version 2020.04.02
#

library(ggplot2)
library(ggdendro)
library(ggpubr)
library(hashmap)
library(stringr)
library(ggsci)
library(ape)
library(DescTools)

a_summary_df <- read.csv('../calc/a_summary_df.csv')
y_summary_df <- read.csv('../calc/y_summary_df.csv')

#########################################################################
## Global Parameters
#########################################################################

alpha <- 0.01
Y_ENDPOINT <- 'YMDEYR';
Y_ENDPOINT_SCREENABLE <- 1:2;
A_ENDPOINT <- 'AMDEYR';
A_ENDPOINT_SCREENABLE <- 1:2;

#########################################################################
## Load Data
#########################################################################

comparable_fields <- read.csv('../data/screening/screenvars.csv',
                              sep=',', na.strings = c('.'));
screenable <- lapply(as.character(comparable_fields$ScreenableValues), function(x) {
  eval(parse(text=x))
})
comparable_fields$ParsedSceenableVals <- screenable;

boolvars <- vapply(screenable, function(vals) {
  length(vals)
}, FUN.VALUE = integer(1)) == 2

# Core swas_scan function
swas_scan <- function(rdf, ENDPOINT, ENDPOINT.SCREENABLE.VALS, conf.level=0.95) {
  endpoint_valid_df <- rdf[ (rdf[,ENDPOINT] %in% ENDPOINT.SCREENABLE.VALS), ];
  
  raw_assoc_list <- lapply(which(boolvars), function(i) {
    
    compare_colname <- as.character(comparable_fields$ColName)[i];
    compare_colname_screenvals <- comparable_fields$ParsedSceenableVals[[i]];
    
    t_df <- endpoint_valid_df[ (endpoint_valid_df[, compare_colname]
                                %in% compare_colname_screenvals ), ];
    
    contingency_tab <- as.matrix(table(t_df[,ENDPOINT], t_df[,compare_colname]));
    
    if (dim(contingency_tab)[1] == 2 && dim(contingency_tab)[2] == 2) {
      test_results <- fisher.test(contingency_tab, conf.level = conf.level);
      manual.or.est <- (
        (contingency_tab[[1,1]] * contingency_tab[[2,2]])
        / (contingency_tab[[1,2]] * contingency_tab[[2,1]])
      )
    } else {
      test_results <- list(
        p_value = Inf,
        estimate = NA
      )
      manual.or.est <- NA
    }

    valid.val.freqs <- table(t_df[,compare_colname])
    
    if (length(valid.val.freqs) == 0) {
      valid.val.freqs <- c(0, 0)
    } else if (length(valid.val.freqs) == 1) {
      valid.val.freqs <- c(valid.val.freqs, 0)
    }
    
    list(
      QNAME=as.character(comparable_fields$ColName)[i],
      VALS=dim(t_df)[1],
      VAR=var(t_df[,compare_colname]),
      N_1=valid.val.freqs[[1]],
      N_2=valid.val.freqs[[2]],
      P_VALUE=as.numeric(test_results$p.value),
      CONF_LOW=as.numeric(test_results$conf.int[1]),
      CONF_HIGH=as.numeric(test_results$conf.int[2]),
      ODDS_RATIO=as.numeric(test_results$estimate),
      MANUAL_ODDS_RATIO=manual.or.est
    )
  })
  
  summary_df <- data.frame(
    Name = as.character(sapply(raw_assoc_list, function(x) { x['QNAME'] })),
    NVals = as.integer(sapply(raw_assoc_list, function(x) { x['VALS'] })),
    NVal1 = as.integer(sapply(raw_assoc_list, function(x) { x['N_1'] })),
    NVal2 = as.integer(sapply(raw_assoc_list, function(x) { x['N_2'] })),
    Var = as.numeric(sapply(raw_assoc_list, function(x) { x['VAR'] })),
    PValue = as.numeric(sapply(raw_assoc_list, function(x) { x['P_VALUE'] })),
    ConfLow = as.numeric(sapply(raw_assoc_list, function(x) { x['CONF_LOW'] })),
    ConfHigh = as.numeric(sapply(raw_assoc_list, function(x) { x['CONF_HIGH'] })),
    OddsRatio = as.numeric(sapply(raw_assoc_list, function(x) { x['ODDS_RATIO'] })),
    ManOddsRatio = as.numeric(sapply(raw_assoc_list, function(x) { x['MANUAL_ODDS_RATIO'] }))
  );
  
  return(summary_df);
}

rdf <- read.csv('../data/raw/NSDUH_2017_Tab.tsv',
                sep='\t', na.strings = c('.'));

yrdf <- subset(rdf, CATAGE == 1)
ardf <- subset(rdf, CATAGE != 1)

#########################################################################
## Generate Contingency Tables for Screenable Variables
#########################################################################
#
# Using the Procedure of Bejamini and Yekutieli for False Discovery Rate-
# Adjusted Multiple Confidence Intervals, determine "R" after BH correction
# for the adult scan and youth scans.
#
# Set q = 0.05 for control of the simultaneous false coverage-statement rate (FCR)
#
q = 0.05

a_summary_df$AdjP <- p.adjust(a_summary_df$PValue, method = "BH")
R.a <- sum(a_summary_df$AdjP < q, na.rm = T)

y_summary_df$AdjP <- p.adjust(y_summary_df$PValue, method = "BH")
R.y <- sum(y_summary_df$AdjP < q, na.rm = T)

#########################################################################
## Run SWAS
#########################################################################

m <- sum(boolvars);

print("Running Youth SWAS");
y_summary_df <- swas_scan(yrdf, Y_ENDPOINT, Y_ENDPOINT_SCREENABLE, 
                          conf.level = (1 - R.y * (q / m)) );
y_sig_df <- subset(y_summary_df, PValue <= (alpha/m));

print("Running Adult SWAS");
a_summary_df <- swas_scan(ardf, A_ENDPOINT, A_ENDPOINT_SCREENABLE,
                          conf.level = (1 - R.a * (q / m)) );
a_sig_df <- subset(a_summary_df, PValue <= (alpha/m));

#########################################################################
## Impute Data
#########################################################################
print("Imputing data and merging youth with adult call sets")

y_summary_imputed_df <- y_summary_df;
y_summ_nona <- na.omit(y_summary_imputed_df)
y_removed <- setdiff(y_summary_imputed_df$Name, y_summ_nona$Name)

ctr <- 2:ncol(y_summary_imputed_df);

# impute data
for (n in y_removed) { 
  longsub <- gsub("^AD", "YO", n)
  shortsub <- gsub("^A", "Y", n)
  
  row_to_replace <- which(y_summary_imputed_df$Name == n);
  
  if ( sum(y_summ_nona$Name == longsub) == 1 ) {
    y_summary_imputed_df[which(y_summary_imputed_df$Name == n) , ctr] <-
      y_summ_nona[which(y_summ_nona$Name == longsub), ctr]
  }
  else if ( sum(y_summ_nona$Name == shortsub) == 1 ) {
    y_summary_imputed_df[which(y_summary_imputed_df$Name == n) , ctr] <-
      y_summ_nona[which(y_summ_nona$Name == shortsub), ctr]
  }
}

y_summ_impute_nona <- na.omit(y_summary_imputed_df)

a_summary_imputed_df <- a_summary_df;
a_summ_nona <- na.omit(a_summary_imputed_df);
a_removed <- setdiff(a_summary_imputed_df$Name, a_summ_nona$Name);

ctr <- 2:ncol(y_summary_imputed_df);

# impute data
for (n in a_removed) { 
  longsub <- gsub("^YO", "AD", n)
  shortsub <- gsub("^Y", "A", n)
  
  if ( sum(a_summ_nona$Name == longsub) == 1 ) {
    a_summary_imputed_df[which(a_summary_imputed_df$Name == n) , ctr] <-
      a_summ_nona[which(a_summ_nona$Name == longsub), ctr]
  }
  else if ( sum(a_summ_nona$Name == shortsub) == 1 ) {
    a_summary_imputed_df[which(a_summary_imputed_df$Name == n) , ctr] <-
      a_summ_nona[which(a_summ_nona$Name == shortsub), ctr]
  }
}

a_summ_impute_nona <- na.omit(a_summary_imputed_df)

#########################################################################
## Merge and Call Differences
#########################################################################

merged_df <- merge(y_summary_imputed_df, a_summary_imputed_df, 
                   by=c('Name'), suffixes = c('.youth', '.adult'))

sig_merged_df <- subset(merged_df, PValue.youth < (alpha/m) | PValue.adult < (alpha/m))

# prune incomplete records
merged_call_df <- na.omit(sig_merged_df)

diff_questions_df <- subset(merged_call_df, 
                            ConfHigh.youth < ConfLow.adult
                            | ConfHigh.adult < ConfLow.youth)

#########################################################################
## Save Intermediate Results
#########################################################################
print("Writing intermediate results")

write.csv(y_summary_df, file='../calc/fdr_y_summary_df.csv', row.names= FALSE);
write.csv(a_summary_df, file='../calc/fdr_a_summary_df.csv', row.names= FALSE);
write.csv(merged_df, file='../calc/fdr_merged_df.csv', row.names= FALSE);
write.csv(sig_merged_df, file='../calc/fdr_sig_merged_df.csv', row.names= FALSE);
write.csv(diff_questions_df, file='../calc/fdr_diff_questions_df.csv', row.names = FALSE);

print("Generate Scatter")

plottable_merged_df <- merged_df;
plottable_merged_df <- subset(plottable_merged_df, 
                              abs(log10(ConfLow.youth) - log10(ConfHigh.youth)) < 3
                              & abs(log10(ConfLow.adult) - log10(ConfHigh.adult)) < 3);
plottable_merged_df$LOD.youth <- log10(plottable_merged_df$OddsRatio.youth);
plottable_merged_df$LOD.adult <- log10(plottable_merged_df$OddsRatio.adult);

ggplot(plottable_merged_df, aes(x=LOD.youth, y=LOD.adult)) +
  geom_point() +
  xlab(bquote(log(OR[youth]))) +
  ylab(bquote(log(OR[adult]))) +
  theme(axis.title = element_text(size=28),
        axis.text = element_text(size=20));

ggsave('../calc/FDRAdultYouthScatter.png', height = 8, width = 8)

print(paste(nrow(plottable_merged_df), "valid variables plotted", sep=" "));
plottable_lm <- lm(LOD.youth ~ LOD.adult, data = plottable_merged_df);
print("linear model of LOD.youth ~ LOD.adult")
summary(plottable_lm)

lin.corr <- CCC(plottable_merged_df$LOD.youth, plottable_merged_df$LOD.adult)
print(lin.corr$rho.c)

print("All done.");

#########################################################################
## Restructure AD and YO
#########################################################################

clean.names <- diff_questions_df$Name
clean.names <- gsub("^YO", "", clean.names)
clean.names <- gsub("^Y", "", clean.names)
clean.names <- gsub("^AD", "", clean.names)
clean.names <- gsub("^A", "", clean.names)
clean.names <- gsub("^_", "", clean.names)

length(unique(clean.names))
