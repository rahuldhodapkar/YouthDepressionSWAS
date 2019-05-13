#!/usr/bin/env Rscript
#
# Generate tSNE plots. Currently exploratory code, should not be considered
# as complete.
#

library(RColorBrewer)

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
## Configure Plotting Palette
#########################################################################
color_palette <- c(brewer.pal(4, name="Set1"), brewer.pal(12, name="Set3"))

#########################################################################
## Clean Data for tSNE
#########################################################################
print("Age of Onset of Depression")

boolean_fields <- comparable_fields[
    lapply(comparable_fields$ParsedSceenableVals, length) == 2,];

adf <- subset(rdf, AMDELT == 1);
adf_bool <- adf[, colnames(adf) %in% boolean_fields$ColName];

clean_and_impute <- function(df, constraints) {
  cdf <- df;
  
  for (c in colnames(df)) {
    cpos <- match(c, constraints$ColName);
    cdf[!(cdf[,c] %in% constraints$ParsedSceenableVals[[cpos]]), c] <- NA;
  }
  
  return(cdf)
}

cleaned_df <- clean_and_impute(adf_bool, boolean_fields);
cleaned_df$AMDEYR <- NULL;

#########################################################################
## Run tSNE
#########################################################################

library(dplyr)
cleaned_pruned_df <- cleaned_df %>%
  select_if(~ !any(is.na(.)))

library(Rtsne)
rtsne_out <- Rtsne(cleaned_pruned_df, check_duplicates=F);

#########################################################################
## Plot With Active MDE Colors
#########################################################################

plot(rtsne_out$Y, asp = 1, pch = 20, col = color_palette[as.factor(adf$AMDEYR)],
     cex = 0.75, cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.5, 
     xlab = "t-SNE dimension 1", ylab = "t-SNE dimension 2", 
     main = "Respondent Lifetime MDE")

legend("topright", 
       legend=c("MDE in past year", "No MDE in past year"),
       fill=color_palette[c(1,2)], cex=0.8)

#########################################################################
## Plot Against Age of First MDE
#########################################################################

young_onset <- factor(adf$ADPBAGE < 18, 
                         levels=c("TRUE", "FALSE"));

plot(rtsne_out$Y, asp = 1, pch = 20, col = color_palette[young_onset],
     cex = 0.75, cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.5, 
     xlab = "t-SNE dimension 1", ylab = "t-SNE dimension 2", 
     main = "Respondent Lifetime MDE")

legend("topright", 
       legend=c("Under 21 Onset", "Over 21 Onset"),
       fill=color_palette[c(1,2)], cex=0.8)

#########################################################################
## Plot Against Age of First MDE
#########################################################################

# Plot tSNE heatmap by number of variants called per cell
#heatPal <- colorRamp(c("red", "blue"), interpolate = 'spline')
#ageNorm <- adf$ADPBAGE / max(adf$ADPBAGE);
#heatColors <- rgb(heatPal(ageNorm), maxColorValue = 255)

#plot(rtsne_out$Y, asp = 1, pch = 20, col = heatColors,
#     cex = 0.75, cex.axis = 1.25, cex.lab = 1.25, cex.main = 1.5, 
#     xlab = "t-SNE dimension 1", ylab = "t-SNE dimension 2", 
#     main = "Respondent Lifetime MDE")

#########################################################################
## Impute Data
#########################################################################

#md_patterns <- md.pattern(cleaned_df)
#mice_out <- mice(data = cleaned_df, m = 1, method = "pmm", maxit = 50, seed = 500)
#imputed_df <- complete(mice_out,1)



