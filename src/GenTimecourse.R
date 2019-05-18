#!/usr/bin/env Rscript
#
# Generate time-evolution figures for differentially associated variables
#

library(ggplot2)

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
## Predefined Constants
#########################################################################

grouped_age_labels <- c(
  '12-14',
  '15-17',
  '18-21',
  '22-23',
  '24-25',
  '26-29',
  '30-34',
  '35-49',
  '50-64',
  '65+'
);

age_groupings <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9,10),
  c(11),
  c(12),
  c(13),
  c(14),
  c(15),
  c(16),
  c(17));

#########################################################################
## Predefined Functions
#########################################################################

swas_scan <- function(rdf, ENDPOINT, ENDPOINT.SCREENABLE.VALS, conf.level=0.95) {
  endpoint_valid_df <- rdf[ (rdf[,ENDPOINT] %in% ENDPOINT.SCREENABLE.VALS), ];
  
  raw_assoc_list <- lapply(which(bernoulli), function(i) {
    
    compare_colname <- as.character(comparable_fields$ColName)[i];
    compare_colname_screenvals <- comparable_fields$ParsedSceenableVals[[i]];
    
    t_df <- endpoint_valid_df[ (endpoint_valid_df[, compare_colname]
                                %in% compare_colname_screenvals ), ];
    
    contingency_tab <- as.matrix(table(t_df[,ENDPOINT], t_df[,compare_colname]));
    
    if (dim(contingency_tab)[1] == 2 && dim(contingency_tab)[2] == 2) {
      test_results <- fisher.test(contingency_tab, conf.level = conf.level);
    } else {
      test_results <- list(
        p_value = Inf,
        estimate = NA
      )
    }
    
    list(
      QNAME=as.character(comparable_fields$ColName)[i],
      VALS=dim(t_df)[1],
      P_VALUE=as.numeric(test_results$p.value),
      CONF_LOW=as.numeric(test_results$conf.int[1]),
      CONF_HIGH=as.numeric(test_results$conf.int[2]),
      ODDS_RATIO=as.numeric(test_results$estimate)
    )
  })
  
  summary_df <- data.frame(
    Name = as.character(sapply(raw_assoc_list, function(x) { x['QNAME'] })),
    NVals = as.integer(sapply(raw_assoc_list, function(x) { x['VALS'] })),
    PValue = as.numeric(sapply(raw_assoc_list, function(x) { x['P_VALUE'] })),
    ConfLow = as.numeric(sapply(raw_assoc_list, function(x) { x['CONF_LOW'] })),
    ConfHigh = as.numeric(sapply(raw_assoc_list, function(x) { x['CONF_HIGH'] })),
    OddsRatio = as.numeric(sapply(raw_assoc_list, function(x) { x['ODDS_RATIO'] }))
  );
  
  return(summary_df);
}

gen_time_evolution <- function(varname, vartitle="Sample Title") {
  # Generate plot for <varname> for each cohort
  pvals <- c();
  ors <- c();
  los <- c();
  his <- c();
  
  for (age_set in age_groupings) {
    srdf <- rdf[rdf$AGE2 %in% age_set, ];
    if(max(age_set) <= 6) {
      t_tab <- table(srdf$YMDEYR, srdf[,varname]);
    } else {
      t_tab <- table(srdf$AMDEYR, srdf[,varname]);
    }
    
    ft <- fisher.test(t_tab);
    pvals <- c(pvals, ft$p.value)
    ors <- c(ors, ft$estimate)
    los <- c(los, ft$conf.int[1])
    his <- c(his, ft$conf.int[2])
  }
  
  target_df <- data.frame(
    age_group = grouped_age_labels,
    est = -log10(ors),
    min_est = -log10(los),
    max_est = -log10(his)
  )
  
  pcok <- ggplot(target_df, aes(x=age_group, y=est)) + 
    ggtitle(vartitle) +
    geom_pointrange(aes(ymin=min_est, ymax=max_est)) +
    ylab(bquote(log(OR))) + xlab('Age Group') + geom_hline(yintercept = 0)
  # Use geom_pointrange
  ggsave(paste("../calc/", varname, "_timecourse.png", sep=""));
}

#########################################################################
## Time-Evolution Plots for Differentially Associated Variables (DAVs)
#########################################################################

#
# Cross-impute ADWRPROB and YOWRPROB for first raw figure generation.
# Manual operation for this *particular* anaysis. Open problem on how to
# perform this operation more generally, open for pull requests.
#

rdf$ADWRPROB[!(rdf$ADWRPROB %in% c(1,2))] <- rdf$YOWRPROB[!(rdf$ADWRPROB %in% c(1,2))];
rdf$YOWRPROB[!(rdf$YOWRPROB %in% c(1,2))] <- rdf$ADWRPROB[!(rdf$YOWRPROB %in% c(1,2))];

for (varname in diff_questions_df$Name) {
  tryCatch({
    print(varname)
    gen_time_evolution(varname, vartitle = paste(varname, "Raw Data", sep = " "));
  }, warning = function(w) {
    print(w);
  }, error = function(e) {
    print(paste("Unable to process", varname, sep=" "));
    print(e);
  });
}

