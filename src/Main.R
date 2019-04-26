#!/usr/bin/env Rscript
#
# Run main analysis and generate core intermediate data files
#

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

rdf <- read.csv('../data/raw/NSDUH_2017_Tab.tsv',
                  sep='\t', na.strings = c('.'));

yrdf <- subset(rdf, CATAGE == 1)
ardf <- subset(rdf, CATAGE != 1)

#########################################################################
## Run SWAS
#########################################################################

m <- sum(boolvars);

print("Running Youth SWAS");
y_summary_df <- swas_scan(yrdf, Y_ENDPOINT, Y_ENDPOINT_SCREENABLE, 
                          conf.level = (1 - alpha / (2*m)) );
y_sig_df <- subset(y_summary_df, PValue <= (alpha/m));

print("Running Adult SWAS");
a_summary_df <- swas_scan(ardf, A_ENDPOINT, A_ENDPOINT_SCREENABLE,
                          conf.level = (1 - alpha / (2*m)) );
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

#########################################################################
## Save Intermediate Results
#########################################################################
print("Writing intermediate results")

write.csv(y_summary_df, file='../calc/y_summary_df.csv', row.names= FALSE);
write.csv(a_summary_df, file='../calc/a_summary_df.csv', row.names= FALSE);
write.csv(merged_df, file='../calc/merged_df.csv', row.names= FALSE);
write.csv(sig_merged_df, file='../calc/sig_merged_df.csv', row.names= FALSE);

print("All done.");
