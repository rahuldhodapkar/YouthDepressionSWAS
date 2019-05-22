#!/usr/bin/env Rscript
#
# Extract age dependence 
#

library(ggplot2)
library(hashmap)
library(stringr)
library(ggsci)
library(ape)
library(plyr)

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
## Encode Age Groups (Hardcode)
#########################################################################

agecode2minage <- hashmap(
  seq(1,17),
  c('12', 
    '13', 
    '14', 
    '15', 
    '16', 
    '17', 
    '18', 
    '19', 
    '20', 
    '21', 
    '22-23', 
    '24-25', 
    '26-29', 
    '30-34', 
    '35-49', 
    '50-64', 
    '65+')
);

#########################################################################
## Lifetime MDE
#########################################################################
print("Lifetime (within last year) MDE")

onset_ages_12_17 <- subset(rdf, AGE2 > 0
                           & AGE2 <= 6
                           & YMDELT == 1 
                           & YOPBRMBR == 1
                           & YOPBAGE < 150)$YOPBAGE;

onset_ages_18_25 <- subset(rdf, AGE2 > 6
                           & AGE2 <= 12
                           & AMDELT == 1 
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;

onset_ages_26_34 <- subset(rdf, AGE2 > 12
                           & AGE2 <= 13
                           & AMDELT == 1 
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;


onset_ages_35_49 <- subset(rdf, AGE2 > 14
                           & AGE2 <= 15
                           & AMDELT == 1 
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;

onset_ages_50_plus <- subset(rdf, AGE2 > 15
                             & AMDELT == 1 
                             & ADPBRMBR == 1
                             & ADPBAGE < 150)$ADPBAGE;

age_df <- data.frame(
  age = c(
    onset_ages_12_17,
    onset_ages_18_25,
    onset_ages_26_34,
    onset_ages_35_49,
    onset_ages_50_plus
  ),
  group = c(
    rep(paste('12-17 (n=', length(onset_ages_12_17), ')', sep=''), 
          length(onset_ages_12_17)),
    rep(paste('18-25 (n=', length(onset_ages_18_25), ')', sep=''),
          length(onset_ages_18_25)),
    rep(paste('26-34 (n=', length(onset_ages_26_34), ')', sep=''), 
          length(onset_ages_26_34)),
    rep(paste('35-49 (n=', length(onset_ages_35_49), ')', sep=''),
          length(onset_ages_35_49)),
    rep(paste('50+ (n=', length(onset_ages_50_plus), ')', sep=''),
         length(onset_ages_50_plus))
  )
);

ggplot(age_df, aes(x=age, color=group, fill=group)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  xlab('Recalled Age of First MDE') +
  labs(color="Age Group", fill="Age Group")

ggsave('../calc/lifetime_coarse_onset_age.png', width = 8, height = 7)

all_age_df <- subset(rdf, AMDELT == 1 
                     & ADPBAGE < 150);

agecode2age_with_size <- hashmap(
  unique(all_age_df$AGE2),
  sapply(unique(all_age_df$AGE2), function(x) {
    paste(agecode2minage[[x]], 
          " (n=", sum(all_age_df$AGE2 == x, na.rm = T), ")", 
          sep = '');
  })
)

all_age_df$group <- agecode2age_with_size[[all_age_df$AGE2]]
ggplot(all_age_df, aes(x=ADPBAGE, color=group, fill=group)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  labs(color="Age Group", fill="Age Group") + 
  xlab('Recalled Age of First MDE')

ggsave('../calc/lifetime_granular_onset_age.png', width = 8, height = 7)

#########################################################################
## For Active MDEs
#########################################################################
print("Active (within last year) MDE")

onset_ages_12_17 <- subset(rdf, AGE2 > 0
                           & AGE2 <= 6
                           & YMDEYR == 1 
                           & YOPBRMBR == 1
                           & YOPBAGE < 150)$YOPBAGE;

onset_ages_18_25 <- subset(rdf, AGE2 > 6
                           & AGE2 <= 12
                           & AMDEYR == 1 
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;

onset_ages_26_34 <- subset(rdf, AGE2 > 12
                           & AGE2 <= 13
                           & AMDEYR == 1 
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;


onset_ages_35_49 <- subset(rdf, AGE2 > 14
                           & AGE2 <= 15
                           & AMDEYR == 1 
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;

onset_ages_50_plus <- subset(rdf, AGE2 > 15
                             & AMDEYR == 1 
                             & ADPBRMBR == 1
                             & ADPBAGE < 150)$ADPBAGE;

age_df <- data.frame(
  age = c(
    onset_ages_12_17,
    onset_ages_18_25,
    onset_ages_26_34,
    onset_ages_35_49,
    onset_ages_50_plus
  ),
  group = c(
    rep(paste('12-17 (n=', length(onset_ages_12_17), ')', sep=''), 
        length(onset_ages_12_17)),
    rep(paste('18-25 (n=', length(onset_ages_18_25), ')', sep=''),
        length(onset_ages_18_25)),
    rep(paste('26-34 (n=', length(onset_ages_26_34), ')', sep=''), 
        length(onset_ages_26_34)),
    rep(paste('35-49 (n=', length(onset_ages_35_49), ')', sep=''),
        length(onset_ages_35_49)),
    rep(paste('50+ (n=', length(onset_ages_50_plus), ')', sep=''),
        length(onset_ages_50_plus))
  )
);

ggplot(age_df, aes(x=age, color=group, fill=group)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  xlab('Recalled Age of First MDE') +
  labs(color="Age Group", fill="Age Group")

ggsave('../calc/active_coarse_onset_age.png', width = 8, height = 7)

all_age_df <- subset(rdf, AMDEYR == 1 
                     & ADPBAGE < 150);
agecode2age_with_size <- hashmap(
  unique(all_age_df$AGE2),
  sapply(unique(all_age_df$AGE2), function(x) {
    paste(agecode2minage[[x]], 
          " (n=", sum(all_age_df$AGE2 == x, na.rm = T), ")", 
          sep = '');
  })
)
all_age_df$group <- agecode2age_with_size[[all_age_df$AGE2]]
ggplot(all_age_df, aes(x=ADPBAGE, color=group, fill=group)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  xlab('Recalled Age of First MDE') +
  labs(color="Age Group", fill="Age Group")

ggsave('../calc/active_granular_onset_age.png', width = 8, height = 7)

#########################################################################
## Lifetime but Not Active
#########################################################################
print("Lifetime MDE but *not* active (w/in last year)")

onset_ages_12_17 <- subset(rdf, AGE2 > 0
                           & AGE2 <= 6
                           & YMDELT == 1 
                           & YMDEYR == 2
                           & YOPBRMBR == 1
                           & YOPBAGE < 150)$YOPBAGE;

onset_ages_18_25 <- subset(rdf, AGE2 > 6
                           & AGE2 <= 12
                           & AMDELT == 1 
                           & AMDEYR == 2
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;

onset_ages_26_34 <- subset(rdf, AGE2 > 12
                           & AGE2 <= 13
                           & AMDELT == 1 
                           & AMDEYR == 2
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;


onset_ages_35_49 <- subset(rdf, AGE2 > 14
                           & AGE2 <= 15
                           & AMDELT == 1 
                           & AMDEYR == 2
                           & ADPBRMBR == 1
                           & ADPBAGE < 150)$ADPBAGE;

onset_ages_50_plus <- subset(rdf, AGE2 > 15
                             & AMDELT == 1 
                             & AMDEYR == 2
                             & ADPBRMBR == 1
                             & ADPBAGE < 150)$ADPBAGE;

age_df <- data.frame(
  age = c(
    onset_ages_12_17,
    onset_ages_18_25,
    onset_ages_26_34,
    onset_ages_35_49,
    onset_ages_50_plus
  ),
  group = c(
    rep(paste('12-17 (n=', length(onset_ages_12_17), ')', sep=''), 
        length(onset_ages_12_17)),
    rep(paste('18-25 (n=', length(onset_ages_18_25), ')', sep=''),
        length(onset_ages_18_25)),
    rep(paste('26-34 (n=', length(onset_ages_26_34), ')', sep=''), 
        length(onset_ages_26_34)),
    rep(paste('35-49 (n=', length(onset_ages_35_49), ')', sep=''),
        length(onset_ages_35_49)),
    rep(paste('50+ (n=', length(onset_ages_50_plus), ')', sep=''),
        length(onset_ages_50_plus))
  )
);

ggplot(age_df, aes(x=age, color=group, fill=group)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  xlab('Recalled Age of First MDE') +
  labs(color="Age Group", fill="Age Group")

ggsave('../calc/lifetime_not_active_coarse_onset_age.png', width = 8, height = 7)

all_age_df <- subset(rdf, AMDEYR == 2 & AMDELT == 1
                     & ADPBAGE < 150);
agecode2age_with_size <- hashmap(
  unique(all_age_df$AGE2),
  sapply(unique(all_age_df$AGE2), function(x) {
    paste(agecode2minage[[x]], 
          " (n=", sum(all_age_df$AGE2 == x, na.rm = T), ")", 
          sep = '');
  })
)
all_age_df$group <- agecode2age_with_size[[all_age_df$AGE2]]
ggplot(all_age_df, aes(x=ADPBAGE, color=group, fill=group)) +
  geom_density(alpha=0.4) +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  xlab('Recalled Age of First MDE') +
  labs(color="Age Group", fill="Age Group")

ggsave('../calc/lifetime_not_active_granular_onset_age.png', width = 8, height = 7)
