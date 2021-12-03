#### ANALYSIS CODE TO REPRODUCE BARNBY ET AL 2021 PAPER ###

# NB::: Individuals with CCD are recognised as vulnerable
# therefore, we have removed all personal identifiers from their data
# although kept the analysis code required for the machine learning model.
# This data therefore has age and sex regressed out of the variables.
# We also post the original anonymised data prior to age and sex being
# regressed out. Please contact the authors to retrieve the full set of
# data that includes Age and Sex.

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(easystats)
library(flextable)
library(tidyquant)
library(patchwork)
library(tidyquant)
library(Hmisc)
library(factoextra)
library(caret)
library(ggpubr)
library(bayesplot)

source('CustomFunctions.R')

# Data Load ---------------------------------------------------------------

data              <- read.csv('data/cleaned_data_anonymised.csv') %>% dplyr::select(ID:Group) # main data
ICARCCD           <- read.csv('data/ICARCCD.csv') %>% dplyr::select(ID:ICAR) # to reproduce ICAR analysis
ICARNT            <- read.csv('data/ICARNT.csv') %>% dplyr::select(ID:ICAR)  # to reproduce ICAR analysis
first_wave_NTdata <- read.csv('data/first_wave_data.csv') # for test-retest

# The data below is to reproduce our figures and data from the permutation testing.
# NB ::: the code is written below to perform your own random permutations,
# but of course this will produce slight variations on the figures it produces
# due to the nature of random seeds sampling. To directly reproduce the figures and data
# from our paper, just load the below dfs and go wild!

trainPCA          <- read.csv('data/TrainingData.csv') %>% dplyr::select(-X)# data for reproducing Figure 3D & 3E
testPCA           <- read.csv('data/TestData.csv') %>% dplyr::select(-X) # data for reproducing Figure 3D & 3E

#For the machine learning model:
regressed_data_NT <- read.csv('data/regressed_training_data_NT.csv') %>% dplyr::select(-X)
regressed_data_CCD<- read.csv('data/regressed_test_data_CCD.csv') %>% dplyr::select(-X)
averagedVariance  <- read.csv('data/averagedVariance.csv') %>% dplyr::select(-X)
LOOCVlist         <- read.csv('data/LOOCVlist.csv') %>% dplyr::select(-X)
confusPermute     <- read.csv('data/confusPermute.csv') %>% dplyr::select(-X)
loading_list_absc <- read.csv('data/loading_list_absc.csv') %>% dplyr::select(-X)

# Trust / Fonagy Data -----------------------------------------------------

#Data taken from https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0250264

TrustStudy1 <- read_csv('pone.0250264.s003.csv')
TrustStudy2 <- read_csv('pone.0250264.s003.csv')
TrustFon    <- rbind(TrustStudy1[,1:4], TrustStudy2[,1:4])

TrustFon    <- TrustFon %>%
  rename(ID = id, TrustGSI = 2, CredGSI = 4, MistrustGSI = 3)

# One sided t tests for prior comparisons ---------------------------------

#NT
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(ICAR), mu = 5) # Condon & Revelle, 2014
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(Persec), mu = 11) # Freeman et al., 2021
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(Persuade), mu = 15.77) # Teunisse et al., 2020
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(Insense), mu = 17.84) # Teunisse et al., 2020
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(TrustGSI), TrustFon$TrustGSI)
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(MistrustGSI), TrustFon$MistrustGSI)
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(CredGSI), TrustFon$CredGSI)
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(AutismS), mu = 56.74) # Hoekstra et al., 2013
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(AutismS), mu = 89.63) # Hoekstra et al., 2013
t.test(data %>% filter(Group == 'NT') %>% dplyr::select(SocIntell), mu = 102.62) # Tromso et al., 2011

#CCD
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(ICAR), mu = 5)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(Persec), mu = 11)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(Persuade), mu = 15.77)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(Insense), mu = 17.84)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(TrustGSI), TrustFon$TrustGSI)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(MistrustGSI), TrustFon$MistrustGSI)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(CredGSI), TrustFon$CredGSI)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(AutismS), mu = 56.74)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(AutismS), mu = 89.63)
t.test(data %>% filter(Group == 'CCD') %>% dplyr::select(SocIntell), mu = 102.62)


# Unadjusted Comparisons --------------------------------------------------------------

kruskal.test(ICAR ~ Group, data = data)
kruskal.test(Persec ~ Group, data = data)
kruskal.test(SocRef ~ Group, data = data)
kruskal.test(Trust ~ Group, data = data)
kruskal.test(Mistrust ~ Group, data = data)
kruskal.test(Credulity ~ Group, data = data)
kruskal.test(Persuade ~ Group, data = data)
kruskal.test(Insense ~ Group, data = data)
kruskal.test(ComLadder ~ Group, data = data)
kruskal.test(NatLadder ~ Group, data = data)
kruskal.test(AutismS   ~ Group, data = data)
kruskal.test(AutismR   ~ Group, data = data)
kruskal.test(SocIntell ~ Group, data = data)

# ICAR Reaction time assessments -----------------------------------------------

# Add item difficulty based on Subotic et al., 2017
RTCheck <- ICARCCD %>%
  mutate(Group = 'CCD') %>%
  rbind(ICARNT %>% mutate(Group = 'NT')) %>%
  group_by(ID, Group) %>%
  mutate(Q = 1:10,
         Correct = ifelse(Correct == 1, 'Yes', 'No'),
         Difficulty = c(-0.19, 0.08, 0.32, 0.26, 0.23, 1.72, 0.18, 1.19, 1.01, 0.72))

#Create a logistic model for plotting
log1 <- glm(Correct ~ Difficulty,
            data = RTCheck %>%
              mutate(Correct = ifelse(Correct == 'Yes', 1, 0)) %>%
              filter(Group == 'CCD'),
            family = 'binomial')
log2 <- glm(Correct ~ Difficulty,
            data = RTCheck %>%
              mutate(Correct = ifelse(Correct == 'Yes', 1, 0)) %>%
              filter(Group == 'NT'),
            family = 'binomial')
range <- seq(-0.19, 1.72, 0.05)
guessdf <- data.frame(Difficulty = range)
rownames(guessdf) <- guessdf$Difficulty
log_curveCCD <- predict(log1, newdata = guessdf, type = 'response') %>% as.data.frame() %>% mutate(Difficulty = range) %>% rename(Prob = 1)
log_curveNT  <- predict(log2, newdata = guessdf, type = 'response') %>% as.data.frame() %>% mutate(Difficulty = range) %>% rename(Prob = 1)
log_curve <- rbind(log_curveCCD %>% mutate(Group = 'CCD'), log_curveNT %>% mutate(Group = 'NT'))

CorrCheck <- ICARCCD %>%
  mutate(Group = 'CCD') %>%
  rbind(ICARassess %>% mutate(Group = 'NT')) %>%
  group_by(ID, Group) %>%
  mutate(Q = 1:10, Correct = ifelse(Correct == 1, 'Yes', 'No')) %>%
  group_by(Q, Correct, Group) %>%
  summarise(CorrectSum = n())

#Plots figures
rt <- ggplot(RTCheck %>%
               group_by(Q, Correct, Group) %>%
               summarise(RT = Reaction.Time, .groups = 'keep'),
             aes(factor(Q), RT, fill = factor(Correct)))+
  stat_summary(geom = 'col', color = 'black', position = 'dodge')+
  geom_point(alpha = 0.5, position = position_dodge(width = 1))+
  scale_fill_brewer(palette = 'Set1', name = 'Correct')+
  facet_wrap(~Group)+
  labs(x = 'Question Number', y = 'Reaction Time (ms)')+
  theme_tq()+
  theme(legend.position = c(0.75, 0.75),
        legend.background = element_rect(colour = 'black'))

RTcurve <- ggplot(RTCheck %>% mutate(Correct = ifelse(Correct == 'Yes', 1, 0)),
                  aes(Difficulty, Correct, color = Group)) +
  geom_jitter(height = 0.2)+
  #geom_smooth(method = 'lm')+
  geom_line(data = log_curve, aes(x = Difficulty, y = Prob), size = 1)+
  ggpubr::stat_cor(label.y.npc = 0.5)+
  scale_y_continuous(breaks = c(0, 1), labels = c(0,1))+
  facet_wrap(~Group) +
  labs(x = 'Item Difficulty (Subotic et al., 2017)', y = 'p(Correct | Difficulty) / Correct (1/0)')+
  scale_color_brewer(palette = 'Set2')+
  theme_tq()+
  theme(legend.position = 'none')
RTcurve

corr <- ggplot(CorrCheck, aes(factor(Q), CorrectSum, fill = factor(Correct)))+
  geom_col(color = 'black', position = 'fill')+
  scale_fill_brewer(palette = 'Set1', name = 'Correct')+
  labs(x = 'Question Number', y = 'Percentage')+
  scale_y_continuous(labels = c(0, 25, 50, 75, 100))+
  facet_wrap(~Group)+
  theme_tq()+
  theme(legend.position = 'none',
        legend.background = element_rect(colour = 'black'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())

RTcurve/corr/rt & plot_annotation(tag_levels = 'A') &
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text.x = element_text(size = 18))

brm(RT ~ Group * Correct + (Q|ID),
          data = RTCheck %>%
            group_by(ID, Q, Correct, Group) %>%
            summarise(RT = mean(Reaction.Time)))

brm(Correct ~ Difficulty * Group + (1|ID),
          data = RTCheck %>%
            mutate(Correct = ifelse(Correct == 'Yes', 1, 0)),
          family = 'bernoulli')

# Adjusted Comparisons ----------------------------------------------------

modelB <- data %>% mutate(Group = factor(Group)) %>% na.omit()
b1 <- brm(ICAR      ~ Group + Age + Sex + Education.quantised       , data = modelB); summary(b1); pp_check(b1)
b2 <- brm(Persec    ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b2); pp_check(b2)
b3 <- brm(SocRef    ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b3); pp_check(b3)
b4 <- brm(Trust     ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b4); pp_check(b4)
b5 <- brm(Mistrust  ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b5); pp_check(b5)
b6 <- brm(Credulity ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b6); pp_check(b6)
b7 <- brm(Persuade  ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b7); pp_check(b7)
b8 <- brm(Insense   ~ Group + Age + Sex + Education.quantised + ICAR, data = modelB); summary(b8); pp_check(b8)

b9 <- brm(AutismS   ~ Group + Age + Sex + Education.quantised + ICAR                      , data = modelB); summary(b9) ; pp_check(b9)
b10<- brm(SocIntell ~ Group + Age + Sex + Education.quantised + ICAR                      , data = modelB); summary(b10); pp_check(b10)
b11<- brm(Persuade  ~ Group + Age + Sex + Education.quantised + ICAR + AutismS + SocIntell, data = modelB); summary(b11); pp_check(b11)
b12<- brm(Insense   ~ Group + Age + Sex + Education.quantised + ICAR + AutismS + SocIntell, data = modelB); summary(b12); pp_check(b12)

# Figure 2  -----------------------------------------------------------------

ggarrange(
  plotdiff( data, ICAR, 'ICAR', 2, 12, 0.75),
  plotdiff( data, Persuade, 'Persuadability',2, 12, 0.75),
  plotdiff( data, Insense, 'Insensitivity', 2, 12, 0.75),
  plotdiff( data, Trust, 'Trust', 2, 12, 0.75),
  plotdiff( data, Credulity, 'Credulity', 2, 12, 0.75),
  plotdiff( data, SocIntell, 'Social Intelligence', 2, 12, 0.75),
  plotdiff( data, AutismS, 'Autism', 1, 12, 0.75),
  plotdiff( data, Persec, 'Persecutory', 1, 8, 0.85),
  plotdiff( data, SocRef, 'Social Reference', 2, 8, 0.85),
  plotdiff( data, Mistrust, 'Mistrust', 2, 8, 0.85),
  plotdiff( data, NatLadder, 'National Ladder', 2, 8, 0.85),
  plotdiff( data, ComLadder, 'Community Ladder', 2, 8, 0.85),
  nrow = 6, ncol = 2)

# Test-retest analytics ---------------------------------------------------

base1 <- first_wave_data
base2 <- data %>%
  filter(Group == 'NT') %>%
  na.omit() %>%
  distinct() %>%
  dplyr::select(ID, ICAR, ComLadder, NatLadder, Insense, Persuade, Credulity,
                Mistrust, Trust, Persec, SocRef)

# Add col names suffix to differentiate time points for ICC
colnames(base2)[2:11] <- paste(colnames(base2)[2:11],"2",sep="_")

baseComb <- plyr::join(base1, base2, by = 'ID') %>%
  na.omit() %>%
  distinct_at(vars(ID), .keep_all = TRUE)

#Figure S1

ICCplot <- data.frame(
  mean = c(
    psych::ICC(baseComb[,c('ICAR', 'ICAR_2')])$results[3, 2],
    psych::ICC(baseComb[,c('Persec', 'Persec_2')])$results[3, 2],
    psych::ICC(baseComb[,c('SocRef', 'SocRef_2')])$results[3, 2],
    psych::ICC(baseComb[,c('Persuade', 'Persuade_2')])$results[3, 2],
    psych::ICC(baseComb[,c('Insense', 'Insense_2')])$results[3, 2],
    psych::ICC(baseComb[,c('ComLadder', 'ComLadder_2')])$results[3, 2],
    psych::ICC(baseComb[,c('NatLadder', 'NatLadder_2')])$results[3, 2]),
  lower =c(
    psych::ICC(baseComb[,c('ICAR', 'ICAR_2')])$results[3, 7],
    psych::ICC(baseComb[,c('Persec', 'Persec_2')])$results[3, 7],
    psych::ICC(baseComb[,c('SocRef', 'SocRef_2')])$results[3, 7],
    psych::ICC(baseComb[,c('Persuade', 'Persuade_2')])$results[3, 7],
    psych::ICC(baseComb[,c('Insense', 'Insense_2')])$results[3, 7],
    psych::ICC(baseComb[,c('ComLadder', 'ComLadder_2')])$results[3, 7],
    psych::ICC(baseComb[,c('NatLadder', 'NatLadder_2')])$results[3, 7]),
  upper =c(
    psych::ICC(baseComb[,c('ICAR', 'ICAR_2')])$results[3, 8],
    psych::ICC(baseComb[,c('Persec', 'Persec_2')])$results[3, 8],
    psych::ICC(baseComb[,c('SocRef', 'SocRef_2')])$results[3, 8],
    psych::ICC(baseComb[,c('Persuade', 'Persuade_2')])$results[3, 8],
    psych::ICC(baseComb[,c('Insense', 'Insense_2')])$results[3, 8],
    psych::ICC(baseComb[,c('ComLadder', 'ComLadder_2')])$results[3, 8],
    psych::ICC(baseComb[,c('NatLadder', 'NatLadder_2')])$results[3, 8]),
  scale = c('ICAR', 'Persecutory Ideation', 'Social Reference', 'Persuadability', 'Insensitivity', 'Community Ladder', 'National Ladder'),
  Measure = c('ICAR', 'Paranoia', 'Paranoia', 'Gullibility', 'Gullibility', 'McArthur Ladder', 'McArthur Ladder')
)

topP <- ggplot(ICCplot)+
  annotate('rect', xmin = 0.50, xmax = 0.75, ymin = Inf, ymax = Inf, fill = 'grey', alpha = 0.3 )+
  annotate('rect', xmin = 0.75, xmax = 0.90, ymin = Inf, ymax = Inf, fill = 'grey', alpha = 0.5 )+
  annotate('rect', xmin = 0.90, xmax = Inf , ymin = Inf, ymax = Inf, fill = 'grey', alpha = 0.7 )+
  geom_errorbar(aes(mean, fct_reorder(factor(scale), -mean), color = Measure, xmin = lower, xmax = upper, group = scale), width = 0.5)+
  geom_point(aes(mean, factor(scale), color = Measure, group = scale), size = 3)+
  scale_color_manual(name = 'Scale', values = c('#E8C547', '#30323D', '#A30000', '#00AFB5'))+
  coord_cartesian(xlim = c(0,1))+
  labs(x = 'ICC (3, 1)', y = "")+
  geom_text(aes(y = scale, label = scale), x = 0, check_overlap = T, hjust = 0, size = 6)+
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        legend.position = c(0.4, 0.2),
        legend.background = element_rect(color = 'black'))

topP

# Correlation analysis --------------------------------------------------------

# inital partial correlation matrix:

NTcorDat <- data %>%
  filter(Group == 'NT') %>%
  mutate(Paranoia = Persec + SocRef,
         `McA-SL` = (ComLadder + NatLadder)/2,
         Gullibility = Persuade + Insense) %>%
  dplyr::select(ICAR, `McA-SL`, Gullibility, Credulity, Mistrust, Trust,
                `Social Intelligence` = SocIntell, Autism = AutismS, Paranoia)

CCDcorDat <- data %>%
  filter(Group == 'CCD') %>%
  mutate(Paranoia = Persec + SocRef,
         `McA-SL` = (ComLadder + NatLadder)/2,
         Gullibility = Persuade + Insense) %>%
  dplyr::select(ICAR, `McA-SL`, Gullibility, Credulity, Mistrust, Trust,
                `Social Intelligence` = SocIntell, Autism = AutismS, Paranoia)

NTcormat    <- rcorr(as.matrix(NTcorDat), type = 'spearman')
CCDcormat <- rcorr(as.matrix(CCDcorDat), type = 'spearman')

diff = list()
diff[[1]] <- matrix(0, nrow = length(CCDcormat$r[,1]), ncol = length(CCDcormat$r[,1]))
colnames(diff[[1]]) <- colnames(CCDcormat$r)
rownames(diff[[1]]) <- rownames(CCDcormat$r)
diff[[2]] <- matrix(0, nrow = length(CCDcormat$r[,1]), ncol = length(CCDcormat$r[,1]))
colnames(diff[[2]]) <- colnames(CCDcormat$r)
rownames(diff[[2]]) <- rownames(CCDcormat$r)

for (i in 1:length(CCDcormat$r[,1])){
  for (j in 1:length(CCDcormat$r[,1])){

    tdiff <- psych::r.test(n = NTcormat$n[1],
                           n2 = CCDcormat$n[1],
                           r12 = CCDcormat$r[i,j],
                           r34 = NTcormat$r[i,j])

    diff[[1]][i,j] <- tdiff$z
    diff[[2]][i,j] <- tdiff$p

  }
}

cor1 <- ggcorrplot::ggcorrplot(corr = NTcormat$r, p.mat = NTcormat$P, lab = T, type = 'upper')+
  labs(title = 'Correlation', y = 'NT')+
  theme(legend.position = c(0.75, 0.25),
        plot.title = element_text(size = 18,hjust = 0.5),
        axis.title.y = element_text(size = 18, angle = 90))
cor2 <- ggcorrplot::ggcorrplot(corr = CCDcormat$r, p.mat = CCDcormat$P, lab = T, type = 'upper')+
  theme(legend.position = c(0.75, 0.25),
        axis.title.y = element_text(size = 18, angle = 90))+
  labs(y = 'CCD')
cor3 <- ggcorrplot::ggcorrplot(corr = diff[[1]], p.mat = diff[[2]], lab = T, type = 'upper')+
  theme(legend.position = c(0.75, 0.25),
        axis.title.y = element_text(size = 18, angle = 90))+
  labs(y = 'Difference')+
  scale_fill_gradient2(low = 'white', mid = 'white', high = 'red', name = 'Z-score', midpoint = 1.5, limits = c(0,4))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))

NTpcormat <- ppcor::pcor(NTcorDat, method = 'spearman')
CCDpcormat <- ppcor::pcor(CCDcorDat, method = 'spearman')

diffp = list()
diffp[[1]] <- matrix(0, nrow = length(CCDpcormat$estimate[,1]), ncol = length(CCDpcormat$estimate[,1]))
colnames(diffp[[1]]) <- colnames(CCDpcormat$estimate)
rownames(diffp[[1]]) <- rownames(CCDpcormat$estimate)
diffp[[2]] <- matrix(0, nrow = length(CCDpcormat$estimate[,1]), ncol = length(CCDpcormat$estimate[,1]))
colnames(diffp[[2]]) <- colnames(CCDpcormat$estimate)
rownames(diffp[[2]]) <- rownames(CCDpcormat$estimate)

for (i in 1:length(CCDpcormat$estimate[,1])){
  for (j in 1:length(CCDpcormat$estimate[,1])){

    tdiffp <- psych::r.test(n = NTpcormat$n[1],
                            n2 = CCDpcormat$n[1],
                            r12 = CCDpcormat$estimate[i,j],
                            r34 = NTpcormat$estimate[i,j])

    diffp[[1]][i,j] <- tdiffp$z
    diffp[[2]][i,j] <- tdiffp$p

  }
}

parcor1 <- ggcorrplot::ggcorrplot(corr = NTpcormat$estimate, p.mat = NTpcormat$p.value, lab = T, type = 'upper')+
  theme(legend.position = c(0.75, 0.25),
        plot.title = element_text(size = 18,hjust = 0.5))+
  labs(title = 'Partial Correlation')
parcor2 <- ggcorrplot::ggcorrplot(corr = CCDpcormat$estimate, p.mat = CCDpcormat$p.value, lab = T, type = 'upper')+
  theme(legend.position = c(0.75, 0.25))
parcor3 <- ggcorrplot::ggcorrplot(corr = diffp[[1]], p.mat = diffp[[2]], lab = T, type = 'upper')+
  theme(legend.position = c(0.75, 0.25))+
  scale_fill_gradient2(low = 'white', mid = 'white', high = 'red', name = 'Z-score', midpoint = 1.5, limits = c(0,4))+
  theme(legend.key = element_rect(fill = "white", colour = "black"))

# Figure S2

(cor1|parcor1)/(cor2|parcor2)
cor3|parcor3

# Permutation testing of correlations -------------------------------------

# A test on a correlation coefficient.

r.obtNT  <- cor(NTcorDat$Mistrust, NTcorDat$Credulity, method =
                 'spearman')
r.obtCCD <- cor(CCDcorDat$Mistrust, CCDcorDat$Credulity, method =
                  'spearman')

r.diff = r.obtCCD - r.obtNT

cat("The difference in correlation is ",r.diff,'\n')

nreps <- 10000
r.random <- matrix(NA, nrow = nreps, ncol = 3)

for (i in 1:nreps) {
  Ya <- NTcorDat$Mistrust
  Xa <- sample(corDat$Credulity, length(NTcorDat$Credulity), replace = FALSE)
  Yb <- CCDcorDat$Mistrust
  Xb <- sample(CCDcorDat$Credulity, length(CCDcorDat$Credulity), replace = FALSE)
  r.random[i,1] <- cor(Xa,Ya)
  r.random[i,2] <- cor(Xb,Yb)
  r.random[i,3] <- r.random[i,2] - r.random[i,1]
}

proba <- length(r.random[,3][r.random[,3] >= r.diff])/nreps
cat("Probability randomized r >= r.obtNT",proba)

r.obt <- round(r.diff, digits = 2)

r.random %>%
  as.data.frame() %>%
  rename(Diff = 3) %>%
  ggplot(aes(Diff))+
  geom_histogram(binwidth = 0.01, position = 'dodge')+
  labs(title = expression(paste("Distribution around ",rho, "= 0")),
       x = "r from randomized samples")+
  geom_text(x = r.obt+0.05, y = 175, label = r.obt, check_overlap = T, color = 'blue')+
  geom_vline(xintercept = c(r.obt), color = c('blue'), size = 1.5)+
  theme_bw()

# Dimension reduction slice ---------------------------------------------------------------

#Remove comments to run your own random sample; use the csv files above
#to directly replicate Figure 3D & E in the paper.

#trainCont = sort(sample(nrow(pcadf), nrow(pcadf)*.7))
#variables_list <- c('ICAR', 'Gullibility', 'Credulity', 'Mistrust', 'Trust', 'Social Intelligence', 'McA-SL', 'Autism', 'Paranoia')
#
##Integrate out nuisance variables
#
#integrated_out_df <- data %>%
#  na.omit() %>%
#  ungroup() %>%
#  rename(ICAR = ICAR,
#         #`Insensitivity` = Insense, `Persuadability` = Persuade,
#         Credulity = Credulity, Mistrust = Mistrust, Trust = Trust,
#         `Social Intelligence` = SocIntell, `Autism` = AutismR, Education = Education.quantised) %>%
#  mutate(`McA-SL` = (ComLadder + NatLadder)/2,
#         Paranoia = Persec + SocRef,
#         Gullibility = Persuade + Insense)
#
#
#for (k in 1:length(variables_list)){
#  dependent <- variables_list[k]
#  y <- lm(integrated_out_df[,dependent] ~ Age + Sex, data = integrated_out_df)
#  integrated_out_df[,dependent] <- y$residuals
#}
#
#pcadf <- integrated_out_df  %>%
#  filter(Group == 'NT') %>%
#  dplyr::select(all_of(variables_list))
#
#pcadfCCD <- integrated_out_df %>%
#  filter(Group == 'CCD') %>%
#  dplyr::select(all_of(variables_list))
#
#trainPCA <- pcadf[trainCont,]
#testPCA  <- rbind(pcadf[-trainCont,],
#                  pcadfCCD)
#

res.pca  <- prcomp(trainPCA,
                  scale = TRUE,
                  center = T)

summary(res.pca)

# Use the model to predict out of sample participants
rownames(testPCA) <- 1:length(rownames(testPCA))
pred  <- predict(res.pca, newdata = testPCA);

# create DF for plotting figure 3
plotpoints <- pred %>%
  as.data.frame() %>%
  mutate(Group = c(rep('Test NT', length(testPCA[1:(86-length(trainPCA[,1])),1])),
                   rep('CCD', length(((86-length(trainPCA[,1]))+1):length(testPCA[,1]))))) %>%
  rbind(res.pca$x %>%
          as.data.frame() %>%
          mutate(Group = rep('Train NT', length(res.pca$x[,1]))))

# Permutation testing of LOOCV ----------------------------------

# To directly replicate the empirical data in the paper, load the csvs loaded at the top.
# To perform your own permutations, uncomment the code below.

reps = 250 # no of repetitions
nPC  = 3   # no of PCs to retain

#
#permuteDF        <- regressed_data_NT
#loading_list     <- list()
#LOOCVlist        <- matrix(NA, ncol = 3, nrow = reps)
#confusPermute    <- matrix(0, ncol = 3, nrow = 4)
#averagedVariance <- matrix(0, nrow = reps, ncol = nPC)

#Run the loop to permute the dimension reduction and LOOCV
#Due to the random seed, varying results will occur on each run

#for (i in 1:reps){
#
#  # Set Seed
#  set.seed(mysamp(1, i, 100, 0, 1000, 1000))
#
#  # Take some random rows (70%)
#  trainIndex <- sort(sample(nrow(permuteDF), nrow(permuteDF)*.7))
#
#  #Create the training and test sets
#  Train_permute <- permuteDF[ trainIndex,]
#  Valid_permute <- permuteDF[-trainIndex,]
#  testPCA_permute<- rbind(Valid_permute,
#                          pcadfCCD)
#
#  # Run the PCA
#  res.pca_permute  <- prcomp(Train_permute,
#                             scale = TRUE,
#                             center = T)
#  sumrespermute <- summary(res.pca_permute)
#  averagedVariance[i, 1:nPC] <- as.numeric(sumrespermute$importance[2,1:nPC])
#
#  loading_list[[i]] <- res.pca_permute
#
#  #Predict new data conditioned on the PCA
#  predpermute  <- predict(loading_list[[i]], newdata = testPCA_permute)
#
#  LOOCVmat <- predpermute %>%
#    as.data.frame() %>%
#    mutate(Group = c(rep('NT', length(Valid_permute[,1])),
#                     rep('CCD', length(pcadfCCD[,1]))),
#           Group = factor(Group)
#    )
#
#  # Run the LOOCV
#  ctrl <- trainControl(method = 'LOOCV', number = 10, savePredictions = T, classProbs = T)
#  modelp <- train(Group ~ PC1 + PC2 + PC3, data = LOOCVmat,
#                  method = 'bayesglm',
#                  trControl = ctrl,
#                  tuneLength = 5,
#                  preProc = c("center", "scale"))
#
#  #Compute the confusion matrix
#  confus <- modelp$pred %>%
#    group_by(pred, obs) %>%
#    summarise(n = n()/reps, .groups = 'keep')
#
#  confusPermute[1:4, 3] <- confusPermute[1:4, 3] + as.numeric(confus[1:4, 3] %>% as.matrix)
#  LOOCVlist[i, 1:3]     <- as.matrix(modelp$results)
#
#  #visualise each repetition on the fly
#  cat('\n Completed ', i, ' of ', reps,
#      '| Average Variance = ', round(as.numeric(sumrespermute$importance[2,1:nPC]), 2),
#      ' | Accuracy = ', round(as.numeric(modelp$results[2]), 3))
#
#}

averagedVariance %>%
  as.data.frame() %>%
  pivot_longer(1:nPC, 'PC', values_to = 'Var') %>%
  group_by(PC) %>%
  mutate(Var = as.numeric(Var)) %>%
  summarise(
    meanVar = mean(Var),
    sdVar = sd(Var),
    CIlower = (meanVar - ((1.96 * sdVar)/sqrt(reps))),
    CIupper = (meanVar + ((1.96 * sdVar)/sqrt(reps)))) %>%
  distinct()

permutedLOOCV <- LOOCVlist %>%
  as.data.frame() %>%
  mutate(V2 = as.numeric(V2),
         meanV2 = mean(V2),
         sdV2 = sd(V2),
         CIupper = (meanV2 + ((1.96 * sdV2)/sqrt(reps))),
         CIlower = (meanV2 - ((1.96 * sdV2)/sqrt(reps))))
mean(permutedLOOCV$meanV2)
mean(permutedLOOCV$CIlower)
mean(permutedLOOCV$CIupper)

#Averaged correlation matrix

#for (i in 1:reps) {loading_list[[i]] <- abs(loading_list[[i]]$rotation)/250}
#loading_list_absc <- do.call(rbind,loading_list);
meanPCs <- loading_list_absc %>%
  as.data.frame() %>%
  mutate(Vars = rownames(loading_list_absc)) %>%
  pivot_longer(1:3, 'PC', values_to = 'Absolute Loading') %>%
  group_by(Vars, PC) %>%
  summarise(
    `Absolute Loading SD` = sd(`Absolute Loading`)*250,
    `Absolute Loading` = sum(`Absolute Loading`),
    CIlower = (`Absolute Loading` - ((1.96 * `Absolute Loading SD`)/sqrt(250))),
    CIupper = (`Absolute Loading` + ((1.96 * `Absolute Loading SD`)/sqrt(250))))

# Figure 3 ----------------------------------------------------------------

fvizpca <- fviz_pca_var(res.pca,
                        alpha.var="contrib",
                        col.var = "contrib", # Color by contributions to the PC
                        gradient.cols = c('#00A8E8', '#00171F'),
                        repel = TRUE

)+
  scale_color_gradient(name = 'Contribution',
                       low = '#00A8E8',  high = '#00171F')+
  scale_alpha_continuous(name = 'Contribution')+
  labs(x = 'PC1', y = 'PC2')+
  theme_tq()+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        plot.title = element_blank(),
        legend.position = 'right',
        legend.background = element_rect(color = 'black'),
        legend.key.width = unit(0.2, 'cm'),
        legend.key.height = unit(0.4, 'cm'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) &
  plot_annotation(tag_levels = 'A')

space.plot <- ggplot(plotpoints, aes(PC1, PC2, color = Group, shape = Group, size = PC3, alpha = ifelse(Group == 'Train NT', 0.7, 1)))+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  geom_point()+
  #scale_size_continuous(limits=c(-3, 3), breaks=seq(-3, 3, by=1), range = c(1, 7))+
  scale_shape_discrete()+
  scale_alpha_continuous(guide = 'none', limits = c(0.5, 1))+
  scale_color_brewer(palette = 'Set1')+
  theme_bw()+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 15),
        legend.position = 'right',
        legend.background = element_rect(color = 'black'),
        legend.key.size = unit(0.75, 'cm'))

permutedLOOCVplot <- ggplot(permutedLOOCV, aes(V2)) +
  geom_histogram(binwidth = 0.01)+
  geom_vline(xintercept = median(as.numeric(LOOCVlist[,2])), size = 1.5, color = 'red', linetype = 2)+
  geom_label(aes(x = median(as.numeric(LOOCVlist[,2])) - 0.1, y = 30),
             label = paste('Mean = ', round(mean(as.numeric(LOOCVlist[,2])), 3)), size = 8, color = 'red')+
  labs(x = 'Accuracy of permuted LOOCV model (250 reps)', y = 'Count')+
  coord_cartesian(xlim =c(0, 1))+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        panel.background = element_blank())

dimload2 <- meanPCs %>%
  filter(`Absolute Loading` > 0.30) %>%
  ggplot(aes(Vars, PC, fill = `Absolute Loading`))+
  geom_tile()+
  geom_text(aes(label = round(`Absolute Loading`, 2)), size = 5, color = 'white')+
  scale_color_gradient(breaks = c(0.25, 0.7))+
  theme(axis.title = element_blank(),
        axis.text  = element_text(size = 18),
        axis.text.x  = element_text(angle = 45, vjust = 0.7),
        panel.background = element_rect(fill = 'NA'),
        legend.title = element_text(size = 12),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.background = element_rect(color = 'black'),
        legend.key.width = unit(1.3,'cm'))

confmat2 <- ggplot(confusPermute %>%
                     as.data.frame() %>%
                     rename(pred = 1, obs = 2, n = 3) %>%
                     mutate(pred = confus$pred,
                            obs = confus$obs),
                   aes(pred, obs, fill = n))+
  geom_tile()+
  geom_label(aes(label = round(n, 2)), size = 12)+
  scale_fill_gradient(name = 'Averaged Count', low = '#BFDBF7', high = '#DB222A')+
  labs(x = 'Predicted', y = 'Observed')+
  theme_tq()+
  theme(legend.position = 'top',
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,4,1,4), 'cm'),
        legend.key.width = unit(1, 'cm'),
        legend.background = element_rect(color = 'black'))

(dimload2 | confmat2)
(permutedLOOCVplot / (space.plot|fvizpca))
