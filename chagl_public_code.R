library(tidyverse)
library(lme4)

####load data

setwd() #enter your working directory here and save data file to it
fulldf <- read.csv('chagl_public_data.csv')

#explanation of variables
#subject: a unique code denoting the subject
#group: four levels, PAC, RAC, PSC, RSC (Progressive Affix Control, Regressive Affix Control, Progressive Stem Control, etc.)
#trial_num: trial number
#correct: a binary variable indicating if the subject answered correctly for that trial. 1 indicates correct, 0 indicates incorrect.
#correct_answer: the correct answer in string form
#first: the nonword that appeared first in the 2AFC pair for that trial
#second: the nonword that appeared second in the 2AFC pair for that trial
#selected_item: the item the subject selected that trial
#nits: number of intervening syllables between target and trigger; ranges 0 - 3
#direction: progressive or regressive
#locus: stem control or affix control
#language: the subject's native language (English or Spanish)
#item: a numeric variable coding the lexical item for that trial. Both /s/ and /sh/ variants of an item are included under a single item number.
#biphone.prob: the biphone probability of the correct item, according to Vitevitch & Luce (2004)
#phoneme.prob: the phonemic probability of the correct item, according to Vitevitch & Luce (2004)

#center the variables Trial & nits and subset the data

english <- subset(fulldf, language =="English") %>%
  mutate(trial.c = scale(trial_num),
         nits.c = scale(nits))

spanish <- subset(fulldf, language == "Spanish") %>%
  mutate(trial.c = scale(trial_num),
         nits.c = scale(nits))

#########################################
#Models
###########################################

#model for Study 1: 
#binomial logistic regression 
#with predictors group, number of intervening transparent syllables (nits - centered), and trial (centered)
#as well as predictor of biphone probability and interactions of group and trial and of nits and trial
#and response variable of 'correct'
#with random slope for subject

engmod  <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob +  (1 + trial.c + nits.c|subject) + (1|item), 
                   data=english, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                           optCtrl=list(maxfun=2e5)))
summary(engmod)

#add predicted values back to dataframe
english$y_mod_glmer <- fitted(engmod)

################################
#Model for Study 2

########
#binomial logistic regression 
#with predictors group, number of intervening transparent syllables (nits - centered), and trial (centered)
#as well as predictors of biphone probability and phoneme probability 
#and interactions of group and trial and of nits and trial
#and response variable of 'correct'
#with random slope for subject

spanmod <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob + phoneme.prob + (1 + trial.c|subject) + (1|item), 
                   data=spanish, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                           optCtrl=list(maxfun=2e5)))
summary(spanmod)

#add predicted values back to dataframe
spanish$y_mod_glmer <- fitted(spanmod)

#################################################
#Figures
#################################################

#Figure 1: plot by learner success, English learners

#calculate percent correct by subject

engsum <- english %>%
  group_by(subject, group) %>%
  summarise(correct = mean(correct, na.rm = TRUE))

#add a variable for success

engsum <- engsum %>%
  mutate(success = ifelse(correct>=0.6,"Successful Learner", "Non-learner"))

#count successful learners and transfer counts to dataframe 'engsum'

engsmallsum <- engsum %>%
  group_by(success, group) %>%
  summarise(count = length(unique(subject)))

engsmallsum <- engsmallsum %>%
  mutate(ID = paste(group, success, sep = " "))

engsum <- engsum %>%
  mutate(ID = paste(group, success, sep = " "))

engsum$count <- engsmallsum$count[match(engsum$ID,engsmallsum$ID)]

#plot Figure 1

fig1 <- ggplot(engsum)+
  aes(group, correct, fill=factor(success))+
  geom_boxplot(position = position_dodge()) +  
  theme_classic() +
  scale_fill_brewer(palette = "Accent")  +
  ggtitle("Proportion Correct by Group and Learner Sucess") +
  #coord_cartesian(ylim=c(0, 1)) +
  theme(text = element_text(size=14)) +
  annotate("text", x = .8, y = .475, label = "13") +
  annotate("text", x = 1.2, y = .83, label = "13") +
  annotate("text", x = 1.8, y = .505, label = "13") +
  annotate("text", x = 2.2, y = .9, label = "13") +
  annotate("text", x = 2.8, y = .49, label = "15") +
  annotate("text", x = 3.2, y = .755, label = "10") +
  annotate("text", x = 3.8, y = .52, label = "16") +
  annotate("text", x = 4.2, y = .78, label = "9") +
  # geom_text(aes(label = count), vjust = 7, color = "black") +
  labs(x=NULL, y = "Proportion Correct", fill = NULL) +
  geom_jitter(aes(color = success), size = 2, shape = 2, width = .1) +
  guides(color = "none") +
  scale_color_manual(values = c("#497449", "#72687f"))
fig1
ggsave("Fig1.tiff", width = 2400, height = 1800, units = "px", dpi = 300)

#Figure 2: English learners' performance over time


fig2 <- english %>%
  group_by(trial_num, group, language, locus, direction, subject) %>%
  summarise(correct = mean(correct, na.rm = TRUE), 
            fitted = mean(y_mod_glmer), na.rm = TRUE) %>%
  ggplot()+
  aes(x = trial_num, y = fitted, color = direction, linetype = locus)+
  scale_color_manual(values=c("#af7ac5", "#48c9b0"), labels=c("Progressive", "Regressive"))  +
  scale_linetype(labels=c("Affix-Controlled", "Stem-Controlled"))+
  theme_classic() +
  #  geom_point(alpha = 0.4)+
  coord_cartesian(ylim = c(0.4, 1))+
  annotate("text", x = 102, y = .715, label = "PAC", color = "gray20") +
  annotate("text", x = 102, y = .74, label = "PSC", color = "gray20") +
  annotate("text", x = 102, y = .675, label = "RAC", color = "gray20") +
  annotate("text", x = 102, y = .63, label = "RSC", color = "gray20") +
  labs(x="Trial Number", y = "Proportion Correct", color = "Direction", linetype = "Locus") +
  geom_smooth(method = "loess", size = 1.5, fill = "gray40")+
  guides(linetype = guide_legend(override.aes = list(color ="gray40"), linewidth = 0.7))
fig2
ggsave("Fig2.tiff", width = 2400, height = 1800, units = "px", dpi = 300)


#Figure 3: proportion correction by group and TTD, English learners

engsum2 <- english %>%
  group_by(group, nits, direction, locus, language) %>%
  summarise(
    correct.sd = sd(correct),
    correct.se = sd(correct)/sqrt(n()),
    correct = mean(correct))

fig3 <- ggplot(engsum2)+
  aes(group, correct, fill=factor(nits))+
  geom_col(position = position_dodge()) +  
  geom_errorbar(aes(ymin = correct - correct.se,
                    ymax = correct + correct.se),
                width = 0.2, color = "gray40",
                position = position_dodge(0.9))+
  theme_classic() +
  scale_fill_brewer(palette = "Accent")  +
  ggtitle("Proportion Correct by Group and Number of Intervening \nTransparent Syllables") +
  coord_cartesian(ylim=c(0, 1)) +
  theme(text = element_text(size=14)) +
  labs(x=NULL, y = "Proportion Correct", fill = "Number of \nIntervening \nTransparent \nSyllables") 
fig3
ggsave("Fig3.tiff", width = 2400, height = 1800, units = "px", dpi = 300)

#Figure 4: plot of learner success, Spanish learners

#calculate percent correct by subject

spansum <- spanish %>%
  group_by(subject, group) %>%
  summarise(correct = mean(correct, na.rm = TRUE))

#add a variable for success

spansum <- spansum %>%
  mutate(success = ifelse(correct>=0.6,"Successful Learner", "Non-learner"))

#count successful learners and transfer counts to dataframe 'spansum'

spansum <- spansum %>%
  mutate(ID = paste(group, success, sep = " "))

spansmallsum <- spansum %>%
  group_by(success, group) %>%
  summarise(correct = mean(correct, na.rm = TRUE), 
            count = length(unique(subject)))

#create figure

fig4 <- ggplot(spansum)+
  aes(group, correct, fill=factor(success))+
  geom_boxplot(position = position_dodge()) +  
  theme_classic() +
  scale_fill_brewer(palette = "Accent")  +
  ggtitle("Proportion Correct by Group and Learner Sucess") +
  #coord_cartesian(ylim=c(0, 1)) +
  theme(text = element_text(size=14)) +
  annotate("text", x = .8, y = .5, label = "12") +
  annotate("text", x = 1.2, y = .93, label = "13") +
  annotate("text", x = 1.8, y = .505, label = "18") +
  annotate("text", x = 2.2, y = .905, label = "8") +
  annotate("text", x = 2.8, y = .46, label = "10") +
  annotate("text", x = 3.2, y = .85, label = "15") +
  annotate("text", x = 3.8, y = .52, label = "12") +
  annotate("text", x = 4.2, y = .875, label = "13") +
  # geom_text(aes(label = count), vjust = 7, color = "black") +
  labs(x=NULL, y = "Proportion Correct", fill = NULL) +
  geom_jitter(aes(color = success), size = 2, shape = 2, width = .1) +
  guides(color = "none") +
  scale_color_manual(values = c("#497449", "#72687f"))
fig4
ggsave("Fig4.tiff", width = 2400, height = 1800, units = "px", dpi = 300)

#Figure 5: Spanish learners' performance over time
fig5 <- spanish %>%
  group_by(trial_num, group, language, locus, direction, subject) %>%
  summarise(correct = mean(correct, na.rm = TRUE), 
            fitted = mean(y_mod_glmer), na.rm = TRUE) %>%
  ggplot()+
  aes(x = trial_num, y = fitted, color = direction, linetype = locus)+
  scale_color_manual(values=c("#af7ac5", "#48c9b0"), labels=c("Progressive", "Regressive"))  +
  scale_linetype(labels=c("Affix-Controlled", "Stem-Controlled"))+
  theme_classic() +
  #  geom_point(alpha = 0.4)+
  coord_cartesian(ylim = c(0.4, 1))+
  annotate("text", x = 102, y = .76, label = "RAC", color = "gray20") +
  annotate("text", x = 102, y = .705, label = "PAC", color = "gray20") +
  annotate("text", x = 102, y = .675, label = "RSC", color = "gray20") +
  annotate("text", x = 102, y = .63, label = "PSC", color = "gray20") +
  labs(x="Trial Number", y = "Proportion Correct", color = "Direction", linetype = "Locus") +
  geom_smooth(method = "loess", size = 1.5, fill = "gray40")+
  guides(linetype = guide_legend(override.aes = list(color ="gray40"), linewidth = 0.7))
fig5
ggsave("Fig5.tiff", width = 2400, height = 1800, units = "px", dpi = 300)

#Figure 6: proportion correction by group and TTD, Spanish learners

spansum2 <- spanish %>%
  group_by(group, nits, direction, locus, language) %>%
  summarise(
    correct.sd = sd(correct),
    correct.se = sd(correct)/sqrt(n()),
    correct = mean(correct))

fig6 <- ggplot(spansum2)+
  aes(group, correct, fill=factor(nits))+
  geom_col(position = position_dodge()) +  
  geom_errorbar(aes(ymin = correct - correct.se,
                    ymax = correct + correct.se),
                width = 0.2, color = "gray40",
                position = position_dodge(0.9))+
  theme_classic() +
  scale_fill_brewer(palette = "Accent")  +
  ggtitle("Proportion Correct by Group and Number of Intervening \nTransparent Syllables") +
  coord_cartesian(ylim=c(0, 1)) +
  theme(text = element_text(size=14)) +
  labs(x=NULL, y = "Proportion Correct", fill = "Number of \nIntervening \nTransparent \nSyllables") 
fig6
ggsave("Fig6.tiff", width = 2400, height = 1800, units = "px", dpi = 300)

###########################################
#rerun models with different reference levels of group
#sub-analysis
###########################################

#English data

#PSC as reference level

english$group <- relevel(factor(english$group), ref = "PSC")

engmod_psc  <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob +  (1 + trial.c + nits.c|subject) + (1|item), 
                     data=english, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                             optCtrl=list(maxfun=2e5)))
summary(engmod_psc)

#RSC as reference level

english$group <- relevel(factor(english$group), ref = "RSC")

engmod_rsc  <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob +  (1 + trial.c + nits.c|subject) + (1|item), 
                     data=english, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                             optCtrl=list(maxfun=2e5)))
summary(engmod_rsc)

#RAC as reference level

english$group <- relevel(factor(english$group), ref = "RAC")

engmod_rac  <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob +  (1 + trial.c + nits.c|subject) + (1|item), 
                     data=english, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                             optCtrl=list(maxfun=2e5)))
summary(engmod_rac)

#spanish data
#PSC as reference level

spanish$group <- relevel(factor(spanish$group), ref = "PSC")

spanmod_psc <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob + phoneme.prob + (1 + trial.c|subject) + (1|item), 
                     data=spanish, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                             optCtrl=list(maxfun=2e5)))
summary(spanmod_psc)

#PSC as reference level

spanish$group <- relevel(factor(spanish$group), ref = "RSC")

spanmod_rsc <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob + phoneme.prob + (1 + trial.c|subject) + (1|item), 
                     data=spanish, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                             optCtrl=list(maxfun=2e5)))
summary(spanmod_rsc)

#RAC as reference level

spanish$group <- relevel(factor(spanish$group), ref = "RAC")

spanmod_rac <- glmer(correct ~ nits.c*trial.c + group * trial.c + biphone.prob + phoneme.prob + (1 + trial.c|subject) + (1|item), 
                     data=spanish, family = 'binomial', control=glmerControl(optimizer="bobyqa",
                                                                             optCtrl=list(maxfun=2e5)))
summary(spanmod_rac)