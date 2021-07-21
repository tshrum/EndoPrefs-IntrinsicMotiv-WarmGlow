## Endogenous Preferences Paper ##
# Created: 02/20/2021



## Dropping 5 participants who failed to choose a charity in the intrinsic and extrinsic treatments
d %>%
  filter(treatment == "intrinsic" & !is.na(charity) | treatment == "extrinsic" & !is.na(charity) | treatment == "control") -> d


#### Modifying Variables ####

d$inc <- d$inc/1000

# Standardizing variables
d$educ_z <- scale(d$edu_n)
d$warmGlowIndexP1 <- scale(d$warmGlowIndexP1)
d$warmGlowIndexP2 <- scale(d$warmGlowIndexP2)
d$pwPEBsIndexP1 <- scale(d$pwPEBsIndexP1)
d$pwPEBsIndexP2 <- scale(d$pwPEBsIndexP2)
d$ccIndexP1 <- scale(d$ccIndexP1)
d$ccIndexP2 <- scale(d$ccIndexP2)
d$idIndexP1 <- scale(d$idIndexP1)
d$idIndexP2 <- scale(d$idIndexP2)

# Treatment variables
d$intrinsic <- ifelse(d$treatment == "intrinsic", 1, 0)
d$extrinsic <- ifelse(d$treatment == "extrinsic", 1, 0)
d$control <- ifelse(d$treatment == "control", 1, 0)

# Treatment X Charity
d$intrinsicSierraClub <- ifelse(d$intrinsic == 1 & d$charity == "Sierra Club - Vermont Chapter", 1, 0)
d$intrinsicEDF <- ifelse(d$intrinsic == 1 & d$charity == "Environmental Defense Fund", 1, 0)
d$intrinsicHistorical <- ifelse(d$intrinsic == 1 & d$charity == "The Vermont Historical Society", 1, 0)
d$extrinsicSierraClub <- ifelse(d$extrinsic == 1 & d$charity == "Sierra Club - Vermont Chapter", 1, 0)
d$extrinsicEDF <- ifelse(d$extrinsic == 1 & d$charity == "Environmental Defense Fund", 1, 0)
d$extrinsicHistorical <- ifelse(d$extrinsic == 1 & d$charity == "The Vermont Historical Society", 1, 0)


d %>%
  filter(treatment != "extrinsic") -> dInCon
d %>%
  filter(treatment != "intrinsic") -> dExCon
d %>%
  filter(treatment != "control") -> dInEx



#### Charity Choice ####
sum(!is.na(d$charity), na.rm = T)
sum(d$charity == "Environmental Defense Fund", na.rm = T)/sum(!is.na(d$charity), na.rm = T)
sum(d$charity == "Sierra Club - Vermont Chapter", na.rm = T)/sum(!is.na(d$charity), na.rm = T)
sum(d$charity == "The Vermont Historical Society", na.rm = T)/sum(!is.na(d$charity), na.rm = T)



#### Treatment Effort ####
taskEffort <- summarySE(d, measurevar="wordCount", groupvars=c("treatment", "charity"))

table(d$treatment, d$charity)

png("taskEffort_byTreatmentCharity.png", units="in", width=7, height=4, res=600)
ggplot(taskEffort, aes(x=treatment, y=wordCount, fill=charity)) + 
  geom_bar(position=position_dodge(), stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin=wordCount-se, ymax=wordCount+se),
                width=.15,                    # Width of the error bars
                position=position_dodge(.7)) +
  xlab("Treatment Group") +
  ylab("Task Effort (word count)")
dev.off()

te <- lm(wordCount ~ treatment, data = d)
te <- lm(wordCount ~ charity, data = d, subset = extrinsic == 1)
te <- lm(wordCount ~ charity, data = d, subset = intrinsic == 1)
te <- lm(wordCount ~ charity*treatment, data = d, subset = control == 0)
te1 <- lm(wordCount ~ treatment + pwPEBsIndexP1 + warmGlowIndexP1 + ccIndexP1 + idIndexP1 +  age + male + inc + edu_n, data = d)
summary(te)

stargazer(te, te1,
          dep.var.labels = c("Task Effort (words)"),
          covariate.labels = c("Extrinsic^I", "Intrinsic^I", "PEBs^z (P1)", "ClimateConcern^z (P1)", "EnvironmentalID^z (P1)", "Age (years)", "Male^I", "Income (1000s)", "Education^z"),
          no.space = T,
          omit.stat = c("rsq", "f", "res.dev"),
          font.size = "small",
          label = "taskEffort")

#### Basic Analysis ####
summary(lm(intentPEBsIndexP1 ~ treatment + pwPEBsIndexP1, data = d))
summary(lm(intentPEBsIndexP2 ~ treatment + pwPEBsIndexP1, data = d))
summary(lm(pwPEBsIndexP2 ~ treatment + pwPEBsIndexP1, data = d))

summary(lm(intentPEBsIndexP1 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1, data = d))
summary(lm(intentPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1, data = d))
summary(lm(pwPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1, data = d))

summary(lm(intentPEBsIndexP1 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d))
summary(lm(intentPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d))
summary(lm(pwPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d))

m1 <- lm(intentPEBsIndexP1 ~ treatment + pwPEBsIndexP1, data = d)
m2 <- lm(intentPEBsIndexP2 ~ treatment + pwPEBsIndexP1, data = d)
m3 <- lm(pwPEBsIndexP2 ~ treatment + pwPEBsIndexP1, data = d)

stargazer(m1, m2, m3,
          dep.var.labels = c("PEB Intentions^z (P1)", "PEB Intentions^z (P2)", "Reported PEBs^z (P2)"),
          covariate.labels = c("Extrinsic^I", "Intrinsic^I", "Reported PEBs^z (P1)"),
          no.space = T,
          omit.stat = c("rsq", "f", "res.dev"),
          font.size = "small",
          label = "basicPEBs_TreatmentOnly")

m1 <- lm(intentPEBsIndexP1 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1, data = d)
m2 <- lm(intentPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1, data = d)
m3 <- lm(pwPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1, data = d)

stargazer(m1, m2, m3,
          dep.var.labels = c("PEB Intentions^z (P1)", "PEB Intentions^z (P2)", "Reported PEBs^z (P2)"),
          covariate.labels = c("Extrinsic^I", "Intrinsic^I", "Reported PEBs^z (P1)", "Warm Glow (P2)", "Warm Glow X Extrinsic", "Warm Glow X Intrinsic"),
          no.space = T,
          omit.stat = c("rsq", "f", "res.dev"),
          font.size = "small",
          label = "basicPEBs_TreatmentWarmGlow")


m1 <- lm(intentPEBsIndexP1 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d)
m2 <- lm(intentPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d)
m3 <- lm(pwPEBsIndexP2 ~ treatment*warmGlowIndexP2 + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d)

stargazer(m1, m2, m3,
          dep.var.labels = c("PEB Intentions^z (P1)", "PEB Intentions^z (P2)", "Reported PEBs^z (P2)"),
          covariate.labels = c("Extrinsic^I", "Intrinsic^I", "Reported PEBs^z (P1)", "Warm Glow (P2)", 
                               "Enviromentalist ID^z (P1)", "Climate Concern^z (P1)", "Age", "Female^I",
                               "Education^z", "Income (1000s)", "Region: South", "Region: Midwest",
                               "Region: West",
                               "Warm Glow X Extrinsic", "Warm Glow X Intrinsic"),
          no.space = T,
          omit.stat = c("rsq", "f", "res.dev"),
          font.size = "small",
          label = "basicPEBs_TreatmentWarmGlowDems")

m1 <- lm(intentPEBsIndexP1 ~ treatment + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d)
m2 <- lm(intentPEBsIndexP2 ~ treatment + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d)
m3 <- lm(pwPEBsIndexP2 ~ treatment + pwPEBsIndexP1 + idIndexP1 + ccIndexP1 + age + female + educ_z + inc + region, data = d)

stargazer(m1, m2, m3,
          dep.var.labels = c("PEB Intentions^z (P1)", "PEB Intentions^z (P2)", "Reported PEBs^z (P2)"),
          covariate.labels = c("Extrinsic^I", "Intrinsic^I", "Reported PEBs^z (P1)",  
                               "Enviromentalist ID^z (P1)", "Climate Concern^z (P1)", "Age", "Female^I",
                               "Education^z", "Income (1000s)", "Region: South", "Region: Midwest",
                               "Region: West"),
          no.space = T,
          omit.stat = c("rsq", "f", "res.dev"),
          font.size = "small",
          label = "basicPEBs_TreatmentWarmGlowDems")

summary(m1)
summary(m2)
summary(m3)
intent1_df <- data.frame(summary(m1)$coefficients[2:3,1:2])
intent1_df <- cbind(Treatment = c("Extrinsic", "Intrinsic"), intent1_df)
row.names(intent1_df) <- c("extrinsic", "intrinsic")

ggplot(data = intent1_df, aes(x = Treatment, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - `Std..Error`, ymax = Estimate + `Std..Error`), width = .2) +
  ylim(-.5,.5) +
  geom_hline(aes(yintercept = 0), alpha = .3) +
  ylab("Intentions to Perform PEBs (standardized)")

PEBs_df <- data.frame(summary(m3)$coefficients[2:3,1:2])
PEBs_df <- cbind(Treatment = c("Extrinsic", "Intrinsic"), PEBs_df)
row.names(PEBs_df) <- c("extrinsic", "intrinsic")

ggplot(data = PEBs_df, aes(x = Treatment, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - `Std..Error`, ymax = Estimate + `Std..Error`), width = .2) +
  ylim(-.5,.5) +
  geom_hline(aes(yintercept = 0), alpha = .3) +
  ylab("PEB Performance (standardized)")




#### Warm Glow Analysis ###
wg1a <- lm(warmGlowIndexP1 ~ treatment, data = d)
wg1b <- lm(warmGlowIndexP1 ~ treatment + pwPEBsIndexP1 + ccIndexP1 + idIndexP1 + age + male + inc + edu_n, data = d)
wg2a <- lm(warmGlowIndexP2 ~ treatment, data = d)
wg2b <- lm(warmGlowIndexP2 ~ treatment + pwPEBsIndexP1 + ccIndexP1 + idIndexP1 + age + male + inc + edu_n, data = d)
wg2a <- lm(warmGlowIndexP2 ~ treatment + pwPEBsIndexP1 + ccIndexP1 + idIndexP1 + age + male + inc + edu_n, data = d)
summary(wg2a)

stargazer(wg1a, wg1b, wg2a, wg2b,
          dep.var.labels = c("Warm Glow^z (P1)", "Warm Glow^z (P1)", "Warm Glow^z (P2)", "Warm Glow^z (P2)"),
          covariate.labels = c("Extrinsic^I", "Intrinsic^I", "PEBs^z (P1)", "ClimateConcern^z (P1)", "EnvironmentalID^z (P1)", "Age (years)", "Male^I", "Income (1000s)", "Education^z"),
          no.space = T,
          omit.stat = c("rsq", "f", "res.dev"),
          font.size = "small",
          label = "warmGlow")

summary(wg1a)
summary(wg1b)
summary(wg2a)
summary(wg2b)

wg_df <- data.frame(summary(wg2a)$coefficients[2:3,1:2])
wg_df <- cbind(Treatment = c("Extrinsic", "Intrinsic"), wg_df)
row.names(wg_df) <- c("extrinsic", "intrinsic")

ggplot(data = wg_df, aes(x = Treatment, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = Estimate - `Std..Error`, ymax = Estimate + `Std..Error`), width = .2) +
  ylim(-.5,.5) +
  geom_hline(aes(yintercept = 0), alpha = .3) +
  ylab("Anticipated Warm Glow (standardized)")


#### Mediation Analysis ###


sem_model_intrinsic = '
  warmGlowIndexP2 ~ a*intrinsic
  pwPEBsIndexP2 ~ c*intrinsic + b*warmGlowIndexP2
 
  # direct effect
  direct := c
 
  # indirect effect
  indirect := a*b
 
  # total effect
  total := c + (a*b)
'

model_sem_intrinsic = sem(sem_model_intrinsic, data=dInCon, se='boot', bootstrap=10000)
summary(model_sem_intrinsic, rsq=T) 

summary(lm(pwPEBsIndexP2 ~ intrinsic + warmGlowIndexP2, data = d))

sem_model_extrinsic= '
  warmGlowIndexP2 ~ a*extrinsic 
  pwPEBsIndexP2 ~ c*extrinsic + b*warmGlowIndexP2
 
  # direct effect
  direct := c
 
  # indirect effect
  indirect := a*b
 
  # total effect
  total := c + (a*b)
'

model_sem_extrinsic = sem(sem_model_extrinsic, data=dExCon, se='boot', bootstrap=10000)
summary(model_sem_extrinsic, rsq=T) 


sem_model_intrinsicBaselinePEBs = '
  warmGlowIndexP2 ~ a*intrinsic + pwPEBsIndexP1
  pwPEBsIndexP2 ~ c*intrinsic + pwPEBsIndexP1 + b*warmGlowIndexP2
 
  # direct effect
  direct := c
 
  # indirect effect
  indirect := a*b
 
  # total effect
  total := c + (a*b)
'

model_sem_intrinsicBaselinePEBs = sem(sem_model_intrinsicBaselinePEBs, data=dInCon, se='boot', bootstrap=10000)
summary(model_sem_intrinsicBaselinePEBs, rsq=T)  

sem_model_extrinsicBaselinePEBs = '
  warmGlowIndexP2 ~ a*extrinsic + pwPEBsIndexP1
  pwPEBsIndexP2 ~ c*extrinsic + pwPEBsIndexP1 + b*warmGlowIndexP2
 
  # direct effect
  direct := c
 
  # indirect effect
  indirect := a*b
 
  # total effect
  total := c + (a*b)
'

model_sem_extrinsicBaselinePEBs = sem(sem_model_extrinsicBaselinePEBs, data=dExCon, se='boot', bootstrap=10000)
summary(model_sem_extrinsicBaselinePEBs, rsq=T)  


