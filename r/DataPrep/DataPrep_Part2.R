# Data Prep for part one of Intrinsic vs. Extrinsic Motivation 2019 study

r <- read_csv("/Users/tshrum/Projects/Intrinsic-vs-Extrinsic/data/Intrinsic_vs_Extrinsic_Motivation-Part_2_October_18_2019.csv")
r <- tail(r, -2)  # taking out first two rows
r <- filter(r, Finished == "True" & Q2 != "test")  # removing people who didn't complete survey
r <- filter(r, ResponseId != "R_3IRfef4Y6qZrfBM" & Q2 != "A19D0SH9AQX9S9")  # removing duplicate mturk id and mturk id missing from first run where had no treatment completed


d <- data.frame(rowID_P2 = 1:length(r$StartDate))  # creating new dataframe for usable data  

d$mturk <- r$Q2  # Checked for duplicated IDs, 1 present - removed instance
d$responseId_P2 <- r$ResponseId
d$duration_P2 <- as.numeric(r$`Duration (in seconds)`)/60
d$startTime_P2 <- r$StartDate

d$NAcount_P2 <- rowSums(is.na(r[,20:69]))


#### Environmental Identity and Attitudes ####
d$id_environmentalistP2 <- agree(r$Q13)
d$id_exaggeratedCrisisP2 <- reverse_agree(r$Q15)  #negatively impacts index
d$id_spaceshipP2 <- agree(r$Q16)
d$id_ecoCatastropheP2 <- agree(r$Q17)
d$id_natureStrongerP2 <- reverse_agree(r$Q18)  #negatively impacts index
d$id_humansAbusingEnviroP2 <- agree(r$Q19)

# Attitude and Identity Index
IDvars <- select(d,id_environmentalistP2:id_humansAbusingEnviroP2)

# checking index consistency
# psych::alpha(IDvars, na.rm = TRUE, check.keys = TRUE) # std.alpha = .86, strong consistency, negative variables reversed

IDvars <- data.frame(scale(IDvars))

p <- sapply(IDvars[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$idIndexP2 = rowMeans(pdf)

rm(IDvars, p, pdf)

#### Past Week Pro-environmental Behaviors (PEBs) ####

# Conversation and Sharing of Climate Change Content
d$pw_watchReadP2 <- numtimes(r$Q21)
d$pw_shareP2 <- numtimes(r$Q22)
d$pw_socialMediaConvoP2 <- numtimes(r$Q23)
d$pw_participateConvoP2 <- numtimes(r$Q24)
d$pw_startConvoP2 <- numtimes(r$Q25)

# Converse PEBs Index Only
pwConvoPEBsvars <- select(d,pw_watchReadP2:pw_startConvoP2) 

#checking index consistency
#psych::alpha(pwConvoPEBsvars, na.rm = TRUE) # alpha = .85, strong consistency

pwConvoPEBsvars <- data.frame(scale(pwConvoPEBsvars))

p <- sapply(pwConvoPEBsvars[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$pwConverseIndexP2 = rowMeans(pdf)

rm(pwConvoPEBsvars, p, pdf)


# Daily PW PEBS
d$pw_recycleP2 <- times(r$Q27)
d$pw_lightsOffP2 <- times(r$Q28)
d$pw_compostP2 <- times(r$Q29)
d$pw_avoidMeatP2 <- times(r$Q30)
d$pw_productImpactP2 <- times(r$Q31)
d$pw_altTransportationP2 <- times(r$Q32)
d$pw_reduceWaterP2 <- times(r$Q33)
d$pw_reusableBottleP2 <- times(r$Q34)
d$pw_washColdP2<- times(r$Q35)
d$pw_hangDryP2 <- times(r$Q36)
d$pw_reusableBagsP2 <- times(r$Q39)
d$pw_useDisposableProductsP2 <- reverse_times(r$Q37)  #negatively impacts index
d$pw_useKeurigP2 <- reverse_times(r$Q38)  #negatively impacts index
d$pw_usePlasticWaterP2 <- reverse_times(r$Q40)  #negatively impacts index

# creating index of P2 PEBs (including conversation vars)
pwPEBsvars <- select(d,pw_watchReadP2:pw_usePlasticWaterP2, -pwConverseIndexP2)
# mutate(pw_useDisposableProductsP2 = pw_useDisposableProductsP2*-1,
#        pw_useKeurigP2 = pw_useKeurigP2*-1,
#        pw_usePlasticWaterP2 = pw_usePlasticWaterP2*-1)

#checking index consistency
# psych::alpha(pwPEBsvars, na.rm = TRUE, check.keys=FALSE, keys=c("pw_useDisposableProductsP2", "pw_useKeurigP2", "pw_usePlasticWaterP2")) # std.alpha = .83, strong consistency, negative variables reversed

pwPEBsvarsS <- data.frame(scale(pwPEBsvars))

p <- sapply(pwPEBsvarsS[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$pwPEBsIndexP2 = rowMeans(pdf)

rm(pwPEBsvars, pwPEBsvarsS, p, pdf)

#### Climate Concern ####
d$cc_SeriousnessP2 <- serious(r$Q41)
d$cc_HarmTimeP2 <- when(r$Q42)
d$cc_PersonalHarmP2 <- concern(r$Q43)

d$ccIndexP2 <- 0
d$ccIndexP2 <- actions(d$cc_SeriousnessP2, d$ccIndexP2)
d$ccIndexP2 <- actions(d$cc_HarmTimeP2, d$ccIndexP2)
d$ccIndexP2 <- actions(d$cc_PersonalHarmP2, d$ccIndexP2)
d$ccIndexP2 <- as.vector(d$ccIndexP2)

d$cc_CauseP2 <- factor(r$Q44)  
d$cc_CauseP2_n <- dplyr::recode(d$cc_CauseP2, "I don't know" = 0,
                         "The Earth is getting warmer mostly because of human activity such as burning fossil fuels" = 3,
                         "The Earth is getting warmer mostly because of natural patterns in the Earth’s environment" = 2,
                         "There is no solid evidence that the Earth is getting warmer" = 1)
ccVars <- select(d, cc_SeriousnessP2:cc_PersonalHarmP2)
# psych::alpha(ccVars, na.rm = TRUE)  # std.alpha = .85

rm(ccVars)

#### Warm Glow ####
d$wg_goodP2 <- likelys(r$Q59)
d$wg_positiveP2 <- likelys(r$Q60)

# Index of Warm Glow responses
d$warmGlowIndexP2 <- 0
d$warmGlowIndexP2 <- actions(d$wg_goodP2, d$warmGlowIndexP2)
d$warmGlowIndexP2 <- actions(d$wg_positiveP2, d$warmGlowIndexP2)
d$warmGlowIndexP2 <- as.vector(d$warmGlowIndexP2)

#checking index consistency
# wgVars <- select(d,wg_goodP2:wg_positiveP2)
# psych::alpha(wgVars, na.rm = TRUE) # alpha = 0.94, very strong consistency
# 
# rm(wgVars)

#### PEBs Intentions ####

# Intent to Converse and Share Climate Change Content
d$intent_watchReadP2 <- likely5(r$Q62)
d$intent_shareP2 <- likely5(r$Q63)
d$intent_socialMediaConvoP2 <- likely5(r$Q64)
d$intent_participateConvoP2 <- likely5(r$Q65)
d$intent_startConvoP2 <- likely5(r$Q66)

# Intent Converse PEBs Index Only
intentConvoPEBsvars <- select(d,intent_watchReadP2:intent_startConvoP2) 

# checking index consistency
# psych::alpha(intentConvoPEBsvars, na.rm = TRUE) # alpha = .91, very strong consistency

intentConvoPEBsvars <- data.frame(scale(intentConvoPEBsvars))

p <- sapply(intentConvoPEBsvars[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$intentConverseIndexP2 = rowMeans(pdf)

rm(intentConvoPEBsvars, p, pdf)


# Intent to Perform Daily PEBS
d$intent_recycleP2 <- likely5(r$Q67)
d$intent_lightsOffP2 <- likely5(r$Q68)
d$intent_compostP2 <- likely5(r$Q69)
d$intent_avoidMeatP2 <- likely5(r$Q70)
d$intent_productImpactP2 <- likely5(r$Q71)
d$intent_altTransportationP2 <- likely5(r$Q72)
d$intent_reduceWaterP2 <- likely5(r$Q73)
d$intent_reusableBottleP2 <- likely5(r$Q74)
d$intent_washColdP2<- likely5(r$Q75)
d$intent_hangDryP2 <- likely5(r$Q76)
d$intent_reusableBagsP2 <- likely5(r$Q79)
d$intent_useDisposableProductsP2 <- reverse_likely(r$Q77)  #negatively impacts index
d$intent_useKeurigP2 <- reverse_likely(r$Q78)  #negatively impacts index
d$intent_usePlasticWaterP2 <- reverse_likely(r$Q80)  #negatively impacts index


# creating index of P2 intentions (including conversation vars)
intentPEBsvars <- select(d,intent_watchReadP2:intent_usePlasticWaterP2, -intentConverseIndexP2)
  # mutate(intent_useDisposableProductsP2 = intent_useDisposableProductsP2*-1,
  #        intent_useKeurigP2 = intent_useKeurigP2*-1,
  #        intent_usePlasticWaterP2 = intent_usePlasticWaterP2*-1)

# checking index consistency
# psych::alpha(intentPEBsvars, na.rm = TRUE, check.keys = TRUE) # alpha = .85,  strong consistency

intentPEBsvarsS <- data.frame(scale(intentPEBsvars))

p <- sapply(intentPEBsvarsS[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$intentPEBsIndexP2 = rowMeans(pdf)

rm(intentPEBsvars, intentPEBsvarsS, p, pdf)

d <- filter(d, mturk != "test")
dP2 <- d
rm(r, d)  #Removing r, d, and stateRegion dataframes from environment to keep clean
