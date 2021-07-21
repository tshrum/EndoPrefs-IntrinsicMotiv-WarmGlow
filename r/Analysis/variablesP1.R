# Part 1, Variables

d <- data.frame(rowID = 1:length(r$StartDate))  # creating new dataframe for usable data  

d$mturk <- r$Q2  # Checked for duplicated IDs, 1 present - removed instance
d$duration <- as.numeric(r$`Duration (in seconds)`)/60
d$startTime <- r$StartDate

d$NAcount.1 <- rowSums(is.na(r[,35:63])) + rowSums(is.na(r[,68:88]))

#### Demographic Variables ####
d$age <- 2019 - as.numeric(r$Q4)

d$female <- as.numeric(r$Q5 == "Female")
d$male <- as.numeric(r$Q5 == "Male")

d$americanIndian <- ifelse(is.na(r$Q6_1), 0, ifelse(r$Q6_1 == "American Indian or Alaskan Native", 1, 0))
d$asian <- ifelse(is.na(r$Q6_2), 0, ifelse(r$Q6_2 == "Asian", 1, 0))
d$black <- ifelse(is.na(r$Q6_3), 0, ifelse(r$Q6_3 == "Black or African American", 1, 0))
d$latinx <- ifelse(is.na(r$Q6_4), 0, ifelse(r$Q6_4 == "Hispanic or Latino Origin", 1, 0))
d$pacificIslander <- ifelse(is.na(r$Q6_5), 0, ifelse(r$Q6_5 == "Native Hawaiian or Other Pacific Islander", 1, 0))
d$white <- ifelse(is.na(r$Q6_6), 0, ifelse(r$Q6_6 == "White", 1, 0))
d$other <- as.numeric(ifelse(is.na(r$Q6_7_TEXT), 0, 1))  # Only three others - mixed and native american

# creating regions according to US Census breakup
stateRegion <- data.frame(state = state.name, region = state.region)
stateRegion <- rbind(stateRegion , data.frame(state="District of Columbia", region="South"))
stateRegion$region <- dplyr::recode(stateRegion$region, "North Central" = "Midwest")
d$state <- r$Q7
d <- d %>%
  left_join(stateRegion, by = "state")

# Changing education from ordered factor to numeric for comparative analysis
d$edu<- r$Q8
d$edu_n <- ifelse(d$edu== "Less than high school diploma or equivalent", 1, NA)
d$edu_n <- ifelse(d$edu== "High school diploma or equivalent", 2, d$edu_n)
d$edu_n <- ifelse(d$edu== "Associates degree", 3, d$edu_n)
d$edu_n <- ifelse(d$edu== "Trade school degree or certificate", 4, d$edu_n)
d$edu_n <- ifelse(d$edu== "Bachelor's degree", 5, d$edu_n)
d$edu_n <- ifelse(d$edu== "Graduate degree (Master’s, PhD, MD, JD, etc)", 6, d$edu_n)

#Changing income from ordered factor to numeric
d$inc <- r$Q9
d$inc_n <- ifelse(d$inc== "Less than $25,000", 12500, NA)
d$inc_n <- ifelse(d$inc== "$25,000 - $49,999", 32500, d$inc_n)
d$inc_n <- ifelse(d$inc== "$50,000 - $74,999", 62500,d$inc_n)
d$inc_n <- ifelse(d$inc== "$75,000 - $99,999", 87500,d$inc_n)
d$inc_n <- ifelse(d$inc== "$100,000 - $149,999", 125000,d$inc_n)
d$inc_n <- ifelse(d$inc== "$150,000 - $199,999", 175000,d$inc_n)
d$inc_n <- ifelse(d$inc== "$200,000 or more", 200000,d$inc_n)
d$inc <- d$inc_n/1000
d <- dplyr::select(d, -inc_n)

# Political Party Spectrum
poliFactor <- factor(r$Q10)
d$poli_n <- dplyr::recode(poliFactor, "Half of the time, I vote for Democrats and half of the time, I vote for Republicans" = 0, 
                          "I nearly always vote for Democrats" = 2, 
                          "I vote for Democrats more often than I vote Republicans" = 1,
                          "I vote for Republicans more often than I vote for Democrats" = -1,
                          "I am eligible to vote, but I never vote" = NULL, 
                          "I nearly always vote for Republicans" = -2,
                          "I am not eligible to vote" = NULL)

d$voteD <- ifelse(d$poli_n>0,1,0)
d$voteR <- ifelse(d$poli_n<0,1,0)


#### Environmental Identity and Attitudes ####
d$id_environmentalistP1 <- agree(r$Q13)
d$id_exaggeratedCrisisP1 <- reverse_agree(r$Q15)  #negatively impacts index
d$id_spaceshipP1 <- agree(r$Q16)
d$id_ecoCatastropheP1 <- agree(r$Q17)
d$id_natureStrongerP1 <- reverse_agree(r$Q18)  #negatively impacts index
d$id_humansAbusingEnviroP1 <- agree(r$Q19)

# Attitude and Identity Index
IDvars <- dplyr::select(d,id_environmentalistP1:id_humansAbusingEnviroP1)

#checking index consistency
#psych::alpha(IDvars, na.rm = TRUE, check.keys = TRUE) # std.alpha = .85, strong consistency

IDvars <- data.frame(scale(IDvars))

p <- sapply(IDvars[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$idIndexP1 = rowMeans(pdf)

rm(IDvars, p, pdf)

#### Past Week Pro-environmental Behaviors (PEBs) ####

# Conversation and Sharing of Climate Change Content
d$pw_watchReadP1 <- numtimes(r$Q21)
d$pw_shareP1 <- numtimes(r$Q22)
d$pw_socialMediaConvoP1 <- numtimes(r$Q23)
d$pw_participateConvoP1 <- numtimes(r$Q24)
d$pw_startConvoP1 <- numtimes(r$Q25)

# Converse PEBs Index Only
pwConvoPEBsvars <- dplyr::select(d,pw_watchReadP1:pw_startConvoP1) 

# checking index consistency
# psych::alpha(pwConvoPEBsvars, na.rm = TRUE) # alpha = .86, strong consistency

pwConvoPEBsvars <- data.frame(scale(pwConvoPEBsvars))

p <- sapply(pwConvoPEBsvars[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$pwConverseIndexP1 = rowMeans(pdf)

rm(pwConvoPEBsvars, p, pdf)


# Daily PW PEBS
d$pw_recycleP1 <- times(r$Q27)
d$pw_lightsOffP1 <- times(r$Q28)
d$pw_compostP1 <- times(r$Q29)
d$pw_avoidMeatP1 <- times(r$Q30)
d$pw_productImpactP1 <- times(r$Q31)
d$pw_altTransportationP1 <- times(r$Q32)
d$pw_reduceWaterP1 <- times(r$Q33)
d$pw_reusableBottleP1 <- times(r$Q34)
d$pw_washColdP1<- times(r$Q35)
d$pw_hangDryP1 <- times(r$Q36)
d$pw_reusableBagsP1 <- times(r$Q39)
d$pw_useDisposableProductsP1 <- reverse_times(r$Q37)  #negatively impacts index
d$pw_useKeurigP1 <- reverse_times(r$Q38)  #negatively impacts index
d$pw_usePlasticWaterP1 <- reverse_times(r$Q40)  #negatively impacts index

# creating index of P1 PEBs (including conversation vars)
pwPEBsvars <- dplyr::select(d,pw_watchReadP1:pw_usePlasticWaterP1, -pwConverseIndexP1)
# mutate(pw_useDisposableProductsP1 = pw_useDisposableProductsP1*-1,
#        pw_useKeurigP1 = pw_useKeurigP1*-1,
#        pw_usePlasticWaterP1 = pw_usePlasticWaterP1*-1)

# checking index consistency
# psych::alpha(pwPEBsvars, na.rm = TRUE, check.keys=TRUE) # std.alpha = .85, strong consistency, negative variables reversed
#testPWPEBIndexP1 <- composite(pwPEBsvars, R = 17:19, Zitems = TRUE, rel = TRUE)  # Used to confirm that index results are correct, and they are the same


pwPEBsvarsS <- data.frame(scale(pwPEBsvars))

p <- sapply(pwPEBsvarsS[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$pwPEBsIndexP1 = rowMeans(pdf)

rm(pwPEBsvars, pwPEBsvarsS, p, pdf)

#### Climate Concern ####
d$cc_SeriousnessP1 <- serious(r$Q41)
d$cc_HarmTimeP1 <- when(r$Q42)
d$cc_PersonalHarmP1 <- concern(r$Q43)

d$ccIndexP1 <- 0
d$ccIndexP1 <- actions(d$cc_SeriousnessP1, d$ccIndexP1)
d$ccIndexP1 <- actions(d$cc_HarmTimeP1, d$ccIndexP1)
d$ccIndexP1 <- actions(d$cc_PersonalHarmP1, d$ccIndexP1)
d$ccIndexP1 <- as.vector(d$ccIndexP1)

ccVars <- dplyr::select(d, cc_SeriousnessP1:cc_PersonalHarmP1)
# psych::alpha(ccVars, na.rm = TRUE)  # alpha = .85 

rm(ccVars)

d$cc_CauseP1 <- factor(r$Q44)  
d$cc_CauseP1_n <- dplyr::recode(d$cc_CauseP1, "I don't know" = 0,
                                "The Earth is getting warmer mostly because of human activity such as burning fossil fuels" = 3,
                                "The Earth is getting warmer mostly because of natural patterns in the Earth’s environment" = 2,
                                "There is no solid evidence that the Earth is getting warmer" = 1)

#### Warm Glow ####
d$wg_goodP1 <- likelys(r$Q59)
d$wg_positiveP1 <- likelys(r$Q60)

# Index of Warm Glow responses
d$warmGlowIndexP1 <- 0
d$warmGlowIndexP1 <- actions(d$wg_goodP1, d$warmGlowIndexP1)
d$warmGlowIndexP1 <- actions(d$wg_positiveP1, d$warmGlowIndexP1)
d$warmGlowIndexP1 <- as.vector(d$warmGlowIndexP1)

#checking index consistency
# wgVars <- dplyr::select(d,wg_goodP1:wg_positiveP1) 
# psych::alpha(wgVars, na.rm = TRUE) # alpha = .92, strong consistency
# 
# rm(wgVars)

#### PEBs Intentions ####

# Intent to Converse and Share Climate Change Content
d$intent_watchReadP1 <- likely5(r$Q62)
d$intent_shareP1 <- likely5(r$Q63)
d$intent_socialMediaConvoP1 <- likely5(r$Q64)
d$intent_participateConvoP1 <- likely5(r$Q65)
d$intent_startConvoP1 <- likely5(r$Q66)

# Intent Converse PEBs Index Only
intentConvoPEBsvars <- dplyr::select(d,intent_watchReadP1:intent_startConvoP1) 

# checking index consistency
# psych::alpha(intentConvoPEBsvars, na.rm = TRUE) # alpha = .9, very strong consistency

intentConvoPEBsvars <- data.frame(scale(intentConvoPEBsvars))

p <- sapply(intentConvoPEBsvars[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$intentConverseIndexP1 = rowMeans(pdf)

rm(intentConvoPEBsvars, p, pdf)


# Intent to Perform Daily PEBS
d$intent_recycleP1 <- likely5(r$Q67)
d$intent_lightsOffP1 <- likely5(r$Q68)
d$intent_compostP1 <- likely5(r$Q69)
d$intent_avoidMeatP1 <- likely5(r$Q70)
d$intent_productImpactP1 <- likely5(r$Q71)
d$intent_altTransportationP1 <- likely5(r$Q72)
d$intent_reduceWaterP1 <- likely5(r$Q73)
d$intent_reusableBottleP1 <- likely5(r$Q74)
d$intent_washColdP1<- likely5(r$Q75)
d$intent_hangDryP1 <- likely5(r$Q76)
d$intent_reusableBagsP1 <- likely5(r$Q79)
d$intent_useDisposableProductsP1 <- reverse_likely(r$Q77)  #negatively impacts index
d$intent_useKeurigP1 <- reverse_likely(r$Q78)  #negatively impacts index
d$intent_usePlasticWaterP1 <- reverse_likely(r$Q80)  #negatively impacts index


# creating index of P1 intentions (including conversation vars)
intentPEBsvars <- dplyr::select(d,intent_watchReadP1:intent_usePlasticWaterP1, -intentConverseIndexP1) 
# mutate(intent_useDisposableProductsP1 = intent_useDisposableProductsP1*-1,
#        intent_useKeurigP1 = intent_useKeurigP1*-1,
#        intent_usePlasticWaterP1 = intent_usePlasticWaterP1*-1)

# checking index consistency
# psych::alpha(intentPEBsvars, na.rm = TRUE, check.keys = TRUE) # alpha = .88, very strong consistency

intentPEBsvarsS <- data.frame(scale(intentPEBsvars))

p <- sapply(intentPEBsvarsS[,], function(x, y) {x <- ifelse(is.na(x), 0, x)
z <- x + y
z}, y = 0)
pdf <- data.frame(p)
d$intentPEBsIndexP1 = rowMeans(pdf)

rm(intentPEBsvars, intentPEBsvarsS, p, pdf)

#### Treatments ####
# extrinsic - Pay participants and donate to nonprofit
# intrinsic - Donate to nonprofit only
# control - Pay participants only

d$treatment <- r$trt
d$task <- coalesce(r$Q49, r$Q129, r$Q54, r$Q130, r$Q58, r$Q131)
d$taskSpaced <- gsub(",", " ", d$task)  # replacing commas with spaces to properly count those who only used commas 
d$wordCount <- sapply(d$taskSpaced, wordcount)
d$charity <- coalesce(r$Q46, r$Q51)
d$taskTime <- as.numeric(coalesce(r$`Q47_Page Submit`, r$`Q52_Page Submit`, r$`Q56_Page Submit`))/60

d$intrinsic <- ifelse(d$treatment == "intrinsic", 1, 0)
d$extrinsic <- ifelse(d$treatment == "extrinsic", 1, 0)
d$control <- ifelse(d$treatment == "control", 1, 0)

dP1 <- d
#rm(r, d, stateRegion)  #Removing r, d, and stateRegion dataframes from environment to keep clean
