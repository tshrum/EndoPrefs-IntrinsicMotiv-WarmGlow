# Retention Analysis #

r <- read_csv("/Users/tshrum/Projects/Intrinsic-vs-Extrinsic/data/Intrinsic_vs_Extrinsic_Motivation-Part_1_October_16_2019_edited.csv")
r <- tail(r, -2)  # taking out first two rows

r <- filter(r, ResponseId != "R_tQdQTzAIRYHyEKZ") %>%  # removing duplicate mturk id, first run had no treatment completed
      filter(Q2 != "TS" | is.na(Q2)) -> d1  # removing test run "TS"

d1[duplicated(d1$Q2),] -> dupsP1 

d1 %>%
  filter(ResponseId != "R_1Fb4P2BlFOGvL3l") %>%  # <mturk id> started first survey, stopped, started again. Answers don't match well. Consider dropping.
  filter(ResponseId != "R_1PXV9ZwBBiYiw6v") %>%  # <mturk id> started first survey, stopped, started again. Answers don't match well. Consider dropping.
  filter(ResponseId != "R_1OJIHAgIzm9THkJ") %>%  # <mturk id> started first survey, stopped, started again.
  filter(ResponseId != "R_54NLJxA08sIuPu1") -> d1  # <mturk id> started first survey, stopped, started again.

nrow(d1)
# 465 participants began first survey 

d1 %>%
  filter(is.na(Q2)) %>%
  nrow()
# 6 participants dropped out at the consent stage

d1 %>%
  filter(Finished == "FALSE") -> dropOutP1
nrow(dropOutP1)
# 13 participants started but did not complete the survey

d1 %>%
  filter(Finished == "TRUE") -> completedP1
nrow(completedP1)
1-nrow(dropOutP1)/nrow(completedP1)
# 452 participants completed the first survey out of 465 recruited, 96% completion rate

r <- read_csv("/Users/tshrum/Projects/Intrinsic-vs-Extrinsic/data/Intrinsic_vs_Extrinsic_Motivation-Part_2_October_18_2019.csv")
r <- tail(r, -2)  # taking out first two rows

r <- filter(r, ResponseId != "R_3IRfef4Y6qZrfBM") %>%  # removing duplicate mturk id, mturk id missing from first run where had no treatment completed
  filter(Q2 != "<mturk id>" | is.na(Q2)) %>%
  filter(Q2 != "test" | is.na(Q2)) -> d2

d2$mturk <- d2$Q2

d2 %>%
  filter(ResponseId != "R_33j8ALFMrcHSBfj") -> d2  # <mturkID> started second survey, didn't complete it. Came back 15 min later, started again, and finished it.
  
nrow(d2)
d2 %>%
  filter(Finished == "True") -> completedP2

d2 %>%
  filter(Finished == "False") -> dropOutP2

nrow(completedP2)/nrow(d1)
# 405 completed the second survey, for a start to finish retention and completion rate is 87%

r <- d1
source('~/Projects/Intrinsic-vs-Extrinsic/r/Analysis/variablesP1.R', echo=TRUE)

# Merging to compare groups
d %>%
  left_join(d2, by = "mturk") -> d

d[duplicated(d$mturk),] -> dups  # 5 missing MTurkID
dups %>%
  filter(!is.na(mturk)) -> dups

d %>%
  filter(Finished == "False" | is.na(Finished)) -> q  # Those who did not complete

d %>%
  filter(Finished == "True") -> f  # Those who finished both surveys

d$dropOut <- ifelse(d$Finished != "True" | is.na(d$Finished), 1, 0)

retention <- compareGroups(dropOut ~ treatment + charity + wordCount + 
                             pwPEBsIndexP1 + intentPEBsIndexP1 + 
                             ccIndexP1 + idIndexP1 + + voteR + voteD +
                             age + male + edu_n + inc + white + black + latinx 
                             , data = d)
retention
summary(retention)

retentionTable <- createTable(retention,  type = 1, 
                      hide.no = "yes", show.n = FALSE)

export2latex(retentionTable, header.labels = c(p.overall = "p-value"),
             size = "footnotesize",
             caption = "Retention analysis of demographic and other key variables.",
             label = "Retention",
             loc.caption = "bottom")

