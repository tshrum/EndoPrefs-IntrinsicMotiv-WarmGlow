# Merging data from 2 parts

# this combines data from the two waves into a final dataset called dAll
dAll <- full_join(dP1, dP2, by = "mturk")
dAll <- select(dAll, -mturk)  # removing identifiers
dAll <- dAll[-c(453),] # don't have P1 for them, not sure how they got access to P2... 


dP1 <- select(dP1, -mturk)
dP2 <- select(dP2, -mturk)





