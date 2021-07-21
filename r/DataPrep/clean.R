# Clean

dAll$duration_P1

table(dAll$duration_P2)


dAll %>%
  filter(is.na(dAll$duration_P2)) -> missing


dAll %>%
  filter(duration_P2 > quantile(dAll$duration_P2, 0.025, na.rm = T)) -> d

d %>%
  filter(duration_P1 > quantile(d$duration_P1, 0.025, na.rm = T)) -> d
