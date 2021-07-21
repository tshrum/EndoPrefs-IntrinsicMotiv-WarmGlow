# Treatment Assignment


random <- compareGroups(treatment ~ charity + wordCount + 
                             pwPEBsIndexP1 + 
                             ccIndexP1 + idIndexP1 + voteR + voteD +
                             age + male + edu_n + inc + white + black + latinx,
                             data = d)
summary(random)

randomTable <- createTable(random,  type = 1, 
                              hide.no = "yes", show.n = FALSE)

export2latex(randomTable, header.labels = c(p.overall = "p-value"),
             size = "footnotesize",
             caption = "Randomization check for the two treatment groups and the control group.",
             label = "Randomized",
             loc.caption = "bottom")

pairwise.t.test(d$pwPEBsIndexP1, d$treatment)
