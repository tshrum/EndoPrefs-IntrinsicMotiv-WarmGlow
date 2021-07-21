*Intrinsic vs. Extrinsic Motivation

*Data file produced with R from Intrinsic-vs-Extrinsic git project. 
*Master plus hatchGrantAnalysis

clear
import delimited /Users/tshrum/Projects/Intrinsic-vs-Extrinsic/data/timeseries.csv, numericcols(5 6 7 8 9 10 11 12 13 14 15 16 17)

encode cluster, gen(cluster_factor)

egen zcc_seriousness = std(cc_seriousness)
egen zcc_harmtime = std(cc_harmtime)
egen zcc_cause = std(cc_cause)
egen zcc_personalharm = std(cc_personalharm)
egen zcc_avg = rmean(zcc_seriousness zcc_harmtime zcc_personalharm)

gen ccindex_extrinsic = ccindex * extrinsic
gen ccindex_intrinsic = ccindex * intrinsic
gen warmglow_extrinsic = warmglow * extrinsic
gen warmglow_intrinsic = warmglow * intrinsic
gen intent_extrinsic = intent * extrinsic
gen intent_intrinsic = intent * intrinsic

*Analyzing treatment impact on intent
reg intent i.treatment if (time == 1)
reg intent pebs i.treatment if (time == 1)
reg intent pebs i.treatment if (time == 2)
reg intent pebs i.treatment ccindex  warmglow enviroid if (time == 1)

*Analyzing treatment impact on pebs
reg pebs i.treatment if (time == 2)
reg pebs i.treatment intent if (time == 2)
reg pebs i.treatment warmglow ccindex if (time == 2)
reg pebs i.treatment ccindex ccindex_extrinsic ccindex_intrinsic if (time == 2)
reg pebs i.treatment warmglow warmglow_extrinsic warmglow_intrinsic if (time == 2)
reg pebs i.treatment ccindex ccindex_extrinsic ccindex_intrinsic warmglow warmglow_extrinsic warmglow_intrinsic if (time == 2)


diff pebs if (treatment == 0 | treatment == 1), t(treatment) p(time) cluster(rowidx)
diff warmglow if (treatment == 0 | treatment == 1), t(treatment) p(time) cluster(rowidx)
diff enviroid if (treatment == 0 | treatment == 1), t(treatment) p(time) cluster(rowidx)
diff ccindex if (treatment == 0 | treatment == 1), t(treatment) p(time)  cluster(rowidx)

diff pebs if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff warmglow if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff enviroid if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff ccindex if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff zcc_avg if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)

diff id_enviro if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)


diff cc_seriousness if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff zcc_seriousness if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)

diff zcc_harmtime if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff zcc_cause if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff zcc_personalharm if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)


diff wg_good if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff wg_positive if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)

reg pebs cluster_factor if (time == 1)
reg pebs intent##intrinsic intent##extrinsic cluster_factor##extrinsic cluster_factor##intrinsic if (time == 2)
reg pebs intent cluster_factor##intrinsic if (time == 2)


tobit pebs intrinsic extrinsic if time ==2

tobit pebs intrinsic##after##wordcount extrinsic##after wordcount cc

tobit intent intrinsic extrinsic pebs if time ==1
tobit intent intrinsic extrinsic if time ==2

diff pebs if (treatment == 0 | treatment == 2), t(treatment) p(time) cluster(rowidx) 

diff pebs if (treatment == 0 | treatment == 1), t(treatment) p(time) cluster(rowidx)
diff pebs if (treatment == 0 | treatment == 2), t(treatment) p(time) cluster(rowidx) ddd(cluster_factor)

diff pebs if (treatment == 0 & cluster_factor == 1 | treatment == 2 & cluster_factor == 1), t(intrinsic) p(after) cluster(rowidx) cov(intent)
diff pebs if (treatment == 0 & cluster_factor == 2 | treatment == 2 & cluster_factor == 2), t(intrinsic) p(after) cluster(rowidx)
diff pebs if (treatment == 0 & cluster_factor == 3 | treatment == 2 & cluster_factor == 3), t(intrinsic) p(after) cluster(rowidx)

diff pebs if (treatment == 0 & cluster_factor == 1 | treatment == 1 & cluster_factor == 1), t(extrinsic) p(after) cluster(rowidx)
diff pebs if (treatment == 0 & cluster_factor == 2 | treatment == 1 & cluster_factor == 2), t(extrinsic) p(after) cluster(rowidx)
diff pebs if (treatment == 0 & cluster_factor == 3 | treatment == 1 & cluster_factor == 3), t(extrinsic) p(after) cluster(rowidx)

diff pebs if (treatment == 2 & cluster_factor == 1 | treatment == 1 & cluster_factor == 1), t(extrinsic) p(after) cluster(rowidx)
diff pebs if (treatment == 2 & cluster_factor == 2 | treatment == 1 & cluster_factor == 2), t(extrinsic) p(after) cluster(rowidx)
diff pebs if (treatment == 2 & cluster_factor == 3 | treatment == 1 & cluster_factor == 3), t(extrinsic) p(after) cluster(rowidx)



diff pebs if (treatment == 0 | treatment == 1), t(extrinsic) p(after) cluster(rowidx) 
diff pebs if (treatment == 0 & cluster_factor == 1 | treatment == 1 & cluster_factor == 1), t(extrinsic) p(after) cluster(rowidx) 
diff pebs if (treatment == 0 & cluster_factor == 2 | treatment == 1 & cluster_factor == 2), t(extrinsic) p(after) cluster(rowidx) 
diff pebs if (treatment == 0 & cluster_factor == 3 | treatment == 1 & cluster_factor == 3), t(extrinsic) p(after) cluster(rowidx) 


diff pebs if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx) cov(ccindex)
diff pebs if (treatment == 0 & cluster_factor == 1 | treatment == 2 & cluster_factor == 1), t(intrinsic) p(after) cluster(rowidx) 
diff pebs if (treatment == 0 & cluster_factor == 2 | treatment == 2 & cluster_factor == 2), t(intrinsic) p(after) cluster(rowidx) 
diff pebs if (treatment == 0 & cluster_factor == 3 | treatment == 2 & cluster_factor == 3), t(intrinsic) p(after) cluster(rowidx) 

diff warmglow if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff warmglow if (treatment == 0 & cluster_factor == 1 | treatment == 2 & cluster_factor == 1), t(intrinsic) p(after) cluster(rowidx) 
diff warmglow if (treatment == 0 & cluster_factor == 2 | treatment == 2 & cluster_factor == 2), t(intrinsic) p(after) cluster(rowidx) 
diff warmglow if (treatment == 0 & cluster_factor == 3 | treatment == 2 & cluster_factor == 3), t(intrinsic) p(after) cluster(rowidx) 

diff enviroid if (treatment == 0 | treatment == 2), t(intrinsic) p(after) cluster(rowidx)
diff enviroid if (treatment == 0 & cluster_factor == 1 | treatment == 2 & cluster_factor == 1), t(intrinsic) p(after) cluster(rowidx) 
diff enviroid if (treatment == 0 & cluster_factor == 2 | treatment == 2 & cluster_factor == 2), t(intrinsic) p(after) cluster(rowidx) 
diff enviroid if (treatment == 0 & cluster_factor == 3 | treatment == 2 & cluster_factor == 3), t(intrinsic) p(after) cluster(rowidx) 

diff ccindex if (treatment == 0 | treatment == 2), t(intrinsic) p(after)  cluster(rowidx)
diff ccindex if (treatment == 0 & cluster_factor == 1 | treatment == 2 & cluster_factor == 1), t(intrinsic) p(after) cluster(rowidx) 
diff ccindex if (treatment == 0 & cluster_factor == 2 | treatment == 2 & cluster_factor == 2), t(intrinsic) p(after) cluster(rowidx) 
diff ccindex if (treatment == 0 & cluster_factor == 3 | treatment == 2 & cluster_factor == 3), t(intrinsic) p(after) cluster(rowidx) 


diff ccindex if (treatment == 0 | treatment == 1), t(extrinsic) p(after)  cluster(rowidx)
diff ccindex if (treatment == 0 & cluster_factor == 1 | treatment == 1 & cluster_factor == 1), t(extrinsic) p(after) cluster(rowidx) 
diff ccindex if (treatment == 0 & cluster_factor == 2 | treatment == 1 & cluster_factor == 2), t(extrinsic) p(after) cluster(rowidx) 
diff ccindex if (treatment == 0 & cluster_factor == 3 | treatment == 1 & cluster_factor == 3), t(extrinsic) p(after) cluster(rowidx) 


diff intent if (treatment == 0 | treatment == 2), t(intrinsic) p(after)  cluster(rowidx)
diff ccind if (treatment == 0 & cluster_factor == 1 | treatment == 2 & cluster_factor == 1), t(intrinsic) p(after) cluster(rowidx) 
diff ccindex if (treatment == 0 & cluster_factor == 2 | treatment == 2 & cluster_factor == 2), t(intrinsic) p(after) cluster(rowidx) 
diff ccindex if (treatment == 0 & cluster_factor == 3 | treatment == 2 & cluster_factor == 3), t(intrinsic) p(after) cluster(rowidx) 




