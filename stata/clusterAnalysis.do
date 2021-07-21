*Intrinsic vs. Extrinsic Motivation
* Cluster analysis - did not produce very interesting results

*Data file produced with R from Intrinsic-vs-Extrinsic git project. 
*Master plus hatchGrantAnalysis

clear
import delimited /Users/tshrum/Projects/Intrinsic-vs-Extrinsic/data/timeseries.csv, numericcols(5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)


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
