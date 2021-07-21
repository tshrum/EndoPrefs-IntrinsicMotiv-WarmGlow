* Analysis for Paper Submission - Final
*Intrinsic vs. Extrinsic Motivation

*Data file produced with R from Intrinsic-vs-Extrinsic git project. 
*Run Master.R then stataPrep.R

clear
import delimited /Users/tshrum/Projects/Intrinsic-vs-Extrinsic/data/all.csv, numericcols(_all)



gen envirocharity_wordcount = envirocharity*wordcount
gen warmglow_intrinsic = warmglowindexp2*intrinsic
gen warmglow_extrinsic = warmglowindexp2*extrinsic

egen zrecycle1 = std(pw_recyclep1)
egen zlights1 = std(pw_lightsoffp1)
egen zcompost1 = std(pw_compostp1)
egen zavoidmeat1 = std(pw_avoidmeatp1)
egen zproductimpact1 = std(pw_productimpactp1)
egen zalttransport1 = std(pw_alttransportationp1)
egen zreducewater1 = std(pw_reducewaterp1)
egen zreusablebottle1 = std(pw_reusablebottlep1)
egen zwashcold1 = std(pw_washcoldp1)
egen zhangdry1 = std(pw_hangdryp1)
egen zreusablebags1 = std(pw_reusablebagsp1)
egen zdisposable1 = std(pw_usedisposableproductsp1)
egen zkeurig1 = std(pw_usekeurigp1)
egen zplasticwater1 = std(pw_useplasticwaterp1)
egen zparticipateconvo1 = std(pw_participateconvop1)
egen zstartconvo1 = std(pw_startconvop1)
egen zclimateinfo1 = std(pw_watchreadp1)
egen zsocialmediaconvop1 = std(pw_socialmediaconvop1)
egen zsharep1 = std(pw_sharep1)
egen pebs1 = rmean(zplasticwater1 zkeurig1 zdisposable1 zrecycle1 zlights1 zcompost1 zavoidmeat1 zproductimpact1 zalttransport1 zreducewater1 zreusablebottle1 zwashcold1 zhangdry1 zreusablebags1 zclimateinfo1 zparticipateconvo1 zstartconvo1 zsocialmediaconvop1 zsharep1)
egen pebs1_subset = rmean(zlights1 zproductimpact1 zreducewater1 zreusablebottle1 zwashcold1 zclimateinfo1 zparticipateconvo1 zstartconvo1)
egen pebs1_warmglowsubset = rmean(zproductimpact1 zreducewater1 zwashcold1 )
egen pebs1_cherry = rmean(zlights1 zavoidmeat1 zproductimpact1 zalttransport1 zreducewater1 zwashcold1 zreusablebags1 zplasticwater1 zparticipateconvo1 zstartconvo1)
egen pebs1_ = rmean(zrecycle1 zlights1 zcompost1 zavoidmeat1 zproductimpact1 zalttransport1 zreducewater1 zreusablebottle1 zwashcold1 zhangdry1 zreusablebags1 zclimateinfo1 zparticipateconvo1 zstartconvo1)
gen pebs1_w = zrecycle1/(0.5669) + zlights1/(0.3241) + zcompost1/(.6515) + zavoidmeat1/.594 + zproductimpact1/.4748 + zalttransport1/.6531 + zreducewater1/.4566 + zreusablebottle1/.5917 + zwashcold1/.4476 + zhangdry1/.5364 + zreusablebags1/.5508 + zclimateinfo1/.2162 + zparticipateconvo1/.3656 + zstartconvo1/.3627

egen zrecycle2 = std(pw_recyclep2)
egen zlights2 = std(pw_lightsoffp2)
egen zcompost2 = std(pw_compostp2)
egen zavoidmeat2 = std(pw_avoidmeatp2)
egen zproductimpact2 = std(pw_productimpactp2)
egen zalttransport2 = std(pw_alttransportationp2)
egen zreducewater2 = std(pw_reducewaterp2)
egen zreusablebottle2 = std(pw_reusablebottlep2)
egen zwashcold2 = std(pw_washcoldp2)
egen zhangdry2 = std(pw_hangdryp2)
egen zreusablebags2 = std(pw_reusablebagsp2)
egen zdisposable2 = std(pw_usedisposableproductsp2)
egen zkeurig2 = std(pw_usekeurigp2)
egen zplasticwater2 = std(pw_useplasticwaterp2)
egen zparticipateconvo2 = std(pw_participateconvop2)
egen zstartconvo2 = std(pw_startconvop2)
egen zclimateinfo2 = std(pw_watchreadp2)
egen zsocialmediaconvop2 = std(pw_socialmediaconvop2)
egen zsharep2 = std(pw_sharep2)
egen pebs2_all = rmean(zplasticwater2 zkeurig2 zdisposable2 zrecycle2 zlights2 zcompost2 zavoidmeat2 zproductimpact2 zalttransport2 zreducewater2 zreusablebottle2 zwashcold2 zhangdry2 zreusablebags2 zclimateinfo2 zparticipateconvo2 zstartconvo2 zsocialmediaconvop2 zsharep2)
egen pebs2 = rmean(zplasticwater2 zkeurig2 zdisposable2 zrecycle2 zlights2 zcompost2 zavoidmeat2 zproductimpact2 zalttransport2 zreducewater2 zreusablebottle2 zwashcold2 zhangdry2 zreusablebags2 zclimateinfo2 zparticipateconvo2 zstartconvo2)
egen pebs2_subset = rmean(zlights2 zproductimpact2 zreducewater2 zreusablebottle2 zwashcold2 zclimateinfo2 zparticipateconvo2 zstartconvo2)
egen pebs2_warmglowsubset = rmean(zproductimpact2 zreducewater2 zwashcold2 zclimateinfo2 zparticipateconvo2 zstartconvo2)
egen pebs2_warmglowsubset1 = rmean(zproductimpact2 zreducewater2 zwashcold2)
egen pebs1_warmglowsubset1 = rmean(zproductimpact1 zreducewater1 zwashcold1)
egen pebs2_cherry = rmean(zlights2 zavoidmeat2 zproductimpact2 zalttransport2 zreducewater2 zwashcold2 zreusablebags2 zplasticwater2 zparticipateconvo2 zstartconvo2)
egen pebs2_ = rmean(zrecycle2 zlights2 zcompost2 zavoidmeat2 zproductimpact2 zalttransport2 zreducewater2 zreusablebottle2 zwashcold2 zhangdry2 zreusablebags2 zclimateinfo2 zparticipateconvo2 zstartconvo2)
gen pebs2_w = zrecycle2/(0.5669) + zlights2/(0.3241) + zcompost2/(.6515) + zavoidmeat2/.594 + zproductimpact2/.4748 + zalttransport2/.6531 + zreducewater2/.4566 + zreusablebottle2/.5917 + zwashcold2/.4476 + zhangdry2/.5364 + zreusablebags2/.5508 + zclimateinfo2/.2162 + zparticipateconvo2/.3656 + zstartconvo2/.3627


gen change_pebs = pebs2 - pebs1
gen change_warmglow = warmglowindexp2 - warmglowindexp1
gen change_pebs_subset = pebs2_subset - pebs1_subset
gen change_pebs_warmglowsubset = pebs2_warmglowsubset - pebs1_warmglowsubset
gen change_pebs_warmglowsubset1 = pebs2_warmglowsubset1 - pebs1_warmglowsubset1
gen change_pebs_cherry = pebs2_cherry - pebs1_cherry

gen cc_naturalcausep1 = 1 if cc_causep1_n == 2
replace cc_naturalcausep1 = 0 if cc_causep1_n != 2


* PEB Index Analysis

reg pw_recyclep1 ccindexp1 
reg pw_lightsoffp1 ccindexp1
reg pw_compostp1 ccindexp1 
reg pw_avoidmeatp1 ccindexp1 
reg pw_productimpactp1 ccindexp1 
reg pw_alttransportationp1 ccindexp1 
reg pw_reducewaterp1 ccindexp1
reg pw_reusablebottlep1 ccindexp1 
reg pw_useplasticwaterp1 ccindexp1 
reg pw_washcoldp1 ccindexp1 
reg pw_hangdryp1 ccindexp1 
reg pw_reusablebagsp1 ccindexp1 
reg pw_usedisposableproductsp1 ccindexp1 
reg pw_usekeurigp1 ccindexp1 
reg pw_watchreadp1 ccindexp1 
reg pw_sharep1 ccindexp1 
reg pw_socialmediaconvop1 ccindexp1
reg  pw_participateconvop1 ccindexp1 
reg pw_startconvop1 ccindexp1 

reg pw_recyclep1 idindexp1 
reg pw_lightsoffp1 idindexp1
reg pw_compostp1 idindexp1 
reg pw_avoidmeatp1 idindexp1 
reg pw_productimpactp1 idindexp1 
reg pw_alttransportationp1 idindexp1 
reg pw_reducewaterp1 idindexp1
reg pw_reusablebottlep1 idindexp1 
reg pw_useplasticwaterp1 idindexp1 
reg pw_washcoldp1 idindexp1 
reg pw_hangdryp1 idindexp1 
reg pw_reusablebagsp1 idindexp1 
reg pw_usedisposableproductsp1 idindexp1 
reg pw_usekeurigp1 idindexp1 
reg pw_watchreadp1 idindexp1 
reg pw_sharep1 idindexp1 
reg pw_socialmediaconvop1 idindexp1
reg  pw_participateconvop1 idindexp1 
reg pw_startconvop1 idindexp1

reg pw_recyclep1 warmglowindexp1 
reg pw_lightsoffp1 warmglowindexp1
reg pw_compostp1 warmglowindexp1 
reg pw_avoidmeatp1 warmglowindexp1 
reg pw_productimpactp1 warmglowindexp1 
reg pw_alttransportationp1 warmglowindexp1 
reg pw_reducewaterp1 warmglowindexp1
reg pw_reusablebottlep1 warmglowindexp1 
reg pw_useplasticwaterp1 warmglowindexp1 
reg pw_washcoldp1 warmglowindexp1 
reg pw_hangdryp1 warmglowindexp1 
reg pw_reusablebagsp1 warmglowindexp1 
reg pw_usedisposableproductsp1 warmglowindexp1 
reg pw_usekeurigp1 warmglowindexp1 
reg pw_watchreadp1 warmglowindexp1 
reg pw_sharep1 warmglowindexp1 
reg pw_socialmediaconvop1 warmglowindexp1
reg  pw_participateconvop1 warmglowindexp1 
reg pw_startconvop1 warmglowindexp1

reg pw_recyclep2 pw_recyclep1
reg pw_lightsoffp2 pw_lightsoffp1
reg pw_compostp2 pw_compostp1
reg pw_avoidmeatp2 pw_avoidmeatp1
reg pw_productimpactp2 pw_productimpactp1
reg pw_alttransportationp2 pw_alttransportationp1
reg pw_reducewaterp2 pw_reducewaterp1
reg pw_reusablebottlep2 pw_reusablebottlep1
reg pw_useplasticwaterp2 pw_useplasticwaterp1]
reg pw_washcoldp2 pw_washcoldp1
reg pw_hangdryp2 pw_hangdryp1
reg pw_reusablebagsp2 pw_reusablebagsp1
reg pw_usedisposableproductsp2 pw_usedisposableproductsp1
reg pw_usekeurigp2 pw_usekeurigp1
reg pw_watchreadp2 pw_watchreadp1 
reg pw_sharep2 pw_sharep1
reg pw_socialmediaconvop2 pw_socialmediaconvop1
reg pw_participateconvop2  pw_participateconvop1
reg pw_startconvop2 pw_startconvop1 


reg pw_socialmediaconvop1 cc_denialp1 cc_humancausep1 cc_naturalcausep1

egen pebs1 = rmean(zplasticwater1 zkeurig1 zdisposable1 zrecycle1 zlights1 zcompost1 zavoidmeat1 zproductimpact1 zalttransport1 zreducewater1 zreusablebottle1 zwashcold1 zhangdry1 zreusablebags1 zclimateinfo1 zparticipateconvo1 zstartconvo1 zsharep1)
egen pebs2 = rmean(zplasticwater2 zkeurig2 zdisposable2 zrecycle2 zlights2 zcompost2 zavoidmeat2 zproductimpact2 zalttransport2 zreducewater2 zreusablebottle2 zwashcold2 zhangdry2 zreusablebags2 zclimateinfo2 zparticipateconvo2 zstartconvo2 zsharep2)
