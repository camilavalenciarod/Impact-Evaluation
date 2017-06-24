*********************************************************
* IMPACT EVALUATION										*
* LECTURE: EXPERIMENTS	WITH SPILLOVERS					*
* MIGUEL ET AL (2004): WORMS							*
*														*
* Stanislao Maldonado									*
* Universidad del Rosario								*
* E-mail: stanislao.maldonado@urosario.edu.co			*
* Last change: 03/07/17									*
*********************************************************

set more off

cd "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\7. Class Material\2. Replication\1. Experiments\Miguel et al (2004)\Data\"
 

*----------------------------------*
* 1. PRE-TREATMENT CHARACTERISTICS *
*----------------------------------*

**** TABLE 1: PANEL A

* Start with Namelist data
	use "namelist.dta", clear 
	
* Describing dataset
	describe

* Each school is a distinct data point, weighted by number of pupils
	keep if visit==981 
	collapse sex elg98 stdgap yrbirth wgrp* (count) np=pupid, by (sch98v1) 

* Table
	bys wgrp: summ sex elg98 stdgap yrbirth [aw=np] 
	foreach var in sex elg98 stdgap yrbirth { 
		regress `var' wgrp1 wgrp2 [aw=np] 
		} 

		
**** TABLE 1: PANEL C

* Use School data
	use "schoolvar.dta", clear

* Describing dataset
	describe

* Create worm group indicators
	gen wgrp1 = (wgrp==1) 
	gen wgrp2 = (wgrp==2) 
	gen wgrp3 = (wgrp==3) 

* Normalize 1996 mock tests to be in units of individual std dev, equivalent to 1998, 1999
	replace mk96_s = mk96_s*(0.4357)/(0.8318) 

* Table
	bys wgrp: summ mk96_s distlake pup_pop latr_pup z_inf98 pop1_3km_updated pop1_36k_updated popT_3km_updated popT_36k_updated  
	foreach var in mk96_s distlake pup_pop latr_pup z_inf98 pop1_3km_updated pop1_36k_updated popT_3km_updated popT_36k_updated { 
			regress `var' wgrp1 wgrp2 
		} 

*-------------------------------------------------------------*
* 2. SIMPLE COMPARISON OF HEALTH OUTCOMES BY TREATMENT STATUS *
*-------------------------------------------------------------*

**** TABLE 5, PANEL A 

* Incorporating data on eligibility and parasitological exams
	use "namelist", clear 
	keep if visit==981 
	keep pupid sch98v1 wgrp elg98 elg99 
	sort pupid 
	save "f1", replace 

	use "wormed", clear 
	sort pupid 
	merge pupid using "f1" 
	tab _merge 
	keep if _merge==3 
	drop _merge 

* Generate low HB indicator
	gen hb100 = (hb<100) 
	replace hb100=. if hb==. 

* Restrict sample appropriately
	keep if any_ics99~=. 

* Table
	summ any_ics98 any_ics99 hw99_ics al99_who sm99_who tt99_ics if wgrp==1
	summ any_ics98 any_ics99 hw99_ics al99_who sm99_who tt99_ics if wgrp==2
	foreach var in any_ics99 hw99_ics al99_who sm99_who tt99_ics { 
		regress `var' wgrp1 if (wgrp==1 | wgrp==2), robust cluster(sch98v1) 
		} 

		
*---------------------------------------*
* 3. WITHIN-SCHOOL HEALTH EXTERNALITIES	*
*---------------------------------------*

* PREPARING DATA

* Incorporate namelist information
	use "namelist", clear 
	keep if visit==981 
	keep pupid elg98 elg99 sch98v1 wgrp* totpar98 stdgap std 
	sort pupid 
	save "f1", replace 
	clear 

*Incorporate compliance information	
	use "comply" 
	keep pupid any98 any99 
	sort pupid 
	merge pupid using "f1" 
	tab _merge 
	drop if _merge==1 
	drop _merge 
	sort pupid 
	save "f1", replace 
	clear 

*Incorporate infection information	
	use "wormed" 
	keep pupid numics98 numics99 hw99_ics al99_who sm99_who tt99_ics any_ics98 any_ics99 
	sort pupid 
	merge pupid using "f1" 
	tab _merge 
	drop if _merge==1 
	drop _merge 
	sort pupid 
	save "f1", replace 
	clear 

*Incorporate Pupil Questionnaire information	
	use "pupq" 
	keep pupid havelatr_98_33 waz_98 malaria_98_48 clean_98_15 
	sort pupid 
	merge pupid using "f1" 
	tab _merge 
	drop if _merge==1 
	drop _merge 
	sort pupid 

* Construct indicator for "child clean"
	gen     Iclean_98 = (clean_98_15==1) 
	replace Iclean_98 = . if clean_98_15==. 

* Restrict to those with non-missing eligibility data
	keep if elg98~=. 
	save "f1", replace 

* Generate indicator for in sample both 1998, 1999
	gen     sample99 = . 
	replace sample99 = 0 if (any_ics98~= . & any_ics99==.) 
	replace sample99 = 1 if (any_ics98~= . & any_ics99~=.) 

* Only consider groups 1,2 children
	keep if wgrp==1 | wgrp==2 


**** TABLE 6, PANEL A
	
	* Any moderate-heavy infection, Group 1, Treated 1998
		summ any_ics98 if (wgrp==1 & any98==1) & sample99==1 & elg98==1
	
	* Any moderate-heavy infection, Group 1, Untreated 1998
		summ any_ics98 if (wgrp==1 & any98==0) & sample99==1 & elg98==1
	
	* Proportion of 1998 parasitological, Group 1, Treated 1998
		summ sample99 if (wgrp==1 & any98==1) & any_ics98~=. & elg98==1
		
	* Proportion of 1998 parasitological, Group 1, Untreated 1998
		summ sample99 if (wgrp==1 & any98==0) & any_ics98~=.  & elg98==1

	foreach var in havelatr_98_33 stdgap waz_98 malaria_98_48 Iclean_98 { 
		summ `var' if (wgrp==1 & any98==1) & any_ics99~=. & elg98==1
		summ `var' if (wgrp==1 & any98==0) & any_ics99~=. & elg98==1 
		summ `var' if (wgrp==2 & any99==1) & any_ics99~=. & elg98==1
		summ `var' if (wgrp==2 & any99==0) & any_ics99~=. & elg98==1 

	* G1 treated 1998 vs G2 treated 1999
		regress `var' wgrp1 if ((wgrp==1 & any98==1) | (wgrp==2 & any99==1)) & any_ics99~=. & elg98==1, robust cluster(sch98v1) 
		
	* G1 untreated 1998 vs G2 untreated 1999
		regress `var' wgrp1 if ((wgrp==1 & any98==0) | (wgrp==2 & any99==0)) & any_ics99~=. & elg98==1, robust cluster(sch98v1) 
		}


**** TABLE 6, PANEL B

	*Girls <13 years, and all boys
		foreach var in any_ics99 hw99_ics al99_who sm99_who tt99_ics { 
			summ `var' if (wgrp==1 & any98==1) & any_ics99~=. & elg98==1
			summ `var' if (wgrp==1 & any98==0) & any_ics99~=. & elg98==1 
			summ `var' if (wgrp==2 & any99==1) & any_ics99~=. & elg98==1 
			summ `var' if (wgrp==2 & any99==0) & any_ics99~=. & elg98==1 
			regress `var' wgrp1 if ((wgrp==1 & any98==1) | (wgrp==2 & any99==1)) & any_ics99~=. & elg98==1, robust cluster(sch98v1) 
			regress `var' wgrp1 if ((wgrp==1 & any98==0) | (wgrp==2 & any99==0)) & any_ics99~=. & elg98==1, robust cluster(sch98v1)
		} 

	*Girls >=13 years
		summ any_ics98 if (wgrp==1 & any98==1) & sample99==1 & elg98==0
		summ any_ics98 if (wgrp==1 & any98==0) & sample99==1 & elg98==0
		summ any_ics99 if (wgrp==1 & any98==1) & any_ics99~=. & elg98==0
		summ any_ics99 if (wgrp==1 & any98==0) & any_ics99~=. & elg98==0
		summ any_ics99 if (wgrp==2 & any99==1) & any_ics99~=. & elg98==0
		summ any_ics99 if (wgrp==2 & any99==0) & any_ics99~=. & elg98==0
		regress any_ics99 wgrp1 if ((wgrp==1 & any98==1) | (wgrp==2 & any99==1)) & any_ics99~=. & elg98==0, robust cluster(sch98v1) 
		regress any_ics99 wgrp1 if ((wgrp==1 & any98==0) | (wgrp==2 & any99==0)) & any_ics99~=. & elg98==0, robust cluster(sch98v1)


	erase "f1.dta"		
		
*-----------------------------------------------------------*
* 4. WITHIN-SCHOOL AND ACROSS SCHOOL HEALTH EXTERNALITIES	*
*-----------------------------------------------------------*

* Incorporate namelist information
	use "namelist", clear
	keep if visit==981
	keep pupid sex elg98 elg99 sch98v1 wgrp* sap* totpar98 std Isem*

* Create standard indicators based upon std98v1
	gen std_fs = std if (std>-1 & std<9)
	replace std_fs = -1 if (std==55)
	tab std_fs, gen(Istd)
	summ Istd*
	drop Istd10 std_fs

	sort pupid
	save "f1", replace
	clear

* Incorporate compliance data - use older file as used in original analysis
	use "comply_old"
	keep pupid any98 any99
	sort pupid
	merge pupid using "f1"
	tab _merge

* Keep those with compliance data
	drop if _merge==1
	drop _merge
	sort pupid
	save "f1", replace
	clear

* Incorporate school-level information
	use "schoolvar"
	keep *sch* *pop* mk96_s z_inf98 distlake

* Normalize the test score to units of individual standard deviation
	replace mk96_s = mk96_s/0.8318*0.4357

	rename schid sch98v1
	sort sch98v1
	save "f2", replace
	
	use "f1"
	sort sch98v1
	merge sch98v1 using "f2"
	tab _merge
	drop _merge
	sort pupid
	save "f1", replace

* Incorporate parasitological test data
	use "wormed"
	keep pupid numics98 numinf98 hw98_ics al98_who sm98_who tt98_ics ///
		numics99 numinf99 hw99_ics al99_who sm99_who tt99_ics ///
		any_hw99 any_sm99 any_al99 any_tt99 ///
		al99 any_ics98 any_98 any_hw98 any_al98 any_sm98 any_tt98 ///
		any_ics99 any_geo99_updated any_geo99_original
	sort pupid
	merge pupid using "f1"
	tab _merge

* Keep those with parasitological test data
	drop if _merge==1
	drop _merge
	sort pupid
	save "f1", replace

* Add new controls, for G1 and G2 together
	gen sc12_3km_original = sch1_3km_original + sch2_3km_original
	gen sc12_36k_original = sch1_36k_original + sch2_36k_original
	gen po12_3km_original = pop1_3km_original + pop2_3km_original
	gen po12_36k_original = pop1_36k_original + pop2_36k_original
	gen wgrp12 = max(wgrp1, wgrp2)

* Treatment group - population density interactions
	gen Ipop1_3_original = wgrp1*pop1_3km_original
	gen IpopT_3_original = wgrp1*popT_3km_original
	gen Ipop1_36_original = wgrp1*pop1_36k_original
	gen IpopT_36_original = wgrp1*popT_36k_original
	gen IIpop1_3_original = wgrp2*pop1_3km_original
	gen IIpopT_3_original = wgrp2*popT_3km_original
	gen IIpop1_36_original = wgrp2*pop1_36k_original
	gen IIpopT_36_original = wgrp2*popT_36k_original

* Incorporate ratios of Group 1 / Total Pupils;
	gen ratio_03_original = 0 
	replace ratio_03_original = pop1_3km_original/popT_3km_original if popT_3km_original~=0
	gen ratio_36_original = 0
	replace ratio_36_original = pop1_36k_original/popT_36k_original if popT_36k_original~=0

	drop if sch98v1>300
	save "f1", replace

* Weight each school by its total initial namelist population
	use "f1"
	collapse (count) nsch=pupid (count) npar=numics99, by(sch98v1)
	sort sch98v1
	save "f2", replace
	use "f1", clear
	sort sch98v1
	merge sch98v1 using "f2"
	tab _merge
	drop _merge
	gen indiv_weight = nsch/npar
	save "f1", replace

* Generate selection into treatment indicator variable;
	gen select=0
	replace select=1 if (wgrp==1 & any98==1) | (wgrp==2 & any99==1)
	replace select=. if (any98==. & any99==.)
	gen Iwgrp1_select = wgrp1*select

* Generate eligibility interaction
	gen Iwgrp1_elg = wgrp1*elg98

* Generate difference in infection rates between 1998, 1999
	gen ics_d = any_ics99 - any_ics98
	gen sm_d  = sm99_who - sm98_who
	summ *_d, detail

* Define sample of infection micro-regressions
	keep if (sch98v1~=. & (any_ics99~=. | any_ics98~=.) & Istd1~=. & pop1_3km_updated~=. & wgrp~=. & distlake~=.)
	save "f2", replace
	drop if (any_ics99==. | sm99_who==. | any_geo99_updated==.)
	save "f1", replace

*Average number of G1 individuals within 0-3 and 3-6 km as reported in published paper
	use "f2", clear
	collapse (mean) pop1_3km* pop1_36k* [aw=indiv_weight] if wgrp12==1
	sum pop1* 

*Average number of G1 individuals within 0-3 and 3-6 km for comparison schools
	use "f2", clear
	collapse (mean) pop1_3km* pop1_36k* [aw=indiv_weight] if wgrp==2 
	sum pop1* 

*Average number of G1 individuals within 0-3 and 3-6 km for treatment schools
	use "f2", clear
	collapse (mean) pop1_3km* pop1_36k* [aw=indiv_weight] if wgrp==1 
	sum pop1*

* Define regression controls
	use "f2", clear
	global x_base = "sap* Istd4-Istd9 mk96_s"

**** TABLE 7;
	local i=1
	foreach var in any_ics99 sm99_who any_geo99_original {
		display "******** TABLE 7, (`i')********" 
		dprobit `var' wgrp1 pop1_3km_original popT_3km_original pop1_36k_original popT_36k_original $x_base [pw=indiv_weight] if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
		local i=`i'+1
		display "******** TABLE 7, (`i')********" 
		dprobit `var' wgrp1 pop1_3km_original popT_3km_original pop1_36k_original popT_36k_original select Iwgrp1_select $x_base [pw=indiv_weight] if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
		local i=`i'+1
		display "******** TABLE 7, (`i')********" 
		dprobit `var' wgrp1 pop1_3km_original popT_3km_original pop1_36k_original popT_36k_original Ipop1_3_original Ipop1_36_original $x_base [pw=indiv_weight] if (wgrp==1 | wgrp==2), robust cluster(sch98v1)
		local i=`i'+1
		}

clear all

	
*---------------------------------------------------------------------------*
* 5. WITHIN-SCHOOL AND ACROSS SCHOOL EXTERNALITIES IN SCHOOL PARTICIPATION	*
*---------------------------------------------------------------------------*


* Incorporate Namelist data
	use "namelist_old", clear

* Create further school assistance controls
	gen Y98 = 0
	replace Y98 = 1 if (visit>980 & visit<990)
	gen Y98sap1 = sap1*Y98
	gen Y98sap2 = sap2*Y98
	gen Y98sap3 = sap3*Y98
	gen Y98sap4 = sap4*Y98
	save "f9", replace

* Incorporate school and zonal variables
	use "schoolvar"
	keep schid mk96_s z9899* pop1_3km* pop2_3km* popT_3km* pop1_36k* pop2_36k* popT_36k* distlake
	rename schid sch98v1
	sort sch98v1
	save "f1", replace
	
	clear
	use "f9"
	sort sch98v1
	merge sch98v1 using "f1"
	sum _merge mk96_s 
	drop _merge

* Normalize and adjust mock scores to individual units
	replace mk96_s = mk96*0.4357/0.8318

* Generate year measure
	gen yr = .
	replace yr = 1 if (visit>=981 & visit<993) 
	replace yr = 2 if (visit>992 & visit<999)

* Create treatment indicators
	* First year of treatment
	gen t1 = 0
	replace t1 = 1 if (wgrp==1 & visit>981 & visit<993)
	replace t1 = 1 if (wgrp==2 & visit>992 & visit<999)
	replace t1 = . if wgrp==.
	* Second year of treatment
	gen t2 = 0
	replace t2 = 1 if (wgrp==1 & visit>992 & visit<999)
	replace t2 = .  if wgrp==.

* Other indicators
	gen t1e = elg*t1
	gen t2e = elg*t2

* Create standard-specific measure of zonal infection rate
	gen p1 = z9899_34
	replace p1 = z9899_56 if (std98v1==5 | std98v1==6)
	replace p1 = z9899_78 if (std98v1==7 | std98v1==8)
	drop z9899*

* Create standard indicators, based on 1998 visit 1 standard
	gen std_fs = std98v1 if (std98v1>-1 & std98v1<9)
	replace std_fs = -1 if (std98v1==55)
	tab std_fs, gen(Istd)
	summ Istd*
	drop Istd10 std_fs
	save "f9", replace
	clear

* Incorporate compliance data
	use "comply_old"
	keep pupid any98 any99
	sort pupid
	save "f2", replace
	clear
	use "f9"
	sort pupid
	merge pupid using "f2"
	tab _merge
	drop _merge

* THREE PERIODS: pre-treatment (981), Year 1 (982-992), Year 2 (993-998)
	* First year of treatment indicator
	gen treat_y1 = 0 if visit == 981
	replace treat_y1 = any98 if (visit>981 & visit<993)
	replace treat_y1 = any99 if (any98==0 & visit>992 & visit<999)
	replace treat_y1 = 0 if (any98==1 & visit>992 & visit<999)
	* Second year of treatment indicator
	* If treated in first and second years
	gen treat_y2 = 0 if visit<993
	replace treat_y2 = 1 if (any98==1 & any99==1 & visit>992 & visit<999)
	* Pupils who were treated in 1998 and not in 1999 are counted as not treated
	replace treat_y2 = 0 if (any98==1 & any99==0 & visit>992 & visit<999)
	replace treat_y2 = 0 if (any98==0 & visit>992 & visit<999)
	compress
	save "f9", replace

* Collapse data by pupil and year, where YEAR1 = 982-992, YEAR2 = 983-998
	sort pupid yr
	collapse (mean) sch98v1 prs t1 t2 elg p1 mk96_s ///
		Y98sap1 Y98sap2 Y98sap3 Y98sap4 sap1 sap2 sap3 sap4 ///
		Istd1 Istd2 Istd3 Istd4 Istd5 Istd6 Istd7 Istd8 Istd9 ///
		Isem1 Isem2 Isem3 pop1_3km* pop2_3km* pop1_36k* pop2_36k* popT_36k* popT_3km* ///
		any98 any99 wgrp (sum) obs ///
		if (t1~=. & elg~=. & sch98v1~=. &  mk96_s~=. &  p1~=. & Istd2~=. & pop1_3km_original~=.), by(pupid yr)
	keep e* t* p* sap* Y98sap* sch98v1 prs* mk96_s Istd* Isem* pop* sch* obs yr

* Create measure of population in the vicinity in the year of treatment
	gen pop_3km_original = pop1_3km_original
	gen pop_36k_original = pop1_3km_original 
		/*Note that this line above has a coding error: should be gen pop_36k_original = pop1_36k_original. 
		Leaving in coding error in order to replicate original resuls*/
	replace pop_3km_original = pop_3km_original + pop2_3km_original if yr==2
	replace pop_36k_original = pop_36k_original + pop2_36k_original if yr==2

* Create an indicator for whether the school received treatment
	gen t_any = 0
	replace t_any=1 if (t1==1 | t2==1)
	replace t_any=. if t1==. | t2==.
	save "table9a", replace

**** TABLE 9, COLUMN 1	
	sum prs [aw=obs] if  (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=.)
	
	regress prs t_any elg p1 mk96_s Y98sap* sap* Istd* Isem* [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. ///
		& pop1_3km_original~=.), robust cluster(sch98v1)

**** TABLE 9, COLUMN 2	
regress prs t1 t2 elg p1 mk96_s Y98sap* sap* Istd* Isem* [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. ///
	& pop1_3km_original~=.), robust cluster(sch98v1)

**** TABLE 9, COLUMN 3
regress prs t1 t2 elg p1 mk96_s Y98sap* sap* Istd* Isem* pop_3km_original popT_3km_original pop_36k_original popT_36k_original [aw=obs] ///
	if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=.), robust cluster(sch98v1)

* Locals for population density for school attendance sample
	summ pop_3km_original [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=.) 
	local num_3km = round(r(mean))
	
	summ pop_36k_original [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=.) 
	local num_36k = round(r(mean))


* Incorporate data on number of infections in 1999
	use "wormed", clear 
	keep pupid numics99
	gen ics99 = numics99
	replace ics99 = 1 if (numics>1 & numics<5)
	sort pupid
	save "f4", replace
	clear
	
	use "f9"
	sort pupid
	merge pupid using "f4"
	tab _merge
	drop _merge
	sort pupid
	save "f9", replace

* Only consider year 1 of treatment
	keep if visit<993 & visit>981
	sort pupid yr
	collapse (mean) sch98v1 prs t1 t2 elg p1 mk96_s t1e t2e ///
		Y98sap1 Y98sap2 Y98sap3 Y98sap4 sap1 sap2 sap3 sap4 ///
		Istd1 Istd2 Istd3 Istd4 Istd5 Istd6 Istd7 Istd8 Istd9 ///
		pop1_3km_original pop2_3km_original pop1_36k_original pop2_36k_original popT_36k_original popT_3km_original ///
		ics99 any98 any99 wgrp distlake ///
		(sum) obs ///
		if (t1~=. & elg~=. & sch98v1~=. &  mk96_s~=. &  p1~=. & Istd2~=. & pop1_3km_original~=.), by(pupid yr)

* Generate selection into treatment indicator variable, and interaction with first year treatment
	gen select=.
	replace select=1 if (wgrp==1 & any98==1) | (wgrp==2 & any99==1)
	replace select=0 if (wgrp==1 & any98==0) | (wgrp==2 & any99==0)
	gen t1_select = t1*select
	save "table9b", replace

**** TABLE 9, COLUMN 4
	sum prs [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=. & select~=. & (wgrp==1 | wgrp==2)) 
	regress prs t1 elg p1 mk96_s Y98sap* Istd* [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=. ///
		& select~=. & (wgrp==1 | wgrp==2)), robust cluster(sch98v1)

**** TABLE 9, COLUMN 5
regress prs t1 select t1_select ///
         p1 mk96_s Y98sap* Istd* pop1_3km_original popT_3km_original pop1_36k_original popT_36k_original ///
         [aw=obs] ///
         if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & pop1_3km_original~=. & select~=.  & (wgrp==1 | wgrp==2)), ///
         robust cluster(sch98v1)

**** TABLE 9, COLUMN 6
sum prs [aw=obs] if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & ics99~=. & pop1_3km_original~=.)
regress prs ics99 elg mk96_s p1 Y98sap* Istd* popT_36k_original popT_3km_original [aw=obs] ///
	if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & ics99~=. & pop1_3km_original~=.), ///
	robust cluster(sch98v1)

**** TABLE 9, COLUMN 7 - SCHOOL AVERAGES
collapse prs ics99 t1 pop1_3km_original pop1_36k_original popT_36k_original popT_3km_original elg mk96_s p1 Y98sap* Istd* wgrp (sum) obs ///
	if (t1~=. & elg~=. & sch98v1~=. & mk96_s~=. & p1~=. & Istd2~=. & ics99~=. & pop1_3km_original~=.), ///
	by (sch98v1)
regress prs ics99 elg mk96_s p1 Y98sap* Istd* popT_36k_original popT_3km_original ///
	(t1 pop1_3km_original pop1_36k_original popT_36k_original popT_3km_original elg mk96_s p1 Y98sap* Istd*) ///
	[aw=obs] if (wgrp==1 | wgrp==2), robust
	
clear


	erase "f1.dta"
	erase "f2.dta"
	erase "f4.dta"
	erase "f9.dta"
	erase "table9a.dta"
	erase "table9b.dta"
