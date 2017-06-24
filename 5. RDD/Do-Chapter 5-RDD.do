*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 5: REGRESSION DISCONTINUITY DESIGNS			*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 01/15/17									*
*********************************************************

* Inputs were provided by Yhonny Campana

set more off

log using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\5. Regression Discontinuity Designs\4. Log\Chapter5-RDD.log", append text /*Change if another computer*/

*-----------------------*
* 1. OPENING DATASET	*
*-----------------------*

set more off
clear all

global path="C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\"

use"$path/PanelPROGRESA_Enrollment_97_99.dta", clear


* Creating some auxiliary variables

destring year, replace

gen aux=D if year==1998
egen D_assig=mean(aux),by(villid)

gen aux1=pov_HH if year==1998
egen aux2=mean(aux1),by(hogid)
replace pov_HH=aux2 if year==1997
drop aux1 aux2

* Distribution of the sample: eligibility and treatment status
tab D_assig pov_HH if year==1997

*------------------------*
* 2. BASIC DATA ANALYSIS *
*------------------------*

gen sampleRD = (pov_HH==1& D_assig==1)|(pov_HH==0 & D_assig==0)

* Inspecting the assignment variable (yycali)

kdensity yycali if sampleRD==1 & year==1997, graphregion(fcolor(white)) title("")

twoway (kdensity yycali if sampleRD==1 & pov_HH==1 & D_assig==1 & year==1997)  ///
	   (kdensity yycali if sampleRD==1 & pov_HH==0 & D_assig==0 & year==1997), ///
	   legend(lab(1 Poor) lab(2 Non-Poor)) graphregion(fcolor(white)) title("")	   
	   
	   
* Disentangle thresholds among entities

levelsof entidad, local(entidades)
foreach j of local entidades {
	summ yycali if year==1997 & pov_HH==1 & entidad==`j'
	scalar max_`j'=r(max)
	twoway (kdensity yycali if D_assig==1 & sampleRD==1 & year==1997 & entidad==`j') (kdensity yycali if D_assig==0 & sampleRD==1 & year==1997 & entidad==`j'), graphregion(fcolor(white)) ti(Region `j') ytitle("") legend(off) saving("C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\5. Regression Discontinuity Designs\graph`j'.gph", replace)
	
}
scalar list

cd "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\5. Regression Discontinuity Designs" 

graph combine graph12.gph graph13.gph graph16.gph graph21.gph graph22.gph graph24.gph graph30.gph

* Centering assignment variable

gen double z = 0
foreach j of local entidades {
	replace z = yycali - max_`j' if entidad==`j' & D_assig==1
	replace z = yycali - max_`j' if entidad==`j' & D_assig==0
}

kdensity z if sampleRD==1 & year==1997, graphregion(fcolor(white))


* Identifying eligible untreated individuals

keep if sampleRD==1
gen out = ~((z<=0 & pov_HH==1) | (z>0 & pov_HH==0))
gen E = z<=0
label var E "1 = z<0 "

tab entidad out if year==1997

*-----------------------*
* 3. GRAPHICAL ANALYSIS	*
*-----------------------*

preserve
keep if z>=-200 & z<=200
keep if out==0
keep if year==1999
xtile hl = z if D_assig==1, n(30)
xtile hu = z if D_assig==0, n(30)
gen hd = -hl if D_assig==1
replace hd = hu if D_assig==0

collapse (mean) z enroll D_assig, by(hd)
gen z2 = z^2

reg enroll z z2  if D_assig==1
predict yhat0 if e(sample)

reg enroll z z2  if D_assig==0
predict yhat1 if e(sample)

sort z

tw  (scatter enroll z if D_assig==1) (line yhat0 z if D_assig==1) || ///
	(scatter enroll z if D_assig==0) (line yhat1 z if D_assig==0), ///
	ylabel(0 1) xline(0) legend(off) graphregion(fcolor(white)) 
restore


*---------------------------*
* 4. REGRESSION ANALYSIS	*
*---------------------------*

gen z2 = z^2


* SHARP RD

* 1997
reg enroll D_assig z if out==0 & year==1997, vce(cluster villid)
estimates store r1_97

reg enroll D_assig z z2 if out==0 & year==1997, vce(cluster villid)
estimates store r2_97

* 1999
reg enroll D_assig z if out==0 & year==1999, vce(cluster villid)
estimates store r3_99

reg enroll D_assig z z2 if out==0 & year==1999, vce(cluster villid)
estimates store r4_99

xml_tab r1_97 r2_97 r3_99 r4_99, replace save("RD_TableI.xml") ///
	title("Table I: Sharp RD for Enrollment") below stats(N r2)


* FUZZY RD

* 1997
ivregress 2sls enroll (D_assig=E) z if year==1997, vce(cluster villid)
estimates store r1_97 

ivregress 2sls enroll (D_assig=E) z z2 if year==1997, vce(cluster villid)
estimates store r2_97

* 1999
ivregress 2sls enroll (D_assig=E) z if year==1999, vce(cluster villid) 
estimates store r3_99 

ivregress 2sls enroll (D_assig=E) z z2 if year==1999, vce(cluster villid)  
estimates store r4_99

xml_tab r1_97 r2_97 r3_99 r4_99, replace save("RD_TableII.xml") ///
	title("Table II: Fuzzy RD for Enrollment") below stats(N r2)

*---------------------------*
* 5. REGRESSION ANALYSIS	*
*---------------------------*

* HIGHER ORDER POLYNOMIALS

gen z3 = z^3
gen z4 = z^4
gen z5 = z^5

reg enroll D_assig z z2 z3 if out==0 & year==1999, vce(cluster villid)
estimates store r1

reg enroll D_assig z z2 z3 z4 if out==0 & year==1999, vce(cluster villid)
estimates store r2

reg enroll D_assig z z2 z3 z4 z5 if out==0 & year==1999, vce(cluster villid)
estimates store r3

xml_tab r1 r2 r3, replace save("RD_TableIII.xml") ///
	title("Table III: Sensitivity of functional form assumptions") below stats(N r2)

	
* ADDING COVARIATES

reg enroll D_assig z z2 age sex lang if out==0 & year==1999, vce(cluster villid)
estimates store r1

reg enroll D_assig z z2 age sex lang ageHH sexHH eduHH if out==0 & year==1999, vce(cluster villid)
estimates store r2

reg enroll D_assig z z2 age sex lang ageHH sexHH eduHH i.entidad if out==0 & year==1999, vce(cluster villid)
estimates store r3

xml_tab r1 r2 r3, replace save("RD_TableIV.xml") ///
	title("Table IV: Sensitivity to additional covariates") below stats(N r2)

	
* PLACEBO EXPERIMENTS

* Testing different cut offs on the left

foreach k of numlist 30 60 90 {
	gen zf = z + `k'
	gen zf_2 = zf^2
	gen zf_3 = zf^3
	gen Ef = zf < 0
	reg enroll Ef zf zf_2 zf_3 if out==0 & year==1999, vce(cluster villid)
	estimates store SHL`k'
	drop Ef zf zf_2 zf_3
}

* Testing different cut offs on the right

foreach k of numlist 30 60 90 {
	gen zf = z - `k'
	gen zf_2 = zf^2
	gen zf_3 = zf^3
	gen Ef = zf < 0
	reg enroll Ef zf zf_2 zf_3 if out==0 & year==1999, vce(cluster villid)
	estimates store SHR`k'
	drop Ef zf zf_2 zf_3
}

xml_tab SHL30 SHL60 SHL90 SHR30 SHR60 SHR90, replace save("RD_TableV.xml") ///
	title("Table V: Placebo discontinuity points") below stats(N r2)


* BALANCING TEST (JUST BASELINE COVARIATES AND A FUZZY RDD)

replace edu=edu/100
replace eduHH=eduHH/100
foreach var of varlist sex lang edu ageHH sexHH eduHH {
	reg `var' D_assig z z2 z3 if out==0 & year==1997, vce(cluster villid)
	estimates store BT`var'
}
xml_tab BTsex BTlang BTedu BTageHH BTsexHH BTeduHH, replace save("RD_TableVI.xml") ///
	title("Table VI: Pre-treatment balance") below stats(N r2)

	
* MCCRARY DENSITY TEST

DCdensity z if out==0 & year==1997, breakpoint(0) generate(Xj Yj r0 fhat se_fhat)
drop Yj Xj r0 fhat se_fhat


*---------------------------*
* 6. NONPARAMETRIC ANALYSIS	*
*---------------------------*

* SHARP: Triangle Kernel and bandwidth=3

rd enroll z if out==0 & year==1999, z0(0) bwidth(3) mbw(100) 


* FUZZY: Triangle Kernel and bandwidth=3

rd enroll D_assig z if year==1999, z0(0) bwidth(3) mbw(100)  

/*
Using Imbens' RDOB stata command
	rdob enroll z if out==0&year==9, c(0) 	//Shar RD
	rdob enroll z if year==9, fuzzy(D) c(0) //Fuzzy RD
*/


* BANDWIDTH CHOICES (JUST FOR SHARP RD)

* Imbens & Kalyaraman Optimal bandwidth

rd enroll z if out==0 & year==1999, mbw(100) z0(0) 

* Sensitivity of impact effects to changes in bandwidth size (graph)

rd enroll z if out==0 & year==1999, z0(0) mbw(50(25)200) bdep ox


* LOG CLOSE
log close
