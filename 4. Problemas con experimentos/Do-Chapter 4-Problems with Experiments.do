*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 4: PROBLEMS WITH EXPERIMENTS					*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 01/15/17									*
*********************************************************

* Inputs were provided by Yhonny Campana


set mem 800m		/*Setting memory size (for STATA lower than 12)*/

log using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\4. Problems with Experiments\5. Log\Chapter4.log", append text /*Change if another computer*/

*-----------------------*
* 1. OPENING DATASET	*
*-----------------------*

set more off
clear all

global path="C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\"


use"$path/PanelPROGRESA_97_99year.dta", clear

keep if year==1999

* Keep information of children 6-7 years
keep if age>=6 & age<=7

* Creating Region Fixed Effects
gen entidad=substr(villid,1,2)
gen munici =substr(villid,3,3)
gen locali =substr(villid,6,3)
destring  entidad munici locali, replace


*-------------------------------*
* 2. SECTION 1: NON-COMPLIANCE	*
*-------------------------------*

* 2.1. DESCRIPTIVE ANALYSIS
*--------------------------
 
* OLS enrollment against D_HH and D

areg enroll D_HH, absorb(entidad) vce(cluster villid)

areg enroll D, absorb(entidad) vce(cluster villid)

areg enroll D_HH famsize langhead sexhead agehead, absorb(entidad) vce(cluster villid)

areg enroll D famsize langhead sexhead agehead, absorb(entidad) vce(cluster villid)


* Assigment versus treatment state

tab D D_HH


* 2.2. INSTRUMENTAL VARIABLES
*----------------------------

tab entidad, gen(enti)

global entidad="enti2 enti3 enti4 enti5 enti6 enti7"

* IV
ivregress 2sls enroll (D_HH=D) $entidad, vce(cluster villid)

ivregress 2sls enroll (D_HH=D) sex langhead agehead sexhead $entidad, vce(cluster villid)

* FIRST STAGE
ivregress 2sls enroll (D_HH=D) $entidad, vce(cluster villid) first

ivregress 2sls enroll (D_HH=D) sex langhead agehead sexhead $entidad, vce(cluster villid) first


* INTENTION TO TREAT

* Intention to treat
reg enroll D $entidad, vce(cluster villid)
scalar ITT=_b[D]

* Compliance rate
* E[D_HH|D==1]
summ D_HH if D==1, meanonly
scalar D_HHD1=r(mean)
disp D_HHD1

* E[D_HH|D==0]
summ D_HH if D==0, meanonly
scalar D_HHD0=r(mean)
disp D_HHD0

scalar CR = D_HHD1-D_HHD0
disp CR

* LATE
scalar LATE=ITT/(D_HHD1-D_HHD0)
disp LATE


*-----------------------------------*
* 3. SECTION 2: SPILLOVER EFFECTS	*
*-----------------------------------*

clear

use"$path/PanelPROGRESA_Enrollment_97_99.dta", clear

destring year, replace

* Creating some auxiliary variables

gen aux=D if year==1998
egen D_assig=mean(aux),by(villid)

gen aux1=pov_HH if year==1998
egen aux2=mean(aux1),by(hogid)
replace pov_HH=aux2 if year==1997
drop aux1 aux2

gen Y8=year==1998
gen Y9=year==1999

*Double interactions

gen int_D_PovHH=D_assig*pov_HH

gen int_PovHH_Y = pov_HH if year==1998 | year==1999
replace int_PovHH_Y = 0 if year==1997

gen int_D_Y = D_assig if year==1998 | year==1999
replace int_D_Y = 0 if year==1997


* Triple interactions

gen int_D_Y_PovHH=pov_HH*int_D_Y


* 3.1. CROSS-SECTION ESTIMATES
*-----------------------------

reg enroll D if pov_HH==1 & year==1999, vce(cluster villid)

reg enroll D if pov_HH==0 & year==1999, vce(cluster villid)

reg enroll D pov_HH int_D_PovHH if year==1999, vce(cluster villid)
test D + int_D_PovHH = 0

* Adding controls

reg enroll D sex age ageHH sexHH eduHH lang if pov_HH==1 & year==1999, vce(cluster villid)

reg enroll D sex age ageHH sexHH eduHH lang if pov_HH==0 & year==1999, vce(cluster villid)

reg enroll D pov_HH int_D_PovHH sex age ageHH sexHH eduHH lang if year==1999, vce(cluster villid)
test D + int_D_PovHH = 0


* 3.2. DIFFERENCE IN DIFFERENCES ESTIMATES
*-----------------------------------------

reg enroll int_D_Y D_assig Y8 Y9 if pov_HH==1, vce(cluster villid)

reg enroll int_D_Y D_assig Y8 Y9 if pov_HH==0, vce(cluster villid)

reg enroll int_D_Y_PovHH int_D_Y int_D_PovHH int_PovHH_Y D_assig pov_HH Y8 Y9, vce(cluster villid)
test int_D_Y + int_D_Y_PovHH = 0

* Adding controls

reg enroll int_D_Y D_assig Y8 Y9 sex age ageHH sexHH eduHH lang if pov_HH==1, vce(cluster villid)

reg enroll int_D_Y D_assig Y8 Y9 sex age ageHH sexHH eduHH lang if pov_HH==0, vce(cluster villid)

reg enroll int_D_Y_PovHH int_D_Y int_D_PovHH int_PovHH_Y D_assig pov_HH Y8 Y9 sex age ageHH sexHH eduHH lang, vce(cluster villid)
test int_D_Y + int_D_Y_PovHH = 0


*---------------------------*
* 4. SECTION 3: ATTRITION	*
*---------------------------*

clear

use"$path/PanelPROGRESA_Enrollment_97_99.dta", clear

destring year, replace


* Creation of auxiliary variables

gen aux=D if year==1998
egen D_assig=mean(aux),by(villid)

gen int_D_Y = D_assig if year==1998 | year==1999
replace int_D_Y = 0 if year==1997

gen aux1=pov_HH if year==1998
egen aux2=mean(aux1),by(hogid)
replace pov_HH=aux2 if year==1997
drop aux1 aux2

gen Y8=year==1998
gen Y9=year==1999

* Panel setting
egen id_=concat(iid)
encode id_, g(id)
xtset id year

* Atrittion patterns

tab paths if year==1997


* 4.1. TESTING ATTRITION
*-----------------------

* Testing if attrition differs beetwen treated and untreated

tab attrit D_assig if pov_HH==1 & year==1997, nof col


* Testing if attrition is correlated with covariates

reg attrit D_assig if pov_HH==1 & year==1997

reg attrit D_assig age sex lang eduHH sexHH yycali i.entidad if pov_HH==1 & year==1997, vce(cluster villid)

reg attrit age sex lang eduHH sexHH yycali i.entidad if pov_HH==1 & year==1997 & D_assig==1, vce(cluster villid)

reg attrit age sex lang eduHH sexHH yycali i.entidad if pov_HH==1 & year==1997 & D_assig==0, vce(cluster villid)


* 4.2. REWEIGHTING APPROACHES
*----------------------------

* PREPARING ANALYSIS

* Construction of weights

gen noattrit=1-attrit

probit noattrit enroll age sex lang eduHH sexHH yycali D_assig i.entidad if pov_HH==1 & year==1997, vce(cluster villid) 
gen sample=e(sample)
predict prF if sample==1

probit noattrit enroll age sex D_assig if pov_HH==1 & year==1997, vce(cluster villid) 
predict prR if sample==1

* Weights

gen weight=prR/prF if sample==1

* Graph of predicted probabilities

twoway (kdensity prF) (kdensity prR)

* Graph of the weights

kdensity weight
egen weight_y=mean(weight), by(id)


* REWEIGHTED ESTIMATES

* Simple Difference Estimator

reg enroll int_D_Y if pov_HH==1 & (year==1998 | year==1999) [w=weight_y] 
estimates store WWNCS
gen samp_ws = e(sample)

* Fixed Effect Estimator

xtreg enroll int_D_Y Y8 Y9 if pov_HH==1 [w=weight_y], fe 
estimates store WWNCF
gen samp_wf = e(sample)


* UNWEIGHTED ESTIMATES

* Simple Difference Estimator

reg enroll int_D_Y if samp_ws==1 & pov_HH==1 &(year==1998|year==1999), 
estimates store UWNCS

* Fixed Effect Estimator

xtreg enroll int_D_Y Y8 Y9 if samp_wf==1 & pov_HH==1, fe 
estimates store UWNCF


* COMPARING APPROACHES

* Simple difference
hausman WWNCS UWNCS

* Fixed effects
hausman WWNCF UWNCF 


* 4.3. LEE(2009) BOUNDS
*----------------------

reg enroll int_D_Y if pov_HH==1 & (year==1998|year==1999)

leebounds enroll int_D_Y if pov_HH==1 & (year==1998|year==1999), select(noattrit)

* CLOSING LOG
log close
