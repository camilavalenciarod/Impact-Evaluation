*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 3: EXPERIMENTS								*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 01/15/17									*
*********************************************************


set mem 800m		/*Setting memory size (for STATA lower than 12)*/

log using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\3. Experiments\5. Log\Chapter3-Regression.log", append text /*Change if another computer*/

*-------------------------------------------*
* 1. CREATING A HOUSEHOLD LEVEL DATA SET	*
*-------------------------------------------*

set more off
clear all

global path="C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\ENCEL 2007\Final"


*Creating HH level sample (run this only once)
use"$path/DataFinal_ENCEL07.dta", clear

sort villid hogid iid
gen laborhead=HH*labor
gen healthhead=HH*health
collapse (max) IncomeLabHH1 ExperienceLabHH1 laborhead healthhead (mean) agehead sexhead pov_HH famsize IncomeLab Lanhead , by(villid hogid)
keep if IncomeLab!=.
gen exper= ExperienceLabHH1>12
drop if IncomeLab>100000

drop if (pov_HH==.|Lanhead==.)

save "DataFinal_HH_ENCEL07.dta", replace


*-----------------------------------------------------------*
* 3. SECTION 5: HYPOTHESIS TESTING FOR DIFFERENCE IN MEANS	*
*-----------------------------------------------------------*

* 3.1. LOGIC OF T-TEST
*---------------------
 
 * SCENARIO 1

clear all
set obs 100

gen group = 1 in 1/50
replace group = 2 in 51/100

label var group "Treatment status"
label define group 1 "Control" 2 "Treatment"
label values group group

gen x=rnormal(915, 250) in 1/50 
replace x=rnormal(1050, 325) in 51/100 

label var x "Labor income"  

graph box x, by(group) graphregion(fcolor(white)) ylabel (0(500)2000)


 * SCENARIO 2

clear all
set obs 100

gen group = 1 in 1/50
replace group = 2 in 51/100

label var group "Treatment status"
label define group 1 "Control" 2 "Treatment"
label values group group

gen x=rnormal(915, 250) in 1/50 
replace x=rnormal(1260, 420) in 51/100 

label var x "Labor income"  

graph box x, by(group) graphregion(fcolor(white)) ylabel (0(500)2000)


 * SCENARIO 3

clear all
set obs 100

gen group = 1 in 1/50
replace group = 2 in 51/100

label var group "Treatment status"
label define group 1 "Control" 2 "Treatment"
label values group group

gen x=rnormal(915, 100) in 1/50 
replace x=rnormal(1050, 75) in 51/100 

label var x "Labor income"  

graph box x, by(group) graphregion(fcolor(white)) ylabel (500(500)1500)


* 3.2. ROLE OF SAMPLE SIZE
*-------------------------

clear 
set obs 500
gen nvar1=.
gen pvar1=.
gen nvar2=.
gen pvar2=.
gen var1=.
gen var2=.
set seed 999
forvalues x = 1/500 {
  quietly replace var1=rnormal(915, 250) in 1/`x'
  quietly replace var2=rnormal(1050, 325) in 1/`x'
  
  sum var1 , meanonly
  quietly replace pvar1=r(mean) in `x'
  quietly replace nvar1=`x' in `x'
  
  sum var2 , meanonly
  quietly replace pvar2=r(mean) in `x'
  quietly replace nvar2=`x' in `x'
  }
*
#delimit ;
twoway (line pvar1 nvar1) (line pvar2 nvar2)
 , ylabel(600(200)1600,angle(horizontal)) xlabel(1 50 100(200)500)
   ytitle(Mean) 
   xtitle("Sample Size (log scale)",height(5))
   title("Statistical Regularity for Control and Treatment" , size(*0.9))
   xscale(log) graphregion(fcolor(white)) legend(off)
 ;
#delimit cr


* 3.3. SAMPLING DISTRIBUTION OF T-TEST UNDER THE NULL HYPOTHESIS
*---------------------------------------------------------------

* SIMULATION

clear 
set obs 10000
gen x=.  										
gen tstat=.  									
gen treatment = 1 in 1/50
replace treatment = 2 in 51/100
label define treatment 1 "treatment" 2 "control"
label values treatment treatment
set seed 123

forvalues i = 1/10000 {
  quietly replace x=rnormal(915, 250) in 1/50   	
  quietly replace x=rnormal(915, 250) in 51/100   
  quietly ttest x, by(treatment) unequal 
  quietly replace tstat=r(t) in `i' 			
  quietly list tstat in 1/`i'
}


sum tstat

*HISTOGRAM

histogram tstat , bin(41) graphregion(fcolor(white)) xlabel(-4(1)4) title("Simulated t-statistics under H0(10,000 simulations)")

sort tstat

#delimit ;
twoway (kdensity tstat, lcolor(red) lwidth(*2))
       (function y=tden(98,x), range(-4 4) lcolor(navy) lwidth(*2) lpattern(dash))
 , legend(off) ytitle("Density (Proportion)")
   xtitle("t statistic",height(5)) graphregion(fcolor(white)) title("Theoretical vs Empirical T-distribution under H0")
 ;
#delimit cr

* 3.4. SAMPLING DISTRIBUTION OF T-TEST UNDER HA
*----------------------------------------------

* SIMULATION

clear 
set obs 10000
gen x=.  										
gen tstat=.  									
gen treatment = 1 in 1/50
replace treatment = 2 in 51/100
label define treatment 1 "treatment" 2 "control"
label values treatment treatment
set seed 123

forvalues i = 1/10000 {
  quietly replace x=rnormal(915, 250) in 51/100   	
  quietly replace x=rnormal(1050, 325) in 1/50   
  quietly ttest x, by(treatment) unequal 
  quietly replace tstat=r(t) in `i' 			
  quietly list tstat in 1/`i'
}


sum tstat

*HISTOGRAM

histogram tstat , bin(41) graphregion(fcolor(white)) xlabel(-4(1)8) title("Simulated t-statistics under HA(10,000 simulations)") ///
	addplot(function y=tden(98,x), range(-4 4) lcolor(navy) lwidth(*1.5)) legend(off) text(0.25 -1.4 "H0")

	
* 3.5. TESTING DIFFERENCES IN MEANS
*----------------------------------	

* disp invttail(98,.025) // t critical value for 98 degrees of freedom = 1.9844675

#delimit ;
graph twoway (function y=tden(98,x), range(-4 -1.98) bcolor(red) 
              recast(area) plotregion(style(none)))
  	       (function y=tden(98,x), range(1.98 4) bcolor(red) 
              recast(area) plotregion(style(none)))
			  (function y=tden(98,x), range(-4 4) clstyle(line)
              clcolor(navy) clwidth(*1.5)
              plotregion(style(none)))
			  (kdensity tstat if tstat<6, clstyle(line) clcolor(red) clwidth(*1.5)) 
 ,yscale(off) legend(off)
  xlabel( -1.98 "t = -1.98"  0 "t = 0" 1.98 "t = 1.98") 
  xtitle("t statistic", height(5))
  title(Testing Difference in Means)
  text(.2 2.1 "HA")
  text(.2 0.1 "H0")
  graphregion(fcolor(white))
  xline(-1.98 0 1.98, lcolor(navy) lpattern(dash))
  xlabel(-4(1)6)
 ;
#delimit cr	


*-------------------------------------------*
* 4. SECTION 6: RANDOMIZATION IN PRACTICE	*
*-------------------------------------------*

* 4.1. SIMPLE RANDOMIZATION
*--------------------------

use "DataFinal_HH_ENCEL07.dta", clear

* Create a random number
gen random = uniform()

* Sort observations according this random number
sort random

* Create a treatment variable (1=treatment/0=control) 
gen treatment_simple = 0
replace treatment_simple = 1 if _n <= _N/2

* Computing descriptive statistics by treatment status 
tabstat IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_simple) s(mean sd)

* Reset
drop random


* 4.2. BLOCK-RANDOMIZED DESIGNS
*------------------------------

/*
we use poverty status and language to create blocks. 
*/

* Create a random number
gen random = uniform()

* Sort the random number according to poverty and languague
sort pov_HH Lanhead random

* Defining the size of each stratum 
by pov_HH Lanhead: gen strata_size = _N

* Assigning a value for each household according to the order the household within each stratum
by pov_HH Lanhead: gen strata_index = _n

* Create a treatment variable (1=treatment/0=control) 
gen treatment_block = 0
replace treatment_block = 1 if strata_index <= (strata_size/2)

* Computing descriptive statistics by treatment status 
tabstat IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_block) s(mean sd)

sort strata_size
by strata_size: tabstat IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_block) s(mean sd)

* Reset
drop random strata_index 


* 4.3. CLUSTER-RANDOMIZED DESIGNS
*--------------------------------

* Create an order variable
gen long order = _n

* Select one observation per cluster (village)
egen treatment_cluster = tag(villid)

* Create and sort random number
gen random = runiform()
sort treatment_cluster random 

* Record the number of clusters
qui sum treatment_cluster
scalar nro=r(sum)

* Assign treatment status according to order of observation in each cluster 
replace treatment_cluster = _n > (_N - nro/2)

* Assign treatment to all households in a given village
bysort villid (treatment_cluster): replace treatment_cluster = treatment_cluster[_N]
sort order

* Computing descriptive statistics by treatment status 
tabstat IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_cluster) s(mean sd)

* Reset
drop random order


*------------------------------------------------*
* 5. SECTION 7: EVALUATING PRE-TREATMENT BALANCE *
*------------------------------------------------*

* 5.1. SIMPLE RANDOMIZATION

*	Means
foreach covariates of varlist IncomeLab famsize Lanhead sexhead agehead pov_HH {
	ttest `covariates', by(treatment_simple) unequal
	}
	
ttable2 IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_simple)

*	Distribution
foreach covariates of varlist IncomeLab famsize agehead {
	ksmirnov `covariates', by(treatment_simple) 
	}

	
* 5.2. BLOCK-RANDOMIZED DESIGNS

*AVERAGE

*	Means
foreach covariates of varlist IncomeLab famsize Lanhead sexhead agehead pov_HH {
	ttest `covariates', by(treatment_block) unequal
	}

ttable2 IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_block)

*	Distribution	
foreach covariates of varlist IncomeLab famsize agehead{
	ksmirnov `covariates', by(treatment_block) 
	}
	
*BY STRATUM	
foreach covariates of varlist IncomeLab famsize Lanhead sexhead agehead pov_HH {
	foreach strata of numlist 320 4385 4713 13058 {
		ttest `covariates' if strata_size==`strata', by(treatment_block) unequal
	}
}	
	
	
* 5.3. CLUSTER-RANDOMIZED DESIGNS	

*Means
foreach covariates of varlist IncomeLab famsize Lanhead sexhead agehead pov_HH {
	ttest `covariates', by(treatment_cluster) unequal
	}	

ttable2 IncomeLab famsize Lanhead sexhead agehead pov_HH, by(treatment_cluster)

*Distribution
foreach covariates of varlist IncomeLab famsize agehead {
	ksmirnov `covariates', by(treatment_cluster) 
	}

	
*--------------------------------------------------------* 
* 6. SECTION 8: EVALUATING THE IMPACT OF AN INTERVENTION *
*--------------------------------------------------------*

use "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\PanelPROGRESA_97_99year.dta", clear

keep if HH==1		/*Sample of HH*/

* 6.1. USING HYPOTHESIS TESTING

ttest Income_HH_per if year==1999, by(D_HH) unequal

ksmirnov Income_HH_per if year==1999, by(D_HH) 

kdensity Income_HH_per if D_HH==1 & year==1999 & Income_HH_per<2000,addplot(kdensity Income_HH_per if D_HH==0 & year==1999 & Income_HH_per<2000, lpattern(dash)) ///
	graphregion(fcolor(white)) legend(label(1 "Treatment") label (2 "Control")) xtitle("Household income per-capita") title("")

* 6.2. USING REGRESSION

regress Income_HH_per D_HH if year==1999, vce(cluster villid) 

*-------------------------------------* 
* 7. SECTION 9: MORE GENERAL ANALYSIS *
*-------------------------------------*

* 7.1. ADDING COVARIATES

regress Income_HH_per D_HH famsize langhead sexhead agehead if year==1999, vce(cluster villid)


* 7.2. ADDING REGION (ENTIDAD) FIXED EFFECTS

gen entidad=substr(villid,1,2)
gen munici =substr(villid,3,3)
gen locali =substr(villid,6,3)
destring  entidad munici locali, replace

areg Income_HH_per D_HH famsize langhead sexhead agehead if year==1999, absorb(entidad) vce(cluster villid)


* 7.3. BASELINE DATA

gen aux=.
replace aux=0 if D_HH==0 & year==1998
replace aux=1 if D_HH==1 & year==1998
egen D_asigHH=max(aux),by(hogid)
replace D_asigHH=D_HH if year==1999
drop aux

label var D_asigHH "HH level assignment to treatment" 

drop if year==1998

tab year, gen(dyear)

gen I_dhXdyear2=D_asigHH*dyear2
label var I_dhXdyear2 "Interaction: HH treatment and second period dummy"

regress Income_HH_per D_asigHH dyear2 I_dhXdyear2, vce(cluster villid)


* 7.3. INTERACTIONS (HETEROGENEOUS EFFECTS)

gen I_dhXsexhead=D_HH*sexhead
label var I_dhXsexhead "Interaction: HH treatment and sex of household head"

areg Income_HH_per D_HH sexhead I_dhXsexhead if year==1999, absorb(entidad) vce(cluster villid)

* CLOSING LOG FILE
log close
