*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 1: INTRODUCTION TO STATA						*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 12/28/16									*
*********************************************************


set mem 800m		/*Setting memory size (for STATA lower than 12)*/

log using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\1. Intro to STATA\4. Log\Chapter1-Intro STATA.log", text append  /*Change if another computer*/

*---------------------------------------*
* 1. OPEN A DATA SET					*
*---------------------------------------*

*Type the full path of the data file (change if different computer)
use "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\ENCEL 2007\Final\DataFinal_ENCEL07.dta", clear

*Or by steps
cd "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\ENCEL 2007\Final"  				//Insert the current directory
use "DataFinal_ENCEL07.dta", clear  																				// Then, open the data file

*Insheet
insheet using Example.csv, clear

*---------------------------------------*
* 2. SAVING/EXITING A DATA SET			*
*---------------------------------------*

use "DataFinal_ENCEL07.dta", clear

save "Oportunidades.dta", replace

*exit, clear

*---------------------------------------*
* 3. STATA HELP							*
*---------------------------------------*

*Help
help memory												//Display online help of Stata command MEMORY
help save												//Display online help of Stata command SAVE
help summarize											//Display online help of Stata command SUMMARIZE
help regress											//Display online help of Stata command REGRESS
help reg

*Search
search ols
search eliminate

*Findit
findit xtabond2
findit outreg2

*Lookfor
lookfor poverty
lookfor asset

*---------------------------------------*
* 4 MANAGING THE DATA FILE				*
*---------------------------------------*

use "DataFinal_ENCEL07.dta", clear

*Describe all variables
describe

*Describe some variables
describe famsize sexhead pov_HH							//Describe selected variables: famsize sexhead pov_HH
describe villid - sex									//Describe var. from cow to hens (in the order in which are stored)
describe p_*											//Describe var. which start with p_

*List 
list famsize sexhead in 1/10							//List the first ten observations of selected variables
list pov_HH if (villid=="01001106" & agehead>30)		//List poverty status if both condition are met.

*Sorting the Data
sort villid hogid

*Counting observations
count
count if agehead>35

*Summarizing data
summarize famsize sexhead								//Displays a few of summary statistics
summarize famsize sexhead, d							//Displays more summary statistics (median, percentiles, etc.)

*Tabulate
tabulate sexhead 											//Display one-way frequency distributions of the variable.
tabulate sexhead if pov_HH==1								//Display one-way frequency distributions if condition are met.

tabulate sexhead pov_HH, cell									//Display two-way tables of frequencies of two variables.

*Table 
table D, c(mean pov_HH sd pov_HH mean agehead sd agehead)		//Display statistics (means and s.d.) by a variable.
table D, c(mean pov_HH sd pov_HH) format(%9.3f) center			//Display statistics by a variable with specific format.

table D pov_HH, c(mean agehead)									//Display statistics in a two-way table.

*---------------------------------------*
* 5. CHANGING THE DATA FILE				*
*---------------------------------------*

*Generate variables
generate oldhead=1 if agehead>35
replace oldhead=0 if agehead<=35

generate oldhead1=(agehead>35)

*Egen: Generate extension
egen avage= mean(agehead)										//Generate a mean by a variable.
egen avage_vil= mean(agehead), by (villid)					

*Recode
recode sexhead (0 = 2 "Women") (1 = 1 "Men"), generate(sex1)
recode famsize (1 = 1) (2/5=2) (6/10=3) (11/100=4), generate(famscale)

*Label
label data "Database of Oportunidades CCT"						//Label the database.

label variable oldhead "Family Head over 35 years old"			//Label a specific variable.

label define oldlabel 0 “Under_35” 1 "Over_35”					//Label the values of a specific variable.
label values oldhead oldlabel

*Drop and keep
drop oldhead1 sex1												//Drop a specific variable.
drop if famsize>30												//Drop observations if condition is met.

keep if famsize<=20												//Keep observations if condition is met.

*Destring and tostring
destring villid, generate(village)
tostring famsize, generate(fam_size)

*---------------------------------------*
* 6. COMBINING DATA SET					*
*---------------------------------------*

use "DataFinal_ENCEL07.dta", clear

*Merge
sort villid hogid
merge villid hogid using "asset_data.dta"
tab _m

*Append
append using "additional.dta"

*Collapse
collapse (mean) famsize agehead (sum) asset, by(villid)

*---------------------------------------*
* 7. USING LOG AND DO FILES				*
*---------------------------------------*

/*
log using example, text replace
*[Stata commands]
log close
*/

*---------------------------------------*
* 8. BASIC GRAPH						*
*---------------------------------------*

use "DataFinal_ENCEL07.dta", clear

*Histogram
histogram mwagemale
histogram mwagemale, bin(10)
histogram mwagemale, normal xtitle(Mean wage by village) title(Histogram for mean wage) scheme(sj)

gen logwage=log(1+mwagemale)
histogram logwage, normal xtitle(Log of mean wage by village) title(Histogram for the log of mean wage) scheme(sj)

*Kernel
kdensity mwagemale
kdensity mwagemale, kernel(gaussian)

kdensity mwagemale, bwidth(0.5)
kdensity mwagemale, bwidth(10)

kdensity mwagemale if mwagemale<150, bwidth(0.5) addplot(kdensity mwagemale if mwagemale<150, bwidth(10))

kdensity mwagemale, xtitle(Mean wage by village) title(Kernel density for mean wage) scheme(sj)

*Scatter two-way
twoway (scatter IncomeLab age if HH==1)

twoway (scatter IncomeLab age if HH==1), ytitle(Monthly income) xtitle(Household head age)

twoway (scatter IncomeLab age if HH==1) (lfit IncomeLab age if HH==1, lwidth(medthick)), ytitle(Monthly income) ///
	xtitle(Age of household head) title(Relationship between household head age and income) subtitle(Scatterplot and OLS fitted line) ///
	note(Data is at household level) scheme(sj)

*---------------------------------------*
* 9. MACROS AND LOOPS					*
*---------------------------------------*

global glo_list famsize pov_HH
describe $glo_list
summarize mwagemale $glo_list

local lo_list mwagemale agehead
twoway (scatter `lo_list') 

foreach varx of varlist sex pov_HH agehead {
display "A loop to compute correlation beetween average wage and " "`varx'"
correlate mwagemale `varx'
}

forvalues i=0/1  {
summarize pov_HH mwagemale famsize if D==`i'
}

local i 0
while `i'<=1  {
summarize pov_HH mwagemale famsize if D==`i'
local i = `i' + 1
}

*---------------------------------------*
* 10. REGRESSION						*
*---------------------------------------*

regress pov_HH D 
estimates store reg_1

regress pov_HH D mwagemale
estimates store reg_2

regress pov_HH D mwagemale agehead
estimates store reg_3

regress pov_HH D mwagemale agehead, vce (robust)
estimates store reg_4

estimates table reg_1 reg_2 reg_3 reg_4, b(%9.4f) se stats(N r2 F)

*Save result (xml_tab)
findit xml_tab

xml_tab reg_1 reg_2, save ("C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\1. Intro to STATA\Modulo1_R1.xml") ///
replace title ("Regression for Oportunidades data") sd below sheet("Outputs1")

xml_tab reg_1 reg_2 reg_3 reg_4, save("C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\1. Intro to STATA\Module1_R2.xml") ///
replace title ("Regression for Oportunidades data") sd below sheet("Outputs2")


*ereturn list
regress pov_HH D mwagemale agehead, vce (robust)

ereturn list

scalar obs=e(N)
display obs

matrix list e(b)
matrix betha=e(b)
display betha[1,3]

*Predict
predict yhatpov_HH
summarize yhatpov_HH

predict double resid, residuals
summarize resid

*Marginal Effects
mfx, varlist(D mwagemale) eyex

*---------------------------------------*
* 11. HYPOTHESIS TESTING				*
*---------------------------------------*

* t-test

ttest pov_HH, by(D) unequal  

* F-test
quietly regress pov_HH D mwagemale agehead, vce (robust)
test D mwagemale agehead

test _b[D]=0

scalar t=r(F)^(1/2)
display t

regress pov_HH D mwagemale agehead, vce (robust)

* CLOSING LOG FILE
log close
