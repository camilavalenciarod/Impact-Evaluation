*********************************************************
* APPLIED IMPACT EVALUATION 							*
* REVIEW: INTRO TO STATA								*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org 							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 01/31/17									*
*********************************************************

clear all		/*to clear any previous stuff*/

set mem 1g		/*No needed for STATA 13*/

cap log close
set more off

*******************************
* PART A: INSPECT THE DATASET *
*******************************

/* i.	Assign a path to the directory where you will work using a global labeled “path”. 
Then, create a log file using the name ReviewSTATA.log*/

global path="C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\"
log using "$path\\\1. Modules\1. Intro to STATA\6. Review\Review_STATA.log", replace

/* ii.	Keep a subset of the variables*/

use conglome vivienda hogar dominio estrato codperso p203 p207 p208a p209 using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\1. Intro to STATA\6. Review/enaho01-2011-200.dta", clear

/*iii.	Provide a description of the variables in the database using the identifier to sort them*/

sort conglome vivienda hogar
des

***********************************
* PART B: DUPLICATES OBSERVATION  *
***********************************

/*      Determine whether the unit of analysis is correct */
duplicates report conglome vivienda hogar codperso
duplicates tag conglome vivienda hogar codperso, gen(dup)
drop if dup==1

*****************************************
* PART C: TO CREATE A SINGLE IDENTIFIER *
*****************************************

/* i.	Replicate the original variables to construct a single identifier */
		
gen conglome2=conglome
gen vivienda2=vivienda
gen hogar2=hogar
gen codperso2=codperso

/* ii.	Convert them into numerical variables */

destring conglome2 vivienda2 hogar2 codperso2, replace

/* iii.	Convert them into string variables again */
		
tostring conglome2 vivienda2 hogar2 codperso2, replace

/* iv.	Re-create the original identifiers */

replace conglome2 = "000"+ conglome2 if length(conglome2)==1
replace conglome2 = "00"+ conglome2 if length(conglome2)==2
replace conglome2 = "0"+ conglome2 if length(conglome2)==3

replace vivienda2 = "00"+ vivienda2 if length(vivienda2)==1
replace vivienda2 = "0"+ vivienda2 if length(vivienda2)==2

replace codperso2 = "0"+ codperso2 if length(codperso2)==1

/* v.	Create household and individual identifiers */

gen hhid = conglome2 + vivienda2 + hogar2
label var hhid "HH ID"

gen individ = conglome2 + vivienda2 + hogar2 + codperso2
label var individ "Individual ID"

/* vi.	Perform reverse exercise. 
		(Hint: use the command substr function).  */
		
gen conglome3=substr(individ,1,4)
gen vivienda3 =substr(individ,5,3)
gen hogar3=substr(individ,8,2)
gen codperso3 =substr(individ,10,2) 

list conglome vivienda hogar codperso conglome3 vivienda3 hogar3 codperso3 in 1/10

*****************************************
* PART D: TO CREATE INDICATORS          *
*****************************************

/* i.	From the individual information, generate a variable for family size per household 
		(Hint : use the egen command with the option by)*/
gen aux = 1
egen famsize = sum(aux) , by(hhid)
label var famsize "HH size"
 
* ii. Create sexhead1 (1 = Male , 0 = Female)

tab p203

tab p203, nol

tab p207

tab p207, nol

gen auxsexhh=0
replace auxsexhh=1 if p203==1 & p207==1

egen sexhead= max(auxsexhh), by(hhid)
label var sexhead "Gender of HH head: 1= M, 0=F"

/* iii.	Creating a new database with the average at household level 
		using the command collapse.*/
		
collapse(first)conglome vivienda hogar sexhead famsize, by(hhid) 

sort conglome vivienda hogar

*- Save this database with the name temphh.dta.
save "$path\\1. Modules\1. Intro to STATA\6. Review\temphh.dta", replace

********************************
* PART E: HOUSEHOLD DATASET    *
********************************

/* i.	Upload the variables conglome vivienda hogar pobreza inghog2d.*/	
		
use conglome vivienda hogar pobreza inghog2d using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\1. Intro to STATA\6. Review\sumaria-2011.dta", clear

/* ii.	Sort variables before merging */

sort conglome vivienda hogar

*- Save this database with the name temp_sumaria.dta.
save "$path//1. Modules\1. Intro to STATA\6. Review/temp_sumaria.dta", replace

************************************
* PART F: MERGE DATASETS           *
************************************

/*      Using the current master database (with household level information), 
		add the variables available in temphh.dta (hint: use the merge command). */

merge 1:1 conglome vivienda hogar using "$path\\1. Modules\1. Intro to STATA\6. Review\temphh.dta" 
tab _merge

*- Save this new dataset under the name hh_2011.dta.

save "$path//1. Modules\1. Intro to STATA\6. Review/hh_2011.dta", replace

************************************
* PART G: ANALYSIS OF DATA         *
************************************

/* i.	Let’s now create a database with the most important statistics by level of household poverty. */
use "$path//1. Modules/1. Intro to STATA/6. Review/hh_2011.dta", clear
global Varlist famsize sexhead

/* ii.	Let’s now create the household poverty variable. */

tab pobreza
tab pobreza, nol

gen pov_HH=0
replace pov_HH=1 if pobreza==1 | pobreza==2

label def poor 0"Nonpoor" 1"Poor"
label val pov_HH poor
drop if pov_HH ==.

tabstat $Varlist, by(pov_HH) s(mean sd) save

tabstatmat Mean		/*Install the program from the web*/

*- Then, export it to an excel file (Hint: use the tabstat command and then xml_tab).	

xml_tab Mean, title (Poor vs non Poor)  ///
sheet("Stats Mean") save ("$path//1. Modules/1. Intro to STATA/6. Review/HW1_yourlastnamename.xls") replace

/* ii.	Use a t-test to evaluate whether there are statistical differences between the socio-economic 
		characteristics you chose in the previous part according to household level poverty. */

global Varlist famsize sexhead 
foreach var of  global Varlist{
	ttest `var',by(pov_HH)
	}
	
* CLOSING LOG-FILE

log close
