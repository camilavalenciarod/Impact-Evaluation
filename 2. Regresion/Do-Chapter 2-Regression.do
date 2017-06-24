*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 2: REGRESSION									*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 01/09/17									*
*********************************************************


set mem 800m		/*Setting memory size (for STATA lower than 12)*/

log using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\2. Review of Regression\4. Log\Chapter2-Regression.log", text append  /*Change if another computer*/

*---------------------------------------*
* 1. OPEN A DATA SET					*
*---------------------------------------*

*Encuesta Permanente de Hogares-EPH 2006 (Argentina)
use "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\EPH_2006.dta", clear

*-----------------------*
* 2. CONDITIONAL MEANS	*
*-----------------------*

*Conditional mean
sum income if eduyears==12.5
sum income if eduyears==17

*Conditional mean function
tabstat income, by(eduyears) stat(mean n)


egen meanincome=mean(income),by(eduyears)
label var meanincome "Conditional Mean Income"

*Plotting 
twoway (scatter income eduyears, mcolor(gray) msize(small)) (scatter meanincome eduyears) if income<10000

*Plotting
twoway (scatter income eduyears, mcolor(gray) msize(small)) (scatter meanincome eduyears) (lfit meanincome eduyears) if income<3000

*-------------------------------*
* 3. EXAMPLES OF DISTRIBUTIONS	*
*-------------------------------*

*NORMAL
#delimit ;
graph twoway (function y=normalden(x,100,5), range(80 120))
 ,yscale(off) legend(off)
	title(Normal density with mean=100 and s.d=5)
	xline(100, lcolor(navy) lpattern(dash))
	graphregion(fcolor(white))
 ;
#delimit cr  

*STANDARD NORMAL
#delimit ;
graph twoway (function y=normalden(x), range(-4 4))
 ,yscale(off) legend(off)
	title(Standard normal density)
	xline(0, lcolor(navy) lpattern(dash))
	graphregion(fcolor(white))
 ;
#delimit cr  

*CHI-SQUARE
findit chidemo
chidemo 20 0.01

*T-STUDENT
#delimit ;
graph twoway (function y=tden(10,x), range(-4 4))
 ,yscale(off) legend(off)
	title(Student's t density)
	xline(0, lcolor(navy) lpattern(dash))
	graphregion(fcolor(white))
 ;
#delimit cr 

*F-DISTRIBUTION
#delimit ;
graph twoway (function y=Fden(5,10,x), range(0 5))
			(function y=Fden(25,50,x), range(0 5) lpattern(dash))
			(function y=Fden(50,100,x), range(0 5)lpattern(dash dot))
 , legend(label(1 "F(5,10)") label (2 "F(25,50)") label (3 "F(50,100)"))
	title(F-distribution density)
	ytitle(F-density)
	graphregion(fcolor(white))
 ;
#delimit cr 

*---------------------------------------*
* 4. INFERENCE ABOUT POPULATION MEAN	*
*---------------------------------------*

* DICE EXPERIMENT

dice, nr(1) nd(1)

dice, nr(20) nd(1)

dice, nr(200000) nd(1)

*-----------------------------*
* 5. ASYMPTOTIC DISTRIBUTIONS *
*-----------------------------*

* 1 dice
clear all
set obs 6
gen dice=_n
label variable dice "Rolling One Dice"

summarize dice

* 2 dice
clear all
set obs 6
gen dice1=_n
gen dice2=_n
fillin dice1 dice2
gen avedice=(dice1+dice2)/2
label variable avedice "Average of Two Dices"
histogram avedice, discrete frac xlabel(1(1)6) graphregion(fcolor(white)) w(0.25) xline(3.5)

* 8 dice
clear all
set obs 6

forvalues i= 1(1)8{
	gen dice`i'=_n
}

fillin dice*
gen avedice=(dice1+dice2+dice3+dice4+dice5+dice6+dice7+dice8)/8
label variable avedice "Average of 8 Dices"
histogram avedice, discrete frac xlabel(1(1)6) graphregion(fcolor(white)) w(0.08) xline(3.5)

*-----------------------* 
* 6. HYPOTHESIS TESTING *
*-----------------------*

* NORMAL
#delimit ;
graph twoway (function y=normalden(x), range(-4 -1.96) bcolor(red) 
              recast(area) plotregion(style(none)))
  	       (function y=normalden(x), range(1.96 4) bcolor(red) 
              recast(area) plotregion(style(none)))
             (function y=normalden(x), range(-4 4) clstyle(line)
              clcolor(navy) clwidth(*3)
              plotregion(style(none))) 
 ,yscale(off) legend(off)
  xlabel( -1.96 "z = -1.96"  0 "z = 0" 1.96 "z = 1.96") 
  xtitle("z statistic", height(5))
  title(Normal density)
  text(.06 -2.5 "2.5%") text(.06 2.5 "2.5%")
  text(.1 0 "95%")
  graphregion(fcolor(white))
  xline(-1.96 1.96, lcolor(navy))
 ;
#delimit cr

dis "Critical c for t(29)="=invttail(99,.025) 

* T-STUDENT
#delimit ;
graph twoway (function y=tden(98,x), range(-4 -1.98) bcolor(red) 
              recast(area) plotregion(style(none)))
  	       (function y=tden(99,x), range(1.98 4) bcolor(red) 
              recast(area) plotregion(style(none)))
             (function y=tden(98,x), range(-4 4) clstyle(line)
              clcolor(navy) clwidth(*3)
              plotregion(style(none))) 
 ,yscale(off) legend(off)
  xlabel( -1.98 "t = -1.98"  0 "t = 0" 1.98 "t = 1.98") 
  xtitle("t statistic", height(5))
  title(t distribution for n = 100)
  text(.06 -2.5 "2.5%") text(.06 2.5 "2.5%")
  text(.1 0 "95%")
  graphregion(fcolor(white))
  xline(-1.98 1.98, lcolor(navy))
 ;
#delimit cr


dis "Critical c for t(29)="=invttail(29,.025)

dis "Critical c for t(49)="=invttail(49,.025)
dis "Critical c for t(99)="=invttail(99,.025)
dis "Critical c for t(199)="=invttail(199,.025)

* TESTING INCOME

*Encuesta Permanente de Hogares-EPH 2006 (Argentina)
use "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\EPH_2006.dta", clear


quiet sum income				/*Summary statistics*/
scalar se=r(sd)/(r(N)^(1/2))	/*Computing the SE(Y)*/
scalar t=(r(mean)-1110)/se		/*Computing t-statistic*/
display t						/*Displaying t-statistic*/

scalar df=r(N)-1				/*Degrees of freedom*/
local df=df						
dis "Critical c for t(`df')="=invttail(`df',.025) /*Critical t-value*/


ttest income=1110

* P-VALUE

quiet sum income					/*Summary statistics*/
scalar se=r(sd)/(r(N)^(1/2))		/*Computing the SE(Y)*/
scalar t=(r(mean)-1110)/se			/*Computing t-statistic*/
display t							/*Displaying t-statistic*/

scalar df=r(N)-1					/*Degrees of freedom*/
local df=df						
scalar p_half=ttail(`df',abs(t)) 	/*Half P-value (two-tail test)*/
scalar p=2*p_half					/*P-value*/
dis "P-value that H0=1,110 equal to" p


*---------------* 
* 7. REGRESSION *
*---------------*

* OPENING AND DESCRIBING DATASET

clear all

global path="C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\ENCEL 2007\Final"

use"$path/DataFinal_ENCEL07.dta", clear

keep villid hogid HH HH_AssHo iid IncomeLabHH1 labor ExperienceLabHH1 sex age contrHH health pov_HH famsize 

describe

* REGRESSION

replace  ExperienceLabHH1= ExperienceLabHH1/12
label var ExperienceLabHH1 "HH Experience: Years"

regress IncomeLabHH1 ExperienceLabHH1 sex

listcoef 

* HYPOTHESIS TESTING

quiet regress IncomeLabHH1 ExperienceLabHH1 sex

scalar df=e(N)-3				
local df=df	
di "t(`df') 97.5 th percentile = " invttail(`df',.025)
di "t(`df') 2.5 th percentile = " invttail(`df',.975)

scalar tstat = _b[ExperienceLabHH1]/_se[ExperienceLabHH1]
di "t-statistic for Ho: Beta=0 = " tstat



* DETECTING INFLUENTIAL DATA

summ IncomeLabHH1 ExperienceLabHH1 sex

histogram IncomeLabHH1 if IncomeLabHH1<12000, bin(20) normal xtitle("Primary  Monthly Income HH") note("Source: ENCEL 2007") graphregion(fcolor(white))

histogram ExperienceLabHH1, bin(15) normal xtitle("HH Experience: Years") note("Source: ENCEL 2007") graphregion(fcolor(white))


graph box IncomeLabHH1 if  IncomeLabHH1<12000, graphregion(fcolor(white)) 

graph box ExperienceLabHH1, graphregion(fcolor(white)) ytitle("Household experience (year)")


graph matrix IncomeLabHH1 ExperienceLabHH1, half graphregion(fcolor(white)) 

* Outlier, extreme value and leverage 

regress IncomeLabHH1 ExperienceLabHH1 sex if IncomeLabHH1<12000

quiet regress IncomeLabHH1 ExperienceLabHH1 sex
predict r, rstudent
stem r

hilo r villid hogid iid, h

gen n=_n
lvr2plot, mlabel(n) graphregion(fcolor(white))

predict d, cooksd
gsort -d
list villid hogid iid  IncomeLabHH1 n d  if d>4/20292 & e(sample)

predict dfit, dfits
gsort -dfit
list villid hogid iid  IncomeLabHH1 n dfit if abs(dfit)>2*sqrt(3/20292) & e(sample)

dfbeta
list n _dfbeta_1 _dfbeta_2 in 1/5

avplot ExperienceLabHH1, mlabel(n) graphregion(fcolor(white))

regress IncomeLabHH1 ExperienceLabHH1 sex if abs(dfit)<2*sqrt(3/20292)  


* NORMALITY

drop r
quiet regress IncomeLabHH1 ExperienceLabHH1 sex if abs(dfit)<2*sqrt(3/20292)  
predict r, r
kdensity r if IncomeLabHH1<10000, normal graphregion(fcolor(white))

pnorm r, graphregion(fcolor(white))
qnorm r, graphregion(fcolor(white))

iqr r

swilk r

* HOMOSKEDASTICITY

estat imtest

estat hettest

rvfplot, yline(0) graphregion(fcolor(white)) mcolor(gray)

* MULTICOLINEARITY

vif

* LINEARITY

regress IncomeLabHH1 ExperienceLabHH1 sex age

reganat IncomeLabHH1 ExperienceLabHH1 sex age if IncomeLabHH1<5000, dis(ExperienceLabHH1)biline
reganat IncomeLabHH1 ExperienceLabHH1 sex age if IncomeLabHH1<5000, dis(age)biline

scatter r ExperienceLabHH1 if IncomeLabHH1<20000, graphregion(fcolor(white))
scatter r age if IncomeLabHH1<20000, graphregion(fcolor(white))

quiet regress IncomeLabHH1 ExperienceLabHH1 sex age if IncomeLabHH1<20000
acprplot ExperienceLabHH1, lowess lsopts(bwidth(1)) graphregion(fcolor(white))
acprplot age, lowess lsopts(bwidth(1)) graphregion(fcolor(white))

gen age2 = age*age
lab var age2 "Square of age"
regress IncomeLabHH1 ExperienceLabHH1 sex age age2

* SPECIFICATION TEST

linktest
 
ovtest

*--------------------* 
* 8. STANDARD ERRORS *
*--------------------*

quiet regress IncomeLabHH1 ExperienceLabHH1 sex age
estimates store r1

quiet regress IncomeLabHH1 ExperienceLabHH1 sex age, vce(robust)
estimates store r2

quiet regress IncomeLabHH1 ExperienceLabHH1 sex age, vce(cluster villid)
estimates store r3

xml_tab r1 r2 r3 , replace save("module2.xls") sheet("BasicModel")   ///
title("Table 1: Standard errors for regression") below stats(N r2)


*-------------* 
* A. APPENDIX *
*-------------*

* BOX A.1: SIMULATION OF THE LAW OF LARGE NUMBERS

clear 										
set obs 500									/*Creates a sample of 500 observations*/
gen nvar=.									/*Creates 3 auxiliary variables*/
gen pvar=.
gen var=.
set seed 123								/*Seed to make sure the same results are obtained in each experiment*/
forvalues x = 1/500 {
  quietly replace var=uniform() in 1/`x'	/*Creates uniform variable*/
  quietly recode var (0/0.5=0)(0.5/1=1)		/*Converts uniform variable in binomial (0/1)*/
  sum var, meanonly							/*Recovers the mean*/
  quietly replace pvar=r(mean) in `x'		/*Records the mean value in the variable pvar in a given observation*/
  quietly replace nvar=`x' in `x'			/*Replaces in the nvar variable the number of the observation*/
  }

#delimit ;
twoway (line pvar nvar)
 , ylabel(0 .1(.1)1,angle(horizontal)) xlabel(1 50 100(200)500)
   ytitle(Proportion of One's) 
   xtitle("Sample Size (log scale)",height(5))
   title("Law of Large Numbers for a Binomial Variable (p=0.5)")
   yline(0.5) xscale(log) graphregion(fcolor(white))
 ;
#delimit cr


* BOX A.2: SIMULATION OF CENTRAL LIMIT THEOREM

program drop _all

program cltsim, rclass
	drop _all
	quiet set obs 40			/*Creating a dataset with 40 observation*/
	gen var=runiform()			/*Creates a random variable var with uniform distribution (0-1)*/
	sum var						
	return scalar meansamp = r(mean)
end 

cltsim

simulate var_bar = r(meansamp), seed(123)reps(100) nodots: cltsim
histogram var_bar, width(0.01) normal graphregion(fcolor(white))

simulate var_bar = r(meansamp), seed(123)reps(10000) nodots: cltsim
histogram var_bar, width(0.01) normal graphregion(fcolor(white))

* CLOSING LOG FILE
log close
