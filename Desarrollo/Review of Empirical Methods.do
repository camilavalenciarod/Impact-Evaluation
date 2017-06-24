*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
*    DEVELOPMENT ECONOMICS    *
* Review of Empirical Methods *
*    Stanislao Maldonado      *    
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* Dataset

use "C:\Users\Stan Maldonado\Dropbox\Training Materials\Program Evaluation\Courses\evaluation.dta", clear

*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 0. INSPECTING THE DATASET: PROGRESA SUBSAMPLE  WITH BASELINE *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* DESCRIBING DATASET

describe 

* SUMMARAZING DATASET

sum if round==0 

sum if round==1 

*\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 1. EXPERIMENTAL ANALYSIS *
*\\\\\\\\\\\\\\\\\\\\\\\\\\*

*----------------------------------------------------------*
*1.1 EVALUATING RANDOM ASSIGNMENT: BALANCE IN THE BASELINE *
*----------------------------------------------------------*

* Note: Treatment variable treatcom is defined at community level

* T-TEST FOR COVARIATES
*----------------------

findit ttable2

ttable2 age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal cpc if round==0, by(treatcom)

* OUTCOME VARIABLE AT BASELINE
*-----------------------------

* Graphical evidence
kdensity cpc if treatcom==1 & round==0, addplot(kdensity cpc if treatcom==0 & round==0)

* Kolmogorov-Smirnov test for differences in distribution
ksmirnov cpc if round==0, by(treatcom)

*----------------------*
*1.2 EVALUATING IMPACT *
*----------------------*

* T-test for difference in means
ttest cpc if round==1, by(treatcom)

* Using a regression
regress cpc treatcom if round==1

*-------------------------------------------*
*1.3 EVALUATING IMPACT INCLUDING COVARIATES *
*-------------------------------------------*

regress cpc treatcom age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal if round==1


*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 2. EXPERIMENTS WITH NON-COMPLIANCE *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* Note: We consider treatment at household level (takeup) but assigment is defined by poverty score (puntaje) which defines assigment to treatment (eligible)

*----------------------------------------------------------------*
* 2.1 DESCRIBING TREATMENT (D) VERSUS ASSIGMENT TO TREATMENT (Z) *
*----------------------------------------------------------------*

* TREATED AND ELIGIBLE IN THE SAMPLE
 
* Note: 5,652 of eligible households did not comply with treatment 

tab takeup eligible if round==0

* REGRESSION OF OUTCOMES ON (ENDOGENOUS) TREATMENT 

ttable2 age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal cpc if round==0, by(takeup)

regress cpc takeup if round==1

*--------------------------------------------------*
* 2.2 REDUCED-FORM REGRESSIONS: INTENTION TO TREAT *
*--------------------------------------------------*

* Y on Z
regress cpc eligible if round==1

* D on Z
regress takeup eligible if round==1

*------------------------------------------------*
* 2.3 REGRESSION: LOCAL AVERAGE TREATMENT EFFECT *
*------------------------------------------------*

ivregress 2sls cpc (takeup=eligible) if round==1


*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 3. REGRESSION DISCONTINUITY DESIGNS *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

*--------------------------------------*
* 3.1 DESCRIBING THE RUNNING VARIABLE  *
*--------------------------------------*

* Note: Running variable X is puntaje

sum puntaje   							// Inspecting the running variable

gen running=puntaje-750    				// Normalizing the running variable to have zero as the cutoff

sum running

*-----------------------------------*
* 3.2 EVALUATING INTERNAL VALIDITY  *
*-----------------------------------*

gen z=0
replace z=1 if running>0				// Creating an assignment variable

* CREATING ALTERNATIVE BANDWIDTHS

gen bw3=0
replace bw3=1 if running<3 & running> -3 

gen bw5=0
replace bw5=1 if running<5 & running> -5 

gen bw10=0
replace bw10=1 if running<10 & running> -10 

gen bw20=0
replace bw20=1 if running<20 & running> -20 

* BALANCE CHECKS FOR DIFFERENT BANDWIDTHS

ttable2 age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal cpc if round==0 & bw3==1, by(z)

ttable2 age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal cpc if round==0 & bw5==1, by(z)

ttable2 age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal cpc if round==0 & bw10==1, by(z)

ttable2 age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal cpc if round==0 & bw20==1, by(z)


*--------------------*
* 3.3 RDD ESTIMATOR  *
*--------------------*

findit rd

rd cpc takeup running if round==1, gr        //Non-parametric fuzzy design


*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 4. DIFFERENCE IN DIFFERENCES *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

*--------------------*
* 4.1 DIF ESTIMATOR  *
*--------------------*

* We use treatcom as treatment variable

findit diff

diff cpc, t(treatcom) p(round)

diff cpc, t(treatcom) p(round) cov(age_hh age_sp educ_hh educ_sp)

*----------------------------*
* 4.2 PRE-TREATMENT BALANCE  *
*----------------------------*

diff cpc, t(treatcom) p(round) cov(age_hh age_sp educ_hh educ_sp) test


*\\\\\\\\\\\\\\\\*
* 5. PS MATCHING *
*\\\\\\\\\\\\\\\\*

*--------------------------------------*
* 5.1 ESTIMATING THE PROPENSITY SCORE  *
*--------------------------------------*

findit pscore

pscore takeup age_hh age_sp educ_hh educ_sp ethnicity_hh female_hh hhsize_basal dirtfloor_basal bathroom_basal landhectars_basal min_dist if round==0, pscore(pscore)

*--------------------------------------------------------*
* 5.2 EVALUATING COMMON SUPPORT ON THE PROPENSITY SCORE  *
*--------------------------------------------------------*

kdensity pscore if takeup==1 & round==0, addplot(kdensity pscore if takeup==0 & round==0)

*------------------------------------------------*
* 5.2 COMPUTING ATE USING THE NEAREST NEIGHBOOR  *
*------------------------------------------------*

* We impute the pscore computed for period 0 for period 1
egen propensity=total(pscore), by(hhid) missing

attnw cpc takeup if round==1, pscore(propensity)
