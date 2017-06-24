*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 7: POWER ANALYSIS								*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 05/03/17									*
*********************************************************


set more off

*---------------*
* 1. MOTIVATION	*
*---------------*

*
* Hypothetical program:3 millions
*--------------------------------

clear all

set seed 521

*Creating a dataset
set obs 3000000

*Creating unobservable: talent

generate u_talent = rnormal(0) 

gen aux_random=rnormal(0) 

generate edu_random=0
replace edu_random=1 if aux_random>0
drop aux_random 

generate lny_random = 5 + 0.1*edu_random + u_talent

*Testing impact: regression

regress lny_random edu_random


*Sample size:3000
*----------------

preserve

sample 3000, count

*Testing impact: regression

regress lny_random edu_random

restore


*Sample size:300
*---------------

preserve

sample 300, count

*Testing impact: regression

regress lny_random edu_random

restore


*Sample size:50
*--------------

preserve

sample 50, count

*Testing impact: regression

regress lny_random edu_random

restore


*----------------------------------*
* 2. INDIVIDUAL RANDOMIZED DESIGNS *
*----------------------------------*

use "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\ENCEL 2007\Final\DataFinal_power.dta", clear 


* 1. MEANS AND VARIANCE OF IMPACT INDICATOR

* Variance and standard deviation

sum IncomeLabHH1, detail

scalar mean_income = r(mean)			/*Mean*/

scalar var_income = r(Var)				/*Variance*/

scalar stddev_income = r(sd)			/*Standard deviation*/


* Means for treatment and control units (Assumption: standarized effect size of 0.2 desviaciones estandar)

scalar control_income=mean_income

scalar treat_income=control_income+0.2*stddev_income

local control_m = control_income
local treat_m = treat_income
local sigma_m = stddev_income


* 2. COMPUTING STANDARD ERRORS: (sigma^2/N)^(1/2)

scalar n_sample = 1000 				/*Asumming known sample size and we need to compute power or MDE*/

scalar st_error = (var_income/n_sample)^(1/2)


* 3. PROPORTION OF TREATMENT AND CONTROL UNITS: (1/(P(1-P)))^(1/2) 

local p=0.5								 /*P: proportion of treatment units*/

scalar p_exp=(1/(`p'*(1-`p')))^(1/2)


* 4. T-VALUES FOR ALPHA (SIGNIFICANCE LEVEL) AND 1-K (POWER) *

scalar t_alpha=invnormal(0.975)			/*Note: 0.95 if one-sided test*/

scalar t_beta=invnormal(0.90)


scalar t_alphaplusbeta=t_alpha+t_beta

* Parameters

display control_income
display treat_income

display p_exp

display t_alpha
display t_beta


* 5. OPTION 1: COMPUTING MDE (N and power given) *

/* FORMULA OF MDE:

MDE={t_(1-k)+t_alpha}*{(1/(P(1-P)))^(1/2)}*{(sigma^2/N)^(1/2)}

*/

scalar mde_abs=t_alphaplusbeta*p_exp*st_error

display mde_abs

*Using POWER command(STATA 13)

power twomeans `control_m', sd(`sigma_m') alpha(0.05) power (0.9) n(1000)


* 6. OPTION 2: COMPUTING POWER (N and effect size given) *

/*FORMULA OF POWER:

t_(1-k)=[delta/{(1/(P(1-P)))^(1/2)}*{(sigma^2/N)^(1/2)}]-t_alpha

*/

*Assume effect size (delta=280 mexican pesos or 0.2 std deviation)

scalar delta_n=0.2

scalar delta_a=delta_n*stddev_income

display delta_a

scalar t_power= (delta_a/(p_exp*st_error))- t_alpha

local t_power=t_power

scalar power=normal(`t_power')

display power

*Using POWER command(STATA 13)

power twomeans `control_m' `treat_m', sd(`sigma_m') alpha(0.05) n(1000)


* 7. OPTION 3: DETERMINATION OF SAMPLE SIZE (for a given effect size and power) *

/*FORMULA OF SAMPLE SIZE (n):

n = [sigma*{t_(1-k)+t_alpha}*{(1/(P(1-P)))^(1/2)}/delta]^2

*/

scalar n_sample=[stddev_income*t_alphaplusbeta*p_exp/delta_a]^2

display n_sample

*Using POWER command(STATA 13)

power twomeans `control_m' `treat_m', sd(`sigma_m') alpha(0.05) power(0.9)


* 8. POWER CURVE *

power twomeans `control_m' `treat_m', sd(`sigma_m') alpha(0.05) power(0.1(0.05)0.95) graph(y(power))

*/

*-------------------------------------*
* 3. FACTORS AFFECTING POWER ANALYSIS *
*-------------------------------------*

scalar control_income=mean_income
display control_income

scalar treat_income02=control_income+0.2*stddev_income
display treat_income02

local control_m = control_income
local treat_m02 = treat_income02

local sigma_m = stddev_income

*Baseline
power twomeans `control_m' `treat_m02', sd(`sigma_m') alpha(0.05) n(1000)


* A. EFFECT SIZE

scalar treat_income01=control_income+0.1*stddev_income
display treat_income01

local treat_m01 = treat_income01

power twomeans `control_m' `treat_m01', sd(`sigma_m') alpha(0.05) n(1000)


* B. SAMPLE SIZE

power twomeans `control_m' `treat_m02', sd(`sigma_m') alpha(0.05) n(500)


* C. POWER LEVEL

power twomeans `control_m' `treat_m02', sd(`sigma_m') alpha(0.05) power(0.9)

power twomeans `control_m' `treat_m02', sd(`sigma_m') alpha(0.05) power(0.8)


* D. OUTCOME VARIANCE

local sigma_m2 = stddev_income*1.5

power twomeans `control_m' `treat_m02', sd(`sigma_m2') alpha(0.05) n(1000)


* E. ONE-SIDED VS TWO-SIDED TEST

power twomeans `control_m' `treat_m02', sd(`sigma_m') alpha(0.05) n(1000) onesided


* F. PROPORTION OF TREATMENT AND CONTROLS IN THE SAMPLE

power twomeans `control_m' `treat_m02', sd(`sigma_m') alpha(0.05) n1(750) n2(250)


*----------------------------*
* 4. STANDARIZED EFFECT SIZE *
*----------------------------*

power twomeans 0 0.2, sd(1) alpha(0.05) n(1000)

*/

*------------------------------*
* 5. CLUSTER RANDOMIZED DESIGN *
*------------------------------*

* A. ESTIMATING ICC

* Alternative 1: Computing rho from ANOVA
 
loneway IncomeLabHH1 villid
scalar rho = r(rho)
display rho

* Alternative 2: Computing rho using Maximum Likelihood

findit iccvar

quiet xtmixed IncomeLabHH1 ||  villid: , var
iccvar


* B. COMPUTING MDE

scalar m_sample=20

scalar d_effect=(1+(m_sample-1)*rho)^(1/2)

display d_effect


scalar mde_abs=t_alphaplusbeta*p_exp*st_error

display mde_abs


scalar mde_abs_cluster=t_alphaplusbeta*p_exp*st_error*d_effect

display mde_abs_cluster


* C. CHANGES IN THE ICC

scalar rho_04=0.4

scalar d_effect04=(1+(m_sample-1)*rho_04)^(1/2)

scalar mde_abs_cluster_04=t_alphaplusbeta*p_exp*st_error*d_effect04

display mde_abs_cluster_04


* D. CHANGES IN NUMBER OF OBSERVATIONS PER CLUSTER

scalar m_sample50=50

scalar d_effect50=(1+(m_sample50-1)*rho)^(1/2)

scalar mde_abs_cluster_50=t_alphaplusbeta*p_exp*st_error*d_effect50

display mde_abs_cluster_50
