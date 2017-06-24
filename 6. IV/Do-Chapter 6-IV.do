*********************************************************
* APPLIED IMPACT EVALUATION 							*
* CHAPTER 6: INSTRUMENTAL VARIABLES						*
*														*
* Stanislao Maldonado									*
* www.stanislaomaldonado.org							*
* E-mail: stanislao@stanislaomaldonado.org				*
* Last change: 01/16/17									*
*********************************************************


*log using "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\1. Modules\6. Instrumental Variables\3. Log\Chapter6_IV.log", text append  /*Change if another computer*/

*---------------------------------------*
* OPENING A DATA SET					*
*---------------------------------------*

*Type the full path of the data file (change if different computer)
use "C:\Users\P6069051\Dropbox\Teaching\1. Current\Econometrics\4. Handbook\2. Data\ENCEL 2007\Final\DataFinal_ENCEL07.dta", clear

destring villid, replace


*------------------------*
* 1. BASIC IV/GMM/LIML	 *
*------------------------*

* Instruments: mwagemale (weak) pov_HH (strong) wheat (weak)

* Baseline OLS

regress IncomeLabHH1 D_HH


* Basic IV (Instrument: mwagemale)

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale)

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale), vce(robust)

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale), small


* GMM

ivregress gmm IncomeLabHH1 (D_HH = mwagemale), wmatrix(unadjusted)   				/*Homoskedastic case*/

ivregress gmm IncomeLabHH1 (D_HH = mwagemale) 										/*Robust*/

ivregress gmm IncomeLabHH1 (D_HH = mwagemale), wmatrix(cluster villid) 				/*Cluster robust*/

ivregress gmm IncomeLabHH1 (D_HH = mwagemale), wmatrix(cluster villid) igmm 		/*Iterative GMM instead of two-step*/


* LIML

ivregress liml IncomeLabHH1 (D_HH = mwagemale)

ivreg2 IncomeLabHH1 (D_HH = mwagemale), liml  /*Equivalent result*/


* FULLER MODIFIED LIML

ivreg2 IncomeLabHH1 (D_HH = mwagemale), fuller (1) 

ivreg2 IncomeLabHH1 (D_HH = mwagemale), fuller (4) 


* MORE THAN ONE INSTRUMENT

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale pov_HH)

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale pov_HH wheat)

*------------------------*
* 2. ENDOGENEITY TEST	 *
*------------------------*

* Note: Test is not available for LIML

* Durbin and Wu-Hausman endogeneity test (assumes homokedasticity)
quiet ivregress 2sls IncomeLabHH1 (D_HH = mwagemale) 			
estat endogenous

* Wooldridge's (1995) endogeneity test (robust to heterokedasticity)
quiet ivregress 2sls IncomeLabHH1 (D_HH = mwagemale), vce(robust) 			
estat endogenous

* C-statistic (also known as Difference in Sargan statistic) endogeneity test (results are the same with robust S.E.)
quiet ivregress gmm IncomeLabHH1 (D_HH = mwagemale) 			
estat endogenous

*----------------------------*
* 3. INSTRUMENT RELEVANCE	 *
*----------------------------*

* DETECTING WEAK INSTRUMENTS

correlate D_HH mwagemale pov_HH wheat

* Includes: R2, Adj-R2, partial R2, F-statistic, Cragg and Donald's (1993) minimun eigenvalue statistic (test of weak instrument), Stock and Yogo's (2005) test 

* Single identified case

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale), first 			/*GMM does not deliver Stock-Yogo test. Results for 2sls and liml are the same*/
estat firststage, all

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale), first vce(robust)
estat firststage, forcenonrobust all

* Overidentified case

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale pov_HH), first vce(robust)
estat firststage, all

ivregress 2sls IncomeLabHH1 (D_HH = mwagemale pov_HH wheat), first vce(robust)
estat firststage, all


*----------------------------*
* 4. INSTRUMENT VALIDITY	 *
*----------------------------*

* TEST OF OVERIDENTIFYING RESTRICTIONS

* Sargan and Basmann tests (homokedastic case)
quiet ivregress 2sls IncomeLabHH1 (D_HH = mwagemale pov_HH)
estat overid

* Wooldridge's score test (heterokedastic case)
quiet ivregress 2sls IncomeLabHH1 (D_HH = mwagemale pov_HH), vce (robust)
estat overid

* Hansen's (1982) J statistic
quiet ivregress gmm IncomeLabHH1 (D_HH = mwagemale pov_HH)
estat overid

* Anderson-Rubin's (1950) likelihood ratio test and Basmann's (1960) F test
quiet ivregress liml IncomeLabHH1 (D_HH = mwagemale pov_HH)
estat overid


* SENSITIVITY TO THE CHOICE OF INSTRUMENTS

quiet regress IncomeLabHH1 D_HH, vce(robust)
estimates store ols

quiet ivregress 2sls IncomeLabHH1 (D_HH = mwagemale), vce(robust)
estimates store iv1

quiet ivregress 2sls IncomeLabHH1 (D_HH = pov_HH), vce(robust)
estimates store iv2

quiet ivregress 2sls IncomeLabHH1 (D_HH = wheat), vce(robust)
estimates store iv3

estimates table ols iv1 iv2 iv3, p


* CLOSING LOG FILE
*log close
