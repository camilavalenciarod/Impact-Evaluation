*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
*    DEVELOPMENT ECONOMICS    *
* 	  Growth Regressions      *
*    Stanislao Maldonado      *    
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* Dataset

use "C:\Users\Stanislao\Dropbox\Teaching\1. Current\Development Economics\3. Courses\3. STATA Materials\Replication MRW (1992)\mrw.dta", clear

*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 1. INSPECTING THE DATASET: MANKIW, ROMER AND WEIL (1992)	   *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* DESCRIBING DATASET

describe

* SUMMARAZING DATASET

summarize

* CREATING ADDITIONAL VARIABLES

gen lrgdpw85=ln(rgdpw85)
label var lrgdpw85 "Log of real GDP per working age population in 1985"

gen li_y=ln(i_y/100)
label var li_y "Log of Real investment as a share of real GDP (average 1960-85)"

gen lpop=ln(0.05+popgrowth/100)
label var lpop "Log of Average annual growth rate of the working age population between 1960-1985"

*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 2. BASIC ANALYSIS: SOLOW MODEL *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* REPLICATING TABLE I-UNRESTRICTED MODEL

regress lrgdpw85 li_y lpop if n==1, robust     // Non-oil countries sample
estimates store r1

regress lrgdpw85 li_y lpop if i==1, robust        // Intermediate countries sample
estimates store r2

regress lrgdpw85 li_y lpop if o==1, robust        // OECD countries sample
estimates store r3

estimates table r1 r2 r3, se stats (N r2 F ll)

* EXTENDING MODEL TO INCORPORATE HUMAN CAPITAL (TABLE II OF MRW)

gen lschool=ln(school)
label var lschool "Log of % of working age population in secondary school"

regress lrgdpw85 li_y lpop lschool if n==1, robust        // Non-oil countries sample
estimates store e1

regress lrgdpw85 li_y lpop lschool if i==1, robust        // Intermediate countries sample
estimates store e2

regress lrgdpw85 li_y lpop lschool if o==1, robust        // OECD countries sample
estimates store e3

estimates table e1 e2 e3, se stats (N r2 F ll)


*\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
* 3. TESTING FOR CONVERGENCE *
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* REPLICATING TABLE III OF MRW: UNCONDITIONAL CONVERGENCE

gen lrgdpw60=ln(rgdpw60)
label var lrgdpw60 "Log of real GDP per working age population in 1960"

gen change_lrgdpw=lrgdpw85-lrgdpw60 
label var change_lrgdpw "Change in the log of real GDP per working age population in 1960-1985"

regress change_lrgdpw lrgdpw60 if n==1, robust        // Non-oil countries sample
estimates store u1

regress change_lrgdpw lrgdpw60 if i==1, robust        // Intermediate countries sample
estimates store u2

regress change_lrgdpw lrgdpw60 if o==1, robust        // OECD countries sample
estimates store u3

estimates table u1 u2 u3, se stats (N r2 F ll)


* REPLICATING TABLE IV OF MRW: CONDITIONAL CONVERGENCE

regress change_lrgdpw lrgdpw60 li_y lpop if n==1, robust        // Non-oil countries sample
estimates store c1

regress change_lrgdpw lrgdpw60 li_y lpop if i==1, robust        // Intermediate countries sample
estimates store c2

regress change_lrgdpw lrgdpw60 li_y lpop if o==1, robust        // OECD countries sample
estimates store c3

estimates table c1 c2 c3, se stats (N r2 F ll)


* REPLICATING TABLE V OF MRW: CONDITIONAL CONVERGENCE INCLUDING HUMAN CAPITAL

regress change_lrgdpw lrgdpw60 li_y lpop lschool if n==1, robust        // Non-oil countries sample
estimates store h1

regress change_lrgdpw lrgdpw60 li_y lpop lschool if i==1, robust        // Intermediate countries sample
estimates store h2

regress change_lrgdpw lrgdpw60 li_y lpop lschool if o==1, robust        // OECD countries sample
estimates store h3

estimates table h1 h2 h3, se stats (N r2 F ll)
