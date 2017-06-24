*********************************************************
* CAUSAL INFERENCE AND IMPACT EVALUATION				*
* LECTURE XXX: REGRESSION DISCONTINUITY DESIGN			*
* LEE ET AL (2004): DO VOTERS AFFECT OR ELECT POLICIES?	*
*														*
* Stanislao Maldonado									*
* University of California at Berkeley					*
* E-mail: smaldonadoz@berkeley.edu						*
* Last change: 11/15/11									*
*********************************************************

set mem 600m

cd "C:\Users\Stanislao\Dropbox\Teaching\Econometrics\1. Readings\1. Topics\6. RDD\4. Teaching Materials\Lee et al (2004)\"

* Data
use "Lee et al (2004)_2.dta", clear 

*------------------------*
* 1. Summarizing dataset *
*------------------------*

*Summary statistics
tabstat demvote congress demvoteshare lagdemvoteshare realada totpop medianincome pcturban mnfcng votingpop, by(democrat) save 
tabstatmat A
matrix T1=A'
xml_tab T1, replace save("descriptive1.xml")

*ttest

local vartest "congress demvoteshare lagdemvoteshare realada totpop medianincome pcturban mnfcng votingpop"
	
foreach x of local vartest {
	ttest `x', by(democrat)
}

*Summary statistics restricted to different margin victory

gen margin=demvoteshare-.5

gen m25=.
replace m25=0 if demvoteshare!=. 
replace m25=1 if margin>=-.25 
replace m25=0 if margin>=.25 & m25==1

gen m10=.
replace m10=0 if demvoteshare!=. 
replace m10=1 if margin>=-.1 
replace m10=0 if margin>=.1 & m10==1

gen m3=.
replace m3=0 if demvoteshare!=. 
replace m3=1 if margin>=-.03 
replace m3=0 if margin>=.03 & m3==1

tabstat congress demvoteshare lagdemvoteshare realada totpop medianincome pcturban mnfcng votingpop if m25==1, by(democrat) save 
tabstatmat B
matrix T2=B'
xml_tab T2, replace save("descriptive2.xml")

tabstat congress demvoteshare lagdemvoteshare realada totpop medianincome pcturban mnfcng votingpop if m10==1, by(democrat) save
tabstatmat C
matrix T3=C'
xml_tab T3, replace save("descriptive3.xml")

tabstat congress demvoteshare lagdemvoteshare realada totpop medianincome pcturban mnfcng votingpop if m3==1, by(democrat) save
tabstatmat D
matrix T4=D'
xml_tab T4, replace save("descriptive4.xml")

*kernel density

kdensity demvoteshare, xline(.5) graphregion(fcolor(white)) title("") legend(off) xlabel(0(0.1)1) xti("Democratic vote share")

hist demvoteshare, xline(.5) graphregion(fcolor(white)) title("") legend(off) xlabel(0(0.1)1) xti("Democratic vote share")

gen demvoteshare_f=demvoteshare
replace demvoteshare_f=0.5 if demvoteshare>0.5 & demvoteshare<0.55
replace demvoteshare_f=0.52 if demvoteshare>0.55 & demvoteshare<0.6
replace demvoteshare_f=0.54 if demvoteshare>0.6 & demvoteshare<0.65
replace demvoteshare_f=0.56 if demvoteshare>0.65 & demvoteshare<0.7
replace demvoteshare_f=0.58 if demvoteshare>0.7 & demvoteshare<0.75
replace demvoteshare_f=0.60 if demvoteshare>0.75 & demvoteshare<0.80
replace demvoteshare_f=0.62 if demvoteshare>0.80 & demvoteshare<0.85
replace demvoteshare_f=0.64 if demvoteshare>0.85 & demvoteshare<0.90
replace demvoteshare_f=0.66 if demvoteshare>0.90 & demvoteshare<0.95
replace demvoteshare_f=0.68 if demvoteshare>0.95 & demvoteshare<1

hist demvoteshare_f, xline(.5) graphregion(fcolor(white)) title("") legend(off) xlabel(0(0.1)1) xti("Democratic vote share")

*-----------------*
* 2. Graphical RD *
*-----------------*

*ADA score t+1
*-------------

preserve

gen d1 = 0 if lagdemvote <=.5
replace d1 = 1 if lagdemvote >.5
replace d1 = . if lagdemvote ==.

gen bin2 = int(lagdemvote*100)/100
gen dembin = bin2
label var dembin "Democratic vote share, t"

drop if district==. & dembin==.

sort dembin
collapse meanY100 =realada, by(dembin)

gen x2 = dembin*dembin
gen x3 = dembin*dembin*dembin
gen x4 = dembin*dembin*dembin*dembin

gen dd1 = 0 if dembin<.5
replace dd1 = 1 if dembin>.5
replace dd1 = . if dembin==.

*Linear
reg meanY100 dd1
predict fit1
tw (scatter meanY100 dembin if dd1==1) (line fit1 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit1 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f1_linear,replace) legend(off) yti("ADA score, t+1") xlabel(0(0.1)1)

*Quadratic  
reg meanY100 dd1 x2
predict fit2
tw (scatter meanY100 dembin if dd1==1) (line fit2 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit2 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f1_cuadratic,replace) legend(off) yti("ADA score, t+1") xlabel(0(0.1)1) 
 
*Cubic 
reg meanY100 dd1 x2 x3
predict fit3
tw (scatter meanY100 dembin if dd1==1) (line fit3 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit3 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f1_cubic,replace) legend(off) yti("ADA score, t+1") xlabel(0(0.1)1)

*Fourth polynomial 
reg meanY100 dd1 x2 x3 x4
predict fit4
tw (scatter meanY100 dembin if dd1==1) (line fit4 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit4 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f1_fourth,replace) legend(off) yti("ADA score, t+1") xlabel(0(0.1)1)
   
*Local linear regression 
 
tw (scatter meanY100 dembin if dd1==1)(lpoly meanY100 dembin if dd1==1, k(tri) deg(1)) || (scatter meanY100 dembin if dd1==0) ///
	(lpoly meanY100 dembin if dd1==0, k(tri) deg(1)), xline(0.5) graphregion(fcolor(white)) ///
	saving(f1_locreg_ktri1,replace) legend(off) yti("ADA score, t+1") xti("Democratic vote share, t") xlabel(0(0.1)1)
	
tw (scatter meanY100 dembin if dd1==1)(lpoly meanY100 dembin if dd1==1, k(rec) deg(1)) || (scatter meanY100 dembin if dd1==0) ///
	(lpoly meanY100 dembin if dd1==0, k(rec) deg(1)), xline(0.5) graphregion(fcolor(white)) ///
	saving(f1_locreg_krec1,replace) legend(off) yti("ADA score, t+1") xti("Democratic vote share, t") xlabel(0(0.1)1)
	
tw (scatter meanY100 dembin if dd1==1)(lpoly meanY100 dembin if dd1==1, k(rec) deg(3)) || (scatter meanY100 dembin if dd1==0) ///
	(lpoly meanY100 dembin if dd1==0, k(rec) deg(3)), xline(0.5) graphregion(fcolor(white)) ///
	saving(f1_locreg_krec3,replace) legend(off) yti("ADA score, t+1") xti("Democratic vote share, t") xlabel(0(0.1)1)
   
restore


*ADA score (t)
*-------------

preserve

gen d1 = 0 if demvoteshare <=.5
replace d1 = 1 if demvoteshare >.5
replace d1 = . if demvoteshare ==.

gen bin2 = int(demvoteshare*100)/100
gen dembin = bin2
label var dembin "Democratic vote share, t"

drop if district==. & dembin==.

sort dembin
collapse meanY100 =realada, by(dembin)

gen x2 = dembin*dembin
gen x3 = dembin*dembin*dembin
gen x4 = dembin*dembin*dembin*dembin

gen dd1 = 0 if dembin<=.5
replace dd1 = 1 if dembin>.5
replace dd1 = . if dembin==.

reg meanY100 dd1 x2 x3 x4
predict fit4

tw (scatter meanY100 dembin if dd1==1) (line fit4 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit4 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f2a,replace) legend(off) yti("ADA score, t") xlabel(0(0.1)1)

restore


*Probability of democratic win in t+1
*------------------------------------

preserve

gen d1 = 0 if lagdemvote <=.5
replace d1 = 1 if lagdemvote >.5
replace d1 = . if lagdemvote ==.

gen bin2 = int(lagdemvote*100)/100
gen dembin = bin2
label var dembin "Democratic vote share, t"

drop if district==. & dembin==.

sort dembin
collapse meanY100 =democrat, by(dembin)

gen x2 = dembin*dembin
gen x3 = dembin*dembin*dembin
gen x4 = dembin*dembin*dembin*dembin

gen dd1 = 0 if dembin<.5
replace dd1 = 1 if dembin>.5
replace dd1 = . if dembin==.
 
*4th polynomial 
reg meanY100 dd1 x2 x3 x4
predict fit4
tw (scatter meanY100 dembin if dd1==1) (line fit4 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit4 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f2b,replace) legend(off) yti("Prob of winning, t+1") xlabel(0(0.1)1)
 
restore

*-------------------*
* 3. Regression RDD *
*-------------------*

preserve

*Preparing the dataset
*---------------------

sort state district year
by state district year: g n = _n
keep if n<=2

replace year = year +2
gen lagged = realada
keep state district year n lagged
sort state district year n
save tmp, replace

use "Lee et al (2004)_2.dta", clear 
sort state district year
by state district year: g n = _n
keep if n<=2

sort state district year n
merge state district year n using tmp
rm tmp.dta

drop if year ==1952 | year==1962 | year==1972 | year==1982 | year==1992

gen sample2=0
replace sample2=1 if lagdemvoteshare>.48 & lagdemvoteshare<.52

gen sample5=0
replace sample5=1 if lagdemvoteshare>.45 & lagdemvoteshare<.55

gen sample10=0
replace sample10=1 if lagdemvoteshare>.40 & lagdemvoteshare<.60


drop democrat
gen democrat = 1 if demvoteshare>=.5
replace democrat = 0 if demvoteshare<.5

gen lagdemocrat = 1 if lagdemvoteshare >=.5
replace lagdemocrat = 0 if lagdemvoteshare <.5
gen score = realada
gen lagscore = lagged
egen id = group(state district year n)
keep if score ~=. & lagscore ~=. 
keep if democrat~=. & lagdemocrat ~=. 
keep if id ~=.

* Replicating Table 1
*--------------------

*+2/-2
reg score lagdemocrat if sample2==1, cluster(id)
estimates store r1

reg score democrat if sample2==1, cluster(id)
estimates store r2

reg democrat lagdemocrat if sample2==1, cluster(id)
estimates store r3

*+5/-5
reg score lagdemocrat if sample5==1, cluster(id)
estimates store r4

reg score democrat if sample5==1, cluster(id)
estimates store r5

reg democrat lagdemocrat if sample5==1, cluster(id)
estimates store r6

*+10/-10
reg score lagdemocrat if sample10==1, cluster(id)
estimates store r7

reg score democrat if sample10==1, cluster(id)
estimates store r8

reg democrat lagdemocrat if sample10==1, cluster(id)
estimates store r9

xml_tab r1 r2 r3 r4 r5 r6 r7 r8 r9, replace save("Tables_Lee et al(2004).xml") sheet ("Table1") ///
	title("Table 1: Results based on ADA scores") below stats(N r2)
	
restore

*--------------------=====-----*
* 4. Testing Random Assignment *
*------------------------------*

*Percentage of Black people

preserve

gen d1 = 0 if demvoteshare <=.5
replace d1 = 1 if demvoteshare >.5
replace d1 = . if demvoteshare ==.

gen bin2 = int(demvoteshare*100)/100
gen dembin = bin2
label var dembin "Democratic vote share, t"

drop if district==. & dembin==.

sort dembin
collapse meanY100 = pctblack, by(dembin)

gen x2 = dembin*dembin
gen x3 = dembin*dembin*dembin
gen x4 = dembin*dembin*dembin*dembin

gen dd1 = 0 if dembin<.5
replace dd1 = 1 if dembin>.5
replace dd1 = . if dembin==.
 
*4th polynomial 
reg meanY100 dd1 x2 x3 x4
predict fit4
tw (scatter meanY100 dembin if dd1==1) (line fit4 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit4 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f3a,replace) legend(off) yti("Pencertage of Black People") xlabel(0(0.1)1)
 
restore

*Real income

preserve

gen d1 = 0 if demvoteshare <=.5
replace d1 = 1 if demvoteshare >.5
replace d1 = . if demvoteshare ==.

gen bin2 = int(demvoteshare*100)/100
gen dembin = bin2
label var dembin "Democratic vote share, t"

drop if district==. & dembin==.

sort dembin
collapse meanY100 = realincome, by(dembin)

gen x2 = dembin*dembin
gen x3 = dembin*dembin*dembin
gen x4 = dembin*dembin*dembin*dembin

gen dd1 = 0 if dembin<.5
replace dd1 = 1 if dembin>.5
replace dd1 = . if dembin==.
 
*4th polynomial 
reg meanY100 dd1 x2 x3 x4
predict fit4
tw (scatter meanY100 dembin if dd1==1) (line fit4 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit4 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f3b,replace) legend(off) yti("Real income") xlabel(0(0.1)1)
 
restore

*Percentage of high school

preserve

gen d1 = 0 if demvoteshare <=.5
replace d1 = 1 if demvoteshare >.5
replace d1 = . if demvoteshare ==.

gen bin2 = int(demvoteshare*100)/100
gen dembin = bin2
label var dembin "Democratic vote share, t"

drop if district==. & dembin==.

sort dembin
collapse meanY100 = pcthighschl, by(dembin)

gen x2 = dembin*dembin
gen x3 = dembin*dembin*dembin
gen x4 = dembin*dembin*dembin*dembin

gen dd1 = 0 if dembin<.5
replace dd1 = 1 if dembin>.5
replace dd1 = . if dembin==.
 
*4th polynomial 
reg meanY100 dd1 x2 x3 x4
predict fit4
tw (scatter meanY100 dembin if dd1==1) (line fit4 dembin if dd1==1) || (scatter meanY100 dembin if dd1==0) ///
 (line fit4 dembin if dd1==0), xline(0.5) graphregion(fcolor(white)) saving(f3c,replace) legend(off) yti("Percentage of high school") xlabel(0(0.1)1)
 
restore

*-------------------------*
* 5. McCrary density test *
*-------------------------*

preserve
drop if demvoteshare==1
drop if demvoteshare==0
DCdensity demvoteshare, breakpoint(0.5) generate(Xj Yj r0 fhat se_fhat)
drop Yj Xj r0 fhat se_fhat
restore


