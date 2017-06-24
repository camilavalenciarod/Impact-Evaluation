*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*
*    DEVELOPMENT ECONOMICS    *
*		Poverty Analysis 	  *
*    Stanislao Maldonado      *    
*\\\\\\\\\\\\\\\\\\\\\\\\\\\\\*

* Encuesta Nacional de Hogares Peru, 2014: Module of summary variables

use "C:\Users\Stan Maldonado\Dropbox\Encuestas LAC\PER\Bases\ENAHO\ENAHO2014\sumaria-2014.dta" 


/////////////////////////////////
/*  1. DESCRIBING SURVEY DATA  */
/////////////////////////////////

* Describing sample

describe

summarize

* Computing monthly spending per-capita (poverty line is defined in monthly per-capita terms)

gen gastopc=(gashog2d/mieperho)/12


/////////////////////////////////
/* 2. BASIC POVERTY INDICATORS */
/////////////////////////////////

*------------------------------*
* Computing poverty indicators
*------------------------------*

* Installing program to compute poverty indicators
findit sepov 

* Computing total poverty
sepov gastopc [w=factor07], p(linea) alfa(0) psu(conglome) strata(estrato)

* Computing extreme poverty
sepov gastopc [w=factor07], p(linpe) alfa(0) psu(conglome) strata(estrato)

* Checking with official numbers provided by INEI
tab pobreza [aw=factor07]

*---------------------------------------*
* Computing poverty indicators by region
*---------------------------------------*

* Computing total poverty
sepov gastopc [w=factor07], p(linea) alfa(0) psu(conglome) strata(estrato) by(dominio)

* Computing extreme poverty
sepov gastopc [w=factor07], p(linpe) alfa(0) psu(conglome) strata(estrato) by(dominio)

*--------------------------------*
* Alternative poverty indicators *
*--------------------------------*

findit apoverty

apoverty gastopc [w=factor07], varpl(linea) all


///////////////////////////////
/* 3. POVERTY DECOMPOSITIONS */
///////////////////////////////

findit povdeco

povdeco gastopc [w=factor07], varpline(linea) bygroup(dominio) summarize


////////////////////////
/* 4. POVERTY PROFILE */
////////////////////////

* Merging dataset with socio-economic characteristics

sort conglome vivienda hogar

save "C:\Users\Stan Maldonado\Dropbox\Encuestas LAC\PER\Bases\ENAHO\ENAHO2014\temporal_sumaria.dta", replace


use "C:\Users\Stan Maldonado\Dropbox\Encuestas LAC\PER\Bases\ENAHO\ENAHO2014\enaho01-2014-100.dta"
 
sort conglome vivienda hogar

merge conglome vivienda hogar using "C:\Users\Stan Maldonado\Dropbox\Encuestas LAC\PER\Bases\ENAHO\ENAHO2014\temporal_sumaria.dta"

keep if _merge==3

* Creating poverty dummy

gen poverty=0
replace poverty=1 if pobreza==1 | pobreza==2

* Poverty profile: type of dwelling

tab p101, gen(p101_)
tabstat p101_*,s(mean) by(poverty)

* Poverty profile: type of wall's materials
tab p102, gen(p102_)
tabstat p102_*,s(mean) by(poverty)

* Poverty profile: type of floor's materials
tab p103, gen(p103_)
tabstat p103_*,s(mean) by(poverty)

* Poverty profile: Number of rooms
tabstat p104,s(mean) by(poverty)

* Poverty profile: Water
tab p110, gen(p110_)
tabstat p110_*,s(mean) by(poverty)

* Poverty profile: assets
des p114*
tabstat p114*,s(mean) by(poverty)

* Excercise: construct a profile using type of alumbrado (questions from p1121 to p1126) 
