*

* ------------------------------------------------------------------------------
* Add extra adopath 
* ------------------------------------------------------------------------------
adopath + "Z:\Desktop\COVID-Collateral\code\generic"

* ------------------------------------------------------------------------------
* Run Globals
* ------------------------------------------------------------------------------
do "Z:\Desktop\COVID-Collateral\code\generic\000_globals.do"

* ------------------------------------------------------------------------------
* Clean outcome data
* ------------------------------------------------------------------------------
clear 

cd "$dataDir"
 
cleanDefine, keyword(diabetes) enddate(10aug2020)

* ------------------------------------------------------------------------------
* Generate diabetes type
* ------------------------------------------------------------------------------
clear 
* requires changing the enddate in the dofile at present
do "Z:\Desktop\COVID-Collateral\code\generic\003_cr_diabetestype.do"

* ------------------------------------------------------------------------------
* Load Denominator file
* ------------------------------------------------------------------------------

use "$denomDir\202008_CPRDAurum_AcceptablePats.dta", replace

cd "$dataDir"

* Generate weekly denominators
weeklyDenom, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(diabetes)

* ------------------------------------------------------------------------------
* Load outcome data 
* ------------------------------------------------------------------------------

* Load outcome data
use "$dataDir\cr_diabetes_outcomes", replace // to be update to diabetes_outcomess

* Generate weekly outcome data
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(diabetes) outcomegap(2)

* ------------------------------------------------------------------------------
* Prepare csv for figures
* ------------------------------------------------------------------------------

* Merge and export as .csv
clear
* Load denominator data
use "$dataDir\cr_diabetes_weekly_denoms.dta", replace

* Merge outcome data
merge 1:1 weekDate category stratifier using "$dataDir\cr_diabetes_weekly_outcomes.dta", keepusing(numOutcome) 

* Keep consistent dates between the two datasets
keep if _merge==3
drop _merge

export delimited using "$dataDir\an_diabetes.csv", replace
