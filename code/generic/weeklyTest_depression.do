*

* ------------------------------------------------------------------------------
* Add extra adopath 
* ------------------------------------------------------------------------------
adopath + "Z:\Documents\COVID-Collateral\code\generic"

* ------------------------------------------------------------------------------
* Run Globals
* ------------------------------------------------------------------------------
do "Z:\Documents\COVID-Collateral\code\generic\000_globals.do"

* ------------------------------------------------------------------------------
* Load Denominator file
* ------------------------------------------------------------------------------

use "$denomDir\202005_CPRDAurum_AcceptablePats.dta", replace

cd "$dataDir"

* Generate weekly denominators
weeklyDenom, startdate(01jan2017) enddate(10jul2020) lockdown(23mar2020) study(depression)


* ------------------------------------------------------------------------------
* Clean outcome data
* ------------------------------------------------------------------------------
clear 

cd "$defineDir"
 
cleanDefine, keyword(depression) enddate(10jul2020)
* ------------------------------------------------------------------------------
* Load outcome data 
* ------------------------------------------------------------------------------

* Load outcome data
use "$dataDir\generic_depression_Outcomes", replace // to be update to diabetes_outcomess

* Generate weekly outcome data
weeklyOutcome, startdate(01jan2017) enddate(10jul2020) lockdown(23mar2020) study(depression) outcomegap(2)

* ------------------------------------------------------------------------------
* Prepare csv for figures
* ------------------------------------------------------------------------------

* Merge and export as .csv
clear
* Load denominator data
use "$dataDir\cr_depression_weekly_denoms.dta", replace

* Merge outcome data
merge 1:1 weekDate category stratifier using "$dataDir\cr_depression_weekly_outcomes.dta", keepusing(numOutcome) 

* Keep consistent dates between the two datasets
keep if _merge==3
drop _merge

gen proportion = numOutcome/numEligible
drop numOutcome numEligible

export delimited using "$dataDir\an_depression.csv", replace
export delimited using "Z:\Documents\COVID-Collateral\data\an_depression.csv", replace