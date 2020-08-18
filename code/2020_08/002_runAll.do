* 

* ------------------------------------------------------------------------------
* Add extra adopath 
* ------------------------------------------------------------------------------
* update to where repo is cloned
adopath + "Z:\Desktop\COVID-Collateral\code\2020_08"

* ------------------------------------------------------------------------------
* Run Globals
* ------------------------------------------------------------------------------
* update to where repo is cloned
do "Z:\Desktop\COVID-Collateral\code\generic\000_globals.do"


* ------------------------------------------------------------------------------
* Generate weekly denominators
* ------------------------------------------------------------------------------
cd "$denomDir"

* diabetes
use "$aurumDenomDir\202008_CPRDAurum_AcceptablePats.dta", replace
weeklyDenom, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(diabetes)

* overall 
use "$aurumDenomDir\202008_CPRDAurum_AcceptablePats.dta", replace
weeklyDenom, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(overall)

* ------------------------------------------------------------------------------
* Clean outcome data
* ------------------------------------------------------------------------------
clear 

* Cardiovascular
cd "$cardioDataDir"
local outcomes "cba hf mi tia ua vte"
foreach o of local outcomes {
cleanDefine, keyword(`o') enddate(10aug2020)
}

* Diabetes
cd "$diabDataDir"
cleanDefine, keyword(diabetes) enddate(10aug2020)

* Mental Health
cd "$mentalDataDir"
local outcomes "anxiety depression feedingdisorders ocd selfharm smi"
foreach o of local outcomes {
cleanDefine, keyword(`o') enddate(10aug2020)
}

* Alcohol
cd "$alcDataDir"
cleanDefine, keyword(alcohol) enddate(10aug2020)

* ------------------------------------------------------------------------------
* Generate weekly outcomes
* ------------------------------------------------------------------------------
* outcomegap = 0 excludes outcomes on the same week only 

* Cardiovascular
cd "$cardioDataDir"
use "cr_cba_outcomes", replace
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(cba) outcomegap(52)

use "cr_ua_outcomes", replace
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(ua) outcomegap(26)

use "cr_hf_outcomes", replace
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(hf) outcomegap(0)

use "cr_mi_outcomes", replace
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(mi) outcomegap(52)

use "cr_tia_outcomes", replace
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(tia) outcomegap(26)

use "cr_vte_outcomes", replace
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(vte) outcomegap(52)

* Diabetes
cd "$diabDataDir"
use "cr_diabetes_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(diabetes) outcomegap(0)

* Mental Health
cd "$mentalDataDir"
local outcomes "anxiety depression feedingdisorders ocd selfharm smi"
use "cr_anxiety_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(anxiety) outcomegap(0)

use "cr_depression_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(depression) outcomegap(0)

use "cr_feedingdisorders_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(feedingdisorders) outcomegap(0)

use "cr_ocd_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(ocd) outcomegap(0)

use "cr_selfharm_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(selfharm) outcomegap(0)

use "cr_smi_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(smi) outcomegap(0)

* Alcohol
cd "$alcDataDir"
use "cr_alcohol_outcomes"
weeklyOutcome, startdate(01jan2017) enddate(10aug2020) lockdown(23mar2020) study(alcohol) outcomegap(2)


* ------------------------------------------------------------------------------
* Prepare csv for figures
* ------------------------------------------------------------------------------

/* Merge and export as .csv
clear
* Load denominator data
use "$dataDir\cr_diabetes_weekly_denoms.dta", replace

* Merge outcome data
merge 1:1 weekDate category stratifier using "$dataDir\cr_diabetes_weekly_outcomes.dta", keepusing(numOutcome) 

* Keep consistent dates between the two datasets
keep if _merge==3
drop _merge

export delimited using "$dataDir\an_diabetes.csv", replace*/


