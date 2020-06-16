* ------------------------------------------------------------------------------
* Project: Covid-Collateral
* Program Name: 002_cr_overallPopulation
* Author: John Tazare
* Date Created: 16/06/2020
* Notes: Create to invesitgate trend of diabetes recording over time in whole 
*        population
* Creates: diabetesOverallOutcomes.dta, diabetesOverallDenom.dta,
*		 an_diabetesOverall.csv
* ------------------------------------------------------------------------------
clear

* ------------------------------------------------------------------------------
* Set up log file
loc t: display  "$S_DATE " substr("$S_TIME",1,2) "h" substr("$S_TIME",4,2)
capture log close
log using "$logDir\002_cr_overallPopulation_`t'", replace text

* ------------------------------------------------------------------------------
* Create weekly outcomes
* ------------------------------------------------------------------------------
use "$dataDir\diabetesOutcomes", replace

* Weekly outcomes
gen studyDay = eventdate - td(01jan2017) + 1

* Detect errors: study days beyond 2020
drop if eventdate > td(11Jun2020)

qui summ studyDay
local maxDays = r(max)

* Generate number of weeks 
* If we calculate floor can later drop missing (as these are the last days which don't make up a full week)
local numWeeks = ceil(`maxDays'/7)
di `numWeeks'

* Assign each event to the relevant week
* Check only relevant events included
assert studyDay > 0 

gen week  =. 
qui forvalues i = 1/`numWeeks' { 
	
	local j = `i' - 1

if `i'==1 {
	replace week = `i' if studyDay <= 7
}
* ie for wk 2: studyDay >= 7*1 + 1 = 8 & studyDay <= 14
replace week = `i' if studyDay>=(7*`j' + 1) & studyDay<= (7*`i')

if mod(`i', 20)==0 {
	noi di "Week `i' out of `numWeeks'"
}
}

* Remove patients with multiple records on the same week
bysort patid week: gen dupFlag = _n
keep if dupFlag ==1 
drop dupFlag

* Remove patients with outcome during X week exclusion
gsort patid week
bysort patid: gen a = _n
bysort patid: gen outcomeGap = week[a] - week[a-1]
drop if outcomeGap <= 2

gen numOutcome = 1

* Collapse
collapse (count) numOutcome , by(week)

gen time = week - 1 
drop week 
gen weekDate = td(01Jan2017) + 7*time
format weekDate %td

* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker

save "$dataDir\diabetesOverallOutcomes.dta", replace

* ------------------------------------------------------------------------------
* Create weekly denominators
* ------------------------------------------------------------------------------

* Denominator Files
use "$denomDir\202005_CPRDAurum_AcceptablePats.dta", replace

* Cleaning
gen obsStart = regstartdate + 365.25 
format obsStart %td

gen obsEnd = regenddate
format obsEnd %td

gen lcdDate = lcd
format lcdDate %td 

*assert obsEnd <= lcdDate if obsEnd!=.
replace obsEnd = lcdDate if obsEnd ==.
* br obsEnd lcdDate if obsEnd!=. & obsEnd > lcdDate // extreme obsEnd dates
* Filter extreme obsEnd dates
replace obsEnd = lcdDate if obsEnd > td(01jul2020)

keep patid pracid gender yob mob obsStart obsEnd region

* Remove patients with obsEnd before start of study 
drop if obsEnd < td(01jan2017)

* Find out how many weeks of data
gen studyDay = obsEnd - td(01jan2017) + 1
qui summ studyDay
local maxDays = r(max)
local numWeeks = ceil(`maxDays'/7)
di `numWeeks'

* Check only relevant events included
assert studyDay > 0 

tempname denom
postfile `denom' weekDate numEligible time using "$dataDir\diabetesOverallDenom.dta", replace

* Generate weekly denominators
forvalues i = 1/`numWeeks' {
cap drop eligibleFlag
local k = `i' - 1 
local weekDate = td(01Jan2017) + 7*`k'

* Identify eligible patients 
gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' , 1, 0)

qui count if eligibleFlag == 1
local numEligible = r(N)

if mod(`i', 20)==0 {
	noi di "Week `i' out of `numWeeks'"
	
	}	
	post `denom' (`weekDate') (`numEligible') (`k')
}

postclose `denom'

use "$dataDir\diabetesOverallDenom.dta", clear
format %td weekDate

* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker)

merge 1:1 weekDate using "$dataDir\diabetesOverallOutcomes", keepusing(numOutcome)

* Keep consistent dates between the two datasets
keep if _merge==3
drop _merge
export delimited using "$dataDir\an_diabetesOverall", replace

* ------------------------------------------------------------------------------
capture log close
