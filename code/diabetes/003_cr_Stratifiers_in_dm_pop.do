* ------------------------------------------------------------------------------
* Project: Covid-Collateral
* Program Name: 003_cr_dmtypeStratifiers
* Author: Rohini Mathur based on code by John Tazare
* Date Created: 18/06/2020
* Notes: Create to invesitgate trend of diabetes recording over time in whole 
*        population by diabetes type
* Creates: t1WeeklyDenom.dta, t2WeeklyDenom.dta,
*		
* ------------------------------------------------------------------------------
clear

* ------------------------------------------------------------------------------
* Create weekly denominators by strata
* ------------------------------------------------------------------------------
* Denominator first
use "$denomDir\202005_CPRDAurum_AcceptablePats.dta", replace
merge 1:1 patid using "$dataDir\diabetesType.dta", keep(match) nogen

* Data management
gen obsStart =max((regstartdate + 365.25), dm_diagnosis_date)
format obsStart %td

gen obsEnd = regenddate
format obsEnd %td

gen lcdDate = lcd
format lcdDate %td 

replace obsEnd = lcdDate if obsEnd ==.
replace obsEnd = lcdDate if obsEnd > td(01jul2020)

keep patid pracid gender yob mob obsStart obsEnd region dm_type

* Remove patients with obsEnd before start of study 
drop if obsEnd < td(01jan2017)

* Find out how many weeks of data
gen studyDay = obsEnd - td(01jan2017) + 1
qui summ studyDay
local maxDays = r(max)
local numWeeks = ceil(`maxDays'/7)

* Check only relevant events included
assert studyDay > 0 

* Save denom summary
save "$dataDir\dmtypeStratifiersSummary.dta", replace


* diabetes type
tempname denom
postfile `denom' weekDate numEligible time dm_type using "$dataDir\dmtypeWeeklyDenom.dta", replace
	forvalues g = 1/2 {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = td(01Jan2017) + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & dm_type == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		if mod(`i', 20)==0 {
			noi di "Strata dmtype, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g')
		}
	}
postclose `denom'


* ------------------------------------------------------------------------------
* Create weekly outcomes by strata
* ------------------------------------------------------------------------------

use "$dataDir\diabetesOutcomes", replace

merge m:1 patid using "$dataDir\dmtypeStratifiersSummary", keepusing(dm_type)
*check merge
keep if _merge==3
drop _merge

* Weekly outcomes
gen studyDay = eventdate - td(01jan2017) + 1

* Detect errors: study days beyond 2020 (transform this into a macro)
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
drop outcomeGap a

gen numOutcome = 1

save "$dataDir\weeklyUncollapsedOutcomes_dmtype.dta", replace

* Collapse
preserve
collapse (count) numOutcome , by(week dm_type)

gen time = week - 1 
drop week 
gen weekDate = td(01Jan2017) + 7*time
format weekDate %td

* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker

* keep weeks with data for both
bysort weekDate: gen b = _n
bysort weekDate: egen consistentFlag = max(b)
drop if consistentFlag !=2 
drop b consistentFlag

save "$dataDir\dmtypeOutcomes.dta", replace

* ------------------------------------------------------------------------------
* Format analysis files and export csv
* ------------------------------------------------------------------------------

use "$dataDir\dmtypeWeeklyDenom.dta", clear
format %td weekDate
* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker)

merge 1:1 dm_type weekDate using "$dataDir\dmtypeOutcomes", keepusing(numOutcome)
* keep consitent weeks
keep if _merge==3
drop _merge

export delimited using "$dataDir\an_diabetesdmtype", replace





