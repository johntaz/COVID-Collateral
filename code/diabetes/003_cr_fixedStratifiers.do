* ------------------------------------------------------------------------------
* Project: Covid-Collateral
* Program Name: 002_cr_overallPopulation
* Author: John Tazare
* Date Created: 16/06/2020
* Notes: Create to invesitgate trend of diabetes recording over time in whole 
*        population by fixed strata: sex and region
* Creates: genderWeeklyDenom.dta, regionWeeklyDenom.dta,
*		 an_diabetesOverall.csv
* ------------------------------------------------------------------------------
clear

* ------------------------------------------------------------------------------
* Create weekly denominators by strata
* ------------------------------------------------------------------------------
* Denominator first
use "$denomDir\202005_CPRDAurum_AcceptablePats.dta", replace

* Data management
gen obsStart = regstartdate + 365.25
format obsStart %td

gen obsEnd = regenddate
format obsEnd %td

gen lcdDate = lcd
format lcdDate %td 

replace obsEnd = lcdDate if obsEnd ==.
replace obsEnd = lcdDate if obsEnd > td(01jul2020)

keep patid pracid gender yob mob obsStart obsEnd region

* Remove patients with obsEnd before start of study 
drop if obsEnd < td(01jan2017)

* Strata formatting
* Gender
encode gender, gen(gender1)
drop gender
rename gender1 gender
* drop gender = I 
drop if gender == 2 
replace gender = 2 if gender == 3 // 2 = M, 1 = F
label define genderLab 1 "F" 2 "M"
label values gender genderLab

* Find out how many weeks of data
gen studyDay = obsEnd - td(01jan2017) + 1
qui summ studyDay
local maxDays = r(max)
local numWeeks = ceil(`maxDays'/7)

* Check only relevant events included
assert studyDay > 0 

* Save denom summary
save "$dataDir\fixedStratifiersSummary.dta", replace

* gender
tempname denom
postfile `denom' weekDate numEligible time gender using "$dataDir\genderWeeklyDenom.dta", replace
	forvalues g = 1/2 {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = td(01Jan2017) + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & gender == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		if mod(`i', 20)==0 {
			noi di "Strata gender, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g')
		}
	}
postclose `denom'

* region
qui summ region 
local numRegions = r(max)

tempname denom
postfile `denom' weekDate numEligible time region using "$dataDir\regionWeeklyDenom.dta", replace
	forvalues g = 1/`numRegions' {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = td(01Jan2017) + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & region == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		if mod(`i', 20)==0 {
			noi di "Strata region, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g')
		}
	}
postclose `denom'

* ------------------------------------------------------------------------------
* Create weekly outcomes by strata
* ------------------------------------------------------------------------------

use "$dataDir\diabetesOutcomes", replace

merge m:1 patid using "$dataDir\fixedStratifiersSummary", keepusing(gender region)
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

save "$dataDir\weeklyUncollapsedOutcomes.dta", replace

* Collapse
preserve
collapse (count) numOutcome , by(week gender)

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

save "$dataDir\genderOutcomes.dta", replace
restore

* Collapse
preserve
* Drop missing regions
drop if region ==.
collapse (count) numOutcome , by(week region)

gen time = week - 1 
drop week 
gen weekDate = td(01Jan2017) + 7*time
format weekDate %td

* Bug: in Region 11 some weeks towards end of data collection have 0 events
insobs 1, after(1847)
replace region = 11 in 1848
replace numOutcome = 0 in 1848
replace time = 167 in 1848
replace weekDate = td(15mar2020) in 1848

insobs 1, after(1858)
replace region = 11 in 1859
replace numOutcome = 0 in 1859
replace time = 168 in 1859
replace weekDate = td(22mar2020) in 1859

insobs 1, after(1869)
replace region = 11 in 1870
replace numOutcome = 0 in 1870
replace time = 169 in 1870
replace weekDate = td(29mar2020) in 1870

insobs 1, after(1880)
replace region = 11 in 1881
replace numOutcome = 0 in 1881
replace time = 170 in 1881
replace weekDate = td(05apr2020) in 1881

insobs 1, after(1891)
replace region = 11 in 1892
replace numOutcome = 0 in 1892
replace time = 171 in 1892
replace weekDate = td(12apr2020) in 1892

insobs 1, after(1902)
replace region = 11 in 1903
replace numOutcome = 0 in 1903
replace time = 172 in 1903
replace weekDate = td(19apr2020) in 1903

insobs 1, after(1913)
replace region = 11 in 1914
replace numOutcome = 0 in 1914
replace time = 173 in 1914
replace weekDate = td(26apr2020) in 1914

* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker

* keep weeks with data for all
bysort weekDate: gen b = _n
bysort weekDate: egen consistentFlag = max(b)
drop if consistentFlag !=11
drop b consistentFlag

save "$dataDir\regionOutcomes.dta", replace
restore

* ------------------------------------------------------------------------------
* Format analysis files and export csv
* ------------------------------------------------------------------------------

use "$dataDir\genderWeeklyDenom.dta", clear
format %td weekDate
* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker)

merge 1:1 gender weekDate using "$dataDir\genderOutcomes", keepusing(numOutcome)
* keep consitent weeks
keep if _merge==3
drop _merge

export delimited using "$dataDir\an_diabetesGender", replace



use "$dataDir\regionWeeklyDenom.dta", clear
format %td weekDate

* Lockdown
gen lockdown = cond(weekDate >= td(15Mar2020), 1, 0) // lockdown date marker)

merge 1:1 region weekDate using "$dataDir\regionOutcomes", keepusing(numOutcome)
* keep consitent weeks
keep if _merge==3
drop _merge
export delimited using "$dataDir\an_diabetesRegion", replace





