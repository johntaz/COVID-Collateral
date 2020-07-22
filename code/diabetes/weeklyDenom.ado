capture program drop weeklyDenom
program define weeklyDenom

	syntax, [ startdate(string) enddate(string) lockdown(string) study(string)]

   
	if "`startdate'"!= "" {
		local startdate = td(`startdate')
	}
	else {
	noi di "Enter a start date for study in string format"
	}
	if "`enddate'"!= "" {
		local enddate = td(`enddate')
	}
	else {
	noi di "Enter an end date for study in string format"
	}
	if "`lockdown'"!= "" {
		local lockdown = td(`lockdown')
	}
	else {
	noi di "Enter a lockdown date in string format"
	}
	if "`study'"!= "" {
		local study = `study'
		noi di "Note: please enter 'resp' or 'diabetes' if specifying these study types"
		}
	else {
	noi di "Enter a study type"
	}
	* Cleaning
noi di ""
noi di "***********************************************************************" 	
noi di "Cleaning extract data..."
noi di "***********************************************************************" 

qui {	
	cap drop obsStart 
	cap drop obsEnd 
	cap drop lcdDate 
	cap drop studyDay
gen obsStart = regstartdate + 365.25 
format obsStart %td

gen obsEnd = regenddate
format obsEnd %td

gen lcdDate = lcd
format lcdDate %td 

replace obsEnd = lcdDate if obsEnd ==.

* Filter extreme obsEnd dates
replace obsEnd = lcdDate if obsEnd > `enddate'

keep patid pracid gender yob mob obsStart obsEnd region

* Remove patients with obsEnd before start of study 
drop if obsEnd < `startdate'

* Find out how many weeks of data
gen studyDay = obsEnd - `startdate' + 1
qui summ studyDay
local maxDays = r(max)
local numWeeks = ceil(`maxDays'/7)

* Check only relevant events included
assert studyDay > 0 
}
noi di ""
noi di "***********************************************************************" 
noi di "Generating overall weekly denominators..." 
noi di "***********************************************************************" 

tempname denom
postfile `denom' weekDate numEligible time using "`study'_overall_denom.dta", replace

* Generate weekly denominators
forvalues i = 1/`numWeeks' {
cap drop eligibleFlag
local k = `i' - 1 
local weekDate = `startdate' + 7*`k'

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

use "`save'", clear
format %td weekDate

* Lockdown
gen lockdown = cond(weekDate >= `lockdown', 1, 0) // lockdown date marker)


end
	
	