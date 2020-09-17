
/* Version 1.0: 
To do: - update respiratory definition
	   - add ethnicity
	   - add vulnerable status
	   - update number of records per week/outcome gap info for various studies
	   - age group and age cleaning update
*/

capture program drop weeklyOutcome
program define weeklyOutcome

	syntax, [ startdate(string) enddate(string) lockdown(string) study(string) outcomegap(integer 100)]

   
	if "`startdate'"!= "" {
		local startdate = td(`startdate')
	}
	else {
	noi di as error "Enter a start date for study in string format"
	}
	if "`enddate'"!= "" {
		local enddate = td(`enddate')
	}
	else {
	noi di as error "Enter an end date for study in string format"
	}
	if "`lockdown'"!= "" {
		local lockdown = td(`lockdown')
	}
	else {
	noi di as error "Enter a lockdown date in string format"
	}
	if "`study'"!= "" {
		noi di ""	
		noi di as result "Study type chosen: `study' "
		noi di as result "Note: Enter 'asthma', 'copd' or 'diabetes' if specifying these study types"
		}
	else {
	noi di as error "Enter a study type"
	}

* Cleaning
noi di ""
noi di as text "***********************************************************************" 	
noi di as text "Cleaning outcome data..."


* Restrict to diabetes population 

if "`study'" == "diabetes" {

noi di ""	
noi di as text "Restricting to diabetes population..."

qui merge m:1 patid using "$denomDir\cr_`study'_strat_summary.dta", keep(match) nogen
}

* Restrict to asthma population  

if "`study'" == "asthma" {

noi di ""	
noi di as text "Restricting to asthma population..."
qui merge m:1 patid using "$denomDir\cr_`study'_strat_summary.dta", keep(match) nogen

}

* Restrict to copd population  

if "`study'" == "copd" {

noi di ""	
noi di as text "Restricting to COPD population..."
qui merge m:1 patid using "$denomDir\cr_`study'_strat_summary.dta", keep(match) nogen

}

if "`study'" == "cba" | "`study'" == "hf" | "`study'" == "mi" | "`study'" == "tia" | "`study'" == "ua" | "`study'" == "vte" {
	noi di ""	
noi di as text "Using cvd Aurum population..."
qui merge m:1 patid using "$denomDir\cr_cvd_strat_summary.dta", keep(match) nogen
}

if "`study'" == "alcohol" {
	noi di ""	
noi di as text "Using alcohol Aurum population..."
qui merge m:1 patid using "$denomDir\cr_alcohol_strat_summary.dta", keep(match) nogen
}


if "`study'" != "diabetes" & "`study'" != "asthma" & "`study'" != "copd" & "`study'" != "cba" & "`study'" != "hf" & "`study'" != "mi" & "`study'" != "tia" & "`study'" != "ua" & "`study'" != "vte" {
noi di ""	
noi di as text "Using full Aurum population..."
qui merge m:1 patid using "$denomDir\cr_overall_strat_summary.dta", keep(match) nogen
}


cap drop studyDay
cap drop week	 
cap drop dupFlag
cap drop outcomeGap
cap drop age 
cap drop agegroup
cap label drop genderLab
cap label drop ageLab
	 
* Weekly outcomes
gen studyDay = eventdate - `startdate' + 1

* Drop irrelevant events
qui drop if eventdate > `enddate'
qui drop if eventdate < `startdate'

qui summ studyDay
local maxDays = r(max)

* region
replace region =12 if region ==.
* age

gen startdate = `startdate'
gen year = year(startdate)
drop startdate
gen age = year - yob
drop year

if "`study'" == "alcohol" { 
drop if age <19 | age >100
gen agegroup = 10*ceil(age/10 )
label define ageLab 20 "10 - 20" ///
					30 "21 - 30" ///
					40 "31 - 40" ///
					 50 "41 - 50" ///
					60 "51 - 60" ///
					70 "61 - 70" ///
					80 "71 - 80" ///
					90 "81 - 90" ///
					100 "91 - 100" 
label values agegroup ageLab	
}

if "`study'" == "cba" | "`study'" == "hf" | "`study'" == "mi" | "`study'" == "tia" |"`study'" == "ua" | "`study'" == "vte" { 
drop if age <31 | age >100
gen agegroup = 10*ceil(age/10 )
label define ageLab 40 "31 - 40" /// 
					50 "41 - 50" ///
					60 "51 - 60" ///
					70 "61 - 70" ///
					80 "71 - 80" ///
					90 "81 - 90" ///
					100 "91 - 100" 
label values agegroup ageLab	
}
if "`study'" == "copd" { 
drop if age <41 | age >100
gen agegroup = 10*ceil(age/10 )
label define ageLab  50 "41 - 50" ///
					60 "51 - 60" ///
					70 "61 - 70" ///
					80 "71 - 80" ///
					90 "81 - 90" ///
					100 "91 - 100" 
label values agegroup ageLab	
}

else{
drop if age <10 | age >100
gen agegroup = 10*ceil(age/10 )
label define ageLab 20 "10 - 20" ///
					30 "21 - 30" ///
					40 "31 - 40" ///
					50 "41 - 50" ///
					60 "51 - 60" ///
					70 "61 - 70" ///
					80 "71 - 80" ///
					90 "81 - 90" ///
					100 "91 - 100" 
label values agegroup ageLab
}



* Generate number of weeks 
* If we calculate floor can later drop missing (as these are the last days which don't make up a full week)
local numWeeks = ceil(`maxDays'/7)


noi di as text "Assigning events to relevant week..."

* Assign each event to the relevant week
* Check only relevant events included
assert studyDay > 0 

qui gen week  =. 
qui forvalues i = 1/`numWeeks' { 
	
	local j = `i' - 1

if `i'==1 {
	replace week = `i' if studyDay <= 7
}
else{
* ie for wk 2: studyDay >= 7*1 + 1 = 8 & studyDay <= 14
replace week = `i' if studyDay>=(7*`j' + 1) & studyDay<= (7*`i')
}

}
noi di as text "Removing patients with multiple events on the same week..." 
* Remove patients with multiple records on the same week
bysort patid week: gen dupFlag = _n
qui keep if dupFlag ==1 
drop dupFlag

if `outcomegap' != 0 {
noi di as text "Removing patients with outcomes < `outcomegap' weeks apart..." 
noi di ""
* Remove patients with outcome during X week exclusion
gsort patid week
qui bysort patid: gen a = _n
qui bysort patid: gen outcomeGap = week[a] - week[a-1]
qui drop if outcomeGap <= `outcomegap'
drop outcomeGap a
}
qui gen numOutcome = 1


noi di as text "***********************************************************************" 
noi di as text "Generating overall weekly outcomes..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* overall
tempname denom
qui postfile `denom' weekDate numOutcome time category str15(stratifier) using "cr_`study'_outcome_overall.dta", replace
		forvalues i = 1/`numWeeks' {
		cap drop outcome
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen outcomeFlag = cond(week == `i', 1, 0)

		qui count if outcomeFlag == 1 
		local numOutcome = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numOutcome') (`k') (1) ("overall")
		}
	
postclose `denom'
noi di as text "...Completed"
noi di ""

if "`study'" == "cba" | "`study'" == "hf" | "`study'" == "mi" | "`study'" == "tia" |"`study'" == "ua" | "`study'" == "vte" {
noi di as text "***********************************************************************" 
noi di as text "Generating weekly outcomes by age..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* overall
tempname denom
qui postfile `denom' weekDate numOutcome time category str15(stratifier) using "cr_`study'_outcome_age.dta", replace
	forvalues g = 40(10)100 {
		forvalues i = 1/`numWeeks' {
		cap drop outcome
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen outcomeFlag = cond(week == `i' & agegroup == `g', 1, 0)

		qui count if outcomeFlag == 1 
		local numOutcome = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata age, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numOutcome') (`k') (`g') ("age")
		}
	}
	
postclose `denom'
noi di as text "...Completed"
noi di ""
}


else {
noi di as text "***********************************************************************" 
noi di as text "Generating weekly outcomes by age..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* overall
tempname denom
qui postfile `denom' weekDate numOutcome time category str15(stratifier) using "cr_`study'_outcome_age.dta", replace
	forvalues g = 20(10)100 {
		forvalues i = 1/`numWeeks' {
		cap drop outcome
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen outcomeFlag = cond(week == `i' & agegroup == `g', 1, 0)

		qui count if outcomeFlag == 1 
		local numOutcome = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata age, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numOutcome') (`k') (`g') ("age")
		}
	}
	
postclose `denom'
noi di as text "...Completed"
noi di ""
}

noi di as text "***********************************************************************" 
noi di as text "Generating weekly outcomes by gender..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* overall
tempname denom
qui postfile `denom' weekDate numOutcome time category str15(stratifier) using "cr_`study'_outcome_gender.dta", replace
	forvalues g = 1/2 {
		forvalues i = 1/`numWeeks' {
		cap drop outcome
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen outcomeFlag = cond(week == `i' & gender == `g', 1, 0)

		qui count if outcomeFlag == 1 
		local numOutcome = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata gender, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numOutcome') (`k') (`g') ("gender")
		}
	}
	
postclose `denom'
noi di as text "...Completed"
noi di ""

noi di as text "***********************************************************************" 
noi di as text "Generating weekly outcomes by ethnicity..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* overall
tempname denom
qui postfile `denom' weekDate numOutcome time category str15(stratifier) using "cr_`study'_outcome_ethnicity.dta", replace
	forvalues g = 0/5 {
		forvalues i = 1/`numWeeks' {
		cap drop outcome
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen outcomeFlag = cond(week == `i' & ethnicity == `g', 1, 0)

		qui count if outcomeFlag == 1 
		local numOutcome = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata ethnicity, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numOutcome') (`k') (`g') ("ethnicity")
		}
	}
	
postclose `denom'
noi di as text "...Completed"
noi di ""

noi di as text "***********************************************************************" 
noi di as text "Generating weekly outcomes by region..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
qui summ region 
local numRegions = r(max)
* overall
tempname denom
qui postfile `denom' weekDate numOutcome time category str15(stratifier) using "cr_`study'_outcome_region.dta", replace
	forvalues g = 1/`numRegions' {
		forvalues i = 1/`numWeeks' {
		cap drop outcome
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen outcomeFlag = cond(week == `i' & region == `g', 1, 0)

		qui count if outcomeFlag == 1 
		local numOutcome = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata region, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numOutcome') (`k') (`g') ("region")
		}
	}
	
postclose `denom'
noi di as text "...Completed"
noi di ""

noi di as text "***********************************************************************" 
noi di as text "Appending weekly denominator files..." 
noi di as text "***********************************************************************" 


* Append all denominators 
clear 
local denominators "overall age gender ethnicity region"
	foreach i of local denominators {
		qui append using "cr_`study'_outcome_`i'.dta"
		format weekDate %td
		qui save "cr_`study'_weekly_outcomes.dta", replace
	}
	
	foreach i of local denominators {
  	qui erase "cr_`study'_outcome_`i'.dta"
}

* Final cleaning and redaction of small cell counts
gen lockdown = cond(weekDate >= `lockdown', 1, 0) // lockdown date marker)
qui replace numOutcome = 5 if numOutcome < 5 & numOutcome > 0
qui save "cr_`study'_weekly_outcomes.dta", replace

noi di as text "...Completed"
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Output saved..." 
noi di as text "***********************************************************************" 

noi di as result "Combined weekly outcome file: "
noi di as result "cr_`study'_weekly_outcomes.dta"

end
	
	
