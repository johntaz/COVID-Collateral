
/* Version 1.0: 
To do: - update respiratory definition
	   - add ethnicity
	   - add vulnerable status
	   - add both the above to strat summary
	   - age group and age cleaning update
	   - diabetes type needs to be update to load type data 1 line per patient: patid type dx_date
*/

capture program drop weeklyDenom
program define weeklyDenom

	syntax, [ startdate(string) enddate(string) lockdown(string) study(string)]

   
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
		noi di as result "Note: Enter 'resp' or 'diabetes' if specifying these study types"
		}
	else {
	noi di as error "Enter a study type"
	}

* Cleaning
noi di ""
noi di as text "***********************************************************************" 	
noi di as text "Cleaning denominator data..."


* Restrict to diabetes population 

if "`study'" == "diabetes" {

noi di ""	
noi di as text "Restricting to diabetes denominator population..."
noi di as text "***********************************************************************" 

qui merge 1:1 patid using "diabetesType.dta", keep(match) nogen keepusing(patid) // update to include type
}

* Restrict to respiratory population  // TO BE ADDED

if "`study'" == "resp" {

noi di ""	
noi di as text "Restricting to respiratory data..."
noi di as text "***********************************************************************" 

qui merge 1:1 patid using "diabetesType.dta", keep(match) nogen

}


if "`study'" != "diabetes" & "`study'" != "resp" {
noi di ""	
noi di as text "Using full Aurum denominator population..."
noi di as text "***********************************************************************" 
}

qui {	
	cap drop obsStart 
	cap drop obsEnd 
	cap drop lcdDate 
	cap drop studyDay
	cap label drop genderLab
	cap label drop ageLab

		local startdate = td(01jan2017)
		local enddate = td(20aug2020)
		
gen regstartdate2 = date(regstartdate, "DMY")
drop regstartdate
rename regstartdate2 regstartdate
format regstartdate %td

gen regenddate2 = date(regenddate, "DMY")
drop regenddate
rename regenddate2 regenddate
format regenddate %td

gen lcd2 = date(lcd, "DMY")
drop lcd
rename lcd2 lcd
format lcd %td

format lcd %td

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

* age

gen startdate = `startdate'
gen year = year(startdate)
drop startdate
gen age = year - yob
drop year

*clean values
drop if age < 10 | age > 100 // Update this!
gen agegroup = 10*ceil(age/10 )
label define ageLab 10 "0 - 10" ///
					20 "11 - 20" ///
					30 "21 - 30" ///
					40 "31 - 40" ///
					50 "41 - 50" ///
					60 "51 - 60" ///
					70 "61 - 70" ///
					80 "71 - 80" ///
					90 "81 - 90" ///
					100 "91 - 100" 
label values agegroup ageLab

* Find out how many weeks of data
gen studyDay = obsEnd - `startdate' + 1
qui summ studyDay
local maxDays = r(max)
local numWeeks = ceil(`maxDays'/7)

* Check only relevant events included
assert studyDay > 0 

* merge ethnicity 
merge 1:1 patid using "$ethnDataDir\ethnicity_define.dta" 
drop if _merge==2
replace eth5 = 5 if _merge==1 
rename eth5 eth6
drop _merge
}

noi di ""
noi di as text "***********************************************************************" 
noi di as text "Generating overall weekly denominators..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
tempname denom
qui postfile `denom' weekDate numEligible time category str15(stratifier) using "cr_`study'_denom_overall.dta", replace

* Generate weekly denominators
forvalues i = 1/`numWeeks' {
cap drop eligibleFlag
local k = `i' - 1 
local weekDate = `startdate' + 7*`k'

* Identify eligible patients 
gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' , 1, 0)

qui count if eligibleFlag == 1
local numEligible = r(N)

if mod(`i', 100)==0 {
	noi di as text  "Week `i' out of `numWeeks'"
	
	}	
	post `denom' (`weekDate') (`numEligible') (`k') (1) ("overall")
}

postclose `denom'
noi di as text "...Completed"
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Generating weekly denominators by age..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* gender
tempname denom
qui postfile `denom' weekDate numEligible time category str15(stratifier) using "cr_`study'_denom_age.dta", replace
	forvalues g = 10(10)100 {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & agegroup == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		* Update age each year
		* 2018
qui {
		if td(01jan2018) >= `weekDate' & td(01jan2018) <= `weekDate' + 7 {
			replace age = age + 1 
			replace agegroup = 10*ceil(age/10)
		}
		* 2019
			if td(01jan2019) >= `weekDate' & td(01jan2019) <= `weekDate' + 7 {
			replace age = age + 1 
			replace agegroup = 10*ceil(age/10)
		}
		* 2020
			if td(01jan2020) >= `weekDate' & td(01jan2020) <= `weekDate' + 7 {
			replace age = age + 1 
			replace agegroup = 10*ceil(age/10)
		}
}
		if mod(`i', 100)==0 {
			noi di as text "Strata age, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g') ("age")
		}
	* Correct age for next loop
		qui replace age = age - 3
	}
postclose `denom'
noi di as text "...Completed"
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Generating weekly denominators by gender..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* gender
tempname denom
qui postfile `denom' weekDate numEligible time category str15(stratifier) using "cr_`study'_denom_gender.dta", replace
	forvalues g = 1/2 {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & gender == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata gender, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g') ("gender")
		}
	}
postclose `denom'
noi di as text "...Completed"
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Generating weekly denominators by ethnicity..." 
noi di as text "***********************************************************************" 
noi di as text "Progress..."
* ethnicity
tempname denom
qui postfile `denom' weekDate numEligible time category str15(stratifier) using "cr_`study'_denom_ethnicity.dta", replace
	forvalues g = 0/5 {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & ethnicity == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata ethnicity, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g') ("ethnicity")
		}
	}
postclose `denom'
noi di as text "...Completed"
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Generating weekly denominators by region..." 
noi di as text "***********************************************************************" 

* region
qui summ region 
local numRegions = r(max)
noi di as text "Progress..."
tempname denom
qui postfile `denom' weekDate numEligible time category str15(stratifier) using "cr_`study'_denom_region.dta", replace
	forvalues g = 1/`numRegions' {
		forvalues i = 1/`numWeeks' {
		cap drop eligibleFlag
		local k = `i' - 1 
		local weekDate = `startdate' + 7*`k'

		* Identify eligible patients 
		gen eligibleFlag = cond(obsStart <= `weekDate' & obsEnd >= `weekDate' & region == `g', 1, 0)

		qui count if eligibleFlag == 1 
		local numEligible = r(N)

		if mod(`i', 100)==0 {
			noi di as text "Strata region, level `g' : Week `i' out of `numWeeks'"
	
			}	
	post `denom' (`weekDate') (`numEligible') (`k') (`g') ("region")
		}
	}
postclose `denom'
noi di as text "...Completed"
noi di ""

* Save summary
keep patid pracid gender yob region gender ethnicity
save "cr_`study'_strat_summary.dta", replace
*****

noi di as text "***********************************************************************" 
noi di as text "Appending weekly denominator files..." 
noi di as text "***********************************************************************" 


* Append all denominators 
clear 
local denominators "overall age gender ethnicity region"
	foreach i of local denominators {
		qui append using "cr_`study'_denom_`i'.dta"
		format weekDate %td
		qui save "cr_`study'_weekly_denoms.dta", replace
	}
	
	foreach i of local denominators {
  	qui erase "cr_`study'_denom_`i'.dta"
}

* Final cleaning and redaction of small cell counts
gen lockdown = cond(weekDate >= `lockdown', 1, 0) // lockdown date marker)
qui replace numEligible = 5 if numEligible < 5 & numEligible > 0
qui save "cr_`study'_weekly_denoms.dta", replace

noi di as text "...Completed"
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Output saved..." 
noi di as text "***********************************************************************" 
noi di as result "Summary stratifier file: "
noi di as result "cr_`study'_strat_summary.dta"
noi di ""
noi di as result "Combined weekly denominator file: "
noi di as result "cr_`study'_weekly_denoms.dta"

end
	
	
