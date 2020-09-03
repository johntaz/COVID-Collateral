* ------------------------------------------------------------------------------
* Project: Covid-Collateral
* Program Name: 001_cr_cleanDefine
* Author: Ali Henderson
* Date Created: 22/07/2020
* Notes: Clean define outcome files but generic so it can read in any outcome
* Creates: *Outcomes
* ------------------------------------------------------------------------------
clear
*cd "$defineDir\" 
cd "J:\EHR-Working\Helena\Collateral\feasibility counts\Depression\CarreiraH20200624145146\355749.depression_Define_Inc1_Observation_001_20200624113532.txt"
local keyword "depression"






// Just change up to this point and the rest should run -- be turned into a program
* ------------------------------------------------------------------------------
* Set up log file
loc t: display  "$S_DATE " substr("$S_TIME",1,2) "h" substr("$S_TIME",4,2)
*capture log close
*log using "$logDir\001_cr_cleanDiabetesDefine_`t'", replace text

* ------------------------------------------------------------------------------
* Cleaning define outcome files
* ------------------------------------------------------------------------------

* Identify the extracts
local list : dir . files "*`keyword'*.txt"
di `list'
local num : list sizeof local(list)
di `num'

* Import extracts and clean data
foreach f of local list {
import delimited "`f'", encoding(ISO-8859-2) stringcols(_all) clear
rename eventdate date
gen eventdate = date(date, "DMY")
format eventdate %td
drop date

* missing dates
drop if eventdate == .

* Detect errors: study days beyond 2020 (transform this into a macro)
drop if eventdate > td(11Jun2020) // macro endDate
 
* drop duplicates
bysort patid eventdate : gen dupFlag = _n
keep if dupFlag == 1 
drop dupFlag

save "$dataDir\tempData`f'.dta", replace //changed from `i' by RM
di "`f'"
}

* Append all tempfiles
if `num' > 1 { 
	foreach i in 1/`num' of local list {
		append using "$dataDir\tempData`f'.dta"
		save "$dataDir\generic_`keyword'_Outcomes.dta", replace
	}
}
else if `num' == 1 {
    noi di "There is only 1 file"
	save "$dataDir\generic_`keyword'_Outcomes.dta", replace
}

* Clean up unecessary files
cd "$dataDir\"
local list : dir . files "tempData*.dta"
foreach f of local list {
    erase "`f'"
}
*
* ------------------------------------------------------------------------------
capture log close