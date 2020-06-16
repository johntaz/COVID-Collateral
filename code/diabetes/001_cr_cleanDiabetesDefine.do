* ------------------------------------------------------------------------------
* Project: Covid-Collateral
* Program Name: 001_cr_cleanDiabetesDefine
* Author: John Tazare
* Date Created: 16/06/2020
* Notes: Clean define outcome files
* Creates: diabetesOutcomes
* ------------------------------------------------------------------------------
clear
cd "$defineDir\" 

* ------------------------------------------------------------------------------
* Set up log file
loc t: display  "$S_DATE " substr("$S_TIME",1,2) "h" substr("$S_TIME",4,2)
capture log close
log using "$logDir\001_cr_cleanDiabetesDefine_`t'", replace text

* ------------------------------------------------------------------------------
* Cleaning define outcome files
* ------------------------------------------------------------------------------

* Identify the extracts
local list : dir . files "diabetes*.txt"
di `list'

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

save "$dataDir\tempData`i'", replace
}

* Append all tempfiles
forvalues i = 1/4 {
    append using "$dataDir\tempData`i'"
    save "$dataDir\diabetesOutcomes", replace
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