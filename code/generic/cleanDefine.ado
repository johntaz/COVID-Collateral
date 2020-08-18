/* Version 1.0: */

capture program drop cleanDefine
program define cleanDefine

	syntax, [ keyword(string) enddate(string)]

	if "`enddate'"!= "" {
		local enddate = td(`enddate')
	}
* ------------------------------------------------------------------------------
* Cleaning define outcome files
* ------------------------------------------------------------------------------
* Cleaning
noi di ""
noi di as text "***********************************************************************" 	
noi di as text "Identifying extracts..."
noi di ""

* Identify the extracts
local list : dir . files "*`keyword'*.txt"

local num : list sizeof local(list)
noi di as text "Found `num' extract files" 
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Import and clean data" 	

local i = 1
* Import extracts and clean data
foreach f of local list {
qui import delimited "`f'", encoding(ISO-8859-2) stringcols(_all) clear
rename eventdate date
qui gen eventdate = date(date, "DMY")
format eventdate %td
drop date

* missing dates
qui drop if eventdate == .

* Detect errors: study days beyond 2020 (transform this into a macro)
qui drop if eventdate > `enddate' 
 
* drop duplicates
qui bysort patid eventdate : gen dupFlag = _n
qui keep if dupFlag == 1 
qui drop dupFlag

qui save "$dataDir\tempData`i'.dta", replace 
local i = `i' + 1

}
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Append individual outcome files " 	
noi di as text ""
* Append all tempfiles
if `num' > 1 { 
	forvalues i = 1/`num'  {
		qui append using "$dataDir\tempData`i'.dta"
		qui save "$dataDir\cr_`keyword'_outcomes.dta", replace
	}
}
else if `num' == 1 {
    noi di "There is only 1 file"
	qui save "$dataDir\cr_`keyword'_outcomes.dta", replace
}

* Clean up unecessary files
cd "$dataDir\"
forvalues i = 1/`num' {
    erase "tempData`i'.dta"
}

noi di as text "...Completed " 
noi di ""
noi di as text "***********************************************************************" 
noi di as text "Save output " 	
noi di as text ""
noi di as result "Summary outcome file: "
noi di as result "$dataDir\cr_`keyword'_outcomes.dta"
*
* ------------------------------------------------------------------------------
end