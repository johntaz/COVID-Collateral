* Define ethnicity stratifier
* Author - john tazare
clear
forvalues i = 1/3 {
import delimited "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity\ethnicity_Define_Inc1_Observation_00`i'.txt", encoding(ISO-8859-2) stringcols(1 3)
merge m:1 medcode using"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\202008_medDict"
keep if _merge == 3
drop _merge
keep patid eventdate medcode term
rename medcode medcodeid

merge m:1 medcodeid using "J:\EHR Share\1 Dofiles, codelists and logfiles\3 Health behaviours\Ethnicity\Ethnicity Algorithm 1 (Ethnicity)\codelist_ethnicity_aurum.dta" 

keep if _merge ==3 
tab eth5
tab eth5, nol
drop if eth5 == 5

gen date2 = date(eventdate, "DMY")

format date2 %td
drop if date2==.
drop eventdate
rename eventdate
rename date2 eventdate


bysort patid: egen recent = max(eventdate)

format recent %td
keep if eventdate==recent

bysort patid: gen a =_n
keep if a == 1

isid patid
keep patid eventdate eth5

tab eth5
save "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity_define_`i'.dta", replace 
}


* Append all tempfiles
	forvalues i = 1/3  {
		qui append using "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity_define_`i'.dta"
		qui save "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity_define.dta", replace
	}
	

* Clean up unecessary files
forvalues i = 1/3 {
    erase "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity_define_`i'.dta"
}



use "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity_define.dta", replace
bysort patid: egen recent = max(eventdate)

format recent %td
keep if eventdate==recent

bysort patid: gen a =_n
keep if a == 1

isid patid
keep patid eth5

save "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\ethnicity_define.dta", replace


