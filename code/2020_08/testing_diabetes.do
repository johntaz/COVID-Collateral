use "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\diabetes\cr_diabetes_outcomes_FULL.dta" , clear
keep if eventdate >= td(01jan2017)
gen year = year(eventdate)
tab year
keep if year >= 2017


gen studyDay = eventdate - td(01jan2017) + 1
summ studyDay 
local numWeeks = ceil(r(max)/7)

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
*

*save "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\diabetes\ali_dodgy_temp_investigations_obviouslyDeleteThisFile.dta", replace
use "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\diabetes\ali_dodgy_temp_investigations_obviouslyDeleteThisFile.dta", replace

gen n=1
collapse (count) n, by(medcode week)

gsort week -n


merge m:1 medcode using "J:\EHR-Working\Sinead_Covid_Collaterol\codelists\aurum_codelist_diabetes"
drop _merge
merge m:1 medcode using "J:\EHR-Working\Sinead_Covid_Collaterol\codelists\aurum_codelist_diabemergency_JT_Testing"

gsort week -n

twoway line  n week if medcode == "2622193012" , name(keto, replace)
cap drop plotWeek
gen plotWeek = td(01jan2017) + (week*7)
format plotWeek %td

preserve
gen keto=.
replace keto = 1 if strmatch(term, "*keto*")
keep if keto == 1
tab _merge
twoway line  n  plotWeek, by(term) name(p1, replace)
restore

preserve 
collapse (sum) n, by(plotWeek _merge)
twoway line n plotWeek if plotWeek > td(01sep2019), by(_merge, rescale) name(p2, replace)
restore


preserve
keep if plotWeek == td(05apr2020) | plotWeek == td(26jan2020)
rename _merge emergency
keep plotWeek medcode term emergency n
reshape wide n , i(medcode emergency) j(plotWeek)
cap drop rel_change abs_rel_change
gen rel_change = (n22010-n21940)/n21940
gen abs_rel_change = abs(rel_change)
gsort -abs_rel_change
drop if abs_rel_change < 0.9 
drop if abs_rel_change == .
l emergency term rel_change
restore

preserve 
collapse (sum) n, by(term)
gsort -n
l term n in 1/20
restore

preserve 
gen group26 = 0
replace group26=1 if substr(medcode,1,2)=="26" & length(medcode) == 10
collapse (sum) n, by(plotWeek group26)
twoway line n plotWeek if plotWeek > td(01sep2019), by(group26, rescale) name(p3, replace)
restore


preserve
gen ali_group1 = ""
replace ali_group1 = "mellitus" if strmatch(term, "*mellitus*")
gen ali_group2 = ""
replace ali_group2 = "keto" if strmatch(term, "*ketoacidosis*")
gen ali_group3 = ""
replace ali_group3 = "hyper" if strmatch(term, "*hyperglyc*")
gen ali_group4 = ""
replace ali_group4 = "hypo" if strmatch(term, "*hypoglyc*")
gen ali_group5 = ""
replace ali_group5 = "coma" if strmatch(term, "*coma*")

gen ali_group = ali_group1 + ali_group2 + ali_group3 + ali_group4 + ali_group5
collapse (sum) n, by(plotWeek ali_group)
twoway line n plotWeek if plotWeek > td(01dec2019), by(ali_group, rescale) name(p4, replace)
restore

preserve 
gen new_emergencies = . 
replace new_emergencies = 1 if strmatch(term, "*ketoacidosis*")
replace new_emergencies = 1 if strmatch(term, "*coma*")
replace new_emergencies = 1 if strmatch(term, "*hyperglyc*")
replace new_emergencies = 1 if strmatch(term, "*hypoglyc*")
replace new_emergencies = . if term == "Non-diabetic hyperglycaemia"
collapse (sum) n, by(plotWeek new_emergencies)
twoway line n plotWeek if plotWeek > td(01dec2019), by(new_emergencies, rescale) name(p5, replace)
restore

preserve 
gen new_emergencies = . 
replace new_emergencies = 1 if strmatch(term, "*ketoacidosis*")
replace new_emergencies = 1 if strmatch(term, "*coma*")
replace new_emergencies = 1 if strmatch(term, "*hyperglyc*")
replace new_emergencies = 1 if strmatch(term, "*hypoglyc*")

collapse (sum) n, by(plotWeek new_emergencies)
twoway line n plotWeek if plotWeek > td(01dec2019), by(new_emergencies, rescale) name(p6, replace)
restore
