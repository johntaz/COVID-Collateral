*PATH FOR WORKING FOLDER*
local path "J:\Users\encdslan\Documents\Re-run_hz_june13\Analysis_files_june17\"

clear all
set more off, perm
set mem 15g
cd "`path'"
capture log close

log using pyrs, replace

* All of the UK 
forvalues years = 2000/2011 {
    
insheet using "`path'acceptablepats_from_utspracts_JUN2011.txt", clear


	gen long pracid=mod(patid, 1000)
	sort pracid
	save "path'acceptable_patients.dta", replace
	
	insheet using "`path'allpractices_JUN2011.txt", clear
	merge 1:m pracid using "`path'acceptable_patients.dta"
	tab _merge
	keep if _merge==3
	gen crd2=date(crd, "DMY")
	gen utsdate=date(uts, "DMY")
	gen todate2=date(tod, "DMY")
	gen lcddate=date(lcd, "DMY")
	gen deathdate2=date(deathdate, "DMY")
	drop crd tod deathdate uts
	ren todate2 todate
	ren deathdate2 deathdate
	ren crd2 crd
	gen start = max(crd,utsdate)
    gen enddate = min(todate,lcddate,deathdate)
	format start enddate %dD/N/CY
    drop if start > enddate
    keep if gender==1 | gender==2
    local start_date = date("01/01/"+string(`years'), "DMY")
    local end_date = date("31/12/"+string(`years'), "DMY")
    gen ystart = max(`start_date', start)
    gen yend = min(`end_date', enddate)
    gen diff = (yend - ystart) + 1 
    keep if diff > 0
    if _N>0 {
    gen age = `years' - yob
    keep if age >=18 & age <=115
    collapse (sum) diff (count) patid, by(age gender region)
    }
    gen year = `years'
    if `years' > 2000 {
      append using "persontime.dta"
    }
    save "persontime.dta", replace
}
keep gender age diff patid year region
save "denominator_2000_2011.dta", replace

**calculate person years , taking leap years into account**
gen leap = (mod(year,4)==0 & mod(year,100)!=0) | mod(year,400)==0
gen persontime = diff/365 if leap==0
replace persontime = diff/366 if leap==1
assert !mi(persontime)
rename patid number
order year
sort year age gender 
save "denominator.dta", replace
keep year gender age number persontime region
outsheet using "denominator.txt", replace noquote

capture log close

