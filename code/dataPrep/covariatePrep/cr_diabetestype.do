* ------------------------------------------------------------------------------
* Project: Covid-Collateral
* Program Name: 004_cr_diabetestype
* Author: Rohini Mathur
* Date Created: 18/06/2020
* Notes: Create to identify diabetes type and date of diagnosis to define t1 and t2 populations at the start of each week
* Creates: 
* ------------------------------------------------------------------------------
clear

* ------------------------------------------------------------------------------
* Create diabetes t1 and t2 populations from define
* ------------------------------------------------------------------------------

use "$dataDir\diabetesOutcomes", clear

**merge with diabetes codelist
merge m:1 medcode using  "$pathCodelists\diabetes_codes_hf_aurum.dta", keep(match)

drop _merge medcodeid medcode

*keep codes for t1 and t2
tab diab_cat,m
keep if diab_cat==3 | diab_cat==4
compress

sort patid eventdate
duplicates drop


*drop weird dates
drop if eventdate>td(01june2020) //96 obs dropped
bysort patid diab_cat: gen count=[_n] 
bysort patid diab_cat: gen total=[_N] 
gen first_dm=eventdate if count==1
drop if first_dm==.
format first_dm %d

*reshape wide to get one column for t1 and t2_total
drop eventdate count term

reshape wide first_dm total, i(patid) j(diab_cat)
ren *3 t1_*
ren *4 t2_*
codebook patid //566,419 

gen dm_type=1 if t1_total!=. & t2_total==.
replace dm_type=2 if t2_total!=. & t1_total==.

tab dm_type, m //0.63 have conflicting types

/*
    dm_type |      Freq.     Percent        Cum.
------------+-----------------------------------
          1 |     42,752        7.55        7.55
          2 |    520,112       91.82       99.37
          . |      3,555        0.63      100.00
------------+-----------------------------------
      Total |    566,419      100.00


*/

*assign t1dm if t1_count is >= to t2dm_cou t
replace dm_type=1 if t1_total>=t2_total & dm_type==.
replace dm_type=2 if t2_total>t1_total & dm_type==.
tab dm_type,m


*get dm diagnosis date (first date recorded)
gen dm_diagnosis_date=t1_first_dm if dm_type==1
replace dm_diagnosis_date=t2_first_dm if dm_type==2
format dm_diagnosis_date %d

keep patid dm*


save "$dataDir\diabetesType", replace