/*******************************************************************************

AUTHOR:					Alicia Gayle - alicia.gayle14@imperial.ac.uk
DATE: 					27/07/2020
VERSION:				STATA 16.0
DO FILE NAME:			Aurum_Asthma_exacerbations

STATUS:					

DATASETS USED*: 		PREFIX_DrugIssue_all
						PREFIX_Observation_all
						
						*Assumes that text files have been downloaded, unzipped, 
						read into STATA and combined where neccessary, needs 
						PREFIX to be updated to users source file names 
						
ADO FILES NEEDED:		

DATASETS CREATED:		asthma_exacerbations
						asthma_review
						steroidpatients
						
						
DESCRIPTION OF FILE:	
* Substance strength and quantity combined for dosage information
* OCS list has been anotated with mg for each prodcode so qty multiply by OCS mg gives total prescription dose 
* A threshold of 300mg is used in this dofile but a slightly higher level upto 450mg could be used (sensitivity analysis in Bloom et al, Thorax showed little difference)
* There can be misclassification including patients treated for other diseases but analysis shows in patients under 55 years only a small proportion have other steroid-treatable diseases
* but can also exclude patient with steroid disease medcodes on the first day of OCS
* there can also be misclassification as low doses of steroids may be used for non-asthma 
* This has not been validated yet but appears good correlation where ndd is known with patients aged under 55 years old
* Dont include on annual review days as may be prescribed for self-treatment at home
				
*******************************************************************************/
clear all
set trace on

*******************************************************************************
** STEP 1: Identify OCS events and doses
*******************************************************************************
use "$ocsRaw/OCS_Asth_extract_DrugIssue_all.dta", clear
*merge in the codes
rename prodcodeid ProdCodeId
	merge m:1 ProdCodeId using "$codelists/OCS_codes.dta"
	keep if _merge==3
	drop _m

*convert substancestrength to a numeric from string (mg)
gen ocsnum=substr(substancestrength,1,4)
gen ocsmg = real(ocsnum)
destring quantity duration, replace
drop ocsnum
gen dose= abs(ocsmg*quantity)
*replacing missing information where it occurs
replace dose = (ocsmg*duration) if (quantity == 0 & duration >0)
replace dose = . if dose == 0
gen dose_cat = dose

* dose choosen at 300mg (Jenni and Paul Cullinan), BTS says 40-50mg for 5 days or until resolved
*** change to 150 if Under 5s*************
recode dose_cat 0/300=0 300/max=1
rename issuedate OCSdate
* assume lowest dose is weaning protcol so keep highest dose if duplicates on same day
* also removes spare courses (could be bad as spare taken later and not recorded as exacerbation or good as not over counting)
duplicates drop patid OCSdate dose_cat, force
* now only left with duplicates of different doses on same day, want to keep highest dose (lower is weaning)
gsort patid OCSdate -dose
by patid OCSdate: keep if _n==1
drop if dose_cat==1
keep patid OCSdate dose_cat
gen OCSdate2 = date(OCSdate,"DMY")
drop OCSdate 
rename OCSdate2 OCSdate
format OCSdate %td
* OPTIONAL: remove OCS prescriptions not within study period etc
save "$ocsRaw/asthma_exacerbations", replace
*******************************************************************************
** STEP 2: Merge with Annual review dates (keep if not matched by date)
*******************************************************************************
use "$asthmaRaw/asthma_a_Define_Inc1_Observation_all.dta", clear
keep if medcode=="1488421017"
keep patid obsdate 
rename obsdate matcheventdate
save "$asthmaRaw/asthma_review.dta", replace

use "$ocsRaw/asthma_exacerbations", clear
gen  matcheventdate = OCSdate
format %td matcheventdate 
* merge with OCS doses and only keep master data (all matched removed)
merge 1:m patid matcheventdate using "$asthmaRaw/asthma_review.dta", nogen keep(master)
drop matcheventdate
rename OCSdate exac_date
save "$ocsRaw/asthma_exacerbations", replace

*******************************************************************************
** STEP 3: merge with dates of medcodes for other steroid treated diseases (makes little difference if <55 years)
*******************************************************************************
clear
use "$asthmaWork/steroid_Observation_all.dta", clear
merge m:1 medcodeid using "$codelists/steroid_diseases_A.dta", keep(match) nogenerate
keep patid obsdate
duplicates drop
gen steroid=1
rename obsdate exac_date
save "$asthmaWork/steroidpatients.dta", replace
clear
use "$ocsRaw/asthma_exacerbations"
merge 1:m patid exac_date using "$asthmaWork/steroidpatients.dta", nogen keep(master) keepusing(patid)
save "$asthmaWork/asthma_exacerbations.dta", replace
