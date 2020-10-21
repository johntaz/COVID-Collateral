/*******************************************************************************

AUTHOR:					Alicia Gayle - alicia.gayle14@imperial.ac.uk
DATE: 					27/07/2020
VERSION:				STATA 16.0
DO FILE NAME:			Aurum_copd_exacerbations

STATUS:					

DATASETS USED*: 		PREFIX_DrugIssue_all
						PREFIX_Observation_all
						
						*Assumes that text files have been downloaded, unzipped, 
						read into STATA and combined where neccessary, needs 
						PREFIX to be updated to users source file names 
						
ADO FILES NEEDED:		

DATASETS CREATED:		aecopd_therapy
						any_abx_ocs
						abx_ocs_events
						symptoms_events
						symp_abx_ocs_events
						aecopd_events
												
DESCRIPTION OF FILE:	
						*STEP 1: Create file of OCS and COPD-specific ABX prescriptions
						*STEP 2: Find symptoms
						*STEP 3: Merge prescriptions and symptoms
						*STEP 4: Find AECOPD, label as moderate AECOPD


BASED ON:	Kieran's "find_aecod" do-file on Z:
							
*******************************************************************************/
clear all
set more off, perm

********************************************************************************
** AECOPD recorded in CPRD
********************************************************************************
/*
	AECOPD algorithm:
	
	We use strategy #2 from Table 6 of Rothnie et al. “Validation of the
	Recording of Acute Exacerbations of COPD in UK Primary Care Electronic
	Healthcare Records”, PLoS One, vol. 11, p. e0151357, 2016.

	Prescription of ABX and OCS for 5–14 days; or Symptom definition with
	prescription of antibiotic or OCS; or LRTI code; or AECOPD code
*/
********************************************************************************
*STEP 1: Create file of OCS and COPD-specific ABX prescriptions
********************************************************************************
use "$copdmedsRaw/COPD_meds_Extract_DrugIssue_all.dta", clear
	*Flag COPD inhalers
	*merge in the codes
	rename prodcodeid ProdCodeId
	merge m:1 ProdCodeId using "$codelists/COPD_Med_A.dta"
	gen ocs = 0
	replace ocs = 1 if groups == "OCS"
	drop if _m == 2
	drop _m
	*Flag COPD-specific antibiotics
	merge m:1 ProdCodeId using "$codelists/abx_codes_A.dta", force
	drop if _m == 2
	drop if _m == 1 & ocs == 0
	gen abx = 0
	replace abx = 1 if _m == 3
	*Keep only relevant codes
	keep if ocs == 1 | abx == 1
	drop _merge
	compress
save "$copdmedsRaw/aecopd_therapy.dta", replace

	*Merge in dosage information
	merge m:1 dosageid using "$codelists/common_dosages.dta"
	drop if _m == 2
	drop _m
	compress
save "$copdmedsRaw/aecopd_therapy.dta", replace

*Save file containing patids and obsdates for ABX and OCS prescriptions
preserve
	drop issuedate
	rename issuedate2 issuedate
	keep patid issuedate
save "$copdmedsRaw/any_abx_ocs.dta", replace
restore

use "$copdmedsRaw/aecopd_therapy.dta", clear
/* Find instances of both ABX and OCS prescriptions for 5-14 days
	- This is where need 'daily_dose' variable from 'common dosages' look-up */
	keep patid issuedate ocs abx quantity daily_dose duration
	codebook issuedate
	gen issuedate2 = date(issuedate,"DMY")
	drop issuedate
	rename issuedate2 issuedate
	format issuedate %td
	sort patid issuedate
	destring quantity duration, replace

	*Generate duration of script
	replace duration = quantity/daily_dose if duration == 0
	replace duration = quantity if (duration == . & quantity < 14)

	*Limit to those instances 5-14 days long
	drop if duration < 5
	drop if duration > 14

*Create temporary file of antibiotics events
preserve
	keep if abx == 1
	tempfile abx
	keep patid issuedate abx
	duplicates drop
save `abx'
restore

*Create file of antibiotic and OCS prescriptions with 5-14 days duration
keep if ocs == 1
	merge m:1 patid issuedate using `abx'
	keep if _m == 3
	drop _m

	gen abx_ocs = 1
	keep patid issuedate abx_ocs
	rename issuedate obsdate
	duplicates drop
	compress

save "$copdWork/abx_ocs_events.dta", replace
/*
	This file contains all ABX and OCS prescription events that are 5-14 days
	long for all patients in your patient list.
*/
********************************************************************************
** STEP 2: Find symptoms
********************************************************************************
use "$copdWork/copd_symptoms_Observation_all.dta", clear
rename medcode MedCodeId

*Identify cough
merge m:1 MedCodeId using "$codelists/cough_A.dta"
drop if _m == 2
gen cough = 0
replace cough = 1 if _m == 3
drop _m

*Identify sputum
merge m:1 MedCodeId using "$codelists/sputum_A.dta"
drop if _m == 2
gen sputum = 0
replace sputum = 1 if _m == 3
drop _m

*Identify breathlessness
merge m:1 MedCodeId using "$codelists/breathlessness_A.dta"
drop if _m == 2
gen dyspnoea = 0
replace dyspnoea = 1 if _m == 3
drop _m

*Keep only relevant codes
keep if cough == 1 | sputum == 1 | dyspnoea == 1

*Create temporary file of sputum events
preserve
keep if sputum == 1
keep patid obsdate sputum
duplicates drop
tempfile sputum
save `sputum'
restore

*Create temporary file of cough
preserve
keep if cough == 1
keep patid obsdate cough
duplicates drop
tempfile cough
save `cough'
restore

*Identify instances of 2/3 symptoms being coded
keep if dyspnoea == 1
keep patid obsdate dyspnoea
duplicates drop 

merge 1:1 patid obsdate using `sputum'
drop _m

merge 1:1 patid obsdate using `cough'
drop _m

gen n_symptoms = sputum + cough + dyspnoea
keep if n_symptoms == 2 | n_symptoms == 3

keep patid obsdate
duplicates drop

gen symptoms = 1

compress

save "$copdWork/symptoms_events.dta", replace

/*
	File containing all symptoms events with 2+ symptoms recorded for all
	patients in your relevant patient list
*/

********************************************************************************
** STEP 3: Merge prescriptions and sypmtoms
********************************************************************************
use "$copdmedsRaw/any_abx_ocs.dta", clear
rename issuedate obsdate
merge m:1 patid obsdate using "$copdWork/symptoms_events.dta"
gen symp_abx_ocs = 1 if _m == 3
drop _m

keep patid obsdate symp_abx_ocs
duplicates drop

compress

save "$copdWork/symp_abx_ocs_events", replace

/*
	File containing all events with both symptoms and prescriptions.
*/

********************************************************************************
** STEP 4: Find AECOPD events, label as moderate AECOPD
********************************************************************************
use "$copdWork/copd_exac_Observation_all.dta", clear
rename medcode MedCodeId
*Identify lower respiratory tract infections
merge m:1 MedCodeId using "$codelists/lrti_A.dta"
drop if _m == 2
gen aecopd = 1 if _m == 3
drop desc
drop _m

*Identify AECOPD codes
merge m:1 MedCodeId using "$codelists/aecopd_A.dta"
drop if _m == 2
replace aecopd = 1 if _m == 3
drop _m

*Identify prescriptions on same day as code
merge m:1 patid obsdate using "$copdWork/abx_ocs_events.dta"
replace aecopd = 1 if _m == 3
drop _m

*Identify symptoms + ABX and OCS on same day as code
merge m:1 patid obsdate using "$copdWork/symp_abx_ocs_events"
replace aecopd = 1 if _m == 3
drop _m

*Keep AECOPD events
keep if aecopd == 1
keep patid obsdate aecopd
duplicates drop

*Drop events within 15 days of each other
sort patid obsdate
gen datelastobs = .
by patid: replace datelastobs = obsdate[_n-1]
format datelastobs %td

gen duration = obsdate - datelastobs
drop if duration < 15
drop duration datelastobs

label define aecopd 1 "Moderate AECOPD" 2 "Severe AECOPD"

*Label all events as moderate (treated in primary care)
gen aecopd_severity = 1
label values aecopd_severity aecopd

keep patid obsdate aecopd_severity

compress

* merge with COPD diagnosis patid list to reduce to this cohort
merge m:1 patid using "$copdWork/copd_dx_results.dta"
keep if _merge == 3
codebook obsdate
cou

save "$copdWork/aecopd_events.dta", replace

/*
	File contains all moderate AECOPD events for patients on your relevant
	patient list.
*/