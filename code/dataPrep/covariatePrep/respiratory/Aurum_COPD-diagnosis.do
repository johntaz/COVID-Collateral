/*******************************************************************************

AUTHOR:					Alicia Gayle - alicia.gayle14@imperial.ac.uk
DATE: 					24/07/2020
VERSION:				STATA 16.0
DO FILE NAME:			Aurum_COPD_diagnosis

STATUS:					

DATASETS USED*: 		PREFIX_Observation_all
						PREFIX_Patient_001

						*Assumes that text files have been downloaded, unzipped, 
						read into STATA and combined where neccessary, needs 
						PREFIX to be updated to users source file names 
						
ADO FILES NEEDED:		

DATASETS CREATED:		COPD_dx
						
						
DESCRIPTION OF FILE:	
	- reads in GP records
	- defines age at diagnosis >=40 years
	- finds ex or current smokers
	- outputs patid, copd diagnosis and date of first COPD
ISAC definition:
The population will be adults (≥40) with an established diagnosis of COPD and evidence of a smoking history. Individuals will join the study population from the latest of the start of follow-up in the overall population 1st Jan 2017 and the date of their first record indicating diagnosis of COPD.
*******************************************************************************/
clear all
set more off, perm

*******************************************************************************
** STEP 1: Identify first occuring COPD diagnosis
*******************************************************************************
use "$copdWork/copd_Observation_all", clear
rename medcode MedCodeId
merge m:1 MedCodeId using "$codelists/COPD_A.dta"
keep if _merge==3
drop _merge
duplicates drop
generate copd = 1
local today date(c(current_date),"DMY")
replace obsdate = . if obsdate > `today' | obsdate < -3652 // accepting 01-01-1950 to present observations
bysort patid copd (obsdate): gen count=_n
drop if count!=1 & copd==1
gen date_copd=obsdate if copd==1 & count==1
format date_copd %td
keep patid copd date_copd
save "$copdWork/copd_dx.dta", replace

*******************************************************************************
** STEP 2: Read in patient information - obtain age and drop if age at copd < 35 (? check with Alicia that it should be 40 as was written in the code)
*******************************************************************************
merge 1:1 patid using "$denom\202008_CPRDAurum_AcceptablePats.dta", keep(match master) keepusing(yob) // 52,466 do not match from master - maybe these people are not acceptable?
keep if _m == 3
gen age_copd = year(date_copd)-yob
drop if age_copd < 40
codebook age_copd
summ age_copd, det
keep patid copd date_copd
save "$copdWork/copd_dx.dta", replace

*******************************************************************************
** STEP 3: Smoking history - obtain smoking status
*******************************************************************************
*smoking status
use "$smokWork/smoking_Observation_all", clear
rename medcode MedCodeId
merge m:1 MedCodeId using "$codelists/smoking_A.dta"
keep if _merge==3
drop _merge
duplicates drop
bysort patid (obsdate): keep if _n == _N
keep patid smokstatus
	
*******************************************************************************
** STEP 4: Smoking history - keep only current or ex smokers
*******************************************************************************	
merge 1:1 patid using "$copdWork/copd_dx"
keep if _merge == 2 | _merge == 3 
drop _m
drop if smokstatus == ""
keep patid copd date_copd
cou // 602,887
save "$copdWork/copd_dx", replace
keep patid
save "$copdWork/copd_dx_results.dta", replace
clear

