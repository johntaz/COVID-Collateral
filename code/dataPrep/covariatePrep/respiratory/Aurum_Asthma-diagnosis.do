/*******************************************************************************

AUTHOR:					Alicia Gayle - alicia.gayle14@imperial.ac.uk
DATE: 					07/08/2020
VERSION:				STATA 16.0
DO FILE NAME:			Aurum_Asthma_diagnosis

STATUS:					

DATASETS USED*: 		PREFIX_Observation_all
						PREFIX_Patient
						
						*Assumes that text files have been downloaded, unzipped, 
						read into STATA and combined where neccessary, needs 
						PREFIX to be updated to users source file names 
						
ADO FILES NEEDED:		

DATASETS CREATED:		asthma_dx
						
						
DESCRIPTION OF FILE:	
	- reads in GP records
	- outputs patid, asthma diagnosis and date of first asthma

	ISAC Definition:
- The study population will be all individuals (age 5+) with a current asthma diagnosis (i.e. asthma code in the last two or three years if <18 years or 18+ years, respectively). Individuals will join the study population from the start of follow-up in the overall population if there is a current asthma diagnosis at this time or from the date of their first record indicating an asthma diagnosis within overall follow-up.
- individuals 40 years and over with asthma will be considered as likely to have COPD (and therefore not included in the asthma study population [denominator]) if they have a subsequent COPD diagnosis recorded within the two years following the current asthma record. 
*******************************************************************************/
clear all
set more off, perm


*******************************************************************************
** STEP 1: Identify all Asthma diagnosis
*******************************************************************************
use "$asthmaRaw/asthma_a_Define_Inc1_Observation_all.dta", clear
*remove inplausible dates
local today date(c(current_date),"DMY")
display `today'
drop if obsdate > `today' | obsdate < -3652 // accepting 01-01-1950 to present observations
* drop if obsdate > mdy(01,01,2050)
rename medcode medcodeid
*find asthma
merge m:1 medcodeid using "$codelists/Asthma_A.dta"
keep if _merge==3
drop _merge
duplicates drop	
	
*******************************************************************************
** STEP 2: Read in patient information - obtain age and drop if age <5 at start of study
*******************************************************************************
merge m:1 patid using "$denom\202008_CPRDAurum_AcceptablePats.dta", keep(match master) keepusing(yob)
keep if _m == 3
drop _merge
gen startstudy = mdy(01,01,2017)
gen age_startstudy = year(startstudy) - yob
gen age_asthma = year(obsdate)-yob
	
*Drop the under 5 year olds
drop if age_startstudy < 5
	
*Drop the non current asthma diagnosis
drop if (year(startstudy) - year(obsdate) >2) & age_startstudy < 18
drop if (year(startstudy) - year(obsdate) >3) & age_startstudy >= 18	

*Keep the first occuring asthma diagnosis	
generate asthma = 1
sort patid asthma obsdate
bysort patid asthma: gen count=_n
drop if count!=1 & asthma==1
gen date_asthma=obsdate if asthma==1 & count==1
format date_asthma %td
keep patid asthma date_asthma age_asthma age_startstudy
save "$asthmaWork/asthma_dx.dta", replace
clear

*****************************************************************************
****** STEP 3: Drop people who had first asthma then 2  years later COPD
*****************************************************************************
/* 
PB: Just going to use the COPD pop identified in other do file

use ${data}PREFIX_Observation_all
rename medcodeid MedCodeId
merge m:1 MedCodeId using "$mycodelists/COPD_A.dta"
keep if _merge==3
drop _merge
duplicates drop
generate copd = 1
sort patid copd obsdate
bysort patid copd: gen count=_n
drop if count!=1 & copd==1
gen date_copd=obsdate if copd==1 & count==1
format date_copd %td
keep patid copd date_copd
*/
		
use "$copdWork/copd_dx.dta", clear
*merge in the asthma patient details
merge 1:1 patid using "$asthmaWork/asthma_dx.dta"
drop if _m == 1
* Drop Over 40's who had COPD 2 within 2 years of asthma
drop if age_startstudy >= 40 & (date_copd-date_asthma) < 730
keep patid asthma date_asthma
cou // 1,395,527
save "$asthmaWork/asthma_dx.dta", replace
clear

use "$asthmaWork/asthma_dx.dta", clear
keep patid
save "$asthmaWork/asthma_dx_results.dta", replace