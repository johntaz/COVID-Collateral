/*==============================================================================

DO FILE NAME:			cr_diabetes_population_aurum
AUTHOR:					Rohini Mathur		
DATE VERSION CREATED: 	22/05/2020			
DATABASE:				CPRD Aurum May 2020
DESCRIPTION OF FILE:	Syntax to extract people with diabtes from Angel Wong's aurum datsets (4 files)
DATASETS USED: 			"Z:\GPRD_GOLD\Angel\DOAC_interactions\CPRD_Aurum\Observation_Stata\Observation_append_1.dta
DATASETS CREATED:		diabetes_population_aurum
*=============================================================================*/


**STEP 1: MERGE DIABETES CODES WITH OBSERVATION FILE (CLINICAL FILE) 
**ANGEL HAS 4 OBSERVATION FILES BUT I HAVE JUST USED THE FIRST ONE IN THE INTEREST OF TIME

forvalues i=1/4 {
use "Z:\GPRD_GOLD\Angel\DOAC_interactions\CPRD_Aurum\Observation_Stata\Observation_append_`i'.dta", clear

merge m:1 medcodeid using "$pathCodelists\diabetes_codes_hf_aurum.dta", keep(match) nogen
sort patid obsdate
save "$datadir\diabetes_population_aurum_`i'", replace
}


**STEP 2 APPEND DERIVED DIABETES DATASETS AND REMOVE TRUE DUPLICATES
use "$datadir\diabetes_population_aurum_1", clear
forvalues i=2/4 {
	append using "$datadir\diabetes_population_aurum_`i'"
}

duplicates drop
codebook patid //130,813 
save "$datadir\diabetes_population_aurum", replace