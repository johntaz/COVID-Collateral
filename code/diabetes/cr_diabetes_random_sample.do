/*==============================================================================

DO FILE NAME:			cr_diabetes_random_sample
AUTHOR:					John Tazare
DATE VERSION CREATED: 	28/05/2020			
DATABASE:				CPRD Aurum May 2020
DESCRIPTION OF FILE:	Syntax to extract sample from Rohini Mathurs's 
						Aurum diabetes population file for debugging code
DATASETS USED: 			diabetes_population_aurum
DATASETS CREATED:		diabetes_random_patid
						diabetes_random_popn 			
*=============================================================================*/

local sampleSize 1000

use "$datadir\diabetes_population_aurum", replace

* Identify a random sample of `sampleSize' patients
preserve
bysort patid: gen flag = _n
keep if flag == 1
set seed 20200528
sample `sampleSize' , count
keep patid
gen randomFlag = 1 
save "$datadir\diabetes_random_patid", replace
restore

* Merge random patient list to full population and restrict 
merge m:1 patid using "$datadir\diabetes_random_patid", nogen
keep if randomFlag==1 
drop randomFlag

* Save the random population
save "$datadir\diabetes_random_popn", replace

