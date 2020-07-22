/*=========================================================================
DO FILE NAME:			prog_search_medcodelist

AUTHOR:					Kate Mansfield		
VERSION:				v2
DATE VERSION CREATED: 	2020-Jun-17
					
DATABASE:	developed for 

uses 06/2020 version of CPRD Aurum medcode data file
	
DESCRIPTION OF FILE:	
			aim: given a list of search terms identifies matching medcodes codes
				from a specified list of medcodes

			Search terms are a list of specific conditions
			These search terms are used to identify records within the readterm 
			variable

DATASETS USED: medical.dta
			located here: J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Code browsers\2020_06
									
DATASETS CREATED: 
	creates a dataset containing records for any clinical terms matching the
	supplied search terms in any of the following var: term
	
HOW TO USE: e.g.
	prog_search_medcodelist, medcodefile($pathAKICodelists/medical) searchterm(`searchterm')
*=========================================================================*/



/*******************************************************************************
#1. Define program
*******************************************************************************/
capture program drop prog_search_medcodelist
program define prog_search_medcodelist

syntax, medcodefile(string) searchterm(string) 
	
* medcodefile			// path and name of file containing CPRD medcodes and clinical terms
* searchterm			// string containing path and name of file containing morbidty codes


/*******************************************************************************
#1. OPEN DATA AND PREP FOR SEARCH
*******************************************************************************/
use `medcodefile', clear

* Make readterm lower case
generate Z=lower(term)
drop term
rename Z term



/*******************************************************************************
#2. Label variables
*******************************************************************************/
* label variables
label variable medcodeid "internal CPRD code ID"
label variable term "clinical term"
label variable originalreadcode "Read code"
label variable cleansedreadcode "Read code"
label variable snomedctconceptid "SNOMED CT concept ID"
label variable snomedctdescriptionid "SNOMED CT description ID"
label variable release "release"
label variable emiscodecategoryid "emis code category ID"


/*******************************************************************************
#3. WORD SEARCH OF CPRD MED CODE DICTIONARY
*******************************************************************************/
* Search term variables for words in the searchterm string
generate disease=.
foreach word in `searchterm'{
	replace disease = 1 if strmatch(term, "`word'")
}

keep if disease==1


/*******************************************************************************
#4. Sort
*******************************************************************************/
sort medcode


end

