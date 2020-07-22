/*=========================================================================

DO FILE NAME:	collat_codelist01v2_SNOMED_ETOH

AUTHOR:			Kate Mansfield	
VERSION:		v2 (originally called something different)
DATE VERSION CREATED:  		
				v2 	 2020-Jul-22 // edited to load agreed codelists
				v1 	 2020-Jul-08
				v0.2 2020-Jun-17
				v0.1 2020-May-21			
				
		
DATABASE:		CPRD Aurum SNOMED code dataset


DESCRIPTION OF FILE:	File to identify a SNOMED code list of acute alcohol-related
						codes
						1. SNOMED codes for symptoms/diagnoses
						2. Product codes for antabuse?
						
						
MORE INFORMATION:		1. Uses a list of search terms to identify codes that
							could represent acute alcohol-related things
						2. Narrows the list down and exports to excel spreadsheet
							for review
						3. Checks against Readcode lists from other projects
						4. Imports finalised code list back from excel


DATASETS USED: 	Aurum medical code browser: ${pathBrowsersAurum}/CPRDAurumMedical.dta
				Includes the following variables:
					medcodeid 		// CPRD code
					term 			// clinical term
					originalreadcode // mapped Read code? in one format
					cleansedreadcode // mapped (?) Read code in another format
					snomedctconceptid 	// SNOMED number - use as text not numbers
					snomedctdescriptionid // another SNOMED number
					release	
					
				May also use product dataset if we include antabuse codes:
				${pathBrowsersAurum}/CPRDAurumProduct.dta
				Includes the following variables:
					prodcodeid 
					dmdid 
					termfromemis 
					productname 
					formulation 
					routeofadministration 
					drugsubstancename 
					substancestrength 
					bnfchapter 
					release
				
				readcodes-AUDIT.dta // codes from AUDIT project 	
									// BMJ Open paper
									// Mansfield K, Crellin E, Denholm R, Quint 
									// JK, Smeeth L, Cook S, et al. Completeness and 
									// validity of alcohol recording in general 
									// practice within the UK: a cross-sectional study. BMJ Open. 2019;9:e031537.
									
DO FILES NEEDED:	globals.do	

DATASETS CREATED:	${pathCodelists}/collat_codelist01_SNOMED_ETOH

*=========================================================================*/


/*******************************************************************************
>> HOUSEKEEPING
*******************************************************************************/
version 16
clear all
capture log close

* create a filename global that can be used throughout the file
global filename "collat_codelist01v2_SNOMED_ETOH"

* open log file
log using "${logdir}/${filename}", text replace






/*******************************************************************************
********************************************************************************
#A. IDENTIFY CANDIDATE CODES 
	- using search terms
	- and cross referencing an existing code list
********************************************************************************
*******************************************************************************/





/*******************************************************************************
#1. DEFINE SEARCH TERMS: Create macro containing search strings
	- use MeSH headings to create search terms
	- and search in readterms using the search strings
*******************************************************************************/
* define search terms for searching clinical term variable
local searchterm " "*alcohol*" "
local searchterm "`searchterm' "*intoxication*" "
local searchterm "`searchterm' "*withdrawal*" "
local searchterm "`searchterm' "*delirium tremens*" "
local searchterm "`searchterm' "*dt*" "
local searchterm "`searchterm' "*drunken*" "
local searchterm "`searchterm' "*hangover*" "
local searchterm "`searchterm' "*acute pancreatitis*" "
local searchterm "`searchterm' "*varices*" "
local searchterm "`searchterm' "*wernicke*" "*korsakoff*" "

/*
Look at Liz's initial search terms for the alcohol code lists for the BMJ Open 
Project
*/



/*******************************************************************************
#2. Run search
*******************************************************************************/
* run program for codelist searching
run $dodir/prog_search_medcodelist

prog_search_medcodelist, medcodefile($pathBrowsersAurum/CPRDAurumMedical) searchterm(`searchterm')

order term





/*******************************************************************************
#3. Exclude irrelevant medcodes returned by the code above
	- to make codes more specific
*******************************************************************************/
* seems a reasonable list of 236 codes to consider
* however, need to drop any family history codes
local exterm " "*family history*" "*fh:*" "
local exterm " `exterm' "*leukaemia*" "*polio*" "*vaccin*" "
local exterm " `exterm' "*bacterial food-borne intoxication*" "
local exterm " `exterm' "*dislocation*" "*genital*" "*perineal*" "*vulval*" "
local exterm " `exterm' "*vaginal*" "*jump/push*" "*wool*" "*navy*" "
local exterm " `exterm' "*blood tube*" "*jobst*" "*pelvic*" "*stargardt*" "
local exterm " `exterm' "*nicotine*" "*newborn*" "*midtarsal*" "
local exterm " `exterm' "*maternal*" "*foodborne*" "*pulmonary*" "
local exterm " `exterm' "*tar distillate*" "*ramstedt*" "*oophoritis*" "
local exterm " `exterm' "*foley*" "*fetal*" "*gerhardt*" "
local exterm " `exterm' "*brandt*" "*caffeine*" "*creutzfeldt*" "*gdth iib*" "
local exterm " `exterm' "*testes*" "*testis*" "*bernhardt*" "
local exterm " `exterm' "dtic" "*dtpa *" "
local exterm " `exterm' "*cows *" "*hypertension,*" "
local exterm " `exterm' "*vioxxacute*" "*nicotinyl alcohol*" "*polyvinyl alcohol*" "
local exterm " `exterm' "*coal tar*" "*durogesic*" "*isopropyl alcohol*" "
local exterm " `exterm' "*uriplan*" "*bard dt*" "*oleyl alcohol*" "
local exterm " `exterm' "*act-hib*" "*lanolin alcohol*" "*cetostearyl alcohol*" "
local exterm " `exterm' "*cetyl alcohol*" "*stearyl alcohol*" "*psychoactive substance-induced*" "
local exterm " `exterm' "*blood glucose*" "*pepcidtwo*" "*tdt cells*" "
local exterm " `exterm' "*dichlorobenzyl alcohol*" "* dtp *" "*booster dt*" "
local exterm " `exterm' "*rhinitis*" "social withdrawal" "*benzyl alcohol*" "
local exterm " `exterm' "*sarstedt*" "*contraception*" "*edta gel tube*" "
local exterm " `exterm' "*dt (double)*" "*ndtms - problem substance*" "*opiate withdrawal*" "
local exterm " `exterm' "*new hiv*" "*adfciedtf*" "*auscdtfour*" "*auscdtone*" "*dtwing*" "
local exterm " `exterm' "*hqdtw*" "*jwdtc*" "*sub suprvn rqurmt cndtn rsdce*" "
local exterm " `exterm' "*red blood cell distribution*" "*dta hscic*" "
local exterm " `exterm' "*eolcc record*" "*child witness*" "pericardial effusion*" "
local exterm " `exterm' "*blood withdrawal*" "*bladder gonorrhea*" "*dementia with delirium*" "
local exterm " `exterm' "*wood alcohol*" "*bornyl alcohol*" "*scrotal varices*" "
local exterm " `exterm' "*ddt toxicity*" "*platelet distribution width*" " 
local exterm " `exterm' "*[x]delirium, not induced by alcohol+other psychoactive subs*" "
local exterm " `exterm' "*schmidt*" "*heredtry nephrpthy*" "*narcotic withdrawal*" "
local exterm " `exterm' "*amyl alcohol*" "
local exterm " `exterm' "*[x]oth acute+subacute resp condtns/chemical,gas,fume+vapours*" "
local exterm " `exterm' "*[x]oth iodine-deficncy relatd thyroid disordr+allied condtns*" "
local exterm " `exterm' "*fatty alcohol-nicotinamide adenine dinucleotide *" "
local exterm " `exterm' "*gasserian ganglion*" "
local exterm " `exterm' "*accidental poisoning caused by ddt*" "
local exterm " `exterm' "*phenyl*ethyl alcohol*" "*dt immunisation*" "*dt immunization*" "
local exterm " `exterm' "*schuchardt*" "*senile delirium*" "*drug induced delirium*" "
local exterm " `exterm' "*dragstedt*" "*foetal*" "*fetus*" "edta *" "*edta" "*subacute delirium*" "
local exterm " `exterm' "*foetus*" "*methyl alcohol*" "*antritis*" "
local exterm " `exterm' "*denatured alcohol*" "*rubbing alcohol*" "*non-alcoholic*" "
local exterm " `exterm' "*nonalcoholic*" "*photodynamic therapy*" "*coryza*" "
local exterm " `exterm' "*dacryocystitis*" "*deep transverse arrest*" "*dtp20003 - hgv/publ.serv.claim*" "
local exterm " `exterm' "*aortic root width*" "*nasal catarrh*" "*clostridium botulinum*" "
local exterm " `exterm' "*neonatal*" "*iritis*" "*dtap/ipv*" "ndtms*" "
local exterm " `exterm' "*phenethyl alcohol*" "*bladder gonorrhoea*" "*butyl alcohol*" "
local exterm " `exterm' "*propyl alcohol*" "*dtic-dome*" "*knodt spinal*" "

foreach word in `exterm' {
	display "*********`word'*********"
	list term if strmatch(term,"`word'")
	drop if strmatch(term,"`word'")
}



drop disease




/*******************************************************************************
#4. Tidy up and save working codelist
*******************************************************************************/
* tidy up dataset - drop unnecessary vars and add labels and notes
label data "SNOMED codes for acute alcohol use problems"
notes: $filename / TS


* save in stata format
save $codelistsdir/snomed-ETOH-working, replace








/*******************************************************************************
#5. Cross-check with code list from other sources
	Review codelists from AUDIT project
*******************************************************************************/
/*------------------------------------------------------------------------------
#5.1 load codelist from AUDIT project and merge with SNOMED codes
------------------------------------------------------------------------------*/
* match on cleansedreadcode (non matched when using originalreadcode variable)
use $codelistsdir/readcodes-AUDIT, clear
rename readcode cleansedreadcode
duplicates drop cleansedreadcode, force
merge 1:m cleansedreadcode using $pathBrowsersAurum/CPRDAurumMedical, keep(match master)

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                            36
        from master                        36  (_merge==1)
        from using                          0  (_merge==2)

    matched                               277  (_merge==3)
    -----------------------------------------

*/


* make terms comparable
gen term_lc=lower(term)
gen readterm_lc=lower(readterm)

gen term_match=1 if term_lc==readterm_lc & _merge==3
tab term_match _merge, miss // terms from AUDIT file and SNOMED file match if matched on merge

* R/V unmatched codes
list readterm if _merge==1

* keep records that are matched on cleansedreadcode
keep if _merge==3
drop _merge term_match term readterm_lc
compress
save $codelistsdir/readcodes-AUDIT-cleansedreadcodeMatch, replace





/*------------------------------------------------------------------------------
#6.2 review if possible to merge on clinical term
------------------------------------------------------------------------------*/
* there are a limited number of duplicate terms in the AUDIT dataset so need to drop them
use $codelistsdir/readcodes-AUDIT, clear 
duplicates drop readterm, force
tempfile temp
save `temp'

* merge with the readterm var in the AUDIT code list
use $pathBrowsersAurum/CPRDAurumMedical, clear
gen readterm=lower(term)
merge m:1 readterm using `temp', keep(match using)
drop if _merge==2
drop _merge
drop readterm

* merge with codes identified using cleansedreadcode
merge 1:1 medcodeid using $codelistsdir/readcodes-AUDIT-cleansedreadcodeMatch

/*

    Result                           # of obs.
    -----------------------------------------
    not matched                            24
        from master                        17  (_merge==1) // codes identified from matching terms only
        from using                          7  (_merge==2) // codes identified from matching codes only

    matched                               270  (_merge==3) // codes identified from both codes and terms
    -----------------------------------------

*/


* drop readcodes and terms from orginal AUDIT code list
* and drop vars created that are no longer needed
drop medcode readcode readterm
drop term_lc _merge

gen term_lc=lower(term)
drop term
rename term_lc term
compress
gen fromAuditList=1

order medcodeid term

* add this to existing working code list
merge 1:1 medcodeid using $codelistsdir/snomed-ETOH-working

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                           554
        from master                        78  (_merge==1) // from AUDIT project code list only
        from using                        476  (_merge==2) // from search terms in AURUM browser dataset

    matched                               216  (_merge==3) // from both AUDIT project and AURUM browser
    -----------------------------------------

*/


drop _merge
sort cleansedreadcode
compress
notes: $filename / TS
save $codelistsdir/snomed-ETOH-working, replace

* export to excel
export excel using $codelistsdir/snomed-ETOH-working, replace sheet("snomed") firstrow(variables)












/*******************************************************************************
********************************************************************************
#B. IMPORT AGREED CODELISTS
********************************************************************************
*******************************************************************************/

/*
Candidate code lists reviewed by Sinéad Langan, Sarah Cook and Kate Mansfield.
Agreed on 3 code lists:

1. Main outcome variable 
includes - clear acute events.
aims to capture acute alcohol events. 
Here we’re aiming to identify things that if increased/decreased will cause 
immediate cause for concern.

2. Sensitivity outcome variable: 
aims to capture consultations for any alcohol-related thing, with the idea that 
if someone who consults for some alcohol-related thing that isn’t an immediate 
acute event, then if they don’t consult because of the pandemic then there’s a 
missed opportunity for intervention, while that missed opportunity might lead to 
poor outcomes, it might not do so for some time).

3. Chronic alcohol-related problem – as a stratification variable: 
aiming to capture a measure of people with a propensity to use alcohol in a 
harmful way (i.e. use disorders and conditions that suggest chronic alcohol 
misuse, management of alcoholism).
*/


import excel using $codelistsdir/20200722-snomed-ETOH-consensus.xlsx, firstrow allstring clear
destring  outcomeacute outcomeany chronic, replace
drop if outcomeacute+outcomeany+chronic==0

rename outcomeacute outcome_main
label var outcome_main "main alcohol outcome variable"

rename outcomeany outcome_sens
label var outcome_sens "sensitivity alcohol outcome variable"

label var chronic "stratification variable chronic alcohol use"

* review codes included on each list
list term if outcome_main==1
list term if outcome_sens==1
list term if chronic==1


* save
compress
notes: $filename / TS
notes: snomed lists: main alcohol outcome, sens analysis outcome + chronic stratification
notes: agreed by SL, SC, and KM
save $codelistsdir/snomed-ETOH, replace





