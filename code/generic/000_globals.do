clear
capture set more off

/*******************************************************************************
# Project directories
*******************************************************************************/
global projectDir "J:\EHR-Working\Sinead_Covid_Collaterol"
global codeDir "$projectDir\code"
global logDir "$projectDir\logfiles\diabetes"

/*******************************************************************************
# Data in
*******************************************************************************/
* Denominator files
global denomDir "J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Denominator files\2020_05"

* Define files
global defineDir "J:\EHR-Working\Helena\Collateral\feasibility counts\Depression\CarreiraH20200624145146\355749.depression_Define_Inc1_Observation_001_20200624113532.txt"



/*******************************************************************************
# Look ups and browswers
*******************************************************************************/
global pathBrowsersAurum	"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Code browsers\2020_04"
global pathLookUpsAurum		"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Lookups\2019_08"



/*******************************************************************************
# Data out
*******************************************************************************/
global dataDir "$projectDir\datafiles\generic"
global estimateDir "$projectDir\estimatefiles" /*estimates from regression models*/
global graphDir "$projectDir\graphfiles\diabetes" /*tables and graphs exported from Stata in form used for manuscript*/
global pathCodelists "$projectDir\codelists" /*codelists*/
