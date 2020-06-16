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
global defineDir "J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\CarreiraH20200608122823\"


/*******************************************************************************
# Look ups and browswers
*******************************************************************************/
global pathBrowsersAurum	"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Code browsers\2020_04"
global pathLookUpsAurum		"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Lookups\2019_08"



/*******************************************************************************
# Data out
*******************************************************************************/
global dataDir "$projectDir\datafiles\diabetes"
global estimateDir "$projectDir\estimatefiles" /*estimates from regression models*/
global graphDir "$projectDir\graphfiles\diabetes" /*tables and graphs exported from Stata in form used for manuscript*/
global pathCodelists "$projectDir\codelists" /*codelists*/
