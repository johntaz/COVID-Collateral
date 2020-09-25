
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
* Aurum Denominator files
global aurumDenomDir "J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Denominator files\2020_08"

global denomDir "$projectDir\datafiles\2020_08\denoms"

* Stratifier info
global ethnDataDir "$projectDir\datafiles\2020_08\ethnicity"

* Outcome specific files
global cardioDataDir "$projectDir\datafiles\2020_08\cardiovascular"
global diabDataDir "$projectDir\datafiles\2020_08\diabetes"
global mentalDataDir "$projectDir\datafiles\2020_08\mental"
global alcDataDir "$projectDir\datafiles\2020_08\alcohol"
global asthmaDataDir "$projectDir\datafiles\2020_08\asthma"
global copdDataDir "$projectDir\datafiles\2020_08\copd"

* diabetes type to added (and path updated in code)

/*******************************************************************************
# Data out
*******************************************************************************/

* Graph output 
* Stratifier info
global graphData "$projectDir\datafiles\2020_08\graphdata"