/**************************************************************************
Covid Collateral
***************************************************************************/

/***************************************************************************
CODE LISTS        
***************************************************************************/

/*example 
do "$dodir\cr_codelist_aurum_respiratory_asthma.do"
where "respiratory" is the team and "asthma" is the specific code list. The team would be "general" for code lists used for all analyses

The code list will then be saved as "$codelistdir\cr_codelist_aurum_respiratory_asthma.dta"
*/


/***************************************************************************
CREATE STUDY DATASETS
***************************************************************************/
do "$dodir\cr_diabetes_population_aurum.do"


/***************************************************************************
CONDUCT ANALYSES ***************************************************************************/
