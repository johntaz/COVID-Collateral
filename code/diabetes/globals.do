clear
capture set more off




/*******************************************************************************
# Project directories
*******************************************************************************/
global projectdir "J:\EHR-Working\Sinead_Covid_Collaterol"
global dodir "$projectdir\dofiles"
global logdir "$projectdir\logfiles"



/*******************************************************************************
# Data in
*******************************************************************************/
global rawdatadir "Z:\sec-file-b-volumea\EPH\EHR group\GPRD_GOLD" /*raw CPRD GOLD and linked data files - note separate folders for linked and primary care data*/



/*******************************************************************************
# Look ups and browswers
*******************************************************************************/
global pathBrowsersAurum	"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Code browsers\2020_04"
global pathLookUpsAurum		"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Lookups\2019_08"



/*******************************************************************************
# Data out
*******************************************************************************/
global datadir "$projectdir\datafiles"
global estimatedir "$projectdir\estimatefiles" /*estimates from regression models*/
global graphdir "$projectdir\graphfiles" /*tables and graphs exported from Stata in form used for manuscript*/
global pathCodelists "$projectdir\codelists" /*codelists*/
