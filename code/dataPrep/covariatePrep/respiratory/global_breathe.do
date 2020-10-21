/*******************************************************************************

AUTHOR:					Patrick Bidulka
DATE: 					17/08/2020
DO FILE NAME:			global_breathe.do

						
*******************************************************************************/
clear 
macro drop _all

global teamBreathDo		"J:\EHR-Working\Sinead_Covid_Collaterol\code\teamBreathe\covid_resp_work"
global copdRaw			"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824112027_copd_a_2"
global asthmaRaw		"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200826211438_asthma_a_2"
global copdWork			"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\copd"
global asthmaWork		"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\asthma"
global smokRaw			"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824130745_smoking"
global smokWork			"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\smoking"
global codelists		"J:\EHR-Working\Sinead_Covid_Collaterol\code\teamBreathe\covid_resp_work"
global denom			"J:\EHR Share\3 Database guidelines and info\CPRD Aurum\Denominator files\2020_08"
global ocsRaw			"Z:\sec-file-b-volumea\EPH\EHR group\GPRD_GOLD\Sinead_Covid_Collaterol\CPRD Aurum\DrugIssue"
global copdmedRaw		"Z:\sec-file-b-volumea\EPH\EHR group\GPRD_GOLD\Sinead_Covid_Collaterol\CPRD Aurum\COPD_meds"
global copdmedsRaw		"Z:\sec-file-b-volumea\EPH\EHR group\GPRD_GOLD\Sinead_Covid_Collaterol\CPRD Aurum\COPD_meds"
global aecopdRaw		"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824111933_aecopd_A"
global coughRaw			"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824113104_cough_A"
global lrtiRaw			"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824114335_lrti_A"
global sputumRaw		"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824114713_sputum"
global breathlessnessRaw	"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\CarreiraH20200824135457_breathlessness"
global steroidRaw		"J:\EHR-Working\Sinead_Covid_Collaterol\datafiles\2020_08\steroid_diseases_A_Define_Inc1_Observation_001"
cd $teamBreathDo
