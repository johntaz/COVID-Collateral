adopath + "Z:\Desktop\COVID-Collateral\code\diabetes\000_globalsDiabetes.do"
use "$denomDir\202005_CPRDAurum_AcceptablePats.dta", replace

cd "$dataDir"

weeklyDenom, startdate(01jan2017) enddate(10jul2020) lockdown(23mar2020) save(overallDenom)