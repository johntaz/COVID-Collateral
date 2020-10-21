cd "$graphData\"

foreach t in mi alcohol depression copd asthma diabetes {
clear
import delimited using "$graphData\an_`t'.csv"
    
foreach v in 01jan2017 07jan2018 06jan2019 05jan2020 {
local z = td(`v')
local y = year(`z')

preserve
keep if weekdate == "`v'"
sort stratifier category

keep numeligible stratifier category
order stratifier category numeligible
rename numeligible jan`y'_`t'

summ jan`y'_`t' if stratifier == "overall"
local overall = r(mean) 

gen jan`y'_`t'_percent = 100*round(jan`y'_`t'/`overall', 0.001)
*bysort stratifier: egen tot = max(round(sum(jan`y'_`t'_percent)))

replace category = category + 100 if stratifier == "gender"
replace category = category + 200 if stratifier == "region"
replace category = category + 300 if stratifier == "overall"


label define varLab 0 "White" ///
					1 "South Asian" ///
					2 "Black" ///
					3 "Other" ///
					4 "Mixed" ///
					5 "Missing" ///
					101 "Female" ///
					102 "Male" ///
					20 "11 - 20" ///
					30 "21 - 30" ///
					40 "31 - 40" ///
					50 "41 - 50" ///
					60 "51 - 60" ///
					70 "61 - 70" ///
					80 "71 - 80" ///
					90 "81 - 90" ///
					100 "91 - 100" ///
					201	"North East" ///
					202	"North West" ///
					203	"Yorkshire And The Humber" ///
					204	"East Midlands" ///
					205	"West Midlands" ///
					206	"East of England" ///
					207	"South West" ///
					208	"South Central" ///
					209	"London" ///
					210	"South East Coast" ///
					211	"Northern Ireland" ///
					212 "Region Missing" ///
					301 "Overall denominator"

label values category varLab

save "$graphData\table_`t'_`y'", replace
restore
}

}


foreach t in mi alcohol depression copd asthma diabetes {

    use "$graphData\table_`t'_2017", replace
	
	merge 1:1 stratifier category using "$graphData\table_`t'_2018" , nogen
	merge 1:1 stratifier category using "$graphData\table_`t'_2019" , nogen
	merge 1:1 stratifier category using "$graphData\table_`t'_2020" , nogen
	
	 save "$graphData\table1_`t'_pop", replace
		
   erase "$graphData\table_`t'_2017.dta"
   erase "$graphData\table_`t'_2018.dta"
   erase "$graphData\table_`t'_2019.dta"
   erase "$graphData\table_`t'_2020.dta"
}

use "$graphData\table1_depression_pop", clear
     
  foreach t in mi alcohol copd asthma diabetes {
	
	merge 1:1 stratifier category using "$graphData\table1_`t'_pop" , nogen
  }
	 save "$graphData\table1", replace
		

		
	

