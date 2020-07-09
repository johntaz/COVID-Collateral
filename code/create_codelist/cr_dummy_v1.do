/* 

An header

*/

cd my-location

search terms 

use browser

gen outcome = 1 if strmatch(terms, readterm)

save newcodelist.dta, replace
