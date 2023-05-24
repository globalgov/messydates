
*****************************************
* Clean and expand cases into TSCS data *
*****************************************

set more off
use GWFcases, clear

*Create duration and spell variables
egen ccode = group(cowcode)
egen regimecode= group(gwf_case)
gen byear = gwf_startyr				      	          
gen eyear = gwf_endyr				      	          
replace eyear =2010 if eyear==.
gen spell = (eyear-byear) + 1               	/*create variable for how long each regime lasted in power*/
expand spell								/*expands data set by making a copy of each case observations for the number of years in power*/
gen year = byear
sort regimecode year
bysort regimecode: replace year = year + _n -1    	      /*make year variable by adding the number of observations within ccode to the previous year*/
gen duration = year-(byear-1)
gen failure = 0
replace failure = 1 if year==gwf_endyr    & gwf_endyr~=.	/*create <failure> variable which is coded 1 only in the last year for leadid if failed*/
compress


*Types of regime failures*
rename failure gwf_fail
rename duration gwf_duration
rename spell gwf_spell
gen gwf_fail_type = gwf_fail  				
replace gwf_fail_type =gwf_howend if gwf_fail==1 
gen gwf_fail_subsregime = gwf_fail
replace gwf_fail_subs =gwf_subsreg if gwf_fail==1
gen gwf_fail_violent = gwf_fail
replace gwf_fail_violent = gwf_violent if gwf_fail==1

*Count of past regime failures*
gen gwf_pastregimefail=.    						/*code to calculate past regime failures within all authoritarian spells since 1946*/
local i = 1946
while `i'<2011 {
	sort cowcode year
	bysort cowcode: egen  pastfails`i'= sum(gwf_fail) if year<`i'
	bysort cowcode: gen pastregfails`i' = pastfails`i'[_n-1]
	replace gwf_pastregimefail = pastregfails`i' if year==`i'
	local i = `i' +1
}
drop past*
recode gwf_past (.=0) /*for first year country is in sample*/
label var gwf_fail "Binary variable indicating regime failure (fail==1)"
label var gwf_duration "Time at risk of failure at time t"
label var gwf_spell "Time invariant count of number of years in power"
label var gwf_case "Regime type case name"
label var gwf_regime "Regime type, including hybrids (10)"
label var gwf_fail_subs "Regime failure, transition to: 1=democracy; 2=dictatorship; 3=failed state/no exist"
label var gwf_fail_type "Regime failure, type (howend)"
label var gwf_fail_violent "Regime failure, violence"

*Collapsed regime types
gen gwf_party = gwf_regime=="party-based"  | gwf_regime=="party-military" | gwf_regime=="party-personal" /*
*/ | gwf_regime=="party-personal-military" | gwf_regime=="oligarchy" if gwf_regime~=""
replace gwf_party=1 if gwf_country=="Iran"  & year>=1980 /*This lumps Iran, which is a theocracy in with party regimes*/
gen gwf_personal = gwf_regime=="personal" 
gen gwf_monarch = gwf_regime=="monarchy" 
gen gwf_military = gwf_regime=="military"  | gwf_regime=="military-personal" | gwf_regime=="indirect military" 
label var gwf_party "Party regime (including hybrids, oligarchy, and Iran)"
label var gwf_personal "Personalist regime"
label var gwf_monarch "Monarchy"
label var gwf_military "Military regime (including milpersonal and indirect military)"

keep if year>1945
keep cowcode year   gwf_country gwf_casename gwf_startdate gwf_enddate gwf_spell gwf_duration gwf_fail gwf_fail_type gwf_fail_subsregime gwf_fail_violent gwf_regimetype gwf_party gwf_personal gwf_monarch gwf_military
order cowcode year  gwf_country gwf_casename gwf_startdate gwf_enddate gwf_spell gwf_duration gwf_fail gwf_fail_subsregime gwf_fail_type gwf_fail_violent gwf_regimetype gwf_party gwf_personal gwf_military gwf_monarch
sort cowcode year
saveold GWFtscs, replace
