
*******************************************************************
* Subject: Empirical Political Economy
* Authors: Philipp, Niccolo, Ramzi, Marton & Adam
* Project: "Hit or Miss"
*******************************************************************

*******************
* Preamble
*******************

clear

* load data 
use "C:\Users\Philipp Hilmbauer\Desktop\PhD - Studium @CEU\PhD - 2nd Trimester - Winter 2021\Empirical Political Economy\REPLICATION - exercise with the guys\hit-miss NEW data and code\mergeddata.dta"

* for other users, use...
*use "C:\Users\...\mergeddata.dta" 


*load necessary packages

*ssc install estudy, replace
*ssc install eventstudy, replace
*ssc install eventstudy2, replace
*help estudy

********************************************************************************

******************
* OLD CODE
******************

version 9.2
clear
set mem 300m
set more off
capture  log close
log using MakeTablesAndFigures, t replace

local fixedeffectvars = "weapondum* numserdum* " 		


****************************
* Build variables
****************************

*** Get data
use mergeddata, clear
keep if year >= 1875

* Make tenure variable
g clock = ceil(sumten / 365)

*** Check if there are multiple attempts/successes per given country-year
g event_marker = 0
replace event_marker = 1 if result !=.
sort cowcode year
by cowcode year: egen count_events = sum(event_marker)
by cowcode year: egen num_seriousevents = sum(numseriousattemptsleaderyear )
sum count_events 

g death = 0
replace death = 1 if result>=11 & result<=19
by cowcode year: egen death_event = max(death)

g seriouswoundedtemp = 0
replace seriouswoundedtemp = 1 if result >= 11 & result <= 22
by cowcode year: egen seriouswounded_event = max(seriouswoundedtemp)

g woundedbystandertemp = 0 
replace woundedbystandertemp = 1 if result >= 11 & result <= 23
replace woundedbystandertemp = 1 if woundedinattempt > 0 & woundedinattempt != .
by cowcode year: egen woundedbystander_event = max(woundedbystandertemp)

g woundedtemp = 0
replace woundedtemp = 1 if result >= 11 & result <= 23
by cowcode year: egen wounded_event = max(woundedtemp)

g seriousattempttemp = 0
replace seriousattempttemp = 1 if result >= 11 & result <= 29
by cowcode year: egen seriousattempt_event = max(seriousattempttemp)

sort cowcode year obsid
by cowcode: g tempoutyear = 1 if obsid[_n] != obsid[_n+1]
g naturaldeathtemp = 1 if exit == 2 & tempoutyear == 1 & clock >= 0 & clock != .
by cowcode year: egen naturaldeath_event = max(naturaldeathtemp)
replace event_marker = 1 if naturaldeathtemp == 1

*** Define a year by the worst outcome:  if there was a successful assassination, then this is a treatment
g attempt = 0
replace attempt = 1 if count_events>=1
g seriouswounded = 0
replace seriouswounded = 1 if count_events>=1 & seriouswounded_event == 1
g woundedbystander = 0
replace woundedbystander = 1 if count_events>=1 & woundedbystander_event == 1
g wounded = 0
replace wounded = 1 if count_events>=1 & wounded_event == 1
g seriousattempt = 0
replace seriousattempt = 1 if count_events>=1 & seriousattempt_event == 1
g success = 0
replace success = 1 if count_events>=1 & death_event==1
g naturaldeath = 0
replace naturaldeath = 1 if naturaldeath_event == 1 


* Figure out how many leader changes per year

sort cowcode year obsid
by cowcode: g leadchange = (obsid != obsid[_n-1]) & obsid != "" & obsid[_n-1] != ""
by cowcode  year: egen numleadchange = sum(leadchange) 


* Figure out how many regular / irregular exits
g outdate = date(enddate,"dmy")
g out_yr = year(outdate)

g exitany = 1 if year == out_yr & out_yr != .
g exitregular = (exitany & exit == 1 ) & exitany != . & exit != .
g exitnaturaldeath = (exitany & (exit == 2 | exit == 2.1)) & exitany != . & exit != .
g exitirregular = (exitany & (exit == 3 | exit == 2.2 | exit == 4)) & exitany != . & exit != .

for any any regular naturaldeath irregular: by cowcode year: egen numexitX = sum(exitX)

* Put into country year format, with markers for assassinations and attempts
gsort +cowcode +year -event_marker +obsid
by cowcode year: keep if _n == 1


*** Mark years between successive attempts / successes
by cowcode:  g yr1 = year if count_events>=1
by cowcode:  replace yr1 = year if _n==1
by cowcode:  g yr2 = year if count_events>=1
by cowcode:  replace yr2 = year if _n==_N

by cowcode:  replace yr1 = yr1[_n-1] if yr1[_n-1]!=. & yr1==.
gsort cowcode -year
by cowcode: replace yr2 = yr2[_n-1] if yr2[_n-1]!=. & yr2==. 

sort cowcode year
g distance = yr2 - yr1
replace distance = distance[_n-1] if count_events>=1
replace distance = distance[_n+1] if count_events>=1 & distance[_n+1] < distance[_n-1]

*** create varialbe so that we can drop events other than the first attempt on each leader as robustness
capture drop temp*
g temp1 = 1 if result != .
sort cowcode obsid year
by cowcode obsid: g temp2 = sum(temp1) if result != .
g firstattempt = (temp2 == 1) if temp2 != .
drop temp*



* Compute percent leader change variables
encode cowcode, g(countrynum)
tsset countrynum year

foreach Y of var numleadchange numexit*  {
	for num 1 5 10 20: g `Y'X1 = f.`Y'
	for num 2/5: replace `Y'51 = fX.`Y' + `Y'51 
	for num 2/10: replace `Y'101 = fX.`Y' + `Y'101
	for num 2/20: replace `Y'201 = fX.`Y' + `Y'201 
	
	g `Y'2010 = `Y'201 - `Y'101

	***********
	* Make another version excluding the current leader
	***********
	for num 1 5 10 20: g `Y'NCX1 = f.`Y' if f.year > out_yr
	for num 1 5 10 20: replace `Y'NCX1 = 0 if f.year <= out_yr
	
	for num 2/5: replace `Y'NC51 = fX.`Y' + `Y'NC51  if fX.year > out_yr
	for num 2/10: replace `Y'NC101 = fX.`Y' + `Y'NC101 if fX.year > out_yr
	for num 2/20: replace `Y'NC201 = fX.`Y' + `Y'NC201 if fX.year > out_yr
	
	g `Y'NC2010 = `Y'NC201 - `Y'NC101

}
g numexitanylast5 = l.numexitany + l1.numexitany + l2.numexitany + l3.numexitany + l4.numexitany + l5.numexitany

for any regular irregular: g perexitX11 = numexitX11 / (numexitregular11 + numexitirregular11)
for any regular irregular: g perexitX51 = numexitX51 / (numexitregular51 + numexitirregular51)
for any regular irregular: g perexitX101 = numexitX101 / (numexitregular101 + numexitirregular101)
for any regular irregular: g perexitX201 = numexitX201 / (numexitregular201 + numexitirregular201)
for any regular irregular: g perexitX2010 = numexitX2010 / (numexitregular2010 + numexitirregular2010)


for any regular irregular: g perexitXNC11 = numexitXNC11 / (numexitregularNC11 + numexitirregularNC11)
for any regular irregular: g perexitXNC51 = numexitXNC51 / (numexitregularNC51 + numexitirregularNC51)
for any regular irregular: g perexitXNC101 = numexitXNC101 / (numexitregularNC101 + numexitirregularNC101)
for any regular irregular: g perexitXNC201 = numexitXNC201 / (numexitregularNC201 + numexitirregularNC201)
for any regular irregular: g perexitXNC2010 = numexitXNC2010 / (numexitregularNC2010 + numexitirregularNC2010)

* gen normed vars
g npolity2 = (polity2 + 10) / 20 if polity2 >= -10
g npolity2dummy = (npolity2 > .5) if npolity2 != .

tsset
foreach X of var npolity2 npolity2dummy {
	
	g `X'11 = f.`X' - l.`X'
	g `X'101 = f10.`X' - l.`X'
	g `X'201 = f20.`X' - l.`X'
	
	for var `X'*1 : g absX = abs(X)
	
}
replace polity = . if polity < -10

g lautoc = 1 if l.polity2 <= 0 
replace lautoc = 0 if l.polity2 > 0 & l.polity2 != .
g ldemoc = 1-lautoc
for var success attempt wounded seriousattempt : g Xlautoc = X * lautoc
for var success attempt wounded seriousattempt : g Xldemoc = X * ldemoc

label var successlautoc "Success * Lag Autoc"
label var successldemoc  "Success * Lag Democ"
label var lautoc "Lag Autoc"
label var success "Success"



* Set up the fixed effects variables

* Quarter century dummies
g qtrcenturytemp = floor(year / 25)
replace qtrcenturytemp = 1975/25 if year >= 2000
tab qtrcenturytemp, g(qtrcentury)
*omit first category
drop qtrcentury1
drop qtrcenturytemp 

replace weapon1 = 9 if weapon1 == . 
*** since 9 is 'unknown'
tab weapon1, g(weapondum)
*omit first category
drop weapondum1

g regdumAfrica = (region == 1) if region != .
g regdumAsia = (region == 2 | region == 3 | region == 4 | region == 5) if region != .
g regdumMENA = (region == 6 | region == 11) if region != .
g regdumLatAm = (region == 7 | region == 8) if region != .
g regdumEEur = (region == 9) if region != .
*g regdumWEurUSA = (region == 10) if region != .


* dummy out num serious attempts
g numserdum2 = (num_seriousevents == 2)
g numserdum3 = (num_seriousevents== 3)
g numserdum4 = (num_seriousevents == 4)

* Set up war variables
g prioanywar = 1 if prioconflictextrastate == 1 | prioconflictinterstate == 1 | prioconflictinternal == 1 | prioconflictintinternal == 1
replace prioanywar = 2 if prioconflictextrastate == 2 | prioconflictinterstate == 2 | prioconflictinternal == 2 | prioconflictintinternal == 2
replace prioanywar = 0 if prioconflictextrastate == 0 & prioconflictinterstate == 0 & prioconflictinternal == 0 & prioconflictintinternal == 0

for var prioanywar : replace X = X / 2 
*** so it's on a 0-1 scale

g zGledAnywar = (zGledCivil == 1 | zGledInter == 1 | zGledExt == 1) if zGledInter != . & zGledCivil != . & zGledExt != .

* Make a set of prio and gleditschvariables defined over the same years
g sampleP = 1
for var prioanywar zGledAnywar: replace sampleP = 0 if X == .
for var zGledAnywar prioanywar : g XP = X if sampleP == 1

* Tenure variables
g lclock = l.clock
g durgroup = 1 if lclock <= 10
replace durgroup = 2 if lclock > 10 & lclock != .

save country_year_data, replace


********************************************************************************


***
*** benchmark from the paper
***


**************************************
*** Table 6: Tenure of Leader and Duration Effects
**************************************

* Column 1: All serious attempts

tab npolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4 if seriousattempt == 1  , cluster(cowcode)
*maketablerank using table_6, rhs(success) varcol(All) replace 	pval noastr rankpval(`pvalnonparm')


********************************************************************************
********************************************************************************


*** 			--- IDEAS ARIEDA ---

***
*** eventstudy idea code
***

*preserve original data
*preserve

* prepare data 
*remove the duplicates around the ten year window for eventstudy
replace success=0 if success[_n-1]==1 & cowcode[_n-1]==cowcode
replace success=0 if success[_n-2]==1 & cowcode[_n-2]==cowcode
replace success=0 if success[_n-3]==1 & cowcode[_n-3]==cowcode
replace success=0 if success[_n-4]==1 & cowcode[_n-4]==cowcode
replace success=0 if success[_n-5]==1 & cowcode[_n-5]==cowcode
replace success=0 if success[_n-6]==1 & cowcode[_n-6]==cowcode
replace success=0 if success[_n-7]==1 & cowcode[_n-7]==cowcode
replace success=0 if success[_n-8]==1 & cowcode[_n-8]==cowcode
replace success=0 if success[_n-9]==1 & cowcode[_n-9]==cowcode
replace success=0 if success[_n-10]==1 & cowcode[_n-10]==cowcode

*condition on seriousattempt
gen panel_attempt=1 if seriousattempt[_n+10]==1 & cowcode[_n+10]==cowcode
replace panel_attempt=1 if seriousattempt[_n+9]==1 & cowcode[_n+9]==cowcode
replace panel_attempt=1 if seriousattempt[_n+8]==1 & cowcode[_n+8]==cowcode
replace panel_attempt=1 if seriousattempt[_n+7]==1 & cowcode[_n+7]==cowcode
replace panel_attempt=1 if seriousattempt[_n+6]==1 & cowcode[_n+6]==cowcode
replace panel_attempt=1 if seriousattempt[_n+5]==1 & cowcode[_n+5]==cowcode
replace panel_attempt=1 if seriousattempt[_n+4]==1 & cowcode[_n+4]==cowcode
replace panel_attempt=1 if seriousattempt[_n+3]==1 & cowcode[_n+3]==cowcode
replace panel_attempt=1 if seriousattempt[_n+2]==1 & cowcode[_n+2]==cowcode
replace panel_attempt=1 if seriousattempt[_n+1]==1 & cowcode[_n+1]==cowcode
replace panel_attempt=1 if seriousattempt==1 
replace panel_attempt=1 if seriousattempt[_n-1]==1 & cowcode[_n-1]==cowcode
replace panel_attempt=1 if seriousattempt[_n-2]==1 & cowcode[_n-2]==cowcode
replace panel_attempt=1 if seriousattempt[_n-3]==1 & cowcode[_n-3]==cowcode
replace panel_attempt=1 if seriousattempt[_n-4]==1 & cowcode[_n-4]==cowcode
replace panel_attempt=1 if seriousattempt[_n-5]==1 & cowcode[_n-5]==cowcode
replace panel_attempt=1 if seriousattempt[_n-6]==1 & cowcode[_n-6]==cowcode
replace panel_attempt=1 if seriousattempt[_n-7]==1 & cowcode[_n-7]==cowcode
replace panel_attempt=1 if seriousattempt[_n-8]==1 & cowcode[_n-8]==cowcode
replace panel_attempt=1 if seriousattempt[_n-9]==1 & cowcode[_n-9]==cowcode
replace panel_attempt=1 if seriousattempt[_n-10]==1 & cowcode[_n-10]==cowcode

***
* condition on attempts
***

keep if panel_attempt==1 


***

*for later diagnostics
*create the success indicator variable
gen panel_success=-5 if success[_n+5]==1 & cowcode[_n+5]==cowcode
replace panel_success=-4 if success[_n+4]==1 & cowcode[_n+4]==cowcode
replace panel_success=-3 if success[_n+3]==1 & cowcode[_n+3]==cowcode
replace panel_success=-2 if success[_n+2]==1 & cowcode[_n+2]==cowcode
replace panel_success=-1 if success[_n+1]==1 & cowcode[_n+1]==cowcode
replace panel_success=0 if success==1 
replace panel_success=1 if success[_n-1]==1 & cowcode[_n-1]==cowcode
replace panel_success=2 if success[_n-2]==1 & cowcode[_n-2]==cowcode
replace panel_success=3 if success[_n-3]==1 & cowcode[_n-3]==cowcode
replace panel_success=4 if success[_n-4]==1 & cowcode[_n-4]==cowcode
replace panel_success=5 if success[_n-5]==1 & cowcode[_n-5]==cowcode

*shift to pos. values
replace panel_success=panel_success+6
replace panel_success=0 if panel_success==.


*create the success dummies like discussed
gen success_m5=0
replace success_m5=1 if success[_n+5]==1 & cowcode[_n+5]==cowcode
gen success_m4=0
replace success_m4=1 if success[_n+4]==1 & cowcode[_n+4]==cowcode
gen success_m3=0
replace success_m3=1 if success[_n+3]==1 & cowcode[_n+3]==cowcode
gen success_m2=0
replace success_m2=1 if success[_n+2]==1 & cowcode[_n+2]==cowcode
gen success_m1=0
replace success_m1=1 if success[_n+1]==1 & cowcode[_n+1]==cowcode

gen success_p0=0
replace success_p0=1 if success==1 

gen success_p1=0
replace success_p1=1 if success[_n-1]==1 & cowcode[_n-1]==cowcode
gen success_p2=0
replace success_p2=1 if success[_n-2]==1 & cowcode[_n-2]==cowcode
gen success_p3=0
replace success_p3=1 if success[_n-3]==1 & cowcode[_n-3]==cowcode
gen success_p4=0
replace success_p4=1 if success[_n-4]==1 & cowcode[_n-4]==cowcode
gen success_p5=0
replace success_p5=1 if success[_n-5]==1 & cowcode[_n-5]==cowcode


***
*NEW CODE WITH EVENTSTUDY INTERACT PACKAGE

*INSTALL PACKAGES
*ssc install eventstudyinteract, all replace
*ssc install avar
*ssc install reghdfe
*ssc install ftools

*gen our outcome of interest
gen difpol2= polity2 - l.polity2 if cowcode[_n-1]==cowcode
gen chgpol2= difpol2
replace chgpol2= 0 if difpol2 ==.
replace chgpol2= 1 if chgpol2 >=1
replace chgpol2= -1 if chgpol2 <=-1 


*code the cohort categorical variable based on when the individual first joined the union, which will be
*inputted in cohort(varname).
gen success_year = year if success == 1
bysort cowcode: egen first_success = min(success_year)
*drop success_year
 
*code the relative time categorical variable.
gen relative_year = year - first_success

*for the first example, we take the control cohort to be individuals that never unionized.
gen never_success = (first_success == .)

*we will consider the dynamic effect of union status on income.  We first generate these relative time
*indicators, and leave out the distant leads due to few observations.  Implicitly this assumes that effects
*outside the lead windows are zero.
 
 forvalues k = 10(-1)2 {
 gen g_`k' = relative_year == -`k'
 }
 forvalues k = 0/10 {
 gen g`k' = relative_year == `k'
 }
        
*we use the IW estimator to estimate the dynamic effect on log wage associated with each relative time.
*with many leads and lags, we need a large matrix size to hold intermediate estimates.


********************************************************************************

***
* Panel A with Event Study
*
* Directional change in POLITY2 dummy
***

* use 5 leads and 5 lags as before (now with covariates)

*** All serious attempts
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*** Tenure <= 10
preserve
keep if durgroup == 1
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


*** Tenure > 10
preserve
sort country year
replace durgroup=durgroup[_n-1] if country==country[_n-1] & durgroup[_n-1]==2
replace durgroup=durgroup[_n+1] if country==country[_n+1] & durgroup[_n+1]==2
replace durgroup=durgroup[_n+2] if country==country[_n+2] & durgroup[_n+2]==2
replace durgroup=durgroup[_n+3] if country==country[_n+3] & durgroup[_n+3]==2
replace durgroup=durgroup[_n+4] if country==country[_n+4] & durgroup[_n+4]==2
replace durgroup=durgroup[_n+5] if country==country[_n+5] & durgroup[_n+5]==2

keep if durgroup == 2
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore

***
* AUTOCRATS
***

*** All serious attempts on autocrats
preserve 
keep if lautoc == 1
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore

*** Autocrat with tenure <= 10
preserve 
keep if lautoc == 1 & durgroup == 1
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


*** Autocrats with tenure > 10
preserve 
sort country year
replace durgroup=durgroup[_n-1] if country==country[_n-1] & durgroup[_n-1]==2
replace durgroup=durgroup[_n+1] if country==country[_n+1] & durgroup[_n+1]==2
replace durgroup=durgroup[_n+2] if country==country[_n+2] & durgroup[_n+2]==2
replace durgroup=durgroup[_n+3] if country==country[_n+3] & durgroup[_n+3]==2
replace durgroup=durgroup[_n+4] if country==country[_n+4] & durgroup[_n+4]==2
replace durgroup=durgroup[_n+5] if country==country[_n+5] & durgroup[_n+5]==2

keep if lautoc == 1 & durgroup == 2
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


***
* Panel B with Event Study Design
*
* Percentage of transitions by "regular" means
***

*** All serious attempts
eventstudyinteract exitregular g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*** Tenure <= 10
preserve
keep if durgroup == 1
eventstudyinteract exitregular g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


*** Tenure > 10
preserve
sort country year
replace durgroup=durgroup[_n-1] if country==country[_n-1] & durgroup[_n-1]==2
replace durgroup=durgroup[_n+1] if country==country[_n+1] & durgroup[_n+1]==2
replace durgroup=durgroup[_n+2] if country==country[_n+2] & durgroup[_n+2]==2
replace durgroup=durgroup[_n+3] if country==country[_n+3] & durgroup[_n+3]==2
replace durgroup=durgroup[_n+4] if country==country[_n+4] & durgroup[_n+4]==2
replace durgroup=durgroup[_n+5] if country==country[_n+5] & durgroup[_n+5]==2

keep if durgroup == 2
eventstudyinteract exitregular g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


***
* AUTOCRATS
***

*** All serious attempts on autocrats
preserve 
keep if lautoc == 1
eventstudyinteract exitregular g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


*** Autocrat with tenure <= 10
preserve 
keep if lautoc == 1 & durgroup == 1
eventstudyinteract exitregular g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


*** Autocrats with tenure > 10
preserve 
replace durgroup=durgroup[_n-1] if country==country[_n-1] & durgroup[_n-1]==2
replace durgroup=durgroup[_n+1] if country==country[_n+1] & durgroup[_n+1]==2
replace durgroup=durgroup[_n+2] if country==country[_n+2] & durgroup[_n+2]==2
replace durgroup=durgroup[_n+3] if country==country[_n+3] & durgroup[_n+3]==2
replace durgroup=durgroup[_n+4] if country==country[_n+4] & durgroup[_n+4]==2
replace durgroup=durgroup[_n+5] if country==country[_n+5] & durgroup[_n+5]==2

keep if lautoc == 1 & durgroup == 2
eventstudyinteract exitregular g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


********************************************************************************
***
*** ROBUSTNESS (Part I.1)
***

* use 5 leads and 5 lags as before (now with covariates)
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*only leads
eventstudyinteract difpol2 g0-g10, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*every other obs.
eventstudyinteract difpol2 g_10 g_8 g_6 g_4 g_2 g0 g2 g4 g6 g8 g10, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


***
*** ROBUSTNESS (Part I.2)
***

*** use CHGPOL2 - as dummy
* use 5 leads and 5 lags as before (now with covariates)
eventstudyinteract chgpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*only leads
eventstudyinteract chgpol2 g0-g10, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*every other obs.
eventstudyinteract chgpol2 g_10 g_8 g_6 g_4 g_2 g0 g2 g4 g6 g8 g10, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


********************************************************************************

***
* RESIDUAL DIAGNOSTICS
***

* see if there is a pattern in the residuals...
*directional change
reg chgpol2 weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4 i.countrynum i.year, vce(cluster cowcode)
predict res_chg, res
predict fit_chg, xb 
twoway (scatter res_chg fit_chg)
twoway (scatter res_chg panel_success)

*continuous change variable
reg difpol2 weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4 i.countrynum i.year, vce(cluster cowcode)
predict res_dif, res
predict fit_dif, xb
twoway (scatter res_dif fit_dif)
twoway (scatter res_dif panel_success)

*further diagnostics plots
rvfplot
avplot panel_success


********************************************************************************
*** ROBUSTNESS (Part II)

*different clustering 

*use robust
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(robust)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


*use year
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(year)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


* use region
eventstudyinteract difpol2 g_5-g_2 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(region)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


********************************************************************************
*** ROBUSTNESS (Part III)

*pre-treatment effects seem relatively constant, which might suggest binning the many leads.  TODO: current
*implementation of bins does not follow Sun and Abraham (2020) exactly due to coding challenge.  But it is
*valid if effects in the bin are constant for each cohort.
gen g_l5 = relative_year <= -5

eventstudyinteract difpol2 g_l5 g0-g5, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)

matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)


***
* look at 1-10 years...
***
preserve
gen g_l10 = relative_year <= -10
gen g10x = (relative_year <= 10 & relative_year >= 1)
eventstudyinteract exitregular g_l10 g0 g10x, cohort(first_success) control_cohort(never_success) covariates(weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4) absorb(i.countrynum i.year) vce(cluster cowcode)
matrix C = e(b_iw)
mata st_matrix("A",sqrt(st_matrix("e(V_iw)")))
matrix C = C \ A
matrix list C
coefplot matrix(C[1]), se(C[2]) vertical drop(_cons weapondum2 weapondum3 weapondum4 weapondum5 weapondum6 numserdum2 numserdum3 numserdum4)
restore


********************************************************************************
********************************************************************************

**************
* END
**************