
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
* old code from data
***

tsset
g anywarl1l3 = l.zGledAnywar - l3.zGledAnywar
g npolity2l1l3 = l.npolity2 - l3.npolity2
g lnenergy_pc = ln(energy) - ln(tpop)
g lnpop = ln(tpop)

g pol2dum = 0 if polity2<=0 & polity2!=. 
replace pol2dum = 1 if polity2>0 & polity2!=. 
g lpol2dum = l.pol2dum
g pol2duml1l3 = l.pol2dum - l3.pol2dum
for var npolity2 zGledAnywar lnenergy_pc lnpop age : g lX = l.X

foreach X of var prioanywar prioanywarP zGledAnywar zGledAnywarP {
	g `X'11 = f.`X' - l.`X'
	
}


**************************************
*** Table 9: Propensity score
**************************************

g failure = (seriousattempt == 1 & success == 0) if seriousattempt != . & success != .
	
capture drop lanywar
g lanywar = l.zGledAnywar
g ltenure = l.clock
*label var lnpolity2 "Polity score"
*label var ltenure "Tenure"
*label var lanywar "At war"
*label var llnenergy_pc "Ln energy use p.c."
*label var llnpop "Population"


dprobit seriousattempt lpol2dum , cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se replace title("Predicting attempts") bd(3) adds("P-val",r(p))

dprobit seriousattempt pol2duml1l3 , cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt lanywar , cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt anywarl1l3 , cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt llnenergy_pc , cluster(cowcode) 
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt llnpop, cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt lage, cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt ltenure, cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt lpol2dum pol2duml1l3 lanywar anywarl1l3 llnenergy_pc llnpop lage ltenure , cluster(cowcode)
testparm *
outreg2 using table_9, coefastr se append bd(3) adds("P-val",r(p))




**************************************
*** Table 10: Success vs. failure, institutional change
**************************************
* Set up propensity score variables
for var lpol2dum pol2duml1l3 anywarl1l3 lanywar llnpop llnenergy_pc lage ltenure : g mis_X = (X == .) \ g nonmis_X = X \ replace nonmis_X  = 0 if X == .

pscore seriousattempt nonmis_* mis_* if obsid != "" , pscore(pscoreseriousattempt) blockid(blockseriousattempt)

g failurelautoc = failure*lautoc
g failureldemoc = failure*ldemoc

********************************
* Table 10A
********************************

***
* COLUMNS (1) - (2)
***

eststo clear
quietly reg absnpolity2dummy11 success failure  , cluster(cowcode)
global reg_1s = _b[success]
global reg_1s_se = _se[success]
global reg_1f = _b[failure]
global reg_1f_se = _se[failure]

testparm success
eststo reg1, add(pvalS r(p))
testparm failure
eststo reg1, add(pvalF r(p))


quietly xi: reg absnpolity2dummy11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
global reg_2s = _b[success]
global reg_2s_se = _se[success]
global reg_2f = _b[failure]
global reg_2f_se = _se[failure]

testparm success
eststo reg2, add(pvalS r(p))
testparm failure
eststo reg2, add(pvalF r(p))

*propensity score matching
quietly xi: reg absnpolity2dummy11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
global reg_3s = _b[success]
global reg_3s_se = _se[success]
global reg_3f = _b[failure]
global reg_3f_se = _se[failure]

testparm success
eststo reg3, add(pvalS r(p))
testparm failure
eststo reg3, add(pvalF r(p))


estout, stat(pvalS pvalF)

***
* NEW METHODS
***

*nearest neighbor matching
attnw absnpolity2dummy11 success failure nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attnw_1s = r(attnw)
global attnw_1s_bse = r(bseattnw)

*nearest neighbor matching
attnw absnpolity2dummy11 failure success nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attnw_1f = r(attnw)
global attnw_1f_bse = r(bseattnw)

*kernel-density matching 
attk absnpolity2dummy11 success failure nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attk_1s = r(attk)
global attk_1s_bse = r(bseattk)

*kernel-density matching 
attk absnpolity2dummy11 failure success nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attk_1f = r(attk)
global attk_1f_bse = r(bseattk)

*now I generate my matrix and the summary table
matrix A = $reg_1s, $reg_2s, $reg_3s, $attnw_1s, $attk_1s\ $reg_1s_se, $reg_2s_se, $reg_3s_se, $attnw_1s_bse, $attk_1s_bse\ $reg_1f, $reg_2f, $reg_3f, $attnw_1f, $attk_1f\ $reg_1f_se, $reg_2f_se, $reg_3f_se, $attnw_1f_bse, $attk_1f_bse
matrix rownames A = "Success" "Std_Error" "Failure" "Std_Error"
matrix colnames A = REG REG_con REG_con_pscore ATT_Neighbor ATT_Kernel 
matlist A, format(%15.4f) twidth(40) title(Summary of Results)

esttab matrix(A) using matrix_A.tex, replace



***
* COLUMNS (3) - (4)
***

reg npolity2dummy11 success failure  , cluster(cowcode)
global reg_4s = _b[success]
global reg_4s_se = _se[success]
global reg_4f = _b[failure]
global reg_4f_se = _se[failure]

testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)


xi: reg npolity2dummy11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
global reg_5s = _b[success]
global reg_5s_se = _se[success]
global reg_5f = _b[failure]
global reg_5f_se = _se[failure]

testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)

*propensity score matching
xi: reg npolity2dummy11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
global reg_6s = _b[success]
global reg_6s_se = _se[success]
global reg_6f = _b[failure]
global reg_6f_se = _se[failure]

testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)


***
* NEW METHODS
***

*nearest neighbor matching
attnw npolity2dummy11 success failure nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attnw_2s = r(attnw)
global attnw_2s_bse = r(bseattnw)

*nearest neighbor matching
attnw npolity2dummy11 failure success nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attnw_2f = r(attnw)
global attnw_2f_bse = r(bseattnw)

*kernel-density matching 
attk npolity2dummy11 success failure nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attk_2s = r(attk)
global attk_2s_bse = r(bseattk)

*kernel-density matching 
attk npolity2dummy11 failure success nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attk_2f = r(attk)
global attk_2f_bse = r(bseattk)

*now I generate my matrix and the summary table
matrix B = $reg_4s, $reg_5s, $reg_6s, $attnw_2s, $attk_2s\ $reg_4s_se, $reg_5s_se, $reg_6s_se, $attnw_2s_bse, $attk_2s_bse\ $reg_4f, $reg_5f, $reg_6f, $attnw_2f, $attk_2f\ $reg_4f_se, $reg_5f_se, $reg_6f_se, $attnw_2f_bse, $attk_2f_bse
matrix rownames B = "Success" "Std_Error" "Failure" "Std_Error"
matrix colnames B = REG REG_con REG_con_pscore ATT_Neighbor ATT_Kernel 
matlist B, format(%15.4f) twidth(40) title(Summary of Results)

esttab matrix(B) using matrix_B.tex, replace



***
* COLUMNS (5) - (6)
***


reg perexitregularNC201 success failure  , cluster(cowcode)
global reg_7s = _b[success]
global reg_7s_se = _se[success]
global reg_7f = _b[failure]
global reg_7f_se = _se[failure]

testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)


xi: reg perexitregularNC201 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
global reg_8s = _b[success]
global reg_8s_se = _se[success]
global reg_8f = _b[failure]
global reg_8f_se = _se[failure]

testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)


*propensity score matching
xi: reg perexitregularNC201 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
global reg_9s = _b[success]
global reg_9s_se = _se[success]
global reg_9f = _b[failure]
global reg_9f_se = _se[failure]

testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)



***
* NEW METHODS
***

*nearest neighbor matching
attnw perexitregularNC201 success failure nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attnw_3s = r(attnw)
global attnw_3s_bse = r(bseattnw)

*nearest neighbor matching
attnw perexitregularNC201 failure success nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attnw_3f = r(attnw)
global attnw_3f_bse = r(bseattnw)

*kernel-density matching 
attk perexitregularNC201 success failure nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attk_3s = r(attk)
global attk_3s_bse = r(bseattk)

*kernel-density matching 
attk perexitregularNC201 failure success nonmis_* mis_*, pscore(pscoreseriousattempt) bootstrap reps(100)
*save some r-class data
global attk_3f = r(attk)
global attk_3f_bse = r(bseattk)

*now I generate my matrix and the summary table
matrix C = $reg_7s, $reg_8s, $reg_9s, $attnw_3s, $attk_3s\ $reg_7s_se, $reg_8s_se, $reg_9s_se, $attnw_3s_bse, $attk_3s_bse\ $reg_7f, $reg_8f, $reg_9f, $attnw_3f, $attk_3f\ $reg_7f_se, $reg_8f_se, $reg_9f_se, $attnw_3f_bse, $attk_3f_bse
matrix rownames C = "Success" "Std_Error" "Failure" "Std_Error"
matrix colnames C = REG REG_con REG_con_pscore ATT_Neighbor ATT_Kernel 
matlist C, format(%15.4f) twidth(40) title(Summary of Results)

esttab matrix(C) using matrix_C.tex, replace


********************************************************************************
********************************************************************************

**************
* END
**************
