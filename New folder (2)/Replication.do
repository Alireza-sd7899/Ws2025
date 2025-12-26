
************************************************************************************************************************
************************************************************************************************************************
****************************** Section 4.1 ***************************************************************************
************************************************************************************************************************
************************************************************************************************************************

cd "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)"


************************************
* Table 1
************************************
use "alldata.dta", clear

drop if pietreatment==24
g totaldelay = (delay==1 | pass==0)
bysort treatment session group round bargaininground: gen num=_n

// Large Budget delays frequency and p-values
tab totaldelay treatment if num==1 & piesize!=24, col
probit totaldelay u48 if (m48==1 | u48==1) & num==1 & piesize!=24, vce(cluster uniquesession) //0.062
probit totaldelay u96 if (m96==1 | u96==1) & num==1 & piesize!=24, vce(cluster uniquesession) //0.672

// Small Budget delays frequency and p-values
tab totaldelay treatment if num==1 & piesize==24, col
probit totaldelay u48 if (m48==1 | u48==1) & num==1 & piesize==24, vce(cluster uniquesession) //0.001
probit totaldelay u96 if (m96==1 | u96==1) & num==1 & piesize==24, vce(cluster uniquesession) //<0.001



**************************************
* Deterministic treatments statements
**************************************
use "alldata.dta", clear
g totaldelay = (delay==1 | pass==0)
bysort treatment session group round bargaininground: gen num=_n

tab totaldelay treatment if num==1 & piesize==24 & pietreatment==24, col
probit totaldelay u24 if pietreatment==24 & proposer==membernumber & piesize==24, vce(cluster uniquesession)


************************************
* Diff in diff 26 % points different than 16 % points
************************************
use "alldata.dta", clear
g totaldelay = (delay==1 | pass==0)
bysort treatment session group round bargaininground: gen num=_n

keep if piesize==24

keep if m24==1 | u24==1 | m48==1 | u48==1
g stochastic = (m48==1 | u48==1)
g interaction = (stochastic * unanimity)

// 51 percentage points = 93-42
tab pass treatment if treatment=="m48" | treatment=="u48", col 

* should look at on-the-table proposals and if they pass because that's the only source of disagreement. 
probit pass i.stochastic i.unanimity i.interaction if proposer==membernumber, vce(cluster uniquesession)


************************************
* Diff in diff large vs. small budgets
************************************
use "alldata.dta", clear
bysort treatment session group round bargaininground: gen num=_n

g smallpie = (piesize==24)
g interactionsmall = (smallpie * unanimity)
* should look at on-the-table proposals and if they pass because that's the only source of disagreement. 
probit pass i.smallpie i.unanimity i.interactionsmall if proposer==membernumber & (m48==1 | u48==1), vce(cluster uniquesession)
* should look at on-the-table proposals and if they pass because that's the only source of disagreement. 
probit pass i.smallpie i.unanimity i.interactionsmall if proposer==membernumber & (m96==1 | u96==1), vce(cluster uniquesession)


************************************
* Figure 1
************************************
use "alldata.dta", clear

tab piesize treatment if pass==1, col  // figure manually constructed using excel.

g largepie = (piesize!=24)
probit largepie  u48 if (m48==1 | u48==1) & proposer==membernumber & pass==1, vce(cluster uniquesession)
probit largepie  u96 if (m96==1 | u96==1) & proposer==membernumber & pass==1, vce(cluster uniquesession)



************************************
* Footnote 29
************************************
use "alldata.dta", clear


g largepie = (piesize!=24)

tab largepie treatment if proposer==membernumber & (pass==1 | terminatedearly==1), m col  

probit largepie  u48 if (m48==1 | u48==1) & proposer==membernumber & (pass==1 | terminatedearly==1), vce(cluster uniquesession)
probit largepie  u96 if (m96==1 | u96==1) & proposer==membernumber & (pass==1 | terminatedearly==1), vce(cluster uniquesession)





************************************
* Figure 2 & Statements from "Origins of Delays"
************************************
use "alldata.dta", clear
keep if piesize==24
drop if m24==1 | u24==1

// Frac of delays by proposer
tab delay treatment, col
// frac of proposals rejected by committee members
tab pass treatment if share1!=., col

xtset id 
xtprobit delay u48 if (m48==1 | u48==1) & proposer==membernumber, vce(cluster uniquesession)
probit pass u48 if (m48==1 | u48==1) & proposer==membernumber, vce(cluster uniquesession)

xtprobit delay u96 if (m96==1 | u96==1) & proposer==membernumber, vce(cluster uniquesession)
probit pass u96 if (m96==1 | u96==1) & proposer==membernumber, vce(cluster uniquesession)


//single deciding vote
use "alldata.dta", clear
keep if piesize==24
drop if m24==1 | u24==1
keep if pass==0
g totalno = (3-totalyes)
g oneno = (totalno==1)
tab oneno treatment if proposer==membernumber, col


// Random termination: Footnote 31
use "alldata.dta", clear
drop if pietreatment==24
drop if piesize!=24

bysort uniquesession group round: gen num=_n
bysort uniquesession group round: egen early_end = max(terminatedearly)

tab early_end treatment if num==1, col
probit early_end m48 if (m48==1 | u48==1) & num==1, vce(cluster uniquesession)
probit early_end m96 if (m96==1 | u96==1) & num==1, vce(cluster uniquesession)



// how to people treat equal split proposals 
use "alldata.dta", clear
keep if piesize==24

g equal = (share1==8 & share2==8 & share3==8)

tab pass treatment if proposer==membernumber & equal==1, col


probit pass m24 if (m24==1 | u24==1) & equal==1 & proposer==membernumber, vce(cluster uniquesession)
probit pass m48 if (m48==1 | u48==1) & equal==1 & proposer==membernumber, vce(cluster uniquesession)
probit pass m96 if (m96==1 | u96==1) & equal==1 & proposer==membernumber, vce(cluster uniquesession)












************************************************************************************************************************
************************************************************************************************************************
****************************** Section 4.2 ***************************************************************************
************************************************************************************************************************
************************************************************************************************************************

********************
**** Table 2  ******
********************
use "alldata.dta", clear
keep if pass==1

egen lowshare = rowmin(share1 share2 share3)
egen highshare = rowmax(share1 share2 share3)
g middleshare = piesize - lowshare - highshare

g mwcequal = (highshare==12 & middleshare==12 & lowshare==0) | (highshare==24 & middleshare==24 & lowshare==0) | (highshare==48 & middleshare==48 & lowshare==0)
g mwcunequal = (lowshare==0 & mwcequal==0)
g equal = (highshare==8 & middleshare==8 & lowshare==8) | (highshare==16 & middleshare==16 & lowshare==16) |  (highshare==32 & middleshare==32 & lowshare==32)
g grand = (lowshare!=highshare & lowshare!=0)

g mwc = (lowshare==0)
g all = (lowshare!=0)

// small budgets
*Column1
tab mwc treatment if piesize==24, col
*Column 2
tab mwcequal treatment if mwc==1 & piesize==24, col
*Column 3
tab all treatment if piesize==24, col
*Column 4
tab equal treatment if all==1 & piesize==24, col

g propingroup = (membernumber==proposer)
g propshare = share1 if proposer==1 
replace propshare = share2 if proposer==2 
replace propshare = share3 if proposer==3
*Column 5 
bysort treatment: sum propshare if propingroup==1 & piesize==24


// big budgets
* Column 1
tab mwc treatment if piesize!=24, col
* Column 2
tab mwcequal treatment if mwc==1 & piesize!=24, col
* Column 3
tab all treatment if piesize!=24, col
* Column 4
tab equal treatment if all==1 & piesize!=24, col


bysort treatment: sum propshare if propingroup==1 & piesize!=24 


***************
*** footnote 32
***************
use "alldata.dta", clear
keep if pass==1

egen lowshare = rowmin(share1 share2 share3)
egen highshare = rowmax(share1 share2 share3)
g middleshare = piesize - lowshare - highshare

g mwcequal = (highshare==12 & middleshare==12 & lowshare==0) | (highshare==24 & middleshare==24 & lowshare==0) | (highshare==48 & middleshare==48 & lowshare==0)
g equal = (highshare==8 & middleshare==8 & lowshare==8) | (highshare==16 & middleshare==16 & lowshare==16) |  (highshare==32 & middleshare==32 & lowshare==32)
g all = (lowshare!=0)

bysort sessioncode group round: gen num=_n
g equalreg = (mwcequal==1 | equal==1)
probit equalreg all if m24==1 & membernumber==proposer, cluster(uniquesession)
probit equalreg all if m48==1 & membernumber==proposer, cluster(uniquesession)
probit equalreg all if m96==1 & membernumber==proposer, cluster(uniquesession)



****************
**** Figure 3 ****
****************

use "alldata.dta", clear

g payoff = ownshare if pass==1
replace payoff=0 if terminatedearly==1
drop if payoff==.

// Panel A

bysort treatment: cumul payoff, generate(payoffcdf)

sort treatment payoffcdf
twoway (line payoffcdf payoff if treatment=="m48", lcolor(black) lwidth(medthick))  (line payoffcdf payoff if treatment=="u48",lcolor(gs10) lwidth(medthick)) (line payoffcdf payoff if treatment=="m96", lpattern(longdash) lcolor(black) lwidth(medthick))  (line payoffcdf payoff if treatment=="u96",lpattern(longdash) lcolor(gs10) lwidth(medthick)), legend(lab(1 "M48") lab(2 "U48") lab(3 "M96") lab(4 "U96")) xlabel(0(10)70) xtitle(Share) ytitle(ECDFs of Share) graphregion(color(white) fcolor(white) icolor(white) ifcolor(white)) plotr(fcolor(white) color(white) ifcolor(white) margin(white))


// Panel B
drop if terminatedearly==1
bysort treatment session round group: egen totalpayment = total(ownshare)

bysort treatment session round group: egen maxpayment=max(ownshare)
bysort treatment session round group: egen minpayment=min(ownshare)
gen middlepayment=totalpayment-maxpayment-minpayment

generate gini=(4-2*(3*minpayment+2*middlepayment+maxpayment)/(totalpayment))/3

bysort treatment: cumul gini, generate(ginicdf)

sort treatment ginicdf

twoway (line ginicdf gini if treatment=="m48", lcolor(black) lwidth(medthick))  (line ginicdf gini if treatment=="u48",lcolor(gs10) lwidth(medthick)) (line ginicdf gini if treatment=="m96", lcolor(black) lpattern(longdash) lwidth(medthick))  (line ginicdf gini if treatment=="u96",lpattern(longdash) lcolor(gs10) lwidth(medthick)), legend(lab(1 "M48") lab(2 "U48") lab(3 "M96") lab(4 "U96")) xlabel(0(0.1)0.5) xtitle(GINI) ytitle(ECDFs of GINI) graphregion(color(white) fcolor(white) icolor(white) ifcolor(white)) plotr(fcolor(white) color(white) ifcolor(white) margin(white))








************************************************************************************************************************
************************************************************************************************************************
****************************** Section 4.3 ***************************************************************************
************************************************************************************************************************
************************************************************************************************************************

// Agreement between coders for each question Majority and average kappa (footnote 36)
use "Raw Data Chat Coders/NotForSubmission/KappaMajority.dta", clear
egen mymean = rowmean(kap*)
sum mymean 
// Agreement between coders for each question Majority and average kappa (footnote 36)
use "Raw Data Chat Coders/NotForSubmission/KappaUnanimity.dta", clear
egen mymean = rowmean(kap*)
sum mymean

// meaningful conversations across treatments:
use "alldata_withchatcoded_ready.dta", clear
sort treatment session group round bargaininground
bysort treatment session group round: gen num=_n
tab relevant_talk treatment if num==1, col 


**********
*Table 3
**********

use "alldata_withchatcoded_ready.dta", clear

sort treatment session group round bargaininground
bysort treatment session group round: gen num=_n

* test for disagreement counted as yes
replace riskterminationtalk=1 if agreev3==0
replace equalitytalk=1 if agreev4==0
replace equalityinmwctalk=1 if agreev10==0
replace yestodelaytalk=1 if agreev8==0
replace bigpietalk=1 if agreev2==0
replace threatnounequaltalk=1 if agreev5==0
replace threatnosmallbudgettalk=1 if agreev6==0


// topics by treatment (when coders agree)
* first row of top and bottom panel
tab bigpietalk treatment if num==1 &  round<6.5, col
probit bigpietalk m48 if num==1  & (m48==1 | u48==1)& round<6.5, vce(cluster uniquesession)
probit bigpietalk m96 if num==1  & (m96==1 | u96==1)& round<6.5, vce(cluster uniquesession)
* 2nd row of top and bottom panel
tab yestodelaytalk treatment if num==1 & round<6.5, col
probit yestodelaytalk m48 if num==1  & (m48==1 | u48==1)& round<6.5, vce(cluster uniquesession)
probit yestodelaytalk m96 if num==1 & (m96==1 | u96==1)& round<6.5, vce(cluster uniquesession)
* 3rd row of top and bottom panel
tab equalitytalk treatment if num==1 & round<6.5, col
probit equalitytalk m48 if num==1 & (m48==1 | u48==1)& round<6.5, vce(cluster uniquesession)
probit equalitytalk m96 if num==1  & (m96==1 | u96==1)& round<6.5, vce(cluster uniquesession)
* 4th row of top and bottom panel
tab threatnounequaltalk treatment if num==1 & round<6.5, col
probit threatnounequaltalk m48 if num==1 & (m48==1 | u48==1)& round<6.5, vce(cluster uniquesession)
probit threatnounequaltalk m96 if num==1 & (m96==1 | u96==1)& round<6.5, vce(cluster uniquesession)
* 5th row of top and bottom panel
tab threatnosmallbudgettalk treatment if num==1 & round<6.5, col
probit threatnosmallbudgettalk m48 if num==1 & (m48==1 | u48==1)& round<6.5, vce(cluster uniquesession)
probit threatnosmallbudgettalk m96 if num==1 & (m96==1 | u96==1)& round<6.5, vce(cluster uniquesession)
* 6th row of top and bottom panel
tab riskterminationtalk treatment if num==1 & round<6.5, col
probit riskterminationtalk m48 if num==1 &  (m48==1 | u48==1)& round<6.5, vce(cluster uniquesession)
probit riskterminationtalk m96 if num==1 &  (m96==1 | u96==1)& round<6.5, vce(cluster uniquesession)
* 7th row top and bottom panels
tab equalityinmwctalk treatment if num==1 & round<6.5, col
* 8th row top and bottom panels
tab unequalinmwctalk treatment if num==1 & round<6.5, col




**********
*Table 4
**********
use "alldata_withchatcoded_ready.dta", clear



sort treatment session group round bargaininground
bysort treatment session group round: gen num=_n


****
*test // talk didn't happen if both coders say no, if disagreement then code as yes. * pretty good
replace riskterminationtalk=1 if agreev3==0
replace equalitytalk=1 if agreev4==0
replace equalityinmwctalk=1 if agreev10==0
replace yestodelaytalk=1 if agreev8==0
replace bigpietalk=1 if agreev2==0

keep if bargaininground==1


* M48 column
probit delay i.riskterminationtalk i.equalitytalk i.equalityinmwctalk i.yestodelaytalk i.bigpietalk if m48==1 & num==1 & round<6.5 & didnottalk==0, cluster(uniquesession)
margins, dydx(*)
* U48 column
probit delay i.riskterminationtalk i.equalitytalk  i.yestodelaytalk i.bigpietalk if u48==1 & num==1 & round<6.5 & didnottalk==0, cluster(uniquesession)
margins, dydx(*)
* M96 column
probit delay i.riskterminationtalk i.equalitytalk i.equalityinmwctalk i.yestodelaytalk i.bigpietalk if m96==1 & num==1 & round<6.5 & didnottalk==0, cluster(uniquesession)
margins, dydx(*)
* U96 column
probit delay i.riskterminationtalk i.equalitytalk  i.yestodelaytalk i.bigpietalk if u96==1 & num==1 & round<6.5 & didnottalk==0, cluster(uniquesession)
margins, dydx(*)








************************************************************************************************************************
************************************************************************************************************************
****************************** Appendix ***************************************************************************
************************************************************************************************************************
************************************************************************************************************************


************************************
* Appendix D Table 5
************************************

use "alldata.dta", clear

* are there differences across treatments in terms of investment behavior in investment game?
bysort id: gen num=_n
keep if num==1

// Investment Task 1
bysort treatment: sum playerinvested25, d

ssc install qreg2

reg playerinvested25 u24 if (u24==1 | m24==1), cluster(uniquesession)
qreg2 playerinvested25 u24 if (u24==1 | m24==1), cluster(uniquesession)

reg playerinvested25 u48 if (u48==1 | m48==1), cluster(uniquesession)
qreg2 playerinvested25 u48 if (u48==1 | m48==1), cluster(uniquesession)

reg playerinvested25 u96 if (u96==1 | m96==1), cluster(uniquesession)
qreg2 playerinvested25 u96 if (u96==1 | m96==1), cluster(uniquesession)

// Investment Task 2
bysort treatment: sum playerinvested30, d

reg playerinvested30 u24 if (u24==1 | m24==1), cluster(uniquesession)
qreg2 playerinvested30 u24 if (u24==1 | m24==1), cluster(uniquesession)

reg playerinvested30 u48 if (u48==1 | m48==1), cluster(uniquesession)
qreg2 playerinvested30 u48 if (u48==1 | m48==1), cluster(uniquesession)

reg playerinvested30 u96 if (u96==1 | m96==1), cluster(uniquesession)
qreg2 playerinvested30 u96 if (u96==1 | m96==1), cluster(uniquesession)




************************************
* Table 6 
************************************

use "alldata.dta", clear

* install quantile regression package
ssc install qreg2

bysort id: gen num=_n
keep if num==1

* Investment task 1 mean and median
bysort treatment: sum playerinvested25, d


*pvalues
reg playerinvested25 u24 if (u24==1 | m24==1), cluster(uniquesession)
qreg2 playerinvested25 u24 if (u24==1 | m24==1), cluster(uniquesession)

reg playerinvested25 u48 if (u48==1 | m48==1), cluster(uniquesession)
qreg2 playerinvested25 u48 if (u48==1 | m48==1), cluster(uniquesession)

reg playerinvested25 u96 if (u96==1 | m96==1), cluster(uniquesession)
qreg2 playerinvested25 u96 if (u96==1 | m96==1), cluster(uniquesession)


* Investment task 2 mean and median
bysort treatment: sum playerinvested30, d


*pvalues
reg playerinvested30 u24 if (u24==1 | m24==1), cluster(uniquesession)
qreg2 playerinvested30 u24 if (u24==1 | m24==1), cluster(uniquesession)

reg playerinvested30 u48 if (u48==1 | m48==1), cluster(uniquesession)
qreg2 playerinvested30 u48 if (u48==1 | m48==1), cluster(uniquesession)

reg playerinvested30 u96 if (u96==1 | m96==1), cluster(uniquesession)
qreg2 playerinvested30 u96 if (u96==1 | m96==1), cluster(uniquesession)


************************************
* Table 7 -- miror of  table 1 in main text
************************************
use "alldata.dta", clear

keep if bargaininground==1
g totaldelay = (delay==1 | pass==0)
bysort treatment session group round bargaininground: gen num=_n


tab totaldelay treatment if num==1 & piesize==24, col
probit totaldelay u48 if (m48==1 | u48==1) & proposer==membernumber & piesize==24, vce(cluster uniquesession)
probit totaldelay u96 if (m96==1 | u96==1) & proposer==membernumber & piesize==24, vce(cluster uniquesession)









*************************
* Table 8 -- Total committee earnings
*************************
use "alldata.dta", clear

g votingrule = (votingtreatment=="unanimity")

g payout = share1+share2+share3 if pass==1
replace payout = 0 if terminatedearly==1

bysort uniquesession memberid round: egen lastround = max(bargaininground)
keep if bargaininground==lastround

bysort uniquesession group round: gen num=_n

bysort treatment: sum payout if num==1

reg payout  if m24==1 & num==1, cluster(uniquesession)
reg payout  if m48==1 & num==1, cluster(uniquesession)
reg payout if m96==1 & num==1, cluster(uniquesession)

reg payout  if u24==1 & num==1, cluster(uniquesession)
reg payout  if u48==1 & num==1, cluster(uniquesession)
reg payout if u96==1 & num==1, cluster(uniquesession)



reg payout m24 if (m24==1 | u24==1) & num==1, cluster(uniquesession)
reg payout m48 if (m48==1 | u48==1) & num==1, cluster(uniquesession)
reg payout m96 if (m96==1 | u96==1) & num==1, cluster(uniquesession)

reg payout m48 if (m24==1 | m48==1) & num==1, cluster(uniquesession)
reg payout u48 if (u24==1 | u48==1) & num==1, cluster(uniquesession)
reg payout m96 if (m48==1 | m96==1) & num==1, cluster(uniquesession)
reg payout u96 if (u48==1 | u96==1) & num==1, cluster(uniquesession)


*************************
* Figure 13 -- Individual level stuff: delays
*************************
use "alldata.dta", clear

g votingrule = (votingtreatment=="unanimity")

keep sessioncode pietreatment piesize round bargaininground group memberid membernumber proposer delay terminatedearly m24 m48 m96 u24 u48 u96 unanimity votingrule uniquesession

sort uniquesession memberid proposer membernumber

g wasproposer = (proposer==membernumber)

keep if wasproposer==1

bysort uniquesession memberid: egen nbdelays = total(delay)

bysort uniquesession memberid: gen nbtimeprop = _N

g fracdelay = nbdelays/nbtimeprop

bysort uniquesession memberid: gen num=_n
keep if num==1

* Panel A
twoway (histogram fracdelay if u48==1, color(green) gap(5.2) frac) (histogram fracdelay if m48==1, fcolor(none) lcolor(black) gap(5.2) frac), legend(lab(1 "U48") lab(2 "M48")) xtitle(Fraction of Delays) ytitle() graphregion(color(white) fcolor(white) icolor(white) ifcolor(white)) plotr(fcolor(white) color(white) ifcolor(white) margin(white))
* Panel B
twoway (histogram fracdelay if u96==1, gap(5.2) color(green) gap(0) frac) (histogram fracdelay if m96==1, fcolor(none) lcolor(black) gap(5.2) frac), legend(lab(1 "U96") lab(2 "M96")) xtitle(Fraction of Delays) ytitle() graphregion(color(white) fcolor(white) icolor(white) ifcolor(white)) plotr(fcolor(white) color(white) ifcolor(white) margin(white))

reg fracdelay m96 if (m96==1 | u96==1)
reg fracdelay m48 if (m48==1 | u48==1)



*************************
* Table 9 -- first half second half small budget passing
*************************
use "alldata.dta", clear

keep if delay==0
keep if piesize==24
keep if proposer==membernumber

tab  pass treatment if round<6.5, col
tab  pass treatment if round>6.5, col

probit pass m48 if round<6.5 & (m48==1 | u48==1), vce(cluster uniquesession)
probit pass m48 if round>6.5 & (m48==1 | u48==1), vce(cluster uniquesession)

probit pass m96 if round<6.5 & (m96==1 | u96==1), vce(cluster uniquesession)
probit pass m96 if round>6.5 & (m96==1 | u96==1), vce(cluster uniquesession)



*************************
* Table 10 -- 
*************************
use "alldata.dta", clear

keep if piesize==24
drop if m24==1 | u24==1

g nopass = (delay==1 | pass==0)

// Top Panel (First half), first 4 columns
tab nopass treatment if proposer==membernumber & round<6.5, col
tab nopass treatment if proposer==membernumber & round<6.5 & bargaininground==1, col
tab nopass treatment if proposer==membernumber & round<6.5 & bargaininground==2, col
tab nopass treatment if proposer==membernumber & round<6.5 & bargaininground>=2.5, col

probit nopass u48 if (m48==1 | u48==1) & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u48 if (m48==1 | u48==1) & bargaininground==1 & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u48 if (m48==1 | u48==1) & bargaininground==2 & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u48 if (m48==1 | u48==1) & bargaininground>=2.5 & proposer==membernumber & round<6.5, vce(cluster uniquesession)

probit nopass u96 if (m96==1 | u96==1) & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & bargaininground==1 & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & bargaininground==2 & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & bargaininground>=2.5 & proposer==membernumber & round<6.5, vce(cluster uniquesession)


// Bottom Panel (Second half), first 4 columns
tab nopass treatment if proposer==membernumber & round>6.5, col
tab nopass treatment if proposer==membernumber & round>6.5 & bargaininground==1, col
tab nopass treatment if proposer==membernumber & round>6.5 & bargaininground==2, col
tab nopass treatment if proposer==membernumber & round>6.5 & bargaininground>=2.5, col

probit nopass u48 if (m48==1 | u48==1) & proposer==membernumber & round>6.5, vce(cluster uniquesession)
probit nopass u48 if (m48==1 | u48==1) & bargaininground==1 & proposer==membernumber & round>6.5, vce(cluster uniquesession)
probit nopass u48 if (m48==1 | u48==1) & bargaininground==2 & proposer==membernumber & round>6.5, vce(cluster uniquesession)
probit nopass u48 if (m48==1 | u48==1) & bargaininground>=2.5 & proposer==membernumber & round>6.5, vce(cluster uniquesession)

probit nopass u96 if (m96==1 | u96==1) & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & bargaininground==1 & proposer==membernumber & round>6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & bargaininground==2 & proposer==membernumber & round>6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & bargaininground>=2.5 & proposer==membernumber & round>6.5, vce(cluster uniquesession)


//  5th column
use "alldata.dta", clear
keep if piesize!=24
drop if m24==1 | u24==1
g nopass = (delay==1 | pass==0)
* First half
tab nopass treatment if proposer==membernumber & round<6.5, col
probit nopass u48 if (m48==1 | u48==1) & proposer==membernumber & round<6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & proposer==membernumber & round<6.5, vce(cluster uniquesession)
* Second half
tab nopass treatment if proposer==membernumber & round>6.5, col
probit nopass u48 if (m48==1 | u48==1) & proposer==membernumber & round>6.5, vce(cluster uniquesession)
probit nopass u96 if (m96==1 | u96==1) & proposer==membernumber & round>6.5, vce(cluster uniquesession)





********************
* Table 11
********************
use "alldata.dta", clear

keep if piesize==24

egen lowshare = rowmin(share1 share2 share3)
egen highshare = rowmax(share1 share2 share3)
g middleshare = piesize - lowshare - highshare

g mwcequal = (highshare==12 & middleshare==12 & lowshare==0)
g mwcunequal = (lowshare==0 & mwcequal==0)
g equal = (highshare==8 & middleshare==8 & lowshare==8)
g grand = (lowshare<8 & lowshare!=0)

g proposaltype = 1 if mwcequal==1
replace proposaltype = 2 if mwcunequal==1
replace proposaltype = 3 if equal==1
replace proposaltype = 4 if grand==1

tab proposaltype treatment if proposer==membernumber, col



********************
* Table 12
********************
use "alldata.dta", clear

keep if piesize==24

egen lowshare = rowmin(share1 share2 share3)
egen highshare = rowmax(share1 share2 share3)
g middleshare = piesize - lowshare - highshare

g mwcequal = (highshare==12 & middleshare==12 & lowshare==0)
g mwcunequal = (lowshare==0 & mwcequal==0)
g equal = (highshare==8 & middleshare==8 & lowshare==8)
g grand = (lowshare<8 & lowshare!=0)

g proposaltype = 1 if mwcequal==1
replace proposaltype = 2 if mwcunequal==1
replace proposaltype = 3 if equal==1
replace proposaltype = 4 if grand==1

bysort treatment: tab  pass proposaltype if proposer==membernumber, col



********************
* Table 13
********************
use "alldata_withchatcoded_ready.dta", clear


sort treatment session group round bargaininground
bysort treatment session group round: gen num=_n


replace riskterminationtalk=1 if agreev3==0
replace equalitytalk=1 if agreev4==0
replace equalityinmwctalk=1 if agreev10==0
replace yestodelaytalk=1 if agreev8==0
replace bigpietalk=1 if agreev2==0

keep if bargaininground==1

* M48 vs M96
probit bigpietalk m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
probit yestodelaytalk m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
probit equalitytalk m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
probit threatnounequal m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
probit riskterminationtalk m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
probit equalityinmwctalk m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
probit unequalinmwctalk m48 if num==1  & (m48==1 | m96==1), vce(cluster uniquesession)
* U48 vs U96
probit bigpietalk u48 if num==1  & (u48==1 | u96==1), vce(cluster uniquesession)
probit yestodelaytalk u48 if num==1 & (u48==1 | u96==1), vce(cluster uniquesession)
probit equalitytalk u48 if num==1 &  (u48==1 | u96==1), vce(cluster uniquesession)
probit threatnounequal u48 if num==1 &  (u48==1 | u96==1), vce(cluster uniquesession)
probit threatnosmallbudgettalk u48 if num==1 &  (u48==1 | u96==1), vce(cluster uniquesession)
probit riskterminationtalk u48 if num==1 &  (u48==1 | u96==1), vce(cluster uniquesession)














