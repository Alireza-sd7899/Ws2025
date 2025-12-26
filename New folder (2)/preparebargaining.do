import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/data.2019.04.12.xlsx", firstrow clear


egen id = group(sessioncode memberid)

g m24 = (votingtreatment=="majority" & pietreatment==24)
g m48 = (votingtreatment=="majority" & pietreatment==48)
g m96 = (votingtreatment=="majority" & pietreatment==96)

g u24 = (votingtreatment=="unanimity" & pietreatment==24)
g u48 = (votingtreatment=="unanimity" & pietreatment==48)
g u96 = (votingtreatment=="unanimity" & pietreatment==96)

g totalyes = vote1 + vote2 + vote3
g pass = (totalyes==3 & votingtreatment=="unanimity") | (totalyes>=2 & votingtreatment=="majority")
replace pass=. if totalyes==.

g treatment = ""
replace treatment = "m24" if m24==1
replace treatment = "m48" if m48==1
replace treatment = "m96" if m96==1
replace treatment = "u24" if u24==1
replace treatment = "u48" if u48==1
replace treatment = "u96" if u96==1

g unanimity = (votingtreatment=="unanimity")

g session=.
replace session=1 if sessioncode=="50catrnf" | sessioncode=="7f7v4l6r" | sessioncode=="js437fzd" | sessioncode=="m1" | sessioncode=="m5" | sessioncode=="q32zfvnq"
replace session=2 if sessioncode=="9gzbr7vg" | sessioncode=="ax2x1ab5" | sessioncode=="ifm6r51c" | sessioncode=="krgxtmp1" | sessioncode=="m2" | sessioncode=="m6" 
replace session=3 if sessioncode=="6nf5jbi5" | sessioncode=="gx0695sj" | sessioncode=="jvzb19el" | sessioncode=="m3" | sessioncode=="p66q0jhd" | sessioncode=="w2npj4ao" 
replace session=4 if sessioncode=="9fuzcnjg" | sessioncode=="ixhiq3bn" | sessioncode=="job0mwmb" | sessioncode=="jwlbjm39" | sessioncode=="m4" | sessioncode=="wfwdfk01" 

g uniquesession=.
forvalues i = 1(1)4 {
	replace uniquesession = 240`i' if votingtreatment=="majority" & session==`i' & pietreatment==24
	replace uniquesession = 480`i' if votingtreatment=="majority" & session==`i' & pietreatment==48
	replace uniquesession = 960`i' if votingtreatment=="majority" & session==`i' & pietreatment==96
}

forvalues i = 1(1)4 {
	replace uniquesession = 241`i' if votingtreatment=="unanimity" & session==`i' & pietreatment==24
	replace uniquesession = 481`i' if votingtreatment=="unanimity" & session==`i' & pietreatment==48
	replace uniquesession = 961`i' if votingtreatment=="unanimity" & session==`i' & pietreatment==96
}

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/Bargaining.dta", replace


 ************************************
* Format Risk Data
************************************

clear
forvalues i=1(1)4 {	
	import excel using 	"/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.xlsx", ///
	sheet(U1_s`i') firstrow
	if `i'==1 {
		g session=1
		g treatment="u24"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
	}
	else {
		g session=`i'
		g treatment="u24"
		append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
	}
}


use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
drop K-P
g unanimity=1
g pie=24
save, replace


clear
forvalues i=1(1)4 {	
	import excel using 	"/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.xlsx", ///
	sheet(U2_s`i') firstrow
		g session=`i'
		g treatment="u48"
		append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
}

use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
drop G-R
replace unanimity=1 if unanimity==.
replace  pie=48 if pie==.
drop if sessioncode=="" & treatment=="u48"
save, replace


clear
forvalues i=1(1)4 {	
	import excel using 	"/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.xlsx", ///
	sheet(U4_s`i') firstrow
		g session=`i'
		g treatment="u96"
		append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
}

use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
drop G-R
replace unanimity=1 if unanimity==.
replace  pie=96 if pie==.
drop if sessioncode=="" & treatment=="u96"
save, replace
clear



forvalues i=1(1)4 {	
	import excel using 	"/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.xlsx", ///
	sheet(M1_s`i') firstrow
		g session=`i'
		g treatment="m24"
		g unanimity=0
		g pie=24
		append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
}

clear
forvalues i=1(1)4 {	
	import excel using 	"/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.xlsx", ///
	sheet(M2_s`i') firstrow
		g session=`i'
		g treatment="m48"
		g unanimity=0
		g pie=48	
		append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
}

clear
forvalues i=1(1)4 {	
	import excel using 	"/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.xlsx", ///
	sheet(M4_s`i') firstrow
		g session=`i'
		g treatment="m96"
		g unanimity=0
		g pie=96
		append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta"
		save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", replace 
		clear
}






use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/risk.dta", clear

*rename participantcode participantcoderisk

g riskmultiplier = 2 if subsessioninvestment_multiplier==3
replace riskmultiplier = 1 if subsessioninvestment_multiplier==2.5 

rename participantid_in_session memberid
egen id = group(unanimity pie session memberid)

keep participantcode memberid sessioncode riskmultiplier playerinvested id session unanimity


reshape wide playerinvested, i(id) j(riskmultiplier)
rename playerinvested1 playerinvested25
rename playerinvested2 playerinvested30 

drop id
sort sessioncode memberid

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/riskformatted.dta", replace




**********************************
* Merge risk and main data
**********************************


use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/Bargaining.dta", clear

sort sessioncode memberid

merge m:m sessioncode memberid  using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw data Bargaining and Risk/riskformatted.dta"

drop _merge

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata.dta", replace



