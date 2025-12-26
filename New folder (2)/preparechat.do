 ************************************
* metric for agreement between coders Majority
************************************

	
forvalues j=2(2)4 {	
		forvalues k=1(1)4 {
	
import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/majorityChats.xlsx", sheet(M`j'_s`k') firstrow clear
drop participantid_in_session participantcode participantlabel sessioncode groupbigSize subsessionround_number


rename groupgroup_identifier group
sort group playerid_in_group
rename playerm_v11 playerm_v13

reshape wide playerm_v1 playerm_v2 playerm_v3 playerm_v4 playerm_v5 playerm_v6 playerm_v7 playerm_v8 playerm_v9 playerm_v10 playerm_v13 , i(group) j(playerid_in_group)

foreach i in 2 4 9 10 13 {
		replace playerm_v`i'1="1" if playerm_v`i'1!="0"
		destring playerm_v`i'1, replace
		replace playerm_v`i'2="1" if playerm_v`i'2!="0"
		destring playerm_v`i'2, replace
}


forvalues i=1(1)10 {
		g agreev`i' = (playerm_v`i'1==playerm_v`i'2)
}
		g agreev11 = (playerm_v131==playerm_v132)

g session = "m`j's`k'"
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m`j's`k'.dta", replace		
		
keep playerm*1 group agree*		

forvalues i=1(1)10 {
	rename playerm_v`i'1 valueq`i'	
}

rename playerm_v131 valueq11
sort group
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/m`j's`k'.dta", replace

}
}





 ************************************
* kappa coders Majority --- the cohen kappa scores are in ... DataPaper/ChatData/Agreement/KappaSummary.xlsx

************************************

	
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m2s1.dta", clear
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m2s2.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m2s3.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m2s4.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m4s1.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m4s2.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m4s3.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_m4s4.dta"

	
	
	
			* no instances of q6==1
forvalues i=1(1)5 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s1"
	g kap`i'm2s1 = r(kappa)
}
forvalues i=7(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s1"
	g kap`i'm2s1 = r(kappa)
}
	kap playerm_v131 playerm_v132 if session=="m2s1"
	g kap13m2s1 = r(kappa)
			
			* no instances of q5,6==1
forvalues i=1(1)4 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s2"
	g kap`i'm2s2 = r(kappa)
}
forvalues i=7(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s2"
	g kap`i'm2s2 = r(kappa)
}
	kap playerm_v131 playerm_v132 if session=="m2s2"
	g kap13m2s2 = r(kappa)
		
			* no instances of q1,6==1
forvalues i=2(1)5 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s3"
	g kap`i'm2s3 = r(kappa)
}
forvalues i=7(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s3"
	g kap`i'm2s3 = r(kappa)
}
	kap playerm_v131 playerm_v132 if session=="m2s3"
	g kap13m2s3 = r(kappa)
		
			* no instances of q5,6,7==1
forvalues i=1(1)4 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s4"
	g kap`i'm2s4 = r(kappa)
}
forvalues i=8(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m2s4"
	g kap`i'm2s4 = r(kappa)
}
	kap playerm_v131 playerm_v132 if session=="m2s4"
	g kap13m2s4 = r(kappa)

	
	
			* no instances of q1==1
forvalues i=2(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m4s1"
	g kap`i'm4s1 = r(kappa)
}
	kap playerm_v131 playerm_v132 if session=="m4s1"
	g kap13m4s1 = r(kappa)

			* m4s2 all good
forvalues i=1(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m4s2"
	g kap`i'm4s2 = r(kappa)
}
	kap playerm_v131 playerm_v132 if session=="m4s2"
	g kap13m4s2 = r(kappa)

		* no instances of q5, 6, 7==1
forvalues i=1(1)4 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m4s3"
	g kap`i'm4s3 = r(kappa)
}
forvalues i=8(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m4s3"
	g kap`i'm4s3 = r(kappa)
}
kap playerm_v131 playerm_v132 if session=="m4s3"
	g kap13m4s3 = r(kappa)

	
			* m4s4 all good
forvalues i=1(1)10 {
	kap playerm_v`i'1 playerm_v`i'2 if session=="m4s4"
	g kap`i'm4s4 = r(kappa)
}
kap playerm_v131 playerm_v132 if session=="m4s4"
g kap13m4s4 = r(kappa)



save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/KappaMajority.dta", replace
	





 ************************************
* metric for agreement between coders Unanimity
************************************

	
forvalues j=2(2)2 {	
		forvalues k=1(1)4 {
	
import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/unanimityChats.xlsx", sheet(U`j'_s`k') firstrow clear

drop participantid_in_session participantcode participantlabel sessioncode groupbigSize subsessionround_number


rename groupgroup_identifier group
sort group playerid_in_group

reshape wide playeru_v1 playeru_v2 playeru_v3 playeru_v4 playeru_v5 playeru_v6 playeru_v7 playeru_v8, i(group) j(playerid_in_group)

foreach i in 2 4 {
		replace playeru_v`i'1="1" if playeru_v`i'1!="0"
		destring playeru_v`i'1, replace
		replace playeru_v`i'2="1" if playeru_v`i'2!="0"
		destring playeru_v`i'2, replace
}

g session = "u`j's`k'"

forvalues i=1(1)8 {
		g agreev`i' = (playeru_v`i'1==playeru_v`i'2)

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u`j's`k'.dta", replace	


}


keep playeru*1 group agree*		

forvalues i=1(1)8 {
	rename playeru_v`i'1 valueq`i'	
}


sort group		
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta", replace

}
}

forvalues j=4(2)4 {	
		forvalues k=1(1)3 {
	
import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/unanimityChats.xlsx", sheet(U`j'_s`k') firstrow clear

drop participantid_in_session participantcode participantlabel sessioncode groupbigSize subsessionround_number


rename groupgroup_identifier group
sort group playerid_in_group

reshape wide playeru_v1 playeru_v2 playeru_v3 playeru_v4 playeru_v5 playeru_v6 playeru_v7 playeru_v8, i(group) j(playerid_in_group)

foreach i in 2 4 {
		replace playeru_v`i'1="1" if playeru_v`i'1!="0"
		destring playeru_v`i'1, replace
		replace playeru_v`i'2="1" if playeru_v`i'2!="0"
		destring playeru_v`i'2, replace
}

g session = "u`j's`k'"

forvalues i=1(1)8 {
		g agreev`i' = (playeru_v`i'1==playeru_v`i'2)
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u`j's`k'.dta", replace	


}

keep playeru*1 group agree*		

forvalues i=1(1)8 {
	rename playeru_v`i'1 valueq`i'	
}
	
sort group		
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta", replace

}
}


import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/unanimityChats.xlsx", sheet(U4_s4) firstrow clear

drop participantid_in_session participantcode participantlabel sessioncode groupbigSize subsessionround_number


rename groupgroup_identifier group
drop if group==.
sort group playerid_in_group

reshape wide playeru_v1 playeru_v2 playeru_v3 playeru_v4 playeru_v5 playeru_v6 playeru_v7 playeru_v8, i(group) j(playerid_in_group)

foreach i in 2 4 {
		replace playeru_v`i'1="1" if playeru_v`i'1!="0"
		destring playeru_v`i'1, replace
		replace playeru_v`i'2="1" if playeru_v`i'2!="0"
		destring playeru_v`i'2, replace
}

g session = "u4s4"
forvalues i=1(1)8 {
		g agreev`i' = (playeru_v`i'1==playeru_v`i'2)

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u4s4.dta", replace	


}


keep playeru*1 group agree*		

forvalues i=1(1)8 {
	rename playeru_v`i'1 valueq`i'	
}

sort group	
		
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s4.dta", replace




use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s1.dta", clear

			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s2.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s3.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s4.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s1.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s2.dta"
			drop group
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s3.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s4.dta"

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/KappaUnanimity.dta",  replace




 ************************************
* kappa coders Unanimity
************************************

	
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u2s1.dta", clear

			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u2s2.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u2s3.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u2s4.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u4s1.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u4s2.dta"
			drop group
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u4s3.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/Kappa_u4s4.dta"



			* no instances of q7==1
forvalues i=1(1)6 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u2s1"
	g kap`i'u2s1 = r(kappa)
}
kap playeru_v81 playeru_v82 if session=="u2s1"
g kap8u2s1 = r(kappa)

			* no instances of q6==1
forvalues i=1(1)5 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u2s2"
	g kap`i'u2s2 = r(kappa)
}
forvalues i=7(1)8 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u2s2"
	g kap`i'u2s2 = r(kappa)
}

			* no instances of q1 or q7==1
forvalues i=2(1)6 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u2s3"
	g kap`i'u2s3 = r(kappa)
}
forvalues i=8(1)8 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u2s3"
	g kap`i'u2s3 = r(kappa)
}
			* all good for session 4
forvalues i=1(1)8 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u2s4"
	g kap`i'u2s4 = r(kappa)
}


			* all good for session 1
forvalues i=1(1)8 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u4s1"
	g kap`i'u4s1 = r(kappa)
}

			* no instances of q7==1
forvalues i=1(1)6 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u4s2"
	g kap`i'u4s2 = r(kappa)
}
kap playeru_v81 playeru_v82 if session=="u4s2"
g kap8u4s2 = r(kappa)


			* no instances of q1==1
forvalues i=2(1)8 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u4s3"
	g kap`i'u4s3 = r(kappa)
}
			* no instances of q6==1
forvalues i=1(1)5 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u4s4"
	g kap`i'u4s4 = r(kappa)
}
forvalues i=7(1)8 {
	kap playeru_v`i'1 playeru_v`i'2 if session=="u4s4"
	g kap`i'u4s4 = r(kappa)
}
		
	
	save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/KappaUnanimity.dta",  replace
	
		
		
		
		
		* no instances of q7==1
kap playeru_v11 playeru_v12 if session=="u2s1"
kap playeru_v21 playeru_v22 if session=="u2s1"
kap playeru_v31 playeru_v32 if session=="u2s1"
kap playeru_v41 playeru_v42 if session=="u2s1"
kap playeru_v51 playeru_v52 if session=="u2s1"
kap playeru_v61 playeru_v62 if session=="u2s1"
*kap playeru_v71 playeru_v72 if session=="u2s1"
kap playeru_v81 playeru_v82 if session=="u2s1"

			* no instances of q6==1
kap playeru_v11 playeru_v12 if session=="u2s2"
kap playeru_v21 playeru_v22 if session=="u2s2"
kap playeru_v31 playeru_v32 if session=="u2s2"
kap playeru_v41 playeru_v42 if session=="u2s2"
kap playeru_v51 playeru_v52 if session=="u2s2"
*kap playeru_v61 playeru_v62 if session=="u2s2"
kap playeru_v71 playeru_v72 if session=="u2s2"
kap playeru_v81 playeru_v82 if session=="u2s2"



			* no instances of q1, q6==1
*kap playeru_v11 playeru_v12 if session=="m2s3"
kap playeru_v21 playeru_v22 if session=="u2s3"
kap playeru_v31 playeru_v32 if session=="u2s3"
kap playeru_v41 playeru_v42 if session=="u2s3"
kap playeru_v51 playeru_v52 if session=="u2s3"
kap playeru_v61 playeru_v62 if session=="u2s3"
*kap playeru_v71 playeru_v72 if session=="u2s3"
kap playeru_v81 playeru_v82 if session=="u2s3"



			
kap playeru_v11 playeru_v12 if session=="u2s4"
kap playeru_v21 playeru_v22 if session=="u2s4"
kap playeru_v31 playeru_v32 if session=="u2s4"
kap playeru_v41 playeru_v42 if session=="u2s4"
kap playeru_v51 playeru_v52 if session=="u2s4"
kap playeru_v61 playeru_v62 if session=="u2s4"
kap playeru_v71 playeru_v72 if session=="u2s4"
kap playeru_v81 playeru_v82 if session=="u2s4"




kap playeru_v11 playeru_v12 if session=="u4s1"
kap playeru_v21 playeru_v22 if session=="u4s1"
kap playeru_v31 playeru_v32 if session=="u4s1"
kap playeru_v41 playeru_v42 if session=="u4s1"
kap playeru_v51 playeru_v52 if session=="u4s1"
kap playeru_v61 playeru_v62 if session=="u4s1"
kap playeru_v71 playeru_v72 if session=="u4s1"
kap playeru_v81 playeru_v82 if session=="u4s1"


			* no instances of q7==1

kap playeru_v11 playeru_v12 if session=="u4s2"
kap playeru_v21 playeru_v22 if session=="u4s2"
kap playeru_v31 playeru_v32 if session=="u4s2"
kap playeru_v41 playeru_v42 if session=="u4s2"
kap playeru_v51 playeru_v52 if session=="u4s2"
kap playeru_v61 playeru_v62 if session=="u4s2"
*kap playeru_v71 playeru_v72 if session=="u4s2"
kap playeru_v81 playeru_v82 if session=="u4s2"



			* no instances of q1==1
*kap playeru_v11 playeru_v12 if session=="u4s3"
kap playeru_v21 playeru_v22 if session=="u4s3"
kap playeru_v31 playeru_v32 if session=="u4s3"
kap playeru_v41 playeru_v42 if session=="u4s3"
kap playeru_v51 playeru_v52 if session=="u4s3"
kap playeru_v61 playeru_v62 if session=="u4s3"
kap playeru_v71 playeru_v72 if session=="u4s3"
kap playeru_v81 playeru_v82 if session=="u4s3"


			* no instances of q6==1
kap playeru_v11 playeru_v12 if session=="u4s4"
kap playeru_v21 playeru_v22 if session=="u4s4"
kap playeru_v31 playeru_v32 if session=="u4s4"
kap playeru_v41 playeru_v42 if session=="u4s4"
kap playeru_v51 playeru_v52 if session=="u4s4"
*kap playeru_v61 playeru_v62 if session=="u4s4"
kap playeru_v71 playeru_v72 if session=="u4s4"
kap playeru_v81 playeru_v82 if session=="u4s4"







************************************
* merge with data
************************************

forvalues j=2(2)4 {	
		forvalues k=1(1)4 {

import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/chatdata_48and96.xlsx", sheet(M`j'_s`k') firstrow clear
keep Sessioncode GroupId
rename GroupId group
rename Sessioncode sessioncode

sort group
by group: gen num=_n
keep if num==1
drop num

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/m`j's`k'.dta"
drop _merge
sort sessioncode group
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/m`j's`k'.dta", replace
		}
		}
		
forvalues j=4(1)4 {	
		forvalues k=3(1)4 {

import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/chatdata_48and96.xlsx", sheet(U`j'_s`k') firstrow clear
keep Sessioncode GroupId
rename GroupId group
rename Sessioncode sessioncode


sort group
by group: gen num=_n
keep if num==1
drop num

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta"
drop _merge
sort sessioncode group
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta", replace
		}
		}
		

******** Multistage sessions U2s1 and U2s2**********
			
import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/session1_070212.chatswithmembers.xlsx", firstrow clear
keep matchId groupId member1Id member2Id member3Id
rename matchId matchid
rename groupId groupid
rename groupid group
rename member1Id memberid1
rename member2Id memberid2
rename member3Id memberid3
g m="m"
g g="g"
egen groupid = concat(m matchid g group)
drop m g 
bysort groupid: gen num=_n
keep if num==1
drop num
*reshape long memberid, i(groupid) j(number)
*drop number
rename group groupbis
rename groupid group
sort group

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s1.dta"
drop if _m!=3
drop _m
g sessioncode="m1"
egen memberidmax = rowmax(memberid1 memberid2 memberid3)
rename matchid round
replace round=round+1
drop groupbis
rename group groupbis
sort round memberidmax
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s1.dta", replace

import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/session1_070212.chatswithmembers.xlsx", firstrow clear
keep matchId groupId member1Id member2Id member3Id
rename matchId matchid
rename groupId groupid
rename groupid group
rename member1Id memberid1
rename member2Id memberid2
rename member3Id memberid3
g m="m"
g g="g"
egen groupid = concat(m matchid g group)
drop m g 
bysort groupid: gen num=_n
keep if num==1
drop num
*reshape long memberid, i(groupid) j(number)
*drop number
rename group groupbis
rename groupid group
sort group

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s2.dta"
drop if _m!=3
drop if group=="m9g3"
drop _m
g sessioncode="m2"
egen memberidmax = rowmax(memberid1 memberid2 memberid3)
rename matchid round
replace round=round+1
drop groupbis
rename group groupbis
sort round memberidmax
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s2.dta", replace
** problem with m9g3 that exists in the u2s2 sheet...



******** Multistage sessions U2s3 and U2s4********** 
import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/session2_070213.chatswithmembers.xlsx", firstrow clear
keep matchId groupId member1Id member2Id member3Id
rename matchId matchid
rename groupId groupid
rename groupid group
rename member1Id memberid1
rename member2Id memberid2
rename member3Id memberid3
g m="m"
g g="g"
egen groupid = concat(m matchid g group)
drop m g 
bysort groupid: gen num=_n
keep if num==1
drop num
*reshape long memberid, i(groupid) j(number)
*drop number
rename group groupbis
rename groupid group
sort group

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s3.dta"
g sessioncode="m3"
egen memberidmax = rowmax(memberid1 memberid2 memberid3)
rename matchid round
replace round=round+1
sort round memberidmax
drop groupbis
rename group groupbis
drop if _m!=3
drop _m
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s3.dta", replace



import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/session2_070213.chatswithmembers.xlsx", firstrow clear
keep matchId groupId member1Id member2Id member3Id
rename matchId matchid
rename groupId groupid
rename groupid group
rename member1Id memberid1
rename member2Id memberid2
rename member3Id memberid3
g m="m"
g g="g"
egen groupid = concat(m matchid g group)
drop m g 
bysort groupid: gen num=_n
keep if num==1
drop num
*reshape long memberid, i(groupid) j(number)
*drop number
rename group groupbis
rename groupid group
sort group

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s4.dta"
g sessioncode="m4"
egen memberidmax = rowmax(memberid1 memberid2 memberid3)
rename matchid round
replace round=round+1
sort round memberidmax
drop groupbis
rename group groupbis
drop if _m!=3
drop _m
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u2s4.dta", replace
 
		

		
******** Multistage sessions U4s1 and U4s2********** 
import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/session1_070215.chatswithmembers.xlsx", firstrow clear
keep matchId groupId member1Id member2Id member3Id
rename matchId matchid
rename groupId groupid
rename groupid group
rename member1Id memberid1
rename member2Id memberid2
rename member3Id memberid3
g m="m"
g g="g"
egen groupid = concat(m matchid g group)
drop m g 
bysort groupid: gen num=_n
keep if num==1
drop num
*reshape long memberid, i(groupid) j(number)
*drop number
rename group groupbis
rename groupid group
sort group

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s1.dta"
g sessioncode="m5"
egen memberidmax = rowmax(memberid1 memberid2 memberid3)
rename matchid round
replace round=round+1
sort round memberidmax
drop groupbis
rename group groupbis
drop if _m!=3
drop _m
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s1.dta", replace




import excel "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/session1_070215.chatswithmembers.xlsx", firstrow clear
keep matchId groupId member1Id member2Id member3Id
rename matchId matchid
rename groupId groupid
rename groupid group
rename member1Id memberid1
rename member2Id memberid2
rename member3Id memberid3
g m="m"
g g="g"
egen groupid = concat(m matchid g group)
drop m g 
bysort groupid: gen num=_n
keep if num==1
drop num
*reshape long memberid, i(groupid) j(number)
*drop number
rename group groupbis
rename groupid group
sort group

merge 1:1 group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s2.dta"
g sessioncode="m6"
egen memberidmax = rowmax(memberid1 memberid2 memberid3)
rename matchid round
replace round=round+1
sort round memberidmax
drop groupbis
rename group groupbis
drop if _m!=3
drop _m
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u4s2.dta", replace

			
		
		  
		
******************
**** Merge with rest of data...	
******************
clear

		forvalues j=2(2)4 {	
		forvalues k=1(1)4 {
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata.dta", clear	
g multiplier = 2 if pietreatment==48
replace multiplier = 4 if pietreatment==96
replace multiplier = 1 if pietreatment==24
keep if votingtreatment=="majority" & multiplier==`j' & session==`k'
sort sessioncode group			
merge m:1 sessioncode group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/m`j's`k'.dta"
drop _merge
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_m`j's`k'.dta", replace
		}
		}

		forvalues j=4(2)4 {	
		forvalues k=3(1)4 {
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata.dta", clear	
g multiplier = 2 if pietreatment==48
replace multiplier = 4 if pietreatment==96
replace multiplier = 1 if pietreatment==24
keep if votingtreatment=="unanimity" & multiplier==`j' & session==`k'
sort sessioncode group		
merge m:1 sessioncode group using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta
drop _merge
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_u`j's`k'.dta", replace
		}
		}


		forvalues j=2(2)2 {	
		forvalues k=1(1)4 {
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata.dta", clear	
g multiplier = 2 if pietreatment==48
replace multiplier = 4 if pietreatment==96
replace multiplier = 1 if pietreatment==24
keep if votingtreatment=="unanimity" & multiplier==2 & session==`k'
sort sessioncode round group
by sessioncode round group: egen memberidmax = max(memberid)
sort sessioncode round memberidmax
merge m:1 sessioncode round memberidmax using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta
drop _m
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_u`j's`k'.dta", replace
		}
		}
		
		
		forvalues j=4(2)4 {	
		forvalues k=1(1)2 {
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata.dta", clear	
g multiplier = 2 if pietreatment==48
replace multiplier = 4 if pietreatment==96
replace multiplier = 1 if pietreatment==24
keep if votingtreatment=="unanimity" & multiplier==4 & session==`k'
sort sessioncode round group
by sessioncode round group: egen memberidmax = max(memberid)
sort sessioncode round memberidmax
merge m:1 sessioncode round memberidmax using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/u`j's`k'.dta
drop _m
save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_u`j's`k'.dta", replace
		}
		}



clear
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_m2s1.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_u2s1.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_m4s1.dta"
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_u4s1.dta"

forvalues j=2(2)4 {	
		forvalues k=2(1)4 {
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_u`j's`k'.dta"
		}
}

forvalues j=2(2)4 {	
		forvalues k=2(1)4 {
			append using "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/Raw Data Chat Coders/NotForSubmission/alldata_withchatcoded_m`j's`k'.dta"
		}
}

save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata_withchatcoded.dta", replace	



**************
** Create withchatcoded_ready file
use "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata_withchatcoded.dta", clear	


*Majority & Unanimity (v="valueq")
* big pie discussion: v2
*risk game terminated: v3
* support to wait for big pie/delay" v8
* threat to vote no if small budget v6
* threat to vote no if not equal v5
* equality v4
* v10 talk about equality within MWC
*v11 talk about unequal split within mwc.


g totaldelay = (delay==1 | pass==0)


g relevant_talk = (valueq1!=. | valueq2!=. | valueq3!=. | valueq4!=. | valueq5!=. | valueq6!=. | valueq7!=. | valueq8!=. | valueq9!=. | valueq10!=. | valueq11!=.)

g didnottalk = (valueq1==. & valueq2==. &  valueq3==. &  valueq4==. &  valueq5==. &  valueq6==. &  valueq7==. &  valueq8==. &  valueq9==. &  valueq10==. &  valueq11==.)


g bigpietalk = (valueq2==1 & agreev2==1)
replace bigpietalk =. if valueq2==. /// valueq2 is missing when people didn't talk at all.

g yestodelaytalk = (valueq8==1 & agreev8==1)
replace yestodelaytalk =. if valueq8==.

g equalitytalk = (valueq4==1 & agreev4==1)
replace equalitytalk =. if valueq4==.

g threatnounequaltalk = (valueq5==1 & agreev5==1)
replace threatnounequaltalk =. if valueq5==.

g threatnosmallbudgettalk = (valueq6==1 & agreev6==1)
replace threatnosmallbudgettalk =. if valueq6==.

g riskterminationtalk = (valueq3==1 & agreev3==1)
replace riskterminationtalk =. if valueq3==.

g equalityinmwctalk = (valueq10==1 & agreev10==1)
replace equalityinmwctalk =. if valueq10==.

g unequalinmwctalk = (valueq11==1 & agreev11==1)
replace unequalinmwctalk =. if valueq11==.



save "/Users/cjt16/Dropbox/CMH/Submissions/AEJMicro/Publication Materials/Data Files 2 (corrected)/alldata_withchatcoded_ready.dta", replace

