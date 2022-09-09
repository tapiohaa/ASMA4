
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           SAS script 0_referrals.sas          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Read and mutate referrals to specialized healthcare from Hilmo.

Inputs: tutkpalv_1418_hilmo_2021_s
Output: Output: referrals_20YY where YY in (21:22)

Libnames: */

libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";
libname interm "W:\ASMA4\data\interim";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and mutate data on referrals to specialized healthcare. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, read the data;

options nolabel;

data refs;
	set hilmot.tutkpalv_1418_hilmo_2021_s;

	* IDs must be observed;
	* The date of arrival of the referral must be observed;
	where not missing(shnro) and not missing(lanttupva);
	
	* Create a variable for weekday, month and year of the arrival of the referral;
	date = datepart(input(lanttupva, anydtdtm.));
	year = year(date);
	month = month(date);
	day = day(date);

	* Create a dummy for whether we observe the date when the patient came in;
	if not missing(tupva) then contact_started = 1;

	* Create a dummy for whether we observe when the patient is discharged;
	if not missing(lpvm) then discharged = 1;
	
	* Keep only relevant variables;
	keep shnro lant year month day discharged contact_started;

	* Rename columns;
	rename shnro=id lant=ref_origin;

	length month year day discharged contact_started 3;

run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 2021 %TO 2022;

proc export data=refs(where= (year=&i.))
	outfile= "W:\ASMA4\data\interim\referrals_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
