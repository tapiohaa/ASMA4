
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###        SAS script 0_drug_prescriptions.sas    ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: RCT              ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This script extracts data on drug prescriptions and saves them to csv. 

Inputs: lm_72_21_01_06_s + lm_72_21_07_12_s + lm_72_22_01_05_s
Output: prescriptions_X where X in 1:3

Libnames: */

libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

data presc_1;
	set hilmot.lm_72_21_01_06_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro);

	keep shnro date_pk med_init_code atc_code sector;
run;

data presc_2;
	set hilmot.lm_72_21_07_12_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro);

	keep shnro date_pk med_init_code atc_code sector;
run;

data presc_3;
	set hilmot.lm_72_22_01_05_s;

	* Extract only prescriptions, not cancellations or edits;
	where doc_type_code = 1 and not missing(shnro);

	keep shnro date_pk med_init_code atc_code sector;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 1 %TO 3;

proc export data=presc_&i.
	outfile= "W:\ASMA4\data\interim\prescriptions_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
