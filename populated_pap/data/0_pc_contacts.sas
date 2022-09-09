
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###          SAS script 0_pc_contacts.sas         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Extract data on contacts to primary care for 2021-2022.

Inputs: tutkpalv_1418_avohilmo_2021_Y_s where y in (1:2)
		
Output: pc_contacts_20xx where xx in (21:22)

Libnames: */

libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and mutate data from 2021-2022. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/


%MACRO read_data;
%DO i = 1 %TO 2;

data contacts_&i;
	set hilmot.tutkpalv_1418_avohilmo_2021_&i._s;

	/* Not all data are read:
		1) IDs must be observed.
		2) Time stamps must be observed. */

	where not missing(shnro) 
		and not missing(yhteydenotto_ajankohta);
	
	* Create a variable for weekday, month and year of the visit;
	contact_date = input(substr(strip(yhteydenotto_ajankohta), 1, 10), DDMMYY10.);
	month = month(contact_date);
	year = year(contact_date);
	day = day(contact_date);

	* Keep only relevant variables;
	keep shnro yhteydenotto_ajankohta month year day
			palveluntuottaja palveluntuottaja_yksikko;

	* Rename columns;
	rename shnro=id yhteydenotto_ajankohta=date palveluntuottaja=topi_code
			palveluntuottaja_yksikko=sote_code;

	length month year day 3;

run;

%END;
%MEND read_data;

%read_data;


* Concatenate; 

data contacts;
	set contacts_1 contacts_2;
	where year in (2021:2022);
run;


* Take only unique rows;

proc sort data=contacts NODUPRECS;
	by _all_;
run;

data contacts;
	set contacts;
	drop date;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 2021 %TO 2022;

proc export data=contacts(where= (year=&i.))
	outfile= "W:\ASMA4\data\interim\pc_contacts_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
