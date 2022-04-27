
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

Content: Extract data on contacts to primary care for 2019-2020.

Inputs: thl4768_avohilmo_20XX_Y_s where xx in (19:20) and y in (1:2)
		
Output: Output: pc_contacts_xx where xx in (19:20)

Libnames: */

libname hilmot "D:\d66\external\THL_aineisto_2019_2020";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and mutate data from 2019-2020. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* The first subset;

%MACRO read_data;
%DO i = 19 %TO 20;

data contacts_1_&i;
	set hilmot.thl4768_avohilmo_20&i._1_s;

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


* The second subset;

%MACRO read_data;
%DO i = 19 %TO 20;

data contacts_2_&i;
	set hilmot.thl4768_avohilmo_20&i._2_s;

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

data contacts_19;
	set contacts_1_19 contacts_2_19;
run;

data contacts_20;
	set contacts_1_20 contacts_2_20;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Create data for analyses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO contacts_main;

%DO i = 19 %TO 20;

* Take only unique id-datetime rows;

proc sort data=contacts_&i NODUPRECS;
	by _all_;
run;

data contacts_main_&i;
	set contacts_&i;
	drop date;
run;

* Save to cvs;

proc export data=contacts_main_&i
	outfile= "W:\ASMA4\data\interim\pc_contacts_20&i..csv"
	dbms=csv;
run;

%END;
%MEND contacts_main;

%contacts_main;

* End;
