
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            SAS script 0_pc_visits.sas         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Extract primary care visits from Avohilmo for 2021-2022.

Inputs: tutkpalv_u1418_ammatti tutkpalv_u1418_ammattioikeudet
		tutkpalv_1418_avohilmo_2021_Y_s where y in (1:2)
		
Output: pc_visits_20xx where xx in (21:22)

Libnames: */

libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Load classifications for doctors and nurses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* TK ammattiluokitus 2001;
data tk_codes(drop=avo_raportointiryhma_nimi);
	set hilmo.tutkpalv_u1418_ammatti;
	where avo_raportointiryhma_koodi in ('10','30'); * Doctors and nurses;
	rename tarkin_taso_koodi = ammatti;
run;

proc sort data=tk_codes;
	by avo_raportointiryhma_koodi;
run;

* Valvira ammattioikeudet 2008;
data valv_codes(drop=avo_raportointiryhma_nimi);
	set hilmo.tutkpalv_u1418_ammattioikeudet;
	where avo_raportointiryhma_koodi in ('10','30'); * Doctors and nurses;
	rename ammattioikeus_koodi = kaynti_ammattioikeus;
run;

proc sort data=valv_codes;
	by avo_raportointiryhma_koodi;
run;

/*
TK ammattiluokitus 2001:
Doctors: '222','2221','22211','22212','22213'
Nurses: '323','3231','32311','32312','32313','32314','32315','3232'

Valvira ammattioikeudet 2008
Doctors: '000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812'
Nurses: '100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803'
*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read and mutate data from 2021-2022. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/


%MACRO read_data;
%DO i = 1 %TO 2;

data visits_&i;
	set hilmot.tutkpalv_1418_avohilmo_2021_&i._s;

	/* Not all data are read:
		1) IDs must be observed.
		2) Time stamps must be observed.
		3) We only take outpatient care (T11).
		4) Of the contacts, we take those visits where the client physically
			visited professional.
		5) The contact was not cancelled. */

	where not missing(shnro) 
		and not missing(kaynti_alkoi) 
		and kaynti_palvelumuoto = 'T11'
		and kaynti_yhteystapa = 'R10'
		and missing(peruutus_ajankohta) and missing(peruutus_syy);
	

	/* Create variable profession such that:
		-1 = other than doctors, nurses and public health nurses + missing values
		1 = doctors
		2 = nurses and public health nurses */

	ammatti = put(kaynti_ammatti, 6. -L); 

	if ammatti in ('222','2221','22211','22212','22213') or 
		kaynti_ammattioikeus in ('000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812') 
		then profession = 1;
	else if ammatti in ('323','3231','32311','32312','32313','32314','32315') or
		kaynti_ammattioikeus in ('100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803') 
		then profession = 2;
	else if missing(ammatti) and missing(kaynti_ammattioikeus) then profession = -1;
	else profession = -1;

	
	* Create a variable for weekday, month and year of the visit;
	visits_date = input(substr(strip(kaynti_alkoi), 1, 10), DDMMYY10.);
	month = month(visits_date);
	year = year(visits_date);
	day = day(visits_date);

	
	* Create a variable that has value 1 for curative care, 0 for preventive care;
	* and -1 or missing type;
	if kaynti_luonne = 'SH' then curative = 1;
	else if kaynti_luonne = 'TH' then curative = 0;
	else curative = -1;


	* Create a dummy for whether the date of the patient contacting the health center;
	* is observed;
	if not missing(yhteydenotto_ajankohta) then contact_date_d = 1;
	else contact_date_d = 0;
	
	* Create a dummy for whether the date of the triage is observed;
	if not missing(hoidontarve_ajankohta) then triage_date_d = 1;
	else triage_date_d = 0;
	
	* Keep only relevant variables;
	keep shnro kaynti_alkoi curative profession month year day
			palveluntuottaja palveluntuottaja_yksikko contact_date_d triage_date_d;

	* Rename columns;
	rename shnro=id kaynti_alkoi=date palveluntuottaja=topi_code
			palveluntuottaja_yksikko=sote_code;

	length profession curative month year day contact_date_d triage_date_d 3;

run;

%END;
%MEND read_data;

%read_data;


* Concatenate; 

data visits;
	set visits_1 visits_2;
	where year in (2021:2022);
run;


* Take only unique rows;

proc sort data=visits NODUPRECS;
	by _all_;
run;

data visits;
	set visits;
	drop date;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO save_data;
%DO i = 2021 %TO 2022;

proc export data=visits(where= (year=&i.))
	outfile= "W:\ASMA4\data\interim\pc_visits_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
