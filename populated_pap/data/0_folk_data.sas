
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           SAS script 0_folk_data.sas          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: RCT              ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This script merges FOLK modules 'basic information', 'income', and 'family'. 

Inputs: folk_perus_2020_1 + folk_tulo_2020_1 + folk_20112020_tua_perh21tot_1
Output folk_data_20XX where XX in 19

Libnames: */

libname fbasic "D:\ready-made\CONTINUOUS\FOLK_PERUS_C";
libname fincome "D:\ready-made\CONTINUOUS\FOLK_TULO_C";
libname ffamily "D:\ready-made\CONTINUOUS\FOLK_PERH_C";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

options nolabel;

* Read the three datasets and keep only relevant variables + sort them by id;

data basic;
	set fbasic.folk_perus_2020_1;
	where vuosi IN (2020);


	* Feature engineering;

	if syntyp2 in ('11', '12') then origin_finn=1; 
		else origin_finn=0;
	if kieli_k='1' then language_fi=1; 
		else language_fi=0; 
	if sivs='2' then relationship=1; 
		else relationship=0;
	if sivs='5' then widowed=1; 
		else widowed=0;
	if not missing(tyke) then unemployment=1; 
		else unemployment=0;
	if tyke >=7 then unemployment_long=1;  				  		
		else unemployment_long=0;
	if lkm_k > 0 then children=1; 
		else children=0;
	if ututku_aste in ('6','7','8') then educ_university=1;  	
		else educ_university=0;
	if taty='3' then apartment=1; 
		else apartment=0;
	
	* Keep only relevant variables;
	keep vuosi shnro sukup kunta31_12 ika 
		origin_finn language_fi 
		relationship widowed unemployment
		unemployment_long children educ_university apartment;

	* Rename columns;
	rename shnro=id kunta31_12=municipality ika=age 
			sukup=gender vuosi=year;

	length vuosi origin_finn language_fi 
			relationship widowed unemployment
			unemployment_long children educ_university 
			apartment 3;

run;

proc sort Data=basic;
	by id year;
run;


data income;
	set fincome.folk_tulo_2020_1;
	where vuosi IN (2020);

	* Feature engineering;
	taxable_income = svatva + svatvp;

	* Keep only relevant variables;
	keep vuosi shnro kturaha toimtu elatulo taxable_income saiprva;

	* Rename columns;
	rename shnro=id kturaha=income_disp vuosi=year toimtu=social_assistance 
			elatulo=pension_income saiprva=sickness_allowance_e;

	length vuosi 3;

run;

proc sort Data=income;
	by id year;
run;


data family;
	set ffamily.folk_20112020_tua_perh21tot_1;
	where vuosi IN (2020);

	* Keep only relevant variables;
	keep shnro petu pekoko vuosi;

	* Rename columns;
	rename shnro=id petu=family_id pekoko=family_size vuosi=year;

	length vuosi pekoko 3;

run;

proc sort Data=family;
	by id year;
run;


* Next, merge all three datasets by id;
* If family code is missing or if the person has a single-person;
* family, use personal identifier as the family code;

data folk_data;
	merge basic income family;
	by id year;
run;

* if family id is not observed, use person id;

proc sql;
	alter table folk_data
	modify family_id char(16) format=$16.;
quit;

data folk_data;
	set folk_data;
	if family_id=. then family_id=id;
	*'00000000' = Person does not belong to the family population but;
	* does have a adopted/biological child;
	if family_id='00000000' then family_id=id; 
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Compute income deciles (equivalized disposable income).  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, calculate for each family the sum of disposable income;
* Then, calculate the equivalized disposable income;
* Thus, the level is family code;

proc sort data=folk_data;
	by family_id year;
run;

data folk_data;
	set folk_data;
	where not missing(family_id);
run;

proc sql;
	create table family_sum as
	select year, family_id, sum(income_disp) as income_disp_family,
		sum(social_assistance) as social_assistance_family,
		sum(income_disp) / (1+sum(age<14)*0.3+0.5*(count(age)-1-sum(age<14))) as income_eq,
		count(age) as size
	from folk_data
	group by family_id, year;
quit;

data family_sum;
	set family_sum;
	drop income_disp_family size;
run;

* Take only unique rows;

proc sort data=family_sum NODUPRECS;
	by _all_;
run;


* Sort folk_data and family_sum, and merge the latter to the former;

proc sort Data=family_sum;
	by family_id year;
run;
	
data folk_data;
	merge folk_data family_sum;
	by family_id year;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Feature engineering.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

data folk_data;
	set folk_data;
	where not missing(age);

	* Pensioners;
	* (The share of pension income on the total taxable income is at least 0.6);
	if taxable_income >0 then ratio = (pension_income / taxable_income);
	else if taxable_income=. and income_disp >0 
		then ratio = (pension_income / income_disp); else ratio=0;
	if ratio >=0.6 then pension_d=1; else pension_d=0;

	* Social assistance recipients;
	* (A family member has received social assistance);
	if social_assistance_family >0 then social_assistance_d=1; 
		else social_assistance_d=0; 

	* The rest of the feature engineering;
	income_disp = round(income_disp);
	if sickness_allowance_e > 0 then sickness_allowance=1; 
		else sickness_allowance=0;

	drop social_assistance_family taxable_income pension_income ratio 
		social_assistance family_size family_id sickness_allowance_e;

	length pension_d social_assistance_d sickness_allowance 3;

run;

proc sort data=folk_data;
	by id year;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Use the 2022 municipal boundaries.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* There were 27 municipal mergers between 2012 and 2022. Here, we take this into account by aggregating the data based on the 2020 municipal map;
* Replace the municipality number of a merged municipality by the number of the new municipality;

data folk_data;
	set folk_data;

	* The municipality mergers;

	if municipality='099' then municipality='214'; *2021;

	if municipality='911' then municipality='541'; *2020;

	if municipality='442' then municipality='051'; *2017;
	if municipality='174' then municipality='297';

	if municipality='164' then municipality='301'; *2016;
	if municipality='283' then municipality='098';
	if municipality='319' then municipality='783';
	if municipality='532' then municipality='398';

	if municipality='476' then municipality='297'; *2015;
	if municipality='413' then municipality='609';
	if municipality='838' then municipality='423';

	if municipality='863' then municipality='010'; *2013;
	if municipality='248' then municipality='260';
	if municipality='534' then municipality='297';
	if municipality='223' then municipality='444';
	if municipality='540' then municipality='444';
	if municipality='696' then municipality='491';
	if municipality='775' then municipality='491';
	if municipality='084' then municipality='564';
	if municipality='255' then municipality='564';
	if municipality='567' then municipality='564';
	if municipality='972' then municipality='564';
	if municipality='926' then municipality='678';
	if municipality='254' then municipality='790';
	if municipality='246' then municipality='740';
	if municipality='618' then municipality='740';
	if municipality='942' then municipality='905';

run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Save years separately;

%MACRO save_data;
%DO i = 2020 %TO 2020;

proc export data=folk_data(where= (year=&i.))
	outfile= "W:\ASMA4\data\interim\folk_data_&i..csv"
	dbms=csv;
run;

%END;
%MEND save_data;

%save_data;

* End;
