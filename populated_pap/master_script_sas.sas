
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         SAS script master_script_sas.sas      ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT           ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This proposes an order with which to run SAS scripts. 

Note: We do not recommend running all the scripts at once as the SAS work
	library should be (manually) made empty between runs.

Libnames: */

* Folders for processed datasets;
libname raw "W:\ASMA4\data\raw";
libname interm "W:\ASMA4\data\interim";

* RCT data;
libname rct "D:\d66\external";

* Primary care contacts and referrals; drug prescriptions;
libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

* FOLK modules basic, income, and family;
libname fbasic "D:\ready-made\CONTINUOUS\FOLK_PERUS_C";
libname fincome "D:\ready-made\CONTINUOUS\FOLK_TULO_C";
libname ffamily "D:\ready-made\CONTINUOUS\FOLK_PERH_C";

/*###
###*/

* Remove comment symbols before running the scripts one at a time;

/* 

filename storage "W:\ASMA4\data";

* Save the RCT data as csv;
%inc storage("0_rct_data.sas");						* <0.5 minutes;
* Inputs: kokeiludata_uusi_s (.sas7bdat);
* Output: rct (.csv);

* Extract socioeconomic data;
%inc storage("0_folk_data.sas");					* Approximately 5 minutes;
* Inputs: folk_perus_2020_1 + folk_tulo_2020_1 + ;
*		folk_20112020_tua_perh21tot_1 (.sas7bdat);
* Output: folk_data_20xx where xx in (20) (.csv);

* Extract primary care contacts;
%inc storage("0_pc_contacts.sas");						* Approximately 57 minutes;
* Inputs: tutkpalv_1418_avohilmo_2021_Y_s where y in (1:2) (.sas7bdat);
* Outputs: pc_contacts_20xx where xx in (21:22) (.csv);

* Extract primary care visits;
%inc storage("0_pc_visits.sas");						* Approximately 54 minutes;
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			tutkpalv_1418_avohilmo_2021_Y_s where y in (1:2) (.sas7bdat);
* Outputs: pc_visits_20xx where xx in (21:22) (.csv);

* Extract data on drug prescriptions;
%inc storage("0_drug_prescriptions.sas");			* Approximately 6 minutes;
* Inputs: lm_72_21_01_06_s + lm_72_21_07_12_s + lm_72_22_01_05_s (.sas7bdat);
* Outputs: prescriptions_X where X in 1:3 (.csv);

* Extract data on referrals to specialized healthcare;
%inc storage("0_referrals.sas");					* Approximately 8 minutes;
* Inputs: tutkpalv_1418_hilmo_2021_s (.sas7bdat);
* Outputs: referrals_20YY where YY in (21:22) (.csv);

*/


* End;


