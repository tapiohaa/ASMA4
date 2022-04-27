
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

* Primary care contacts and referrals;
libname hilmot "D:\d66\external\THL_aineisto_2019_2020";

* Data on drug prescriptions;
libname drugs "D:\d66\external\laakemaaraykset2018_20";

* FOLK modules basic, income, and family;
libname fbasic "D:\ready-made\FOLK_perus_11a";
libname fincome "D:\ready-made\FOLK_tulo_11a";
libname ffamily "D:\ready-made\FOLK_perh_11a";

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
%inc storage("0_folk_data.sas");					* Approximately 9 minutes;
* Inputs: folk_20112020_tua_perus21tot_1 + folk_20112019_tua_tulo21tot_1 + ;
*		folk_20112020_tua_perh21tot_1 (.sas7bdat);
* Output: folk_data_20xx where xx in (19) (.csv);

* Extract primary care contacts;
%inc storage("0_pc_contacts.sas");						* Approximately 18 minutes;
* Inputs: thl4768_avohilmo_20XX_Y_s where xx in (19:20) and y in (1:2) (.sas7bdat);
* Outputs: pc_contacts_20xx where xx in (19:20) (.csv);

* Extract primary care visits;
%inc storage("0_pc_visits.sas");						* Approximately XX minutes;
* Inputs: tutkpalv_u1418_ammatti + tutkpalv_u1418_ammattioikeudet + ;
*			thl4768_avohilmo_20XX_Y_s where xx in (19:20) and y in (1:2) (.sas7bdat);
* Outputs: pc_visits_20xx where xx in (19:20) (.csv);

* Extract data on drug prescriptions;
%inc storage("0_drug_prescriptions.sas");			* Approximately 7 minutes;
* Inputs: t141_522_2021_lm_20XX_Y_s where XX in 19:20 and Y in 1:2 (.sas7bdat);
* Outputs: prescriptions_Y_20XX where XX in 19:20 and Y in 1:2 (.csv);

* Extract data on referrals to specialized healthcare;
%inc storage("0_referrals.sas");					* Approximately 11 minutes;
* Inputs: tutkpalv_1418_thl_hilmoX_s where X in (1:2) (.sas7bdat);
* Outputs: referrals_YY where YY in (19:20) (.csv);

*/


* End;


