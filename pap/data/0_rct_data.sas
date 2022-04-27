
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            SAS script 0_rct_data.sas          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###         Funded Primary Care: RCT              ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Save the RCT randomization data as a csv. 

Inputs: kokeiludata_uusi_s
Output rct_data

Libnames: */

libname rct "D:\d66\external";

/*###
###*/

data rct;
	set rct.kokeiludata_uusi_s;
	rename Kotikunta=municipality_rct;
run;

proc export data=rct
	outfile= "W:\ASMA4\data\interim\rct_data.csv"
	dbms=csv;
run;

* End;
