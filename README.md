# README for replication files 

Article: “Nudging Null Effects: Reminders Did Not Induce Primary Health Care Utilization” (Haaga, Sääksvuori, and Tervola, 2023)

Version 1 (latest), populated pre-analysis plan, and pre-analysis plan: https://osf.io/g72b5/

Date: March 2023 <br>


## The contents

This replication folder contains the statistical programs used to clean the data and conduct the analyses. The root of the replication folder includes a folder for each publicly available version of the text, from the registered pre-analysis plan to the final research report. Besides the statistical programs, we include in subfolders all result tables and figures that do not appear in the paper nor in its appendix.

## Access to the data

Getting access to the trial data is crucial for any replication attempt. There is one clearly restrictive factor making replications more difficult (or even impossible after our data permits expire): the data permits are only temporary in Finland. Currently, our permits are valid to the end of 2024 after which all data will be deleted from the Fiona remote access environment. We are not sure how the data permit process would work with respect to the trial data. If you are planning a replication, have enough resources and are willing to cooperate, please contact us at least six months before our data permits expire so that we will have time to renew them and clarify to ourselves how the data permit process should be done.

For the other sensitive datasets, the procedures are more clear and established. Two data permit applications must be sent: one to Findata and another to Statistics Finland. The former grants permits to Finnish healthcare data while the latter provides the socioeconomic and sociodemographic data used in the study. The higher the quality of the initial submission, the shorter it takes to get an approval. Once the data permits are approved, the data controllers (THL and Kela in this study) transfer their data to Statistics Finland for pseudonymization. Researchers get access to the pseudonymized data for a fixed period of time via Fiona, a remote access system provided by Statistics Finland.

Each case is naturally different, but we would expect the entire process from application submission to the point where the data work can begin to take approximately a year. However, we would not characterize the process as easy and swift for first-timers. 

There are other barriers to access besides the wait times and filling the application forms correctly. Accessing the data via a remote access system while being in another country is counted as a transfer of personal data, which is regulated by Finland and the EU. The application process, data extractions, and the remote access use naturally contain costs.

## Reproducing the analysis

The biggest threat to reproducibility is getting access to the trial data. Once our temporary data permits expire (currently at the end of 2024), all data will be deleted. The second threat to being able to exactly reproduce the results is that the registries we use are continuously updated. The data from 2021 extracted in 2022 may be different from the data from 2021 extracted in 2023. We do not extract the raw data from databases. Instead, this is done by the data controllers (THL and Kela). However, the differences should be small and the estimates should be robust to the underlying sample changing a little bit.

### Software

Several statistical software are offered in the Fiona remote access system. We first use SAS 9.4 to extract smaller subsets of the larger data tables. Most of the data cleaning, construction, and analyses are conducted with R 4.0.5. 

### Folder structure

Suppose we want to reproduce the placebo results in the pre-analysis plan (PAP). First, create a folder called *ASMA4* to the root of the work directory (W:/ASMA4). Then, copy all the files in the *pap* folder of this replication folder to the *ASMA4* folder in Fiona, and create an R project called *ASMA4* to the root of that folder.

The root of the *ASMA4* folder then contains master scripts for R and SAS codes and File *sessionInfo.txt* that list the R packages and their versions used in analysis. The master scripts contain a lot of important information: 1) the list of all scripts, 2) the order in which the scripts should be run, 3) inputs and outputs of each script, and 4) a short comment on what each script does. Furthermore, they report the running time of each script.

Folder *data* includes the R and SAS scripts used to clean and process the data. Raw, publicly available data are in Subfolder *raw*. Intermediate, processed datasets should be in Folder *interim* while the final, analysis-ready datasets should be in Folder *clean*. Folder *analysis* contains the R scripts for analyses and Subfolders *figures* and *tables* for results. Make sure that all these folders are created.

### File paths

All file paths appearing at the top of each script are absolute file paths. Note that you will have to edit the file paths to the raw microdata depending on where your data is in the data directory (D:/). Once these paths are correct and you have created the folder structure described above, the rest of the absolute file paths should work fine.

### Implementation

SAS scripts need to be run before R scripts. Ideally, the analysis could be reproduced by simply running the following scripts in the order:
1. W:/ASMA4/master_script_sas.sas
2. W:/ASMA4/master_script_r.R

However, when running the scripts at the first time, we highly propose running each of them script by script, guided by the above master scripts.
