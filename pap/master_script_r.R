
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script master_script_r.R         ###
###                Replication file               ###
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Implement the R-scripts required to replicate the analysis.

# R version 4.0.5, RStudio version 1.2.5033.
# Running time approximately 6 hours.
rm(list=ls())

# First, make sure that the SAS scripts (listed in master_script_sas.sas) 
# have run successfully.


# To install packages from a CRAN mirror repository in FIONA:
# 1) Create .Rprofile file to the root where the project is:
#     local({
#       r <- getOption("repos")
#       r["CRAN"] <- "https://cran.isaacus.local/"
#       options(repos = r)
#     })
# 2) Restart RStudio. Now you can load CRAN packages by install.packages():
# 3) Use the library() function to load packages.

# The packages loaded are listed below.
library(data.table)       # Mutating data.
library(readxl)           # Reading xlsx files. 
library(ggplot2)          # Plotting data. 
library(patchwork)        # Print multiple plots into same figure. 
library(openxlsx)         # Save as excel file. 
library(smd)              # Compute standardized mean differences. 
library(stargazer)        # Save as tex file. 
library(lfe)              # Linear fixed effects estimation. 
library(broom)            # Statistical objects into tidy tibbles. 
library(MatchIt)          # Matching methods.
library(mlr3)             # ML ecosystem for R.
library(mlr3tuning)       # Tuning ML models.
library(mlr3learners)     # Several ML algorithms.
library(ranger)           # Random Forest.
library(xgboost)          # XGBoost.
library(GenericML)        # Generic machine learning inference.

writeLines(capture.output(sessionInfo()), 'sessionInfo.txt')


###
###

Sys.time()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 0) Preliminary steps. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Separate publicly-funded primary care from private outpatient care:
source(file="W:/ASMA4/data/0_public_pc.R")
#     Inputs: sote_munies_2021.csv + topi_unit_register.csv + 
#             sote_organisation_register.xlsx + visits_XX.csv (XX in 19:20)
#     Outputs: visits_public_XX.rds (XX in 19:20)
# Running time 3 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Create analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Link different datasets:
source(file="W:/ASMA4/data/1_link_datasets.R")
#     Input: rct_data.csv + folk_data_2019.csv +
#           pc_contacts_public_20XX.rds (XX in 19:20) +
#           pc_visits_public_20.rds (XX in 19:20) +
#           prescriptions_X_20YY.csv (X in 1:2 and YY in 19:20) + 
#           referrals_20XX.csv (XX in 19:20)
#     Outputs: data_population.rds + data_ids.rds + data_aggr.rds
# Running time approximately 14 minutes.


# Analyze data quality, and create analysis data:
source(file="W:/ASMA4/data/1_data_overview.R")
#     Input: data_population.rds + data_ids.rds + data_aggr.rds
#     Outputs: outcome_trends_main.pdf + outcome_trends_spillovers.pdf +
#           analysis_data_aggr.rds 
# Running time 2 minutes.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Analyses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Functions that will be used in many R scripts.
#source(file="W:/ASMA4/analysis/2_balance_tests.R")
#     Functions: extract.data() + save.table()


# Verification of randomization: balance tests:
source(file="W:/ASMA4/analysis/2_balance_tests.R")
#     Input: analysis_data_aggr.rds + 2_shared_functions.R
#     Outputs: covariate_balance(xlsx + tex + rds) + 
#             covariate_balance_tests_main(xlsx + tex + rds) +
#             covariate_balance_tests_supp(xlsx + tex + rds)
# Running time 2 minutes.


# Estimate the main results:
source(file="W:/ASMA4/analysis/2_results_main.R")
#     Input: analysis_data_aggr.rds + 2_shared_functions.R
#     Outputs: plot_dynamic_no_visits.pdf + plot_dynamic_any_visit.pdf +
#             main_no_visits(xlsx + tex + rds) + 
#             main_any_visit(xlsx + tex + rds) +
#             drop_no_visits(xlsx + tex + rds) +
#             drop_any_visit(xlsx + tex + rds)
#             main_t2t3(xlsx + tex + rds) +
#             spill_private_no_visits(xlsx + tex + rds) + 
#             spill_private_any_visit(xlsx + tex + rds) + 
#             spill_households(xlsx + tex + rds)
# Running time approximately 3 minutes.


# Estimate the main results by exploiting covariates.
source(file="W:/ASMA4/analysis/2_results_covariates.R")
#     Input: analysis_data_aggr.rds + 2_shared_functions.R
#     Outputs: ctrl_no_visits(xlsx + tex + rds) +
#             ctrl_any_visit(xlsx + tex + rds) +
#             ctrl_spill_privat_no_visits(xlsx + tex + rds) +
#             ctrl_spill_privat_any_visit(xlsx + tex + rds) +
#             ctrl_spill_households(xlsx + tex + rds) +
#             did_no_visits(xlsx + tex + rds) + 
#             did_any_visit(xlsx + tex + rds) + 
#             did_spill_privat_no_visits(xlsx + tex + rds) + 
#             did_spill_privat_any_visit(xlsx + tex + rds) + 
#             did_spill_households(xlsx + tex + rds) +
#             match_no_visits(xlsx + tex + rds) +
#             match_any_visit(xlsx + tex + rds) +
#             match_spill_privat_no_visits(xlsx + tex + rds) +
#             match_spill_privat_any_visit(xlsx + tex + rds) +
#             main_spill_households_match(xlsx + tex + rds) + 
#             balance_plot_match.pdf
# Running time approximately 9 minutes.


# Examine the potential heterogeneity in treatment effects:
source(file="W:/ASMA4/analysis/2_results_heterogeneity.R")
#     Input: analysis_data_aggr.rds + 2_shared_functions.R
#     Outputs: het_table_no_preregistered(xlsx + tex + rds) + 
#             het_table_any_preregistered(xlsx + tex + rds) + 
#             het_table_drop_preregistered(xlsx + tex + rds) + 
#			  het_table_t1t23_preregistered(xlsx + tex + rds) + 
#             het_pc_area.pdf + het_income_t1_t23.pdf + 
#             tuned_params.rds + tuned_params.txt + 
#             results_generic_ml.rds + het_ml_methods(xlsx + tex + rds) +
#             het_blp(xlsx + tex + rds) + het_gates_nurse.pdf + 
#             het_gates_nurse.pdf + het_clan_nurse(xlsx + tex + rds) +
#             het_clan_gp(xlsx + tex + rds) +
#             het_ml_methods_mean(xlsx + tex + rds)
# Running time approximately 5.5 hours (GenericML takes most of the time).

Sys.time()

# End.
