
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_link_datasets.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RCT.           ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Link different datasets.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.

# Inputs:
input_rct = "W:/ASMA4/data/interim/rct_data.csv"
input_folk = "W:/ASMA4/data/interim/folk_data_2019.csv" 
input_pc_contacts = "W:/ASMA4/data/interim/pc_contacts_public_20" # pc_contacts_public_20XX.rds, XX in 19:20
input_pc_visits = "W:/ASMA4/data/interim/pc_visits_public_20" # pc_visits_public_20.rds, XX in 19:20
input_drugs = "W:/ASMA4/data/interim/prescriptions_" # prescriptions_X_20YY.csv, X in 1:2 and YY in 19:20
input_refs = "W:/ASMA4/data/interim/referrals_20" # referrals_20XX.csv, XX in 19:20

# Outputs:
output_population = "W:/ASMA4/data/interim/data_population.rds"
output_ids = "W:/ASMA4/data/interim/data_ids.rds"
output_data = "W:/ASMA4/data/interim/data_aggr.rds"


###
###

study_years = c(19:20)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) The RCT population. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Read experimental data and FOLK data:
ids = data.table::fread(input_rct)
setnames(ids, old='shnro', new='id')
folk = data.table::fread(input_folk)

# Create an indicator for males:
folk[, male := as.integer(gender==1)]
folk[, gender := NULL]

# Disposable income is reported in thousands:
folk[, income_disp := income_disp / 1000]


# Create a shorter household ID:

# Map the long character IDs to integers:
house.id = unique(ids[, mget('satunnus')])
house.id[, household.id := c(1:nrow(house.id))]

# Merge to ids and paste:
ids = merge(ids, house.id, by='satunnus', all.x = TRUE)
ids[, satunnus := NULL]


# Currently, variable letter is 1 for those who received the letter and 0 for
# those who did not receive the letter. We create a new letter.main variable 
# that instead has letter.main=-1 for those living in a household that received 
# the letter but who did not personally receive the letter.

ids[, letter.main := letter]

households = ids[, .(letter.household = as.integer(sum(letter) > 0)), 
                 by='household.id']

ids = merge(ids, households, by='household.id', all.x = TRUE)
ids[letter.household==1 & letter==0, letter.main := -1]


# In the main analysis, we include those who personally received the letter,
# and their controls. However, the exclusion of non-letter individuals in letter 
# households mechanically affects the covariate balance as single-person
# households differ in many ways from larger households. To solve this, we 
# randomize only one person for main analysis from multi-person households
# that did not receive the letter.

controls.hh = households[letter.household==0, household.id]
controls = ids[household.id %in% controls.hh]

# Set seed and randomize only one individual from households that did not
# receive the treatment:
set.seed(12345)
controls = controls[controls[, .I[sample(.N, 1)], by='household.id']$V1]
controls = controls$id

# The previous group retains letter.main=0, but the rest in non-letter
# households are excluded from main analysis:
ids[!(id %in% controls) & letter.household == 0, letter.main := -1]


# For the analysis of spillovers within the household, we will compare those in 
# letter households who did not receive the letter to those in non-letter
# households that are excluded from main analysis. Single-person households
# are excluded.

# First, check at the household level which letter was sent (conditional
# on letter being sent):

households = ids[, .(letter.type = max(unique(letter)),
                     size = .N), 
                 by='household.id']

ids = merge(ids, households, by='household.id', all.x = TRUE)

# If household size==1 (no non-recipients conditional on household 
# receiving the letter), do not use the household for spillover analysis.
ids[size==1, letter.spill := -1]

# Include those individuals in multi-person letter households that did not 
# personally receive the letter:
ids[letter.household==1 & letter.main==-1 & size > 1, 
    letter.spill := letter.type]

# Exclude those individuals in multi-person letter households that did 
# personally receive the letter:
ids[letter.household==1 & letter.main > 0 & size > 1, 
    letter.spill := -1]

# Exclude those individuals in multi-person no-letter households that were
# included in main analysis.
ids[letter.household==0 & letter.main == 0 & size > 1, 
    letter.spill := -1]

# Include those individuals in multi-person no-letter households that were
# excluded from main analysis.
ids[letter.household==0 & letter.main == -1 & size > 1, 
    letter.spill := 0]

ids[, ':=' (size = NULL, letter.type = NULL, letter.household = NULL)]


# Merge FOLK data to ids:
df.main = merge(ids, folk, by='id', all.x = TRUE)
df.main[, ':=' (year = NULL, municipality = NULL)]
setnames(df.main, old='municipality_rct', new='municipality', 
         skip_absent = TRUE)

# For 0.2 % of the experimental sample, we cannot find the same person
# in the 2019 FOLK data.
100 * colMeans(is.na(df.main))

# Percentage of rows where the family equivalized income is exactly zero:
100 * nrow(df.main[income_eq==0]) / nrow(df.main) # 0.1%
100 * colMeans(is.na(df.main))
df.main[income_eq == 0, income_eq := NA]
df.main[, income_eq := income_eq / 1000]


# Create a variable for the primary care area
df.main[municipality %in% c(153, 405, 416, 441, 580, 689,
                            700, 739, 831), pc_area := 'Eksote']
df.main[municipality %in% c(75, 285, 286, 489, 624, 935), 
        pc_area := 'Kymsote']
df.main[municipality %in% c(16, 81, 98, 316, 398, 504, 560, 576, 616, 142), 
        pc_area := 'PHHYKY']


# Next, we will add indicators for having received any diabetes drug or 
# hypertension drug prescriptions in 9 months leading the trial (Jan-Sep):

print(study_years[[1]])

# Loop over subsets of data:
presc = lapply(1:2, function(subset) {
  
  # Read visits:
  source = paste(input_drugs, subset, '_20', study_years[[1]], ".csv", sep="")
  df = data.table::fread(source)
  
  # Extract prescriptions with the following ATC therapeutic subgroups:
  df[, ATC_CODE := substr(ATC_CODE, 1, 3)]
  df = df[ATC_CODE %in% c('A10', 'C02', 'C03', 'C07', 'C08', 'C09')]
  
  # Create dummies for diabetes and hypertension:
  df[, ':=' (diabetes = as.integer(ATC_CODE == 'A10'),
             hypertension = as.integer(ATC_CODE %in% c('C02', 'C03', 'C07', 
                                                       'C08', 'C09')))]
  
  # Create variables for month and year:
  df[, ':=' (year = as.integer(format(DATE_PK, format="%Y")),
             month = as.integer(format(DATE_PK, format="%m")))]
  
  df = df[, .(shnro, year, month, diabetes, hypertension)]
  setnames(df, old='shnro', new='id')
  
})
presc = do.call(rbind.data.frame, presc)
presc = presc[month %in% c(1:9)][, ':=' (month = NULL, year = NULL)]
presc = presc[, .(diabetes = as.integer(sum(diabetes) > 0), 
                  hypertension = as.integer(sum(hypertension) > 0)), by='id']

# Merge the diabetes/hypertension prescription indicators to 'df.main':
df.main = merge(df.main, presc, by='id', all.x=TRUE)
df.main[is.na(diabetes), diabetes :=0]
df.main[is.na(hypertension), hypertension :=0]


# Compute and store population size by primary care area:
population.pc.area = df.main[, .(population = .N), by='pc_area']
print(population.pc.area)
saveRDS(population.pc.area, output_population)
saveRDS(df.main, output_ids)

# Store experimental sample IDs:
ids = df.main[, .(id, pc_area)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Primary care first contacts ####
### ### ### ### ### ### ### ### ### ### ### ### ###


contacts = lapply(study_years, function(yr) {
  print(yr)
  
  # Read contacts:
  source = paste(input_pc_contacts, yr, ".rds", sep="")
  df = setDT(readRDS(source))
  
  # Extract primary care contacts in target areas:
  df = df[(topi_target_area==1 & topi_pc==1) |
            sote_target_area==1 & sote_pc==1]
  
  # Aggregate at the ID-month level:
  df = df[, .(contacts = .N), by=c('id', 'year', 'month', 'day')]
  
})
contacts = do.call(rbind.data.frame, contacts)

# Keep only sample individuals:
contacts = merge(contacts, ids, by='id')
print("Merge is done")

# Contact type 0 = primary care contacts:
contacts[, contact_type := 0]

# Annualized contacts per capita:
sum(contacts$contacts) / (2*nrow(ids)) 


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Primary care visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


visits = lapply(study_years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_pc_visits, yr, ".rds", sep="")
  df = setDT(readRDS(source))
  
  # Extract primary care contacts in target areas:
  df = df[(topi_target_area==1 & topi_pc==1) |
            sote_target_area==1 & sote_pc==1]
  
  # Aggregate at the ID-month-profession-curative-contact_date_d level:
  df = df[, .(contacts = .N), by=c('id', 'year', 'month', 'day', 
                                   'profession', 'curative', 'contact_date_d',
                                   'triage_date_d')]
  
  print(df[, .(contacts = sum(contacts)), by='contact_date_d'])
  print(df[, .(contacts = sum(contacts)), by='triage_date_d'])
  return(df)
  
})
visits = do.call(rbind.data.frame, visits)


# Keep only sample individuals:
visits = merge(visits, ids, by='id')
print("Merge is done")

# Contact type 1 = nurse visits, contact type 2 = GP visits:
visits[profession==2, contact_type := 1]
visits[profession==1, contact_type := 2]

# Annualized contacts per capita:
sum(visits[contact_type==1, contacts]) / (2*nrow(ids))
sum(visits[contact_type==2, contacts]) / (2*nrow(ids)) 


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Private outpatient visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


visits.pri = lapply(study_years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_pc_visits, yr, ".rds", sep="")
  df = setDT(readRDS(source))
  
  # Extract private outpatient contacts in target areas:
  df = df[(topi_target_area==1 & topi_pc==0) &
            sote_target_area==1 & sote_pc==0]
  
  # Aggregate at the ID-month-profession-curative level:
  df = df[, .(contacts = .N), by=c('id', 'year', 'month', 'day', 
                                   'profession', 'curative')]
  
  # Almost all contacts contain data on curative/preventive categorical:
  print(df[, .(contacts = sum(contacts)), by='curative'])
  
  # We will focus on private doctor appointments:
  print(df[, .(contacts = sum(contacts)), by='profession'])
  
  # Aggregate at the ID-month-profession-curative level:
  df = df[curative==1 & profession==1
          ][, .(contacts = .N), by=c('id', 'year', 'month', 'day')]
  
  return(df)
  
})
visits.pri = do.call(rbind.data.frame, visits.pri)


# Keep only sample individuals:
visits.pri = merge(visits.pri, ids, by='id')
print("Merge is done")


# Contact type 2.5 = private doctor visits:
visits.pri[, contact_type := 2.5]

# Annualized contacts per capita (in 2020):
sum(visits.pri[year==2020, contacts]) / (nrow(ids))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Prescriptions. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Loop over study years:
presc = lapply(study_years, function(yr) {
  
  print(yr)
  
  # Loop over subsets of data:
  presc.year = lapply(1:2, function(subset) {
    
    # Read visits:
    source = paste(input_drugs, subset, '_20', yr, ".csv", sep="")
    df = data.table::fread(source)
    
    # Tidy column names:
    colnames(df) = tolower(colnames(df))
    setnames(df, old='shnro', new='id')
    
    # Create variables for month and year, and day:
    df[, ':=' (year = as.integer(format(date_pk, format="%Y")),
               month = as.integer(format(date_pk, format="%m")),
               day = as.integer(format(date_pk, format="%d")))]
    
  })
  presc.year = do.call(rbind.data.frame, presc.year)
  
  # Aggregate:
  DT = presc.year[, .(contacts = .N), 
                  by=c('id', 'year', 'month', 'day', 'sector')]
  
})
presc = do.call(rbind.data.frame, presc)

# Sector is coded in almost every row:
colMeans(is.na(presc))


# Wrt. South Karelia and Kymenlaakso primary care areas, we will take
# prescriptions written by public sector organizations. Wrt. Paijat-Hame,
# however, the same cannot be done as three municipalities had a private
# primary care provider. For people living in these three municipalities,
# we include all prescriptions, but for people residing in the 
# other municipalities in Paijat-Hame, we include only prescriptions 
# written by public sector organizations.

ids = df.main[, .(id, municipality)]
ids[, ':=' (private_provider = as.integer(municipality %in% c(142, 316, 398)),
            municipality = NULL)]

presc = merge(presc, ids, by='id')


# Prescriptions written by the public sector (or by also the private
# sector in municipalities that had a private provider of primary care):
presc.pub = presc[private_provider==0 & sector==1 | private_provider==1]

# Aggregate. Contact type 3 = prescriptions by public sector:
presc.pub = presc.pub[, .(contacts = sum(contacts)), 
                      by=c('id', 'year', 'month', 'day')]
presc.pub[, contact_type := 3]

# Annualized contacts per capita:
sum(presc.pub$contacts) / (2*nrow(ids))


# Prescriptions written by the private sector (or by also the public
# sector in municipalities that had a private provider of primary care):
presc.pri = presc[sector==2]

# Aggregate. Contact type 3.5 = prescriptions by private sector:
presc.pri = presc.pri[, .(contacts = sum(contacts)), 
                      by=c('id', 'year', 'month', 'day')]
presc.pri[, contact_type := 3.5]

# Annualized contacts per capita:
sum(presc.pri$contacts) / (2*nrow(ids))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Referrals to specialized healthcare. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We observed that almost all referrals contain time stamp for the start of
# the contact. To put it differently, we observe only those referrals that
# have led to an actual contact. Similarly, we observe that some of the
# contacts have not ended, especially when we approach the date of data 
# extraction (January 2022). It does not matter in our analyses.

refs = lapply(study_years, function(year) {
  print(year)
  
  # Read visits:
  source = paste(input_refs, year, ".csv", sep="")
  df = data.table::fread(source, drop=c('discharged'))
  
  # Take those rows where we observe the contact start date:
  df = df[contact_started==1]
  
  # Take unique id-date pairs. More than one referrals could be written
  # on each day, but in our analyses we create a dummy on whether an 
  # individual X received any referrals on date Y:
  df = unique(df)
  
  # Aggregate at the ID-month-day level:
  df = df[, .(contacts = .N), by=c('id', 'year', 'month', 'day', 'ref_origin')]
  
})
refs = do.call(rbind.data.frame, refs)


# Keep only sample individuals:
ids = df.main[, .(id)]
refs = merge(refs, ids, by='id')
print("Merge is done")

# The coding rate wrt. the institution writing the referral is sufficiently
# high. We extract only those referrals that were written by health centers:
refs[, .(contacts = sum(contacts)), by='ref_origin']


# Extract referrals written by health centers and pricate outpatient care:

refs = refs[ref_origin %in% c(1,7)] # health centers, private outpatient care

# Contact type 4 = referrals written by health centers;
# 4.5 = referrals written by private outpatient care:
refs[ref_origin==1, contact_type := 4]
refs[ref_origin==7, contact_type := 4.5]
refs[, ref_origin := NULL]

# Annualized contacts per capita:
sum(refs[contact_type==4, contacts]) / (2*nrow(ids))
sum(refs[contact_type==4.5 & year==2020, contacts]) / (nrow(ids))
sum(refs[contact_type==4.5, contacts]) / (2*nrow(ids))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Concatenate and save. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Concatenate datasets:
data = rbind(contacts, visits, visits.pri, presc.pub, 
             presc.pri, refs, fill=TRUE)
data[, pc_area := NULL]

saveRDS(data, output_data)

# End.
