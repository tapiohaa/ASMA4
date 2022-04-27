
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###             r-script 0_public_pc.R            ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###           Funded Primary Care: RCT.           ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Since 2019, the Avohilmo registry also contains
#           private outpatient care which we want to be excluded from
#           this study.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(readxl)           # Reading xlsx files.

# Inputs:
input_topi = "W:/ASMA4/data/raw/topi_unit_register.csv"
input_sote = "W:/ASMA4/data/raw/sote_organisation_register.xlsx"
input_contacts = "W:/ASMA4/data/interim/pc_contacts_20" # pc_contacts_20XX.csv, XX in 19:20
input_visits = "W:/ASMA4/data/interim/pc_visits_20" # pc_visits_20XX.csv, XX in 19:20

# Outputs:
output_contacts = "W:/ASMA4/data/interim/pc_contacts_public_20" # pc_contacts_public_20XX.rds, XX in 19:20
output_visits = "W:/ASMA4/data/interim/pc_visits_public_20" # pc_visits_public_20XX.rds, XX in 19:20


###
###

# Municipalities on the Åland Islands:
to_drop = c(478, 60, 65, 76, 170, 736, 771, 43, 417,
            438, 35, 62, 295, 318, 766, 941)

# Target municipalities in the three primary care areas:
target_munies = c(153, 405, 416, 441, 580, 689, 700, 739, 831, # Eksote
                  75, 285, 286, 489, 624, 935, # Kymsote
                  16, 81, 98, 316, 398, 504, 560, 576, 616, 142) # PHHYKY

study_years = c(19:20)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read TOPI unit register and extract health stations. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

topi = data.table::fread(
  input_topi, encoding='UTF-8',
  select = c('topi_code', 'topi_code_addition', 'name', 'year', 
             'service_area_code', 'municipality_code')
)

# Take the service area codes that are related to health centers
# (120, 121, and 122):
topi = topi[grepl('120', service_area_code, fixed = TRUE) |
              grepl('121', service_area_code, fixed = TRUE) |
              grepl('122', service_area_code, fixed = TRUE), 
            topi_pc := 1]

# Create an indicator for target municipalities:
topi[, topi_target_area := as.integer(municipality_code %in% target_munies)]

# Take years from 2019 and drop providers from the Åland Islands:
topi =  topi[year >= 2019 & !(municipality_code %in% to_drop)]


# When extracting the primary care units, we observe 257 and 258 municipalities 
# out of 293. Not every municipality has a health station or a health center.
topi[topi_pc==1, .(munies = length(unique(municipality_code))), by='year']

# For each Topi code, set topi_target_area==1 if the respective code is 
# observed in target municipalities.
topi.target = unique(topi[, mget(c('topi_code', 'topi_target_area'))])
topi.target = topi.target[, .(topi_target_area = 
                                as.integer(mean(topi_target_area) > 0)), 
                          by='topi_code']


# Same topi_codes appear in multiple rows. Take only unique combinations:

topi = topi[, mget(c('topi_code', 'year', 'topi_pc'))]
topi = unique(topi)
topi = merge(topi, topi.target, by='topi_code', all.x=TRUE)

topi = topi[, .(topi_pc = sum(topi_pc, na.rm=TRUE)), 
            by=c('topi_code', 'year', 'topi_target_area')
            ][, topi_pc := as.integer(topi_pc > 0)]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read SOTE organization register and extract public sector units. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Read data on social and healthcare organizations:
sote = setDT(readxl::read_excel(input_sote))

# Extract relevant columns:
cols = c('Tunniste', 'Lyhenne', 'Org.Yks.lyhenne', 
         'Hierarkiataso', 'Sijainti kunta')
sote = sote[, mget(cols)]
setnames(sote, old=cols, new=c('sote_code', 'name', 'organization', 
                               'hierarchy', 'municipality'))

# Tidy municipality number:
sote[, municipality := as.integer(substr(municipality, 1, 3))]

# Create an indicator for target municipalities:
sote[, sote_target_area := as.integer(municipality %in% target_munies)]

# Create an indicator whether the organization is one of the three 
# primary care areas:
orgs = c('PHHYKY', 'Kymsote', 'KYmsote', 'Harjun terveys Oy',
         'Etelä-Karjalan sosiaali ja terveyspiiri',
         'Etelä-Karjalan sosiaali- ja tefrveyspiiri',
         'Etelä-Karjalan sosiaali- ja terveyspiiri')
sote[, sote_pc := as.integer(organization %in% orgs)]

# For merging, keep only sote_code and sector:
sote = sote[, mget(c('sote_code', 'sote_target_area', 'sote_pc'))]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Primary care visits. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


visits = lapply(study_years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_visits, yr, ".csv", sep="")
  df = data.table::fread(source)
  
  # Link topi and visits via topi_code:
  topi_yr = topi[year == 2000 + yr][, year := NULL]
  df = merge(df, topi_yr, by='topi_code', all.x = TRUE)
  
  # Link sote and visits via sote_code:
  df = merge(df, sote, by='sote_code', all.x = TRUE)
  
  df[topi_code=='', topi_code := NA_character_]
  df[sote_code=='', sote_code := NA_character_]
  
  return(df)
  
})
visits = do.call(rbind.data.frame, visits)
visits = visits[, mget(colnames(visits))]


# All rows contain topi_code, but in 4% of the rows the linking did not work.
# 0.04% of the rows do not contain sote_code, and in 1% of the rows the
# linking did not work.
100 * colMeans(is.na(visits))
visits[, ':=' (sote_code = NULL, topi_code = NULL)]
visits[, .N, by=c('year', 'topi_pc')]
visits[, .N, by=c('year', 'sote_pc')]

# Fill NA values by -1:
visits[is.na(topi_pc), topi_pc := -1]
visits[is.na(sote_pc), sote_pc := -1]
visits[is.na(topi_target_area), topi_target_area := -1]
visits[is.na(sote_target_area), sote_target_area := -1]

# Clearly, private clinics started to provide data on outpatient contacts
# in 2020:
test = visits[, .(pc = as.integer(topi_pc == 1 | sote_pc == 1),
                  year)]
summary(test[year==2019]$pc) 
summary(test[year==2020]$pc) 

# Save by year:
lapply(study_years, function(yr) {
  
  print(yr)
  visits_yr = visits[year == 2000 + yr]
  source = paste(output_visits, yr, ".rds", sep="")
  saveRDS(visits_yr, file=source)
  
})


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Primary care first contacts. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

contacts = lapply(study_years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_contacts, yr, ".csv", sep="")
  df = data.table::fread(source)
  df = df[year %in% c(2000 + study_years)]
  
  # Link topi and visits via topi_code:
  topi_yr = topi[year == 2000 + yr][, year := NULL]
  df = merge(df, topi_yr, by='topi_code', all.x = TRUE)
  
  # Link sote and visits via sote_code:
  df = merge(df, sote, by='sote_code', all.x = TRUE)
  
  df[topi_code=='', topi_code := NA_character_]
  df[sote_code=='', sote_code := NA_character_]
  
  return(df)
  
})
contacts = do.call(rbind.data.frame, contacts)
contacts = contacts[, mget(colnames(contacts))]


# All rows contain topi_code, but in 4% of the rows the linking did not work.
# 0.04% of the rows do not contain sote_code, and in 1% of the rows the
# linking did not work.
100 * colMeans(is.na(contacts))
contacts[, ':=' (sote_code = NULL, topi_code = NULL)]
contacts[, .N, by=c('year', 'topi_pc')]
contacts[, .N, by=c('year', 'sote_pc')]

# Fill NA values by -1:
contacts[is.na(topi_pc), topi_pc := -1]
contacts[is.na(sote_pc), sote_pc := -1]
contacts[is.na(topi_target_area), topi_target_area := -1]
contacts[is.na(sote_target_area), sote_target_area := -1]

# It seems that few private providers coded the date of first contacts:
test = contacts[, .(pc = as.integer(topi_pc == 1 | sote_pc == 1),
                  year)]
summary(test[year==2019]$pc) 
summary(test[year==2020]$pc) 

# Save by year:
lapply(study_years, function(yr) {
  
  print(yr)
  contacts_yr = contacts[year == 2000 + yr]
  source = paste(output_contacts, yr, ".rds", sep="")
  saveRDS(contacts_yr, file=source)
  
})

# End.
