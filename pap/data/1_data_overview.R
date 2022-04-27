
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 1_data_overview.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Analyze data quality, and create analysis data:
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.

# Inputs:
input_ids = "W:/ASMA4/data/interim/data_ids.rds"
input_data = "W:/ASMA4/data/interim/data_aggr.rds"
input_population = "W:/ASMA4/data/interim/data_population.rds"

# Outputs:
output_evol_main = "W:/ASMA4/analysis/figures/outcome_trends_main.pdf"
output_evol_spill = "W:/ASMA4/analysis/figures/outcome_trends_spillovers.pdf"
output_data = "W:/ASMA4/data/clean/analysis_data_aggr.rds"
output_data_csv = "W:/ASMA4/data/clean/analysis_data_aggr.csv"


###
###


# Preliminary data steps:

ids = readRDS(input_ids)
df = readRDS(input_data)

df = merge(df, ids[, .(id, pc_area)], by='id', all.x = TRUE)

df[, date := as.Date(paste(as.character(year), as.character(month), 
                           as.character(day), sep='-'))]

population = readRDS(input_population)

treatment_year = 2019


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Assess coding rates on whether the pc visit is curative/preventive. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# To extract curative nurse outpatient visits, we need to distinguish 
# curative contacts from preventive contacts. Here, we analyze the coding rate 
# of this variable over time:

DT = df[contact_type %in% c(1,2)
        ][, missing := as.integer(curative==-1)
          ][, .(contacts = sum(contacts)), by=c('year', 'month', 'missing')]

# Pivot wider:
DT = dcast(DT, year + month ~ 
             paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing curative/preventive categorical:
DT[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(DT)


# Conclusion: the curative/preventive variable is almost always coded. 
# Take only curative primary care visits.
df = df[profession %in% c(2, 1, -1) & curative == 1 | 
          is.na(profession)][, curative := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Analyze the share of missing profession (of visits). ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# First, we mutate data so that we can compute the share of visits 
# with missing profession for primary care visits:

DT = df[profession %in% c(2,1,-1)
        ][, missing := as.integer(profession==-1)
          ][, .(contacts = sum(contacts)),
            by=c('year', 'month', 'missing')]

# Pivot wider:
DT = dcast(DT, year + month ~ 
             paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing profession:
DT[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(DT)


# Conclusion: The share of visits with missing profession (or other than
# nurse or GP) has been low and stable. We exclude rows visits where
# the profession is missing:
df = df[profession != -1 | is.na(profession)][, profession := NULL]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) How many visits contain data on the contact date? ####
### ### ### ### ### ### ### ### ### ### ### ### ###

DT = df[contact_type %in% c(1,2)
        ][, missing := as.integer(contact_date_d==0)
          ][, .(contacts = sum(contacts)),
            by=c('year', 'missing')]

# Pivot wider:
DT = dcast(DT, year ~ 
             paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing contact date:
DT[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(DT)

# Only 10% of the actually occurred visits contain data on when the
# patient contacted the health center. The reasons for such a low coding
# rate are unknown.


# Repeat this, but no replace year by pc_area:

DT = df[contact_type %in% c(1,2)
        ][, missing := as.integer(contact_date_d==0)
          ][, .(contacts = sum(contacts)),
            by=c('pc_area', 'missing')]

# Pivot wider:
DT = dcast(DT, pc_area ~ 
             paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing contact date:
DT[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(DT)

# Our conclusion is to not use first contacts to primary care as an outcome
# in this PAP due to data quality issues. However, we will later check with
# the actual trial data whether the coding rates with respect to the date
# of the first contact has improved and whether this outcome could be used.
df = df[contact_type != 0]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) How many visits contain data on the triage date? ####
### ### ### ### ### ### ### ### ### ### ### ### ###

DT = df[contact_type %in% c(1,2)
        ][, missing := as.integer(triage_date_d==0)
          ][, .(contacts = sum(contacts)), by=c('pc_area', 'missing')]

# Pivot wider:
DT = dcast(DT, pc_area ~ 
             paste0('missing_', missing, sep=''), value.var = 'contacts')

# Compute the share of visits with missing contact date:
DT[, share_missing := 100 * missing_1 / (missing_0 + missing_1)]
print(DT)

# The rates are the same as with respect to the contact date variable.
# Our conclusion is to not use triages to primary care as an outcome
# in this PAP due to data quality issues. However, we will later check with
# the actual trial data whether the coding rates have improved.


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 5) Several primary care visits on the same day. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# First, drop variables for contact date and triage date and re-aggregate:
df = df[, .(contacts=sum(contacts)), 
        by=c('id', 'year', 'month', 'day', 'contact_type', 'pc_area', 'date')]

# Some patients appear to have several primary care visits on the same day.
# In some cases, this may be accurate, but observing many visits leads us to ask
# whether the actual reason is the coding practice. If there are multiple visits
# per day, we treat them as a single visit.
summary(df[contact_type %in% c(1,2, 2.5), contacts])

df[contact_type %in% c(1,2,2.5) & contacts > 1, contacts := 1]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 6) Plot the evolution of outcomes. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that plots the evolution of outcomes. ###

plot.evolution = function(data_visits, data_population, ctype, treat_year) {
  # INPUTS:
  # data_visits: 'df'
  # data_population: 'population'
  # ctype: 0 (contacts to primary care); 1 primary care nurse visits;
  #       2 (primary care GP visits); 2.5 (private outpatient doctor visits);
  #       3 (public sector prescriptions); 3.5 (private sector prescriptions);
  #       4 (referrals by health centers); 4.5 (referrals by private outpatient
  #       care)
  # treat_year: treatment year
  # OUTCOME:
  # a ggplot object
  
  
  # Extract, aggregate, and construct the data for plotting:
  
  DT = data_visits[contact_type==ctype]
  DT = DT[, .(contacts = sum(contacts)), by=c('pc_area', 'year', 'month')]
  DT = merge(DT, data_population, by='pc_area')
  DT[, contacts_per_capita := 12 * contacts / population]
  DT[, date := as.Date(paste(as.character(year), 
                             as.character(month), '01', sep='-'))]
  
  # Start date and end date for the plot:
  
  start_date = as.Date(paste(as.character(treat_year), '-01-01', sep=''))
  end_date = as.Date(paste(as.character(treat_year+1), '-06-01', sep=''))
  
  DT = DT[date <= end_date]
  
  
  # Expand the table (if no observation) and fill by zero:

  panel = CJ(unique(DT$pc_area), seq(start_date, end_date, by='months'))
  colnames(panel) = c('pc_area', 'date')
  panel = merge(panel, DT, by=c('pc_area', 'date'), all.x=TRUE)
  panel[is.na(contacts_per_capita), contacts_per_capita := 0]
  
  
  # ggtitle depends on the contact type (ctype):
  
  if(ctype==0) {
    title = 'Contacts to Primary Care' 
  } else if (ctype==1) {
    title = 'Nurse Visits in Primary Care' 
  } else if (ctype==2) {
    title = 'GP Visits in Primary Care' 
  } else if (ctype==2.5) {
    title = 'Private Outpatient GP Visits' 
  } else if (ctype==3) {
    title = 'Prescriptions from Public Sector' 
  } else if (ctype==3.5) {
    title = 'Prescriptions from Private Sector' 
  } else if (ctype==4) {
    title = 'Referrals from Health Centers' 
  } else if (ctype==4.5) {
    title = 'Referrals from Private Clinics' 
  }
  
  # Plot:
  
  p = ggplot(panel, aes(x=date, y=contacts_per_capita, group=pc_area, 
                     color=pc_area, shape=pc_area)) +
    geom_point() +
    geom_line() +
    ggtitle(title) + 
    xlab('Time') + ylab('Ann. contacts per capita') +
    scale_x_date(date_breaks = '6 months', date_labels = "%b\n%Y") +
    theme(text = element_text(size=15),     
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = 'bottom') +
    labs(color = 'Area', group='Area', shape='Area') +
    geom_vline(xintercept = as.Date('2019-09-01'), linetype='dashed')
  
  return(p)
}

p = plot.evolution(data_visits = df, data_population = population,
                   ctype = 1, treat_year = treatment_year)


# Loop over contact types:
contact_types = c(1, 2, 2.5, 3, 3.5, 4, 4.5)

# Plot:

plots = lapply(contact_types, function(type) {
  
  p = plot.evolution(data_visits = df, data_population = population,
                     ctype = type, treat_year = treatment_year)
  
})
names(plots) = contact_types


# Save plots:

cairo_pdf(filename = output_evol_main, width = 10, height = 8)
print(plots$`1` + plots$`2` + plots$`3` + plots$`4` + 
        plot_layout(guides='collect') & theme(legend.position = 'bottom'))
dev.off()

cairo_pdf(filename = output_evol_spill, width = 15, height = 4.5)
print(plots$`2.5` + plots$`3.5` + plots$`4.5` +
        plot_layout(guides='collect') & theme(legend.position = 'bottom'))
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 7) Construct analysis data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will create a dataset that contains the following combinations:
persons = ids[, id]
contact_types = c(1, 2, 2.5, 3, 3.5, 4, 4.5)
follow_ups = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# The dataset that we will fill next:
data = CJ(persons, contact_types, follow_ups)
setnames(data, old=c('persons', 'contact_types', 'follow_ups'),
         new=c('id', 'contact_type', 'follow_up'))


# Loop over contact types:
ctypes = lapply(contact_types, function(ctype) {
  
  # Loop over follow-ups:
  fups = lapply(follow_ups, function(fup) {
    
    if(fup == 0) {
      start_date = as.Date(paste(as.character(treatment_year), 
                                 '-01-01', sep=''))
      end_date = as.Date(paste(as.character(treatment_year), 
                               '-06-30', sep=''))
    } else if (fup %in% c(1:2)) {
      start_date = as.Date(paste(as.character(treatment_year), 
                                 '-10-13', sep=''))
      end_date = as.Date(paste(as.character(treatment_year),
                               as.character(10+fup), '12', sep='-'))
    } else if (fup %in% c(3:10)) {
      start_date = as.Date(paste(as.character(treatment_year), 
                                 '-10-13', sep=''))
      end_date = as.Date(paste(as.character(treatment_year + 1),
                               as.character(fup-2), '12', sep='-'))
    }
    
    print(start_date)
  
    # Extract the right subset of data:
    DT = df[contact_type == ctype & date >= start_date & date <= end_date]
    
    # Aggregate at the ID-by-contact-type level:
    DT = DT[, .(contacts = sum(contacts)), by=c('id', 'contact_type')]
    DT[, follow_up := fup]
    
    # Annualized contacts per capita:
    if(fup==0) {
      DT[, contacts.ann := 12* contacts / 6]
    } else {
      DT[, contacts.ann := 12* contacts / fup]
    }
    
  })
  fups = do.call(rbind.data.frame, fups)
  
}) 
ctypes = do.call(rbind.data.frame, ctypes)


# Merge contacts to the dataset:
data = merge(data, ctypes, by=c('id', 'contact_type', 'follow_up'), all.x=TRUE)

# Replace NAs by zeroes:
data[is.na(contacts), ':=' (contacts = 0, contacts.ann = 0)]


lapply(follow_ups, function(fup) {
  DT = data[follow_up==fup & contact_type ==1]
  print(c(mean(DT$contacts), mean(DT$contacts.ann)))
})


# Merge trial data and FOLK data to 'data':
data = merge(data, ids, by='id', all.x = TRUE)


# Next, we will create covariates of the pre-treatment healthcare use:

# Extract pre-treatment healthcare use:
DT.1 = data[follow_up == 0, .(id, contact_type, contacts.ann)
            ][, contact_type := as.character(contact_type)]

# Pivot wider:
DT.1 = dcast(DT.1, id ~ paste0('contacts.pre.', contact_type, sep=''), 
             value.var = 'contacts.ann')

data = merge(data, DT.1, by='id', all.x = TRUE)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 8) Identify heavy users. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Some individuals have a lot of healthcare contacts (mainly primary care nurse 
# or GP visits or prescriptions), and these outliers can affect the results. In 
# robustness checks, we exclude individuals that we define as high-users:

# First, we find the top 1 percentile value for the number of healthcare 
# contacts in the following four variables:
outcomes = c('contacts.1', 'contacts.2', 'contacts.3', 'contacts.3.5')

# Then, if the person belongs to the top 1% in terms of healthcare use in any
# of the following variables, we define them as a heavy-user.


# Next, we will create covariates of the healthcare use in the main follow-up:

# Extract pre-treatment healthcare use:
DT.2 = data[follow_up == 6, .(id, contact_type, contacts.ann)
            ][, contact_type := as.character(contact_type)]

# Pivot wider:
DT.2 = dcast(DT.2, id ~ paste0('contacts.', contact_type, sep=''), 
             value.var = 'contacts.ann')


# # Create an indicator for whether the person is heavy user based on a
# given outcome. Loop over outcomes:

loop = lapply(outcomes, function(otc) {
  
  qntl = quantile(DT.2[, get(otc)], (100-1)/100)
  DT.2[, paste('high', otc, sep='.') := as.integer(get(otc) >= qntl)]
  
  # Divide the annualized figures by two to get the absolute number of contacts:
  qntl = qntl / 2
  
  print(paste(otc, qntl, sep=': '))
})


# Create an indicator for high-users:

DT.2[, high.user := 
       as.integer(high.contacts.1==1 | high.contacts.2==1 | high.contacts.3==1 |
                    high.contacts.3.5==1 )]

print(100 * DT.2[, mean(high.user)]) # 4.7% of the individuals are "high.users"

DT.2 = DT.2[, mget(c('id', 'high.user'))]
data = merge(data, DT.2, by='id', all.x = TRUE)

saveRDS(data, output_data)
saveRDS(data, output_data_csv)

# End.
