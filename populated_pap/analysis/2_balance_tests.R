
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 2_balance_tests.R         ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Verification of randomization: balance tests.
rm(list=ls())

# Install and load the following packages:
library(data.table)       # Mutating and aggregating data.
library(smd)              # Compute standardized mean differences.
library(lfe)              # Linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.

# Inputs:
input_data = "W:/ASMA4/data/clean/analysis_data_aggr.rds"
functions = "W:/ASMA4/analysis/2_shared_functions.R"

# Outputs:
output_balance = "W:/ASMA4/analysis/tables/covariate_balance"
output_balance.t_main = "W:/ASMA4/analysis/tables/covariate_balance_tests_main"
output_balance.t_supp = "W:/ASMA4/analysis/tables/covariate_balance_tests_supp"

###
###


# First, read the data:
# Exclude those who lived in a household that received the letter, but
# who did not get a letter themselves:
DT = readRDS(input_data)
DT = DT[follow_up == 0 & letter.main != -1 & contact_type==1]

# Read shared functions
source(functions) # save.table()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Correlation table. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will use the following covariates in the later analyses:

covs = c('contacts.pre.1', 'contacts.pre.2', 'contacts.pre.2.5',
         'contacts.pre.3', 'contacts.pre.3.5', 'contacts.pre.4',
         'contacts.pre.4.5', 'age', 'male', 'origin_finn', 'language_fi',
         'relationship', 'widowed', 'children', 'apartment', 'educ_university',
         'pension_d', 'income_disp', 'income_eq', 'unemployment', 
         'unemployment_long', 'social_assistance_d', 'sickness_allowance')


# The above variables will be shown as descriptive statistics, but some of 
# them are highly collinear and we do not include them in F tests. Let's
# find the highly collinear pairs of variables:

# Correlation matrix:
cor.matrix = setDT(data.frame(cor(DT[!is.na(income_eq), mget(covs)])))
cor.matrix$row_names = colnames(cor.matrix)


# Print pairs of variables that are highly correlated:

corrs = lapply(c(1:nrow(cor.matrix)), function(i) {
  
  # Extract variable to be studied:
  col_name = colnames(cor.matrix)[i]
  data = cor.matrix[row_names != col_name, 
                    mget(c(col_name, 'row_names'))]
  setnames(data, old=col_name, new='corr')
  
  # Extract variables where the correlation is more than 0.4 or less than -0.4: # typo: 0.5 in the PAP text but 0.4 in the PAP code; we use 0.4
  data = data[corr > 0.4 | corr < -0.4][, another_var := col_name] 
  
})
corrs = do.call(rbind.data.frame, corrs)
print(corrs)


# Conclusion: the following pairs of covariates are highly collinear. 
# In F tests, we keep the one in parenthesis:
# (contacts.pre.2), contacts.pre.3
# (age), pension_d, widowed
# (language_fi), origin_finn
# (income_eq), income_disp
# (unemployment), unemployment_long
# (unemployment), pension_d


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Balance table. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Create a list containing variables that will be shown in a table and their 
# type and label:

variables = list(
  # Annualized healthcare contacts prior to the reform:
  list(var_name='contacts.pre.1', type='continuous', var_label='Primary care nurse visits'),
  list(var_name='contacts.pre.2', type='continuous', var_label='Primary care GP visits'),
  list(var_name='contacts.pre.2.5', type='continuous', var_label='Private outpatient doctor visits'),
  list(var_name='contacts.pre.3', type='continuous', var_label='Prescriptions from public sector'),
  list(var_name='contacts.pre.3.5', type='continuous', var_label='Prescriptions from private sector'),
  list(var_name='contacts.pre.4', type='continuous', var_label='Referrals from health centers'),
  list(var_name='contacts.pre.4.5', type='continuous', var_label='Referrals from private clinics'),
  
  # Sociodemographic outcomes:
  list(var_name='age', type='continuous', var_label='Age'),
  list(var_name='male', type='indicator', var_label='Is male'),
  list(var_name='origin_finn', type='indicator', var_label='Has Finnish background'),
  list(var_name='language_fi', type='indicator', var_label='Native language Finnish'),
  list(var_name='relationship', type='indicator', var_label='In relationship'),
  list(var_name='widowed', type='indicator', var_label='Widowed'),
  list(var_name='children', type='indicator', var_label='Children living at home'),
  list(var_name='apartment', type='indicator', var_label='Apartment in a block of flats'),
  
  # Education and labor market outcomes:
  list(var_name='educ_university', type='indicator', var_label='Degree from tertiary education'),
  list(var_name='pension_d', type='indicator', var_label='Pensioner'),
  list(var_name='income_disp', type='continuous', var_label='Disposable income'),
  list(var_name='income_eq', type='continuous', var_label='Equivalized disposable income'),
  list(var_name='unemployment', type='indicator', var_label='Unemployed for at least one day'),
  list(var_name='unemployment_long', type='indicator', var_label='Unemployd for at least 197 days'),
  list(var_name='social_assistance_d', type='indicator', var_label='Received social assistance'),
  list(var_name='sickness_allowance', type='indicator', var_label='Received sickness allowance')
)

# Treatment groups:
groups = list(c(0), c(1), c(2), c(3))


# Loop over treatment groups:
table.groups = lapply(groups, function(grp) {

  # Extract the right subset of observations:
  df = DT[letter.main %in% grp]
  
  # Sample sizes:
  n = paste('N=', length(unique(df[, id])), sep='')
  n = data.table(variable='Variable', value=n)
  
  print(paste('Group', grp))
  print(paste('Observations', nrow(df)))
  print(colSums(!is.na(df)))
  
  
  # Loop over variables:
  table.vars = lapply(variables, function(var) {
    
    value = mean(df[, mget(var$var_name)][[1]], na.rm=TRUE)
    
    if(var$type == 'continuous') {
      value = format(round(value, digits=3), nsmall=3)
      
    } else if (var$type == 'indicator') {
      value = paste(format(round(100*value, digits=2), nsmall=2), '%', sep='') }
    
    table = data.table(variable=var$var_label, value=value)
  })
  table.vars = do.call(rbind.data.frame, table.vars)
  table.vars = rbind(n, table.vars)
  
})
table.groups = do.call(cbind.data.frame, table.groups)[, c(1:2,4,6,8)]
colnames(table.groups) = c('Variable','T0','T1','T2','T3')

# Save:
save.table(table = table.groups, output = output_balance,
           label_tex = 'tab:covariate_balance',
           title_tex = 'Covariate Balance')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Balance table: tests. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# The comparisons that will be in the balance test table:
comparisons = list(
  list(control = c(0), treat=c(1,2,3)),
  list(control = c(1), treat=c(2,3)),
  list(control = c(2), treat=c(3)),
  list(control = c(0), treat=c(1)),
  list(control = c(0), treat=c(2)),
  list(control = c(0), treat=c(3))
)


# Loop over comparisons:

table.test = lapply(comparisons, function(comp){
  
  # Extract the data and create a treatment indicator:
  df = DT[letter.main %in% comp$treat | letter.main %in% comp$control]
  df[, treat := as.integer(letter.main %in% comp$treat)]
  
  # We will conduct a Wald test at the end. Create an empty character string:
  form.dt = data.table(form = '')
  

  # Loop over variables:
  table.vars = lapply(variables, function(var) {
    
    # These variables are not used in F-tests:
    if(var$var_name %in% c('pension_d', 'origin_finn', 'income_disp', 
                           'unemployment_long', 'contacts.pre.3', 'widowed')) { 
      return(data.table(variable=NA_character_, value=NA_character_)) }
    
    
    # Create outcome variable based on var$var_name:
    data = df[, mget(colnames(df))]
    setnames(data, old=var$var_name, new='outcome')
    
    # Test the difference:
    reg = lfe::felm(outcome ~ treat | 0 | 0 | 0, data=data)
    reg = setDT(broom::tidy(reg, conf.int=TRUE, se.type='robust'))
    reg = format(round(reg[term=='treat', p.value], digits=3), nsmall=3)
    
    # Collect p-values to a table:
    table = data.table(variable=var$var_label, p.value=reg)
    
    # Standardized mean difference:
    
    stand.diff = smd::smd(x=data[, outcome], g=data$treat, 
                          gref=2, na.rm=TRUE)$estimate
    if(stand.diff < 0) {std.diff.sign = '-'}
    if(stand.diff >= 0) {std.diff.sign = ''}
    
    table[, smd := paste(std.diff.sign, 
                         format(round(abs(stand.diff), digits=3), nsmall=3), 
                         sep='')]
    table[, ':=' (value = paste(smd, ' (', p.value, ')', sep = ''),
                  smd=NULL, p.value=NULL)]
    
    # Collect covariates for later use (joint tests):
    form.dt[, form := paste(form, var$var_name, sep=' + ')]
    
    return(table)
    
  })
  table.vars = do.call(rbind.data.frame, table.vars)
  
  
  # Tidy the covariate character string and create formula for joint F test:
  covs = form.dt[, mget('form')]
  covs = substr(covs, 4, nchar(covs))
  form = as.formula(paste('treat ~', covs, '| 0 | 0 | 0'))
  
  
  # Conduct the joint F tests and store results:
  
  # All individuals, all covariates:
  reg.1 = lfe::felm(form, data=df)
  pval.1 = format(round(summary(reg.1, robust=TRUE)$P.fstat['p.F'], digits=3), 
                  nsmall=3)
 
  # Exclude high-income individuals, all covariates:
  df.2 = df[income_eq < 500 & income_disp < 500]
  reg.2 = lfe::felm(form, data=df.2)
  pval.2 = format(round(summary(reg.2, robust=TRUE)$P.fstat['p.F'], digits=3), 
                  nsmall=3)
  
  # All individuals, leading healthcare use:
  reg.3 = lfe::felm(treat ~ contacts.pre.1 + contacts.pre.2 + contacts.pre.2.5 +
                    contacts.pre.3 + contacts.pre.3.5 + contacts.pre.4 +
                    contacts.pre.4.5, data=df)
  pval.3 = format(round(summary(reg.3, robust=TRUE)$P.fstat['p.F'], digits=3), 
                  nsmall=3)
  
  
  # F-test p-values:
  joint.test = data.table(
    variable=c('All individuals and covariates', 
               'Exclude the high-income folks', 
               'Only prior healthcare use'), 
    value=c(pval.1, pval.2, pval.3)
  )
  
  
  table.vars = rbind(table.vars, joint.test)
  return(table.vars)
})


# Save tables:

print(table.test)[[4]]

table.main = setDT(do.call(cbind.data.frame, table.test)[, c(1:2, 4, 6)])
table.main = table.main[variable!='fstat']
colnames(table.main) = c('Variable', 'Control vs. T1+T2+T3', 'T1 vs. T2+T3',
                         'T2 vs. T3')

table.supp = setDT(do.call(cbind.data.frame, table.test)[, c(1, 8, 10, 12)])
table.supp = table.supp[variable!='fstat']
colnames(table.supp) = c('Variable', 'Control vs. T1', 
                         'Control vs. T2', 'Control vs. T3')

save.table(
  table = table.main, output = output_balance.t_main,
  label_tex = 'tab:covariate_balance_tests_main',
  title_tex = 'Balance Tests: Standardized Mean Differences, and P-values'
)

save.table(
  table = table.supp, output = output_balance.t_supp,
  label_tex = 'tab:covariate_balance_tests_supp',
  title_tex = 'Balance Tests: Standardized Mean Differences, and P-values'
)

# End.
