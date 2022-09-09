
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script 2_results_covariates.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate the main results by exploiting covariates.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(lfe)              # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.
library(MatchIt)          # Matching methods.
library(ggplot2)          # Plotting data.

# Inputs:
input_data = "W:/ASMA4/data/clean/analysis_data_aggr.rds"
functions = "W:/ASMA4/analysis/2_shared_functions.R"

# Outputs:

# Include controls linearly in the regression formula:
output_ctrl_no = "W:/ASMA4/analysis/tables/ctrl_no_visits"
output_ctrl_any = "W:/ASMA4/analysis/tables/ctrl_any_visit"
output_ctrl_privat_no = "W:/ASMA4/analysis/tables/ctrl_spill_privat_no_visits"
output_ctrl_privat_any = "W:/ASMA4/analysis/tables/ctrl_spill_privat_any_visit"
output_ctrl_spill_hh = "W:/ASMA4/analysis/tables/ctrl_spill_households"

# RCT-DID results:
output_did_no = "W:/ASMA4/analysis/tables/did_no_visits"
output_did_any = "W:/ASMA4/analysis/tables/did_any_visit"
output_did_privat_no = "W:/ASMA4/analysis/tables/did_spill_privat_no_visits"
output_did_privat_any = "W:/ASMA4/analysis/tables/did_spill_privat_any_visit"
output_did_spill_hh = "W:/ASMA4/analysis/tables/did_spill_households"

# Subclassification matching:
output_match_no = "W:/ASMA4/analysis/tables/match_no_visits"
output_match_any = "W:/ASMA4/analysis/tables/match_any_visit"
output_match_privat_no = "W:/ASMA4/analysis/tables/match_spill_privat_no_visits"
output_match_privat_any ="W:/ASMA4/analysis/tables/match_spill_privat_any_visit"
output_spill_hh_match = "W:/ASMA4/analysis/tables/main_spill_households_match"
output_balance_match = "W:/ASMA4/analysis/figures/balance_plot_match.pdf"

###
###

df = readRDS(input_data)

# Read shared functions
source(functions) # save.table(), extract.data()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Functions for estimation. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


test1 = extract.data(data=df, follow.up = 6, contact.type = 1, 
                     treat.group = c(1), control.group = 0,
                     outcome = 'no_visits', type='main')


### A function that estimates the results using regression. ###

estimate.lm = function(data, ctrl.type='ctrl') {
  # INPUTS:
  # data: an output from extract.data()
  # ctrl.type: 'ctrl' for including controls linearly or 'did' for RCT-DID.
  # OUTPUT: 
  # regression results as a table
  
  
  # Include only those observations that do not have missing covariates:
  data = data[!is.na(income_eq)]
  
  
  # Estimate the results:
  
  if(ctrl.type == 'ctrl') {
    
    reg = lfe::felm(
      outcome ~ treat + contacts.pre.1 + contacts.pre.2 + contacts.pre.2.5 + 
        contacts.pre.3.5 + contacts.pre.4 + contacts.pre.4.5 + age + male + 
        language_fi + relationship + children + apartment + educ_university + 
        income_eq + unemployment + social_assistance_d + sickness_allowance |
        municipality | 0 | 0, data=data
    )
    
  } else if (ctrl.type == 'did') {
    reg = lfe::felm(outcome.did ~ treat | municipality | 0 | 0, 
                    data=data) 
  }
  
  reg = setDT(broom::tidy(reg, conf.int=TRUE, se.type='robust'))
  reg = reg[term=='treat'][, ':=' (term = NULL, statistic = NULL)]
  
  
  # Control mean and percentage change:
  control.mean = data[treat==0, mean(outcome)]
  change.per = 100 * reg[, estimate] / control.mean
  
  
  # Sample sizes:
  n.treat = nrow(data[treat==1])
  n.control = nrow(data[treat==0])
  
  reg[, ':=' (control.mean = control.mean, change.per = change.per,
              n.treat = n.treat, n.control = n.control, ctrl.type = ctrl.type)]
  return(reg)
}

print(estimate.lm(test1))


### A function that estimates the main results using matching. ###

estimate.match = function(data, subclasses=10000) {
  # INPUTS:
  # data: an output from extract.data()
  # OUTPUT: 
  # regression results as a table
  
  
  # MatchIt does not allow for NAs in covariates:
  DT = data[!is.na(income_eq)][, id := NULL]
  
  # Extract the matched data:
  
  m.out = MatchIt::matchit(
    treat ~ contacts.pre.1 + contacts.pre.2 + contacts.pre.2.5 + 
      contacts.pre.3.5 + contacts.pre.4 + contacts.pre.4.5 + age + male + 
      language_fi + relationship + children + apartment + educ_university + 
      income_eq + unemployment + social_assistance_d + sickness_allowance,
    data=DT, method='subclass', distance='glm', 
    link = 'logit', estimand = 'ATE', discard = 'none', subclass=subclasses
  )
  
  m.data = MatchIt::match.data(m.out)
  
  
  # Take an OLS model to the matched data:
  
  reg = lfe::felm(
    outcome ~ treat | 0 | 0 | 0,
    data=m.data, weights = m.data$weights
  )
  
  reg = setDT(broom::tidy(reg, conf.int=TRUE, se.type='robust'))
  reg = reg[term=='treat'][, ':=' (term = NULL, statistic = NULL)]
  
  
  # Control mean and percentage change:
  control.mean = DT[treat==0, mean(outcome, w=weights)]
  change.per = 100 * reg[, estimate] / control.mean
  
  # Sample sizes:
  n.treat = nrow(DT[treat==1])
  n.control = nrow(DT[treat==0])
  
  reg[, ':=' (control.mean = control.mean, change.per = change.per,
              n.treat = n.treat, n.control = n.control, ctrl.type = 'match')]
  return(reg)
  
}

print(estimate.match(test1))


### A function that estimates the results using several specifications
# that are provided as a list. ###

estimate = function(data, specs) {
  # INPUTS:
  # data: 'df'
  # specs: a list containing follow.ups, outcomes, and contact.types as vectors,
  #       comparisons as a list of lists (containing treat.group and 
  #       control.group), and type as a character string. See below.
  # OUTPUTS:
  # a DT containing the results
  
  
  # Loop over follow-ups:
  table.follow.up = lapply(specs$follow.ups, function(fup) {
    
    print(paste('Follow-up:', fup))
    
    
    # Loop over outcomes:
    table.outcomes = lapply(specs$outcomes, function(otc) {
      
      print(paste('Outcome:', otc))
      
      
      # Loop over contact types:
      table.types = lapply(specs$contact.types, function(type) {
        
        print(paste('Contact type:', type))
        
        
        # Loop over comparisons:
        table.comparisons = lapply(1:length(specs$comparisons), function(i) {
          
          comp = specs$comparisons[[i]]
          
          
          # Extract the data:
          data = extract.data(
            data=data, follow.up = fup, contact.type = type, 
            treat.group = comp$treat.group, control.group = comp$control.group, 
            outcome = otc, type=specs$analysis.type
          )
          
          # Estimate the effects:
          ctrl = estimate.lm(data = data, ctrl.type = 'ctrl')
          did = estimate.lm(data = data, ctrl.type = 'did')
          match = estimate.match(data = data, subclasses = specs$subclasses)
          
          results = rbind(ctrl, did, match)
          results[, comparison := names(specs$comparisons)[i]]
          
        })
        table.comparisons = do.call(rbind.data.frame, table.comparisons)
        table.comparisons = 
          table.comparisons[, mget(colnames(table.comparisons))]
        
        
        table.comparisons[, contact.type := type]
      })
      table.types = do.call(rbind.data.frame, table.types)
      table.types = table.types[, mget(colnames(table.types))]
      
      table.types[, outcome := otc]
    })
    table.outcomes = do.call(rbind.data.frame, table.outcomes)
    table.outcomes = table.outcomes[, mget(colnames(table.outcomes))]
    
    table.outcomes[, follow.up := fup]
    
  })
  table.follow.up = do.call(rbind.data.frame, table.follow.up)
  table.follow.up = table.follow.up[, mget(colnames(table.follow.up))]
  
  table.follow.up[, analysis.type := specs$analysis.type]
  
}


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Estimate. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# The main results:

specs = list(
  follow.ups = c(6),
  outcomes = c('no_visits', 'any_visit'),
  contact.types = c(1, 2, 3, 4),
  comparisons = list(
    'No vs. any reminder' = 
      list(treat.group=c(1,2,3), control.group=c(0)),
    'Base reminder vs. copayment reminder' = 
      list(treat.group=c(2,3), control.group=c(1)),
    'T2 vs. T3' = list(treat.group=c(3), control.group=c(2))
  ), 
  analysis.type = 'main',
  subclasses = 10000
)

main = estimate(data=df, specs=specs)


# Spillovers to private outpatient care:

specs = list(
  follow.ups = c(6),
  outcomes = c('no_visits', 'any_visit'),
  contact.types = c(2.5, 3.5, 4.5),
  comparisons = list(
    'No vs. any reminder' = 
      list(treat.group=c(1,2,3), control.group=c(0)),
    'Base reminder vs. copayment reminder' = 
      list(treat.group=c(2,3), control.group=c(1))
  ), 
  analysis.type = 'main',
  subclasses = 10000
)

supp.pri = estimate(data=df, specs=specs)


# Look for spillovers within households:

specs = list(
  follow.ups = c(6),
  outcomes = c('no_visits', 'any_visit'),
  contact.types = c(1, 2),
  comparisons = list(
    'No vs. any reminder' = 
      list(treat.group=c(1,2,3), control.group=c(0)),
    'Base reminder vs. copayment reminder' = 
      list(treat.group=c(2,3), control.group=c(1))
  ), 
  analysis.type = 'spillover',
  subclasses = 5000
)

supp.spill = estimate(data=df, specs=specs)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Tables. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


create.table = function(data, contact.types) {
  # INPUTS:
  # data: either 'main' or 'supp.spill'
  # contact.types: a vector of contact types, e.g., c(1,2,3,4)
  # OUTPUTS:
  # a list of tidied result tables
  
  
  # We will loop over the following methods, comparisons and outcomes:
  
  methods = c('ctrl', 'did', 'match')
  comparisons = c('No vs. any reminder',
                  'Base reminder vs. copayment reminder')
  outcomes = c('no_visits', 'any_visit')
  
  
  # Loop over methods:
  table.methods = lapply(methods, function(method) {
    
    
    # Loop over outcomes:
    table.outcomes = lapply(outcomes, function(otc) {
      
      
      # Loop over contact types:
      table.ctypes = lapply(contact.types, function(ctype) {
        
        
        # Loop over comparisons:
        table.comp = lapply(comparisons, function(comp) {
          
          row = data[outcome==otc & comparison==comp & contact.type==ctype &
                       ctrl.type==method,
                     .(control.mean, estimate, std.error, p.value, change.per,
                       n.treat, n.control)]
          
          # Mutate shares to percentages is the outcome is 'any_visit':
          if(otc=='any_visit') {
            row[, ':=' (control.mean=100*control.mean, estimate=100*estimate,
                        std.error=100*std.error)]
          }
          
          setnames(row, old=c('control.mean', 'estimate', 'std.error', 
                              'p.value', 'change.per','n.treat', 'n.control'), 
                   new=c('Reference group mean', 'Estimate', 'Std. error', 
                         'P-value', 'Change (%)','N: treated', 
                         'N: reference group'))
          
          DT = data.table(Metric=colnames(row), Value=t(row))
          
        })
        table.comp = do.call(rbind.data.frame, table.comp)
        
        
      })
      keep_cols = seq(2, 2*length(contact.types), by=2)
      table.ctypes = do.call(cbind.data.frame, table.ctypes)[, c(1, keep_cols)]
      colnames(table.ctypes)[2:c(length(contact.types)+1)] =
        paste0('contact_', contact.types)
      
      return(table.ctypes)
    })
    names(table.outcomes) = outcomes
    
    
    return(table.outcomes)
  })
  names(table.methods) = methods
  
  return(table.methods)
}

table.main = create.table(data=main, contact.types = c(1,2,3,4))
table.pri = create.table(data=supp.pri, contact.types = c(2.5, 3.5, 4.5))
table.spill = create.table(data=supp.spill, contact.types = c(1,2))


# Save tables:

# Include controls linearly in the OLS formula:

save.table(
  table = table.main$ctrl$no_visits, output = output_ctrl_no, 
  label_tex = 'tab:ctrl_no_visits',
  title_tex = 'Public Primary Care, OLS with Controls, Visits per Capita')

save.table(table = table.main$ctrl$any_visit, output = output_ctrl_any, 
           label_tex = 'tab:ctrl_any_visit',
           title_tex = 'Public Primary Care, OLS with Controls, Has any Visits')

save.table(table = table.pri$ctrl$no_visits, output = output_ctrl_privat_no, 
           label_tex = 'tab:ctrl_spill_privat_no_visits',
           title_tex = 'Private Services, OLS with Controls, Visits per Capita')

save.table(table = table.pri$ctrl$any_visit, output = output_ctrl_privat_any, 
           label_tex = 'tab:ctrl_spill_privat_any_visit',
           title_tex = 'Private Services, OLS with Controls, Has Any Visit')

table.spill.new = 
  cbind(table.spill$ctrl$no_visits, table.spill$ctrl$any_visit)[, c(1:3, 5:6)]
save.table(table = table.spill.new, output = output_ctrl_spill_hh, 
           label_tex = 'tab:ctrl_spill_households',
           title_tex = 'Household Spillovers, OLS with Controls')


# RCT-DID results:

save.table(
  table = table.main$did$no_visits, output = output_did_no, 
  label_tex = 'tab:did_no_visits',
  title_tex = 'Public Primary Care, RCT-DID, Visits per Capita')

save.table(table = table.main$did$any_visit, output = output_did_any, 
           label_tex = 'tab:did_any_visit',
           title_tex = 'Public Primary Care, RCT-DID, Has any Visits')

save.table(table = table.pri$did$no_visits, output = output_did_privat_no, 
           label_tex = 'tab:did_spill_privat_no_visits',
           title_tex = 'Private Services, RCT-DID, Visits per Capita')

save.table(table = table.pri$did$any_visit, output = output_did_privat_any, 
           label_tex = 'tab:did_spill_privat_any_visit',
           title_tex = 'Private Services, RCT-DID, Has Any Visit')

table.spill.new = 
  cbind(table.spill$did$no_visits, table.spill$did$any_visit)[, c(1:3, 5:6)]
save.table(table = table.spill.new, output = output_did_spill_hh, 
           label_tex = 'tab:did_spill_households',
           title_tex = 'Household Spillovers, RCT-DID')


# Subclassification matching:

save.table(
  table = table.main$match$no_visits, output = output_match_no, 
  label_tex = 'tab:match_no_visits',
  title_tex = 'Public Primary Care, Matching, Visits per Capita')

save.table(table = table.main$match$any_visit, output = output_match_any, 
           label_tex = 'tab:match_any_visit',
           title_tex = 'Public Primary Care, Matching, Has any Visits')

save.table(table = table.pri$match$no_visits, output = output_match_privat_no, 
           label_tex = 'tab:match_spill_privat_no_visits',
           title_tex = 'Private Services, Matching, Visits per Capita')

save.table(table = table.pri$match$any_visit, output = output_match_privat_any, 
           label_tex = 'tab:match_spill_privat_any_visit',
           title_tex = 'Private Services, Matching, Has Any Visit')

table.spill.new = 
  cbind(table.spill$match$no_visits, table.spill$match$any_visit)[, c(1:3, 5:6)]
save.table(table = table.spill.new, output = output_spill_hh_match, 
           label_tex = 'tab:main_spill_households_match',
           title_tex = 'Household Spillovers, Matching')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: Assess balance after subclassification matching. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


specs = list(
  follow.ups = c(6),
  outcomes = c('no_visits'),
  contact.types = c(1),
  comparisons = list(
    'No vs. any reminder' = 
      list(treat.group=c(1,2,3), control.group=c(0)),
    'Base reminder vs. copayment reminder' = 
      list(treat.group=c(2,3), control.group=c(1)),
    'T2 vs. T3' = list(treat.group=c(3), control.group=c(2))
  ), 
  analysis.type = 'main'
)


# Loop over comparisons:
plots.comp = lapply(c(1:length(specs$comparisons)), function(i) {
  comp = specs$comparisons[[i]]
  print(paste('Comparison:', comp))
  
  
  # Extract the data:
  data = extract.data(
    data=df, follow.up = specs$follow.ups, contact.type = specs$contact.types, 
    treat.group = comp$treat.group, control.group = comp$control.group,
    outcome = specs$outcomes, type=specs$analysis.type
  )
  
  # MatchIt does not allow for NAs in covariates:
  data = data[!is.na(income_eq)][, id := NULL]
  
  
  # Subclassification matching based on propensity scores estimated with
  # logistic regression:
  
  form = 'treat ~ contacts.pre.1 + contacts.pre.2 + contacts.pre.2.5 + 
    contacts.pre.3.5 + contacts.pre.4 + contacts.pre.4.5 + age + male + 
    language_fi + relationship + children + apartment + educ_university + 
    income_eq + unemployment + social_assistance_d + sickness_allowance'
  
  m.out = MatchIt::matchit(
    as.formula(form), data=data, method='subclass', distance='glm', 
    link = 'logit', estimand = 'ATE', discard = 'none', subclass=10000
  )
  print(m.out)

  m.data = MatchIt::match.data(m.out)
  m.out = summary(m.out, data=data,
                  addlvariables =  ~ contacts.pre.3 + origin_finn + widowed + 
                    pension_d + income_disp + unemployment_long)
  
  
  # Compute the F-test p-value:
  reg = lfe::felm(as.formula(paste(form, '| 0 | 0 | 0')), 
                  data=m.data, weights = m.data$weights)
  pval = round(summary(reg, robust=TRUE)$P.fstat['p.F'], digits=3)
  

  # Extract data for plotting:
  
  DT = data.table(Variable = rownames(m.out$reduction),
                  All = m.out$sum.all[, 'Std. Mean Diff.'],
                  Matched = m.out$sum.across[, 'Std. Mean Diff.'])
  
  # Replace 'distance' with 'propensity score':
  DT[Variable=='distance', Variable := 'propensity score']

  DT$Variable_f = factor(DT$Variable, levels=rev(DT$Variable))
  DT[, ':=' (Variable_no = rev(c(1:nrow(DT))),
             Variable = NULL)]
  
  DT = melt(DT, id.vars = c('Variable_f', 'Variable_no'), 
            measure.vars = c('All', 'Matched'), variable.name = 'Sample')
  
  DT = DT[, ':=' (value = abs(value),
                  title = names(specs$comparisons)[[i]],
                  y_lab = '',
                  f.pval = pval)]

  return(DT)  
})
plots.comp = do.call(rbind.data.frame, plots.comp)


# Plot:

plots.comp$comp_f = factor(plots.comp$title, levels = names(specs$comparisons))

p = ggplot(plots.comp, aes(x=value, y=Variable_f, 
                           color=Sample, shape=Sample)) +
  geom_point(size=3) +
  xlab('Absolute standardized mean difference') +
  geom_vline(xintercept = 0) + 
  scale_color_manual(values=c('grey50', 'black')) + 
  theme(text = element_text(size=15),   
        axis.text.x = element_text(hjust = 1),
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                        colour = "lightgrey"),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
        legend.position = "bottom",
        axis.title.y = element_blank()) +
  facet_wrap(y_lab ~ comp_f)


# Save plots:
cairo_pdf(filename = output_balance_match, width = 15.0, height = 8.5)
print(p) 
dev.off()

# End.
