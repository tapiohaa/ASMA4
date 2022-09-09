
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            r-script 2_results_main.R          ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Estimate the main results.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(ggplot2)          # Plotting data.
library(smd)              # Compute standardized mean differences.

# Inputs:
input_data = "W:/ASMA4/data/clean/analysis_data_aggr.rds"
functions = "W:/ASMA4/analysis/2_shared_functions.R"

# Outputs:
output_plot_dyn_no = "W:/ASMA4/analysis/figures/plot_dynamic_no_visits.pdf"
output_plot_dyn_any = "W:/ASMA4/analysis/figures/plot_dynamic_any_visit.pdf"
output_main_no = "W:/ASMA4/analysis/tables/main_no_visits"
output_main_any = "W:/ASMA4/analysis/tables/main_any_visit"
output_drop_no = "W:/ASMA4/analysis/tables/drop_no_visits"
output_drop_any = "W:/ASMA4/analysis/tables/drop_any_visit"
output_main_t2t3 = "W:/ASMA4/analysis/tables/main_t2t3"
output_spill_pri_no = "W:/ASMA4/analysis/tables/spill_private_no_visits"
output_spill_pri_any = "W:/ASMA4/analysis/tables/spill_private_any_visit"
output_spill_hh = "W:/ASMA4/analysis/tables/spill_households"


###
###

df = readRDS(input_data)

# Read shared functions
source(functions) # save.table() and extract.data()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Create shared functions. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

test1 = extract.data(data=df, follow.up = 6, contact.type = 1, 
                     treat.group = c(1,2,3), control.group = 0,
                     outcome = 'no_visits', type='main')
test2 = extract.data(data=df, follow.up = 6, contact.type = 2, 
                     treat.group = c(2,3), control.group = 1,
                     outcome = 'no_visits', type='main')

### A function that estimates the main results. ###

estimate.neyman = function(data) {
  # INPUTS:
  # data: an output from extract.data()
  # OUTPUT: 
  # results as a table
  
  
  # The strata based on municipalities:
  strata = c(16, 75, 81, 98, 142, 153, 285, 286, 316, 398, 405, 416, 441, 489,
             504, 560, 576, 580, 616, 624, 689, 700, 739, 831, 935)
  
  
  # Loop over strata:
  
  results.strata = lapply(strata, function(area) {
    
    # Extract the data of a given municipality:
    df.strata = data[municipality == area]
    
    # Within-stratum ATE:
    estimate = df.strata[treat==1, mean(outcome)] - 
      df.strata[treat==0, mean(outcome)]
    
    # Within-stratum variance:
    variance = df.strata[treat==1, var(outcome)] / nrow(df.strata[treat==1]) + 
      df.strata[treat==0, var(outcome)] / nrow(df.strata[treat==0])
    
    # Stratum shares for aggregating stratum-specific effects:
    share = nrow(df.strata) / nrow(data)
    
    # Collect results to a table that will be returned:
    results = data.table(
      municipality = area, est.stratum = estimate, share.stratum = share,
      est.weighted = estimate * share, var.stratum = variance,
      var.weighted = variance * share * share
    )
    
    
  })
  results.strata = do.call(rbind.data.frame, results.strata)
  results.strata = results.strata[, mget(colnames(results.strata))]
  
  
  # Next, estimate the overall ATE and its variance:
  estimate = results.strata[, sum(est.weighted)]
  std.error = sqrt(results.strata[, sum(var.weighted)])
  
  # Confidence intervals:
  conf.low = estimate - qnorm(0.05/2,lower=F) * std.error
  conf.high = estimate + qnorm(0.05/2,lower=F) * std.error
  
  # Control mean and percentage change in outcome:
  control.mean = data[treat==0, mean(outcome)]
  change.per = 100 * estimate / control.mean
  
  # Standardized mean difference:
  std.diff = smd::smd(x=data[, outcome], g=data$treat, 
                      gref=2, na.rm=TRUE)$estimate
  
  # Sample sizes:
  n.treat = nrow(data[treat==1])
  n.control = nrow(data[treat==0])
                   
  results = data.table(estimate, std.error, conf.low, conf.high, control.mean, 
                       change.per, std.diff, n.treat, n.control)

}

print(estimate.neyman(test1)) 


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
          data.all = extract.data(
            data=data, follow.up = fup, contact.type = type, 
            treat.group = comp$treat.group, control.group = comp$control.group, 
            outcome = otc, type=specs$analysis.type
          )
          
          # Create another dataset that drops heavy-users:
          data.drop = data.all[high.user == 0]
          
          # Estimate the effects:
          
          reg.1 = estimate.neyman(data = data.all)
          reg.1[, ':=' (comparison = names(specs$comparisons)[i],
                        drop.heavy.users = 0)]
          
          reg.2 = estimate.neyman(data = data.drop)
          reg.2[, ':=' (comparison = names(specs$comparisons)[i],
                        drop.heavy.users = 1)]
          
          return(rbind(reg.1, reg.2))
          
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
  analysis.type = 'main'
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
  analysis.type = 'main'
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
  analysis.type = 'spillover'
)

supp.spill = estimate(data=df, specs=specs)


# Illustrate how the estimates evolve as a function of the follow-up:

specs = list(
  follow.ups = c(0,1,2,3,4,5,6,7), # c(0:10) in the PAP; currently, we have data only up to 5/2022.
  outcomes = c('no_visits', 'any_visit'),
  contact.types = c(1, 2),
  comparisons = list(
    'No vs. any reminder' = 
      list(treat.group=c(1,2,3), control.group=c(0)),
    'Base reminder vs. copayment reminder' = 
      list(treat.group=c(2,3), control.group=c(1)),
    'T2 vs. T3' = list(treat.group=c(3), control.group=c(2))
  ), 
  analysis.type = 'main'
)

supp.dynamic = estimate(data=df, specs=specs)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Dynamic result plots. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# If outcome=='any_visit', multiply estimate and confidence interval by 100
# to get estimates of percentage point changes:

supp.dynamic[outcome=='any_visit', ':=' (estimate = 100*estimate,
                                         conf.low = 100*conf.low,
                                         conf.high = 100*conf.high)]

# Order the levels of 'contact.type' and 'comparison' for faceting:

supp.dynamic[contact.type == 1, profession := 'Nurse visits']
supp.dynamic[contact.type == 2, profession := 'GP visits']

supp.dynamic$profession_f = factor(supp.dynamic$profession, 
                                   levels=c('Nurse visits', 'GP visits'))

supp.dynamic$comparison_f = factor(supp.dynamic$comparison, 
                                   levels=c(
                                     'No vs. any reminder', 
                                     'Base reminder vs. copayment reminder',
                                     'T2 vs. T3'))


outcomes = c('no_visits', 'any_visit')

# Loop over outcomes:
plots.dynamic = lapply(outcomes, function(otc) {
  
  data = supp.dynamic[outcome==otc & drop.heavy.users == 0]
  
  # Y title depends on the outcome:
  if(otc=='no_visits') {
    y_title = "Change in annualized visits per capita"
  } else if (otc=='any_visit') {
    y_title = "Has any visit: percentage point change"
  }
  
  p.dynamic = ggplot(data=data, aes(x=follow.up, y=estimate)) +
    geom_point() +
    geom_line() +
    geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha = 0.1,
                color='black', linetype='dotted') +
    geom_hline(yintercept= 0, linetype='dashed') +
    facet_grid(profession_f ~ comparison_f) +
    scale_x_continuous(breaks=c(0:10)) + 
    geom_vline(xintercept = 6, linetype='dashed') + 
    xlab('Follow-up in months') + 
    ylab(y_title) +
    theme(text = element_text(size=15),   
          axis.text.x = element_text(hjust = 1),
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                          colour = "lightgrey"),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
          legend.position = "bottom")
  
})
names(plots.dynamic) = outcomes


# Save plots without controls:

cairo_pdf(filename = output_plot_dyn_no, width = 15.0, height = 10.0)
print(plots.dynamic$no_visits) 
dev.off()

cairo_pdf(filename = output_plot_dyn_any, width = 15.0, height = 10.0)
print(plots.dynamic$any_visit) 
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4: Tables. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that creates tidy results tables. ###

create.table = function(data, contact.types, comparisons, drop.high.users=0) {
  # INPUTS:
  # data: either 'main' or 'supp.spill'
  # contact.types: a vector of contact types, e.g., c(1,2,3,4)
  # comparisons: a vector of comparisons, e.g. c('No letter vs. any letter')
  # drop.high.users: if 1, then drop heavy-users (defined below)
  # OUTPUTS:
  # a list of tidied result tables
  
  
  # We will loop over the following comparisons, contact types, and outcomes:
  
  outcomes = c('no_visits', 'any_visit')
  
  
  # Loop over outcomes:
  table.outcomes = lapply(outcomes, function(otc) {
    
    
    # Loop over contact types:
    table.ctypes = lapply(contact.types, function(ctype) {
      
      
      # Loop over comparisons:
      table.comp = lapply(comparisons, function(comp) {
        
        row = data[outcome==otc & comparison==comp & contact.type==ctype & 
                     drop.heavy.users==drop.high.users,
                   .(control.mean, estimate, std.error, conf.low, conf.high, 
                     change.per, std.diff, n.treat, n.control)]
        
        # Mutate shares to percentages is the outcome is 'any_visit':
        if(otc=='any_visit') {
          row[, ':=' (control.mean = 100*control.mean, estimate = 100*estimate,
                      std.error = 100*std.error, conf.low = 100*conf.low,
                      conf.high = 100*conf.high)]
        }
        
        # Confidence interval as a character:
        if(row[1, conf.low] < 0) { sign.low = '-' } else { sign.low = '' }
        if(row[1, conf.high] < 0) { sign.high = '-' } else { sign.high = ''}
        conf.low = format(round(abs(row[1, conf.low]), digits=3), nsmall=3)
        conf.high = format(round(abs(row[1, conf.high]), digits=3), nsmall=3)
        ci = paste('[', sign.low, conf.low, ', ', 
                   sign.high, conf.high, ']', sep='')
        row[, ci := ci]
              
        
        # Round estimates (three digits) and save as character:
        cols = c('control.mean','estimate','std.error','change.per','std.diff')
        row[, (cols) := round(.SD, 3), .SDcols=cols]
        for(col in cols) row[, (col) := format(row[[col]], nsmall=3)]
        
        
        # Select and rename columns:
        
        row = row[, .(control.mean, estimate, std.error, ci, 
                      change.per, std.diff, n.treat, n.control)]
        
        setnames(row, old=c('control.mean', 'estimate', 'std.error', 'ci', 
                            'change.per', 'std.diff', 'n.treat', 'n.control'), 
                 new=c('Reference group mean', 'Estimate', 'Std. error', 
                       'Confidence interval', 'Change (%)',
                       'Std. mean difference', 'N: treated', 
                       'N: reference group'), skip_absent = TRUE)
        
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
}

comps = c('No vs. any reminder', 'Base reminder vs. copayment reminder', 
          'T2 vs. T3')
table.main = create.table(data=main, contact.types = c(1,2,3,4), 
                          comparisons = comps)

rows = nrow(table.main$no_visits)
table.main.no = table.main$no_visits[c(1:(rows * 2/3)), ]
table.main.any = table.main$any_visit[c(1:(rows * 2/3)), ]
table.main.t2t3 = 
  rbind(table.main$no_visits[c((rows * 2/3 + 1) : rows), ],
        table.main$any_visit[c((rows * 2/3 + 1) : rows), ])

comps = c('No vs. any reminder', 'Base reminder vs. copayment reminder')
table.pri = create.table(data=supp.pri, contact.types = c(2.5, 3.5, 4.5),
                         comparisons = comps)
table.spill = create.table(data=supp.spill, contact.types = c(1,2),
                           comparisons = comps)

table.drop = create.table(data=main, contact.types = c(1,2,3,4),
                          comparisons = comps, drop.high.users = 1)

# Save tables:

save.table(table = table.main.no, output = output_main_no, 
           label_tex = 'tab:main_no_visits',
           title_tex = 'Main Results, Visits per Capita')

save.table(table = table.main.any, output = output_main_any,
           label_tex = 'tab:main_any_visit',
           title_tex = 'Main Results, Has any Visits')

save.table(table = table.main.t2t3, output = output_main_t2t3,
           label_tex = 'tab:main_t2t3',
           title_tex = 'Main Results, T2 vs. T3')

save.table(table = table.drop$no_visits, output = output_drop_no, 
           label_tex = 'tab:drop_no_visits',
           title_tex = 'Main Results, Visits per Capita, Excluding High-Users')

save.table(table = table.drop$any_visit, output = output_drop_any,
           label_tex = 'tab:drop_any_visit',
           title_tex = 'Main Results, Has any Visits, Excluding High-Users')

save.table(table = table.pri$no_visits, output = output_spill_pri_no,
           label_tex = 'tab:spill_private_no_visits',
           title_tex = 'Spillovers to Private Services, Visits per Capita')

save.table(table = table.pri$any_visit, 
           output = output_spill_pri_any,
           label_tex = 'tab:spill_private_any_visit',
           title_tex = 'Spillovers to Private Services, Has Any Visit')

table.spill.no.ctrl = cbind(table.spill$no_visits, 
                            table.spill$any_visit)[, c(1:3, 5:6)]

save.table(table = table.spill.no.ctrl, output = output_spill_hh,
           label_tex = 'tab:spill_households',
           title_tex = 'Spillovers to Household Members')

# End.
