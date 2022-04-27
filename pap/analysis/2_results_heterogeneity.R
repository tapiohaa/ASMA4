
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###         r-script 2_results_heterogeneity.R    ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT.          ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Assess the potential heterogeneity in treatment effects.
rm(list=ls())

# Install and load the following packages:

library(data.table)       # Mutating data.
library(lfe)              # linear fixed effects estimation.
library(broom)            # Statistical objects into tidy tibbles.
library(ggplot2)          # Plotting data.
library(patchwork)        # Print multiple plots into same figure.
library(mlr3)             # ML ecosystem for R.
library(mlr3tuning)       # Tuning ML models.
library(mlr3learners)     # Several ML algorithms.
library(ranger)           # Random Forest.
library(xgboost)          # XGBoost.
library(GenericML)        # Generic machine learning inference.

# Inputs:
input_data = "W:/ASMA4/data/clean/analysis_data_aggr.rds"
functions = "W:/ASMA4/analysis/2_shared_functions.R"

# Outputs:
output_table_reg_no = "W:/ASMA4/analysis/tables/het_table_no_preregistered"
output_table_reg_any = "W:/ASMA4/analysis/tables/het_table_any_preregistered"
output_table_reg_drop = "W:/ASMA4/analysis/tables/het_table_drop_preregistered"
output_table_reg_t1t23= "W:/ASMA4/analysis/tables/het_table_t1t23_preregistered"
output_pc = "W:/ASMA4/analysis/figures/het_pc_area.pdf"
output_inc_t1_t23 = "W:/ASMA4/analysis/figures/het_income_t1_t23.pdf"
output_params = "W:/ASMA4/data/clean/tuned_params.rds"
output_params_txt = "W:/ASMA4/data/clean/tuned_params.txt"
output_genericml = "W:/ASMA4/data/clean/results_generic_ml.rds"
output_comp_ml = "W:/ASMA4/analysis/tables/het_ml_methods"
output_blp = "W:/ASMA4/analysis/tables/het_blp"
output_gates_nurse = "W:/ASMA4/analysis/figures/het_gates_nurse.pdf"
output_gates_gp = "W:/ASMA4/analysis/figures/het_gates_gp.pdf"
output_clan_nurse = "W:/ASMA4/analysis/tables/het_clan_nurse"
output_clan_gp = "W:/ASMA4/analysis/tables/het_clan_gp"
output_comp_mean = "W:/ASMA4/analysis/tables/het_ml_methods_mean"

###
###

df = readRDS(input_data)

# Create an indicator for residing in a municipality where the public
# primary care services are outsources:
df[, outsourced := as.integer(municipality %in% c(398, 142, 316))]

# Read shared functions
source(functions) # save.table() and extract.data()

# Do not print the log of tuning steps:
lgr::get_logger('mlr3')$set_threshold('warn')
lgr::get_logger('bbotk')$set_threshold('warn')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1: Tests on pre-specified hypotheses on heterogeneity. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


test1 = extract.data(data=df, follow.up = 6, contact.type = 2, 
                     treat.group = c(1,2,3), control.group = 0,
                     outcome = 'no_visits', type='main')


### A function that estimates the pre-registered heterogeneity tests. ###

estimate.het = function(data, drop.heavy.users=0) {
  # INPUTS:
  # data: 'df'
  # drop.heavy.users: if 1, then drop heavy-users (defined below)
  # OUTPUT:
  # a list of data.tables containing the results
  
  
  # We extract the data on nurse and GP visits in public primary care:
  contact.types = c(1,2)
  
  # We have two outcomes: the number of visits and an indicator of having any
  # visits:
  outcomes = c('no_visits', 'any_visit')
  
  
  # Loop over outcomes:
  df.otc = lapply(outcomes, function(otc) {
    
    
    # Loop over contact types:
    
    df.het = lapply(contact.types, function(ctype) {
      
      # Extract the datasets:
      
      DT.t0.t123 = extract.data(
        data=data, follow.up = 6, contact.type = ctype, treat.group = c(1,2,3), 
        control.group = c(0), outcome = otc, type='main')
      
      DT.t1.t23 = extract.data(
        data=data, follow.up = 6, contact.type = ctype, treat.group = c(2,3), 
        control.group = c(1), outcome = otc, type='main')
      
      # Keep only those whose equivalised family disposable income is observed:
      DT.t0.t123 = DT.t0.t123[!is.na(income_eq)]
      DT.t1.t23 = DT.t1.t23[!is.na(income_eq)]
      
      # Drop or keep heavy-users:
      if(drop.heavy.users == 1) {
        DT.t0.t123 = DT.t0.t123[high.user==0]
        DT.t1.t23 = DT.t1.t23[high.user==0] }
      
      
      # Create several indicators to the datasets:
      
      DT.t0.t123[, ':=' (contacts.pre.d = as.integer(contacts.pre.1 > 0 |
                                                       contacts.pre.2 > 0),
                         age.abov.med = as.integer(age >= median(age)),
                         inc.abov.med = 
                           as.integer(income_eq >= median(income_eq)))]
      
      DT.t1.t23[, ':=' (contacts.pre.d = as.integer(contacts.pre.1 > 0 |
                                                      contacts.pre.2 > 0),
                        age.abov.med = as.integer(age >= median(age)),
                        inc.abov.med = 
                          as.integer(income_eq >= median(income_eq)))]
      
      
      # We will conduct the following tests:
      specs = list(
        list(data='t0.t123', vrbl='inc.abov.med'),
        list(data='t0.t123', vrbl='age.abov.med'),
        list(data='t0.t123', vrbl='contacts.pre.d'),
        list(data='t0.t123', vrbl='outsourced'),
        list(data='t1.t23', vrbl='inc.abov.med'))
      
      
      # Loop over specifications:
      
      results = lapply(specs, function(spec) {
        
        
        # Extract the right data and rename the variable of interest:
        
        if(spec$data=='t0.t123') {
          DT = DT.t0.t123[, mget(c('household.id', 'treat', 'municipality', 
                                   'outcome', spec$vrbl))]
        } else if (spec$data=='t1.t23') {
          DT = DT.t1.t23[, mget(c('household.id', 'treat', 'municipality', 
                                  'outcome', spec$vrbl))] }
        
        setnames(DT, old=spec$vrbl, new='het.group')
        
        
        # Estimate the regression results:
        reg = lfe::felm(outcome ~ treat*het.group | 0 | 0 | 0, data=DT) 
        reg = setDT(broom::tidy(reg, conf.int=TRUE, se.type='robust'))
        print(reg$term)
        
        
        # Estimates and std. errors as characters:
        est = reg[, .(term, estimate, std.error)]
        est[estimate < 0, est.sign := '-']
        est[estimate >= 0, est.sign := '']
        est[, ':=' (est = format(round(abs(estimate), digits=3), nsmall=3),
                    se = format(round(std.error, digits=3), nsmall=3))]
        est[, value := paste(est.sign, est, ' [', se, ']', sep='')]
        
        # P-value of treat:het.group:
        pval = reg[term=='treat:het.group', p.value]
        pval = format(round(pval, digits=3), nsmall=3)
        
        
        # ATE estimates as % changes:
        
        change.0 = 
          100* reg[term=='treat', estimate] / reg[term=='(Intercept)', estimate]
        if(change.0 < 0) { sign.0 ='-'}
        if(change.0 >= 0) { sign.0 =''}
        
        change.0 = paste(sign.0, format(round(abs(change.0), digits=2), 
                                        nsmall=2), '%', sep='')
        
        change.1 = 100 * 
          (reg[term=='treat', estimate] + 
             reg[term=='treat:het.group', estimate]) /
          (reg[term=='(Intercept)', estimate] + 
             reg[term=='het.group', estimate])
        if(change.1 < 0) { sign.1 ='-'}
        if(change.1 >= 0) { sign.1 =''}
        
        change.1 = paste(sign.1, format(round(abs(change.1), digits=2), 
                                        nsmall=2), '%', sep='')
        
        
        # Collect results and return:
        
        results = data.table(
          Variable = c('Intercept', 'TREAT', 'GROUP', 'TREAT:GROUP',
                       'P-value', 'Change (G=0)', 'Change (G=1)'),
          value = c(est[, value], pval, change.0, change.1))
        
      })
      
    })
    names(df.het) = paste0('contact_', contact.types)
    
    # Return as a table:
    
    nurse = do.call(cbind.data.frame, df.het$contact_1)[, c(1:2, 4, 6, 8, 10)]
    gp = do.call(cbind.data.frame, df.het$contact_2)[, c(1:2, 4, 6, 8, 10)]
    
    table = rbind(nurse, gp)
    colnames(table) = c('Variable', 'T0 vs. T1+T2+T3: Income above median',
                        'T0 vs. T1+T2+T3: Age above median', 
                        'T0 vs. T1+T2+T3: any leading nurse or GP contacts', 
                        'T0 vs. T1+T2+T3: outsourced',
                        'T1 vs. T2+T3: Income above median')
    
    return(table)
    
  })
  names(df.otc) = outcomes
  return(df.otc)
  
}

# Estimate the tests:
table.all = estimate.het(data=df, drop.heavy.users = 0)
table.drop = estimate.het(data=df, drop.heavy.users = 1)

# Save tables:

save.table(table = table.all$no_visits[, c(1:5)], output = output_table_reg_no, 
           label_tex = 'tab:het_table_no_preregistered',
           title_tex = 'Heterogeneity Tests, Annualized Visits.')

save.table(table = table.all$any_visit[, c(1:5)], output = output_table_reg_any, 
           label_tex = 'tab:het_table_any_preregistered',
           title_tex = 'Heterogeneity Tests, Has Any Visit.')

save.table(
  table = table.drop$no_visits[, c(1:5)], output = output_table_reg_drop, 
  label_tex = 'tab:het_table_drop_preregistered',
  title_tex = 'Heterogeneity Tests, Annualized Visits Excluding High-Users.')

table.t1t23 = cbind(table.all$no_visits[, c(1, 6)],
                    table.drop$no_visits[, 6],
                    table.all$any_visit[, 6])

save.table(
  table = table.t1t23, output = output_table_reg_t1t23, 
  label_tex = 'tab:het_table_t1t23_preregistered',
  title_tex = 'Heterogeneity Tests, T1 vs. T2+T3, Income above Median.')

  
### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2: Visual analysis of heterogeneity. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# In this section, we illustrate heterogeneity visually w.r.t. primary care 
# area and income quintile, plotting the results by subgroup. We examine the 
# following comparisons and dimensions:

specs = list(
  list(treat.group=c(1,2,3), control.group=c(0), type='Primary care area', 
       drop.heavy.users=0),
  list(treat.group=c(2,3), control.group=c(1), type='Primary care area',
       drop.heavy.users=0),
  list(treat.group=c(2,3), control.group=c(1), type='Quintile',
       drop.heavy.users=0),
  list(treat.group=c(2,3), control.group=c(1), type='Quintile',
       drop.heavy.users=1)
)


# First, create the datasets by looping over specifications:

plots.het = lapply(specs, function(spec) {
  
  
  # Extract the data on nurse and GP visits in public primary care:
  contact.types = c(1,2)
  
  # Loop over contact types:
  
  data.contact = lapply(contact.types, function(ctype) {
    
    DT = extract.data(
      data=df, follow.up = 6, contact.type = ctype, 
      treat.group = spec$treat.group, control.group = spec$control.group,
      outcome = 'no_visits', type='main')
    
    # Drop / keep heavy-users:
    if(spec$drop.heavy.users==1) {
      DT = DT[high.user==0] }
    
    # Keep only those whose equivalised family disposable income is observed:
    DT = DT[!is.na(income_eq)]
    
    # Create variable income_quintile:
    DT[, income_quintile := cut(income_eq,
                                breaks = quantile(income_eq, probs = 0:5/5),
                                labels = 1:5, right = FALSE)]
    
    # Map primary care areas to integers:
    DT[pc_area == 'Kymsote', pc_area_no := 1]
    DT[pc_area == 'PHHYKY', pc_area_no := 2]
    DT[pc_area == 'Eksote', pc_area_no := 3]
    
    
    # We will examine heterogeneity with respect to income quintile,
    # primary care area, and municipality:
    
    if(spec$type=='Quintile') {
      groups = c(1:5)
    } else if (spec$type=='Primary care area') {
      groups = c(1:3) }
    
    
    # Loop over subgroups:
    
    results.groups = lapply(groups, function(grp) {
      
      # Extract the right data:
      if(spec$type=='Quintile') {
        data = DT[income_quintile == grp]
      } else if (spec$type=='Primary care area') {
        data = DT[pc_area_no == grp] } 
      
      # Estimate CATEs by income quintile:
      reg = lfe::felm(outcome ~ treat | municipality | 0 | 0, data=data)
      reg = setDT(broom::tidy(reg, conf.int=TRUE, se.type='robust'))
      
      # Return results as a table:
      reg = reg[term=='treat'][, mget(c('estimate', 'conf.low', 'conf.high'))]
      control.mean = data[treat==0, mean(outcome)]
      reg[, ':=' (geom = 'cate', group = grp, control.mean = control.mean,
                  change.per = 100 * estimate / control.mean,
                  conf.low.per = 100 * conf.low / control.mean,
                  conf.high.per = 100 * conf.high / control.mean)]
      
    })
    results.groups = do.call(rbind.data.frame, results.groups)
    results.groups = results.groups[, mget(colnames(results.groups))]
    
    
    # Estimate the ATE for all individuals:
    ate = lfe::felm(outcome ~ treat | 0 | 0 | 0, data=DT)
    ate = setDT(broom::tidy(ate, conf.int=TRUE, se.type='robust'))
    
    # Return results as a table:
    ate = ate[term=='treat'][, mget(c('estimate', 'conf.low', 'conf.high'))]
    control.mean = DT[treat==0, mean(outcome)]
    ate[, ':=' (geom = 'ate', control.mean = control.mean,
                change.per = 100 * estimate / control.mean,
                conf.low.per = 100 * conf.low / control.mean,
                conf.high.per = 100 * conf.high / control.mean)]
    
    ate = cbind(CJ(group=groups), ate) 
    
    results.groups = rbind(ate, results.groups)
    
    if(ctype==1) { results.groups[, profession.f := 'Nurse visits']}
    if(ctype==2) { results.groups[, profession.f := 'GP visits']}
    
    return(results.groups)
    
  })
  names(data.contact) = paste0('contact_', contact.types)
  
  df.groups = rbind(data.contact$contact_1, data.contact$contact_2)
  df.groups$profession.f = factor(df.groups$profession.f, 
                                  levels=c('Nurse visits', 'GP visits'))
  
  
  # Now, we have data ready for plotting. Next, we will create the plots. 
  # We will show the estimates as annualized visits and % changes:
  
  plot.labs = c('Annualized visits', 'Ann. visits as a % change')
  
  plots = lapply(plot.labs, function(y.lab) {
    
    if(y.lab == 'Annualized visits') {
      
      p = ggplot(data=df.groups[geom=='cate'], aes(x=group, y=estimate)) +
        geom_line(data=df.groups[geom=='ate'], aes(x=group, y=estimate)) +
        geom_ribbon(data=df.groups[geom=='ate'], 
                    aes(ymin=conf.low, ymax=conf.high), alpha = 0.1,
                    color='black') +
        geom_segment(aes(x=group, xend=group, y=conf.low, yend=conf.high))  
      
    } else if (y.lab == 'Ann. visits as a % change') {
      
      p = ggplot(data=df.groups[geom=='cate'], aes(x=group, y=change.per)) +
        geom_line(data=df.groups[geom=='ate'], aes(x=group, y=change.per)) +
        geom_ribbon(data=df.groups[geom=='ate'], 
                    aes(ymin=conf.low.per, ymax=conf.high.per), alpha = 0.1,
                    color='black') +
        geom_segment(
          aes(x=group, xend=group, y=conf.low.per, yend=conf.high.per)) 
      
    }
    
    
    # Quintiles can be ordered. Use geom_line. Also add labels to primary
    # care areas:
    
    if(spec$type=='Quintile') { 
      p = p + geom_line() +
        scale_x_continuous(breaks=c(1:5))
    } else if (spec$type=='Primary care area') {
      p = p + scale_x_continuous(breaks=c(1:3), labels = c('KL', 'PH', 'SK')) }
    
    
    # The rest of the plot:
    p = p + geom_point() +
      facet_grid( ~ profession.f) +
      ylab(y.lab) +
      xlab(spec$type) + 
      geom_hline(yintercept = 0, linetype='dashed') +
      theme(text = element_text(size=15),   
            axis.text.x = element_text(hjust = 1),
            panel.background = element_rect(fill = "white", colour = "white"),
            panel.grid.major = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.grid.minor = element_line(size = 0.25, linetype = "solid", 
                                            colour = "lightgrey"),
            panel.border = element_rect(colour = "black", fill = NA, size= 0.5))
  })
 
}) 
  

# Save plots:

cairo_pdf(filename = output_pc, width = 10.0, height = 10.0)
print(
  patchwork::wrap_elements(panel = plots.het[[1]][[1]] + plots.het[[1]][[2]]) + 
    ggtitle('A. Controls (T0) vs. any reminder (T1+T2+T3)') + 
    patchwork::wrap_elements(panel= plots.het[[2]][[1]] + plots.het[[2]][[2]]) + 
    ggtitle('B. The base reminder (T1) vs. the copayment reminder (T2+T3)') + 
    plot_layout(ncol = 1) & theme(text = element_text(size=15))
) 
dev.off()

cairo_pdf(filename = output_inc_t1_t23, width = 12.0, height = 10.0)
print(
  patchwork::wrap_elements(panel = plots.het[[3]][[1]] + plots.het[[3]][[2]]) + 
    ggtitle('A. All individuals') + 
    patchwork::wrap_elements(panel= plots.het[[4]][[1]] + plots.het[[4]][[2]]) + 
    ggtitle('B. Exclude the high-users') + 
    plot_layout(ncol = 1) & theme(text = element_text(size=15))
) 
dev.off()


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3: Data-driven analysis of treatment effect heterogeneity. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Create dummies for primary care areas:
df[, pc_area_kl := as.integer(pc_area == 'Kymsote')]
df[, pc_area_ph := as.integer(pc_area == 'PHHYKY')]
df[, pc_area_sk := as.integer(pc_area == 'Eksote')]

# Outcomes: curative primary care nurse and GP visits
outcomes = c(1,2)

# Extract the datasets:

datasets = lapply(outcomes, function(otc) {
  
  DT = extract.data(data=df, follow.up = 6, contact.type = otc, 
                    treat.group = c(1,2,3), control.group = 0,
                    outcome = 'any_visit', type='main')
  
  # Include only those whose covariates are observed.
  DT = DT[!is.na(income_eq)]
  
})
names(datasets) = paste0('contact_', outcomes)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3.1: Tuning. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


### A function that tunes a given algorithm. ###

tune.algos = function(data, algo, treat.group) {
  # INPUTS:
  # data: a data.table
  # algo: ML algorithm from the set c('randomforest', 'xgboost')
  # treat.group: 1 for the treated and 0 for the reference group
  # OUTPUT:
  # a list containing the optimized hyperparameters
  
  
  df = data[treat==treat.group, 
            .(outcome, contacts.pre.1, contacts.pre.2, contacts.pre.2.5, 
              contacts.pre.3, contacts.pre.3.5, contacts.pre.4, 
              contacts.pre.4.5, age, male, origin_finn, language_fi, 
              relationship, widowed, children, apartment, educ_university, 
              pension_d, income_disp, income_eq, unemployment, 
              unemployment_long, social_assistance_d, sickness_allowance,
              pc_area_kl, pc_area_ph, pc_area_sk, diabetes, hypertension)]
  
  
  # Set tuner and resampling strategy:
  tuner = tnr('grid_search', resolution = 10)
  resampling = rsmp('holdout')
  
  # Define the task:
  measure = msr('regr.rmse')
  task = as_task_regr(df, target = 'outcome')
  
  
  # Set algorithm-specific parameters:
  
  if(algo == 'randomforest') {
    
    # Set the tuning space:
    
    vars_sqrt = floor(sqrt(length(colnames(df))-1))
    print(vars_sqrt)
    
    search_space = ps(
      mtry = p_int(lower=vars_sqrt-1, upper=vars_sqrt+1),
      num.trees = p_int(lower=80, upper=140)
    )
    
    learner = lrn('regr.ranger')
    
    # Set the budget for optimization:
    evals = trm('evals', n_evals = 30)
    
  } else if (algo == 'xgboost') {
    
    # Set the tuning space:
    search_space = ps(
      nrounds = p_int(lower=50, upper=120), 
      max_leaves = p_int(lower=5, upper=15)
    )
    
    learner = lrn('regr.xgboost', objective='reg:squarederror', eta=0.1,
                  booster='gbtree', tree_method='hist', grow_policy='lossguide')
    
    # Set the budget for optimization:
    evals = trm('evals', n_evals = 60)
    
  }
  
  
  # Initialize and optimize:
  
  instance = TuningInstanceSingleCrit$new(
    task = task,
    learner = learner, 
    resampling = resampling,
    measure = measure,
    search_space = search_space,
    terminator = evals
  )
  set.seed(12345)
  tuner$optimize(instance)
  
  print('Metric of the best model in the holdout sample:')
  print(instance$result_y)
  
  ret = list(instance$result_learner_param_vals, instance$result_y)
  names(ret) = c('params', 'result_y')
  return(ret)
}


# Specify algorithms and functions to be estimated:
algos = c('randomforest', 'xgboost')
treat.groups = c(1)


# Tune the parameters: loop over data (outcome), algorithm and treatment group:

Sys.time()

params = lapply(datasets, function(DT) {
  
  # Loop over algorithms:
  params.algos = lapply(algos, function(algo) {
    
    # Loop over treatment groups:
    params.groups = lapply(treat.groups, function(grp) {
      
      print(paste(algo, grp, sep = ":"))
      params = tune.algos(DT, algo=algo, treat.group=grp)
      
    })
    names(params.groups) = paste0('treat.', treat.groups)
    
    return(params.groups)
  })
  names(params.algos) = algos
  
  return(params.algos)
})

saveRDS(params, output_params)
writeLines(capture.output(print(params)), output_params_txt)
print(Sys.time()) # Tuning takes approximately 10 minutes.
print(params)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3.2: Estimation ####
### ### ### ### ### ### ### ### ### ### ### ### ###

seed = 12345
Sys.time()

# Estimate the results by looping over outcomes:

results = lapply(outcomes, function(otc) {
  
  print(paste('Outcome:', otc))
  
  if(otc==1) {
    DT = datasets$contact_1
    tuned = params$contact_1
  } else if (otc==2) {
    DT = datasets$contact_2 
    tuned = params$contact_2 }
  
  
  # Covariates as a matrix:
  Z = DT[, .(contacts.pre.1, contacts.pre.2, contacts.pre.2.5, contacts.pre.3, 
             contacts.pre.3.5, contacts.pre.4, contacts.pre.4.5, age, male, 
             origin_finn, language_fi, relationship, widowed, children, 
             apartment, educ_university, pension_d, income_disp, income_eq, 
             unemployment, unemployment_long, social_assistance_d, 
             sickness_allowance, pc_area_kl, pc_area_ph, pc_area_sk, diabetes, 
             hypertension)]
  Z = as.matrix(Z)
  
  
  # Collect our hyperparameter values:
  rf.mtry = tuned$randomforest$treat.1$params$mtry
  rf.trees = tuned$randomforest$treat.1$params$num.trees
  xgb.nrounds = tuned$xgboost$treat.1$params$nrounds
  xgb.maxleaves = tuned$xgboost$treat.1$params$max_leaves
  
  
  # Learners
  learners_GenericML = c(
    
    # Random forest (default):
    "mlr3::lrn('ranger')", 
    
    # Random forest (tuned):
    paste("mlr3::lrn('ranger', num.trees=", 
          rf.trees, ", mtry=", rf.mtry, ")", sep=''),
    
    # XGBoost (default):
    "mlr3::lrn('xgboost')",
    
    # XGBoost (tuned):
    paste("mlr3::lrn('xgboost', nrounds=", 
          xgb.nrounds, ", max_leaves=", xgb.maxleaves, 
          ", eta=0.1, booster='gbtree', tree_method='hist'",
          ", grow_policy='lossguide'", ")", sep='')
    
  )
  
  print(learners_GenericML)
  
  
  # Run the algorithm:
  
  x = GenericML::GenericML(
    
    # Covariates, treatment indicators, and outcomes:
    Z = Z, D = DT$treat, Y = DT$outcome,
    
    learner_propensity_score = rep(1/3, nrow(DT)),
    learners_GenericML = learners_GenericML,
    num_splits = 50,
    quantile_cutoffs = c(0.25, 0.50, 0.75),
    significance_level = 0.05,
    prop_aux = 0.5,
    seed = seed,
    store_splits = FALSE,
    
    # Control for BCA in the BLP and GATES regression:
    X1_BLP = setup_X1(funs_Z = c('B')), 
    X1_GATES = setup_X1(funs_Z = c('B')),
    
    # White's heteroskedasticity-robust covariance matrix estimator:
    vcov_BLP = setup_vcov(estimator = 'vcovHC', arguments = list(type = 'HC3')),
    
    # Heteroskedasticity-robust var. estimation of the diff. of 2 CLAN targets:
    equal_variances_CLAN = FALSE,
    
    # No Horvitz-Thompson transformation:
    HT = FALSE,
    
    # Subtract the value among the least affected from the most affected.
    diff_GATES = setup_diff(subtract_from = 'most', subtracted = 1),
    diff_CLAN = setup_diff(subtract_from = 'most', subtracted = 1)
    
  )
  
  return(x)
  
})
names(results) = paste0('contact_', outcomes)
saveRDS(results, output_genericml) # Running time approximately 5 hours.
Sys.time()

# Best algorithms:
# Nurse visits: BLP (XGBoost default) and GATES (Random forest tuned)
# GP visits: BLP (XGBoost tuned) and GATES (Random forest default)
print(results) 


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3.2: Save figures and tables. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# Comparison of ML Methods:

comp = lapply(results, function(x) {
  
  table = data.table(
    'ML method' = c('Random forest (default)', 'Random forest (tuned)',
                    'XGBoost (default)', 'XGBoost (tuned)')
  )
  table = cbind(table, as.data.frame(x$best$overview))
  table[, ':=' ('Best BLP' = format(round(lambda, digits = 6), nsmal=6),
                'Best GATES' = format(round(lambda.bar, digits = 6), 
                                      nsmal=6))]
  table[, ':=' (lambda = NULL, lambda.bar = NULL)]
  
})
comp = do.call(cbind.data.frame, comp)[, c(1:3, 5:6)]

save.table(table = comp, output = output_comp_ml, 
           label_tex = 'tab:het_ml_methods',
           title_tex = 'Comparison of ML Methods.')


# BLP:

BLP = lapply(results, function(x) {
  
  print(get_BLP(x, learner='best', plot=FALSE)) 
  
  # Loop over learners:
  
  learners = rownames(x$best$overview)
  
  BLP = lapply(learners, function(learner) {
    
    data = as.data.table(get_BLP(x, learner=learner, plot=FALSE))
    
    # Round and change to absolute values:
    data[, ':=' (est = format(round(abs(Estimate), digits=3), nsmall=3),
                 low = format(round(abs(get('CB lower')), digits=3), nsmall=3),
                 high = format(round(abs(get('CB upper')), digits=3), nsmall=3),
                 p = format(round(get('Pr(>|z|)'), digits=3), nsmall=3))]
    
    # Collect signs:
    data[which(Estimate < 0), est.sign := '-']
    data[which(get('CB lower') < 0), low.sign := '-']
    data[which(get('CB upper') < 0), high.sign := '-']
    data[is.na(est.sign), est.sign := '']
    data[is.na(low.sign), low.sign := '']
    data[is.na(high.sign), high.sign := '']
    
    # Variables that will be returned:
    data[, ':=' (est = paste(est.sign, est, sep=''),
                 ci = paste('(',low.sign,low,', ', high.sign, high, ')', sep=''),
                 p = paste('[', p, ']', sep=''))]
    data = as.data.table(t(data[, .(est, ci, p)]))
    colnames(data) = c('ATE', 'HET')
    
    df.table = data.table(Variable = c('Estimate', 'CI', 'P-value'))
    df.table = cbind(df.table, data)
    
  })
  BLP = do.call(rbind.data.frame, BLP)
  
})
BLP = do.call(cbind.data.frame, BLP)[, c(1:3, 5:6)]

save.table(table = BLP, output = output_blp, 
           label_tex = 'tab:het_blp',
           title_tex = 'Best Linear Predictor (BLP) of Reminders.')


# GATES:

GATES = lapply(results, function(x) {
  
  learners = rownames(x$best$overview)
  
  p = lapply(learners, function(learner) {
    p = plot(x, learner=learner, type='GATES') 
  })
  
  plot = p[[1]] + ggtitle('Random forest (default)') +
    p[[2]] + ggtitle('Random forest (tuned)') +
    p[[3]] + ggtitle('XGBoost (default)') +
    p[[4]] + ggtitle('XGBoost (tuned)') +
    plot_layout(guides = 'collect') & 
    theme(legend.position = 'bottom', text = element_text(size=15),
          panel.border = element_rect(colour="black", fill=NA, size=0.5))
  
})
names(GATES) = paste0('contact_', outcomes)

cairo_pdf(filename = output_gates_nurse, width = 10.0, height = 10.0)
print(GATES$contact_1)
dev.off()

cairo_pdf(filename = output_gates_gp, width = 10.0, height = 10.0)
print(GATES$contact_2)
dev.off()


# CLAN:

# Create a list of variables that will be compared between the most affected
# and the least affected groups:

variables = list(
  # Annualized healthcare contacts prior to the reform:
  list(var_name='contacts.pre.1', var_label='Primary care nurse visits'),
  list(var_name='contacts.pre.2', var_label='Primary care GP visits'),
  list(var_name='contacts.pre.2.5', var_label='Private outpatient doctor visits'),
  list(var_name='contacts.pre.3', var_label='Prescriptions from public sector'),
  list(var_name='contacts.pre.3.5', var_label='Prescriptions from private sector'),
  list(var_name='contacts.pre.4', var_label='Referrals from health centers'),
  list(var_name='contacts.pre.4.5', var_label='Referrals from private clinics'),
  
  # Sociodemographic outcomes:
  list(var_name='age', var_label='Age'),
  list(var_name='male', var_label='Is male'),
  list(var_name='origin_finn', var_label='Has Finnish background'),
  list(var_name='language_fi', var_label='Native language Finnish'),
  list(var_name='relationship', var_label='In relationship'),
  list(var_name='widowed', var_label='Widowed'),
  list(var_name='children', var_label='Children living at home'),
  list(var_name='apartment', var_label='Apartment in a block of flats'),
  
  # Education and labor market outcomes:
  list(var_name='educ_university', var_label='Degree from tertiary education'),
  list(var_name='pension_d', var_label='Pensioner'),
  list(var_name='income_disp', var_label='Disposable income'),
  list(var_name='income_eq', var_label='Equivalized disposable income'),
  list(var_name='unemployment', var_label='Unemployed for at least one day'),
  list(var_name='unemployment_long', var_label='Unemployd for at least 197 days'),
  list(var_name='social_assistance_d', var_label='Received social assistance'),
  list(var_name='sickness_allowance', var_label='Received sickness allowance')
)


CLAN = lapply(results, function(x) {
  
  # Loop over variables to create a CLAN table:
  
  table = lapply(variables, function(vrbl) {
    
    data = get_CLAN(x, learner='best', plot=FALSE, variable = vrbl$var_name)
    
    # Extract G1, G4, and G4-G1:
    parameters = rownames(data)
    data = as.data.table(data)
    data[, param := parameters]
    data = data[param %in% c('delta.1', 'delta.4', 'delta.4-delta.1')]
    
    # Round and change to absolute values:
    data[, ':=' (est = format(round(abs(Estimate), digits=3), nsmall=3),
                 low = format(round(abs(get('CB lower')), digits=3), nsmall=3),
                 high= format(round(abs(get('CB upper')), digits=3), nsmall=3))]
    data[, ':=' (est = trimws(est), low = trimws(low), high = trimws(high))]
    
    # Collect signs:
    data[which(Estimate < 0), est.sign := '-']
    data[which(get('CB lower') < 0), low.sign := '-']
    data[which(get('CB upper') < 0), high.sign := '-']
    data[is.na(est.sign), est.sign := '']
    data[is.na(low.sign), low.sign := '']
    data[is.na(high.sign), high.sign := '']
    
    # Variables that will be returned:
    data[, ':=' (est = paste(est.sign, est, sep=''),
                 ci= paste('(',low.sign,low,', ',high.sign, high, ')', sep=''))]
    data[, est := paste(est, ci)]
    data = as.data.table(t(data[, .(est)]))
    colnames(data) = c('G1', 'G4', 'G4-G1')
    
    # Add variable name:
    table = data.table(Variable = vrbl$var_label)
    table = cbind(table, data)
    
    # Remove CIs from the table:
    table[, ':=' (G1 = gsub("\\s*\\([^\\)]+\\)", '', G1),
                  G4 = gsub("\\s*\\([^\\)]+\\)", '', G4))]
    
  })
  table = do.call(rbind.data.frame, table)
  
})
names(CLAN) = paste0('contacts_', outcomes)

save.table(table = CLAN$contacts_1, output = output_clan_nurse, 
           label_tex = 'tab:het_clan_nurse',
           title_tex = 'CLAN of Reminders, Nurse Visits.')

save.table(table = CLAN$contacts_2, output = output_clan_gp, 
           label_tex = 'tab:het_clan_gp',
           title_tex = 'CLAN of Reminders, GP Visits.')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3.3: How well does a constant term (outcome mean) predict? ####
### ### ### ### ### ### ### ### ### ### ### ### ###

# Here, we compare default random forest, XGBoost and predicting the outcome 
# mean in 50 random 50:50 splits in terms of predictive accuracy:
set.seed(12345)


# Loop over contact types:

predictions = lapply(outcomes, function(otc) {
  
  
  print(paste('Outcome:', otc))
  
  if(otc==1) {
    DT = datasets$contact_1
    tuned = params$contact_1
  } else if (otc==2) {
    DT = datasets$contact_2 
    tuned = params$contact_2 }
  
  
  # Collect our hyperparameter values:
  rf.mtry = tuned$randomforest$treat.1$params$mtry
  rf.trees = tuned$randomforest$treat.1$params$num.trees
  xgb.nrounds = tuned$xgboost$treat.1$params$nrounds
  xgb.maxleaves = tuned$xgboost$treat.1$params$max_leaves
  
  
  # Extract the relevant variables:
  df = DT[, .(id, treat, outcome, contacts.pre.1, contacts.pre.2, 
              contacts.pre.2.5, contacts.pre.3, contacts.pre.3.5, 
              contacts.pre.4, contacts.pre.4.5, age, male, origin_finn, 
              language_fi, relationship, widowed, children, apartment, 
              educ_university, pension_d, income_disp, income_eq, unemployment, 
              unemployment_long, social_assistance_d, sickness_allowance,
              pc_area_kl, pc_area_ph, pc_area_sk, diabetes, hypertension)]

  
  # Loop over splits:
  
  splits = lapply(1:50, function(i) {
    
    # Split the data into a train set and a holdout set:
    train = df[sample(.N, floor(1/2 * nrow(DT)))]
    test = df[!(id %in% train[, unique(id)])]
    train[, id := NULL]
    test[, id := NULL]
  
    
    
    # Loop over treat.groups:
    
    groups = lapply(c(1,0), function(grp) {
      
      # Extract the datasets:
      train.grp = train[treat==grp][, treat := NULL]
      test.grp = test[treat==grp][, treat := NULL]
      
      
      # Prediction using the outcome mean:
      pred.mean = train.grp[, mean(outcome)]
  
      # RMSE using the outcome mean:
      rmse.mean = 
        sqrt(sum((pred.mean - test.grp[, outcome])^2) / nrow(test.grp))
      
      
      # Random forest:
      
      rf = ranger::ranger(outcome ~ ., data=train.grp,
                          num.trees = rf.trees, mtry = rf.mtry)
      pred.rf = predict(rf, data=test.grp)
      rmse.rf = 
        sqrt(sum((pred.rf$predictions - test.grp[, outcome])^2) / 
               nrow(test.grp))
      
      # XGBoost:
      
      xgb = xgboost::xgboost(
        data = as.matrix(
          train.grp[, mget(setdiff(colnames(train.grp), 'outcome'))]),
        label = train.grp[, outcome],
        nrounds=xgb.nrounds, max_leaves=xgb.maxleaves,, verbose = 0, 
        eta=0.1, booster='gbtree', tree_method='hist', grow_policy='lossguide'
      )
      
      pred.xgb = predict(
        xgb, as.matrix(
          test.grp[, mget(setdiff(colnames(test.grp), 'outcome'))]))
      
      rmse.xgb = 
        sqrt(sum((pred.xgb - test.grp[, outcome])^2) / nrow(test.grp))
      
      
      # Return as a table:
      results = data.table(rmse=c(rmse.mean, rmse.rf, rmse.xgb),
                           method=c('Use outcome mean', 'Random forest (tuned)', 
                                    'XGBoost (tuned)'),
                           treat.group=grp)
      
    })
    groups = do.call(rbind.data.frame, groups)
    
  })
  splits = do.call(rbind.data.frame, splits)
  splits = splits[, mget(colnames(splits))]
  
  splits = splits[, .(rmse = median(rmse)), by=c('method', 'treat.group')]
  
})
predictions = do.call(cbind.data.frame, predictions)[, c(1, 3, 6)]

save.table(table = predictions, output = output_comp_mean, 
           label_tex = 'tab:het_ml_methods_mean',
           title_tex = 'Comparison of ML Methods, Predict Outcome Mean.')

# End.
