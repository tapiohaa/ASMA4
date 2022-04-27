
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 2_shared_functions.R       ###
###                Replication file               ###        
###  Cost Sharing and Accessibility in Publicly   ###
###            Funded Primary Care: RCT           ###
###               2022 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Functions that are used in many R scripts.

# Install and load the following packages:
library(data.table)       # Mutating data.
library(openxlsx)         # Save as excel file.
library(stargazer)        # Save as tex file.


###
###


### A function that returns an analysis dataset. ###

extract.data = function(data, follow.up, contact.type, treat.group, 
                        control.group, outcome, type) {
  # INPUTS:
  # data: 'df'
  # follow.up: the length of the follow-up as an integer (0 = 1/21-6/21)
  # contact.type: type of the healthcare contact (e.g. nurse visits), see 'df'.
  # treat.group: a vector of letter types included, e.g., c(1,2,3) 
  # control.group: a vector of letter types included, e.g., c(0) 
  # outcome: either 'no_visits' for the annualized number of contacts or
  #       'any_visit' for having any visit in the follow-up
  # type: either 'main' or 'spillover', the latter for spillover effects 
  #       within the family.
  
  
  # Extracting the right subset of data:
  
  if(type=='main') {
    
    DT = data[follow_up==follow.up & contact_type==contact.type &
                letter.main %in% c(treat.group, control.group)]
    
    DT[, treat := as.integer(letter.main %in% treat.group)]
    
  } else if (type=='spillover') {
    
    DT = data[follow_up==follow.up & contact_type==contact.type &
                letter.spill %in% c(treat.group, control.group)]
    
    DT[, treat := as.integer(letter.spill %in% treat.group)]
    
  }
  
  
  # The outcome variable:
  
  if(outcome=='no_visits') {
    
    DT[, outcome := contacts.ann]
    DT[, outcome.did := mget(paste('contacts.pre.', as.character(contact.type), 
                                   sep=''))]
    
  } else if (outcome=='any_visit') {
    
    DT[, outcome := as.integer(contacts > 0)] 
    DT[, outcome.did := mget(paste('contacts.pre.', as.character(contact.type), 
                               sep=''))]
    DT[, outcome.did := as.integer(outcome.did > 0)]
    
  }
  
  DT[, outcome.did := outcome - outcome.did]
  
  return(DT)
  
}


### A function that saves tables in three formats: tex, xlsx, rds ###

save.table = function(table, output, label_tex, title_tex) {
  # INPUTS:
  # table: a table to be saved
  # output: file path excluding the file type (e.g. .csv)
  # label_tex: character label for .tex tables
  # title_tex: character title for .tex tables
  
  # tex:
  stargazer::stargazer(
    table, type = "text", summary = F,
    out = paste(output, "tex", sep = "."),
    title = title_tex, label = label_tex,
    rownames = F, header=F)
  
  # xlsx:
  openxlsx::write.xlsx(table, file = paste(output, "xlsx", sep = "."),
                       overwrite = TRUE)
  
  #rds:
  saveRDS(table, file = paste(output, "rds", sep = "."))
  
}


# End.
