# A modelling/templating framework implementing the following functionality:
# 1. Serial/Parallel application of templates on a model(s)
# 2. Running a list of models and producing tidy result tables
# 3. The conversion of result tables into a named vector of values
# 4. The loading/subsitution/writing of result templates
# The code is best understood together with the example.


#install.packages("assertthat")
#install.packages("broom")
#install.packages("tidyr")

library(survival)
library(dplyr)
library(magrittr)
library(XLConnect) # Requires Java


#### Concepts ####

# MODEL: a list object with the following elements
#  model - a logical vector indicating the list is a model
#  predictors - character vector of model covariates
#  outcome_str - string containing Cox Surv() object specification
#  names - character vector of short text strings which will be concatenated to produce the model identifier/name

# MODEL TEMPLATE: a function that takes a model and returns a modified model
#  - Usually adds to, or changes existing model elements (see above).
#  - Usually adds an addition short string to the "names" element

# RESULT OBJECT: an augmented regression fit object with additional elements
#  - original fit object
#  - model specification
#  - additional result table with term names, point estimates, confidence intervals

# POSTPROCESSING: a user-supplied/applied function that adds to the result table whatever 
# formatted columns the user desires, derived from existing columns.

# RESULT VECTOR: named character vector of formatted results derived from the result tables

# RESULT INPUT TEMPLATE: an Excel .xlsx spreadsheet with formatted tables
#   whose cells contain the string identifiers which correspond to
#   the names of values in the RESULT VECTOR.

# RESULT OUTPUT TEMPLATE: an Excel .xlsx spreadsheet with formatted tables
#   whose cells contain the corresponding values substituted from th
#   RESULT VECTOR.

#### Template helper functions ####

# Applies templates to models sequentially
serial_apply <- function(models, templates){
  
  assertthat::assert_that( is.list(models)    )
  assertthat::assert_that( is.list(templates) )
  
  for(template in templates){
    assertthat::assert_that( is.function(template) )
    models <- lapply(models, template)
  }
  models %<>% update_model_names
  return(models)
  
}

# Applies M templates to N models in parallel
# Outputs a list of M*N models
parallel_apply <- function(models, templates){
  
  assertthat::assert_that( is.list(templates)    )
  assertthat::assert_that( is.list(models)       )
  assertthat::assert_that( is.null(models$model) )
  
  models <- lapply(models, 
              function(model, templates){  
                lapply(templates, function(model, template){
                  model %<>% template
                }, model=model)
              }, templates=templates) %>% 
            unlist(recursive = FALSE)
  
  models %<>% update_model_names
  return(models)
  
}

# Updates model names from "names" element in each model
# Used after templates are applied.
update_model_names <- function(models, sep = ""){
  
  assertthat::assert_that( is.list(models) )
  
  models %<>% lapply(function(m){m$fullname = paste0(m$names, collapse=sep); m }) 
  names(models) <- lapply(models, function(m){ paste0(m$names, collapse=sep)}) %>% unlist
  return(models)
  
}

# Runs a model on a dataset ; formats result table
run_model <- function(model, dataset){
  formula_str = paste( model$outcome_str , "~", paste0(model$predictors, collapse=" + "))
  fit <- coxph(as.formula(formula_str), data = dataset %>% model$filter() )
  fit$model <- model
  fit.ci <- fit %>%  confint(.) %>% data.frame
  names(fit.ci) <- c("lcl", "ucl")
  fit$table <- cbind(broom::tidy(fit), fit.ci )
  fit
}

#### Result Templating Functions ####

# Converts result table to a vector of named values
vectorize_result <- function(result){
  result$table %<>% 
    mutate(across(everything(), as.character)) %>% 
    tidyr::pivot_longer(cols = !matches("term"), 
                                        names_to = "stat", 
                                        values_to = "value") %>%
    #mutate(names = paste0(result$model$fullname,".", term, ".", stat))
    mutate(names = paste0(result$model$fullname,".", term, ".", stat))
  result_vector = result$table$value
  names(result_vector) <- result$table$names
  return(result_vector)
}

# Take multiple results and convert values to a single named vector
make_template_values <- function(results){
  results %>% `names<-`(NULL) %>% lapply(vectorize_result) %>% unlist
}


# Load an xlsx template, substitute result values, and write out filled in template
fill_template <- function(result_values, infile, outfile){
  
  substitute_in_template <- 
    function(x){ 
      idx <- x %in% names(result_values) 
      x[idx] <- result_values[x[idx]]
      return(x) 
    }
  
  template <- readWorksheetFromFile(file = infile, sheet = 1, header = FALSE, 
                               startRow=1,  startCol=1, 
                               autofitRow = FALSE, autofitCol = FALSE
                               )
  
  template %<>% mutate(across(everything(), substitute_in_template))
    
  writeWorksheetToFile(file = outfile, data = template, sheet = "Sheet1")
  xlcFreeMemory()
  
}

