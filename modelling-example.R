library(dplyr)

source("modelling-functions.R")

#### Data ####

dataset <- rotterdam %>% 
  lapply(haven::zap_formats) %>% 
  lapply(haven::zap_labels) %>%
  data.frame %>%
  mutate(erpr = case_when(
    er <= 300 & pgr <= 300 ~ "00",
    er  > 300 & pgr <= 300 ~ "10",
    er <= 300 & pgr  > 300 ~ "01",
    er  > 300 & pgr  > 300 ~ "11",
    TRUE ~ NA_character_
  ) %>% factor %>% relevel(ref="00"))

#### Model Configuration ####

treatment_vars = c("hormon", "chemo", "hormon:chemo")
tumor_vars = c("size", "grade", "nodes", "erpr")
demog_vars = c("age", "meno")

base_model = list(
  model = TRUE,
  filter = NULL,
  outcome_str = NULL,
  predictors = c(),
  names = NULL
)

templates = list(
  
  ### Predictor Templates ###
  
  treatment = function(m){
    m$predictors %<>% c(treatment_vars)
    return(m)
  },
  
  tumor = function(m){
    m$predictors %<>% c(tumor_vars)
    return(m)
  },
  
  demog = function(m){
    m$predictors %<>% c(demog_vars)
    return(m)
  },
  
  ### Outcome templates ###
  
  death = function(m){
    m$outcome_str <- "Surv(time=dtime, event=death)"
    m$names %<>% c("Dt")
    return(m)
  },
  
  recur = function(m){
    m$outcome_str = "Surv(time=dtime, event=recur)"
    m$names %<>% c("Rc")
    return(m)
  },
  
  ### Cohort/Sub-cohort templates ###
  
  all = function(m){
    m$filter <- function(d){d}
    m$names %<>% c("Al")
    return(m)
  },
  
  premenop = function(m){
    m$filter <- function(d){d %>% filter(meno == 0)}
    m$names %<>% c("Pr")
    return(m)
  },
  
  postmenop = function(m){
    m$filter <- function(d){d %>% filter(meno == 1)}
    m$names %<>% c("Po")
    return(m)
  }
  
)

#### Custom Post Processing Function ####

postprocess <- function(result){
  result$table %<>%
    mutate(hrci = paste0(format(exp(estimate), digits=1, nsmall=2), " (",  # HR w/ 95% CI
                         format(exp(lcl), digits=1, nsmall=2), ", ",
                         format(exp(ucl), digits=1, nsmall=2), ")"
                        ),
           p = format.pval(p.value, digits = 1, eps=0.0001)) %>%
    select(term, hrci, p)
  result
}

#### Calculate Results ####

results <- list(base_model) %>% 
  parallel_apply(templates[c("death", "recur")])  %>%
  serial_apply(templates[c("treatment", "tumor", "demog")]) %>% 
  parallel_apply(templates[c("all", "premenop", "postmenop")]) %>%
  lapply(run_model, dataset=dataset) %>%
  lapply(postprocess)

result_values <- results %>% make_template_values()

fill_template(result_values, 
                   infile="Example Tables 1 and 2 - Template.xlsx", 
                  outfile="Example Tables 1 and 2 - Output.xlsx")
