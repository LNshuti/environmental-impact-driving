#this file will save preload data
#later we need to figure out better way for preloading

library(tidyverse)

# sourced code requires these:
library(directlabels)
library(glue)
library(scales)
library(deSolve)
library(purrr)
#load functions

today <- as.Date("2022-02-02")
param_date_to_use <- "2022-02-02"

df_pr <- 
  read_rds("../../data/01_case_mix_data.rds")
df_pr %>% 
  write_rds("./df_cmi.rds")
df_incidence_jhu <- 
  read_rds(glue::glue("../../params/incidence-data-{param_date_to_use}.rds"))
df_incidence_jhu %>% 
  write_rds("./df_incidence_jhu.rds")
df_hosp_calib <-  
  read_rds(glue::glue("../../params/hospitalization-calibration-{param_date_to_use}"))
df_hosp_calib %>% 
  write_rds("./df_hosp_calib.rds")
df_R_calibrate_final <-
  read_rds(glue::glue("../../params/R_t-{param_date_to_use}.rds"))
df_R_calibrate_final  %>% 
  write_rds("./df_R_calibrate_final.rds")

df_R_baseline_final <-
  read_rds(glue::glue("../../params/R_t-{param_date_to_use}.rds"))
df_R_baseline_final %>% 
  write_rds("./df_R_baseline_final.rds")

calibrated_params <-
  read_rds(glue("../../params/baseline-parameters-{param_date_to_use}.rds"))
calibrated_params %>% 
  write_rds("./calibrated_params.rds")
calibrated_params[["pTested"]] <- function(t) ifelse(t<=50,calibrated_params[["pTested_per1"]],calibrated_params[["pTested_per2"]])

calibrated_params_mild <- 
  read_rds(glue("../../params/mild-parameters-{param_date_to_use}.rds"))
calibrated_params_mild %>% 
  write_rds("./calibrated_params_mild.rds")
calibrated_params_mild[["pTested"]] <- function(t) ifelse(t<=50,calibrated_params_mild[["pTested_per1"]],calibrated_params_mild[["pTested_per2"]])

calibrated_params_severe <- 
  read_rds(glue("../../params/severe-parameters-{param_date_to_use}.rds"))
calibrated_params_severe %>% 
  write_rds("./calibrated_params_severe.rds")
calibrated_params_severe[["pTested"]] <- function(t) ifelse(t<=50,calibrated_params_severe[["pTested_per1"]],calibrated_params_severe[["pTested_per2"]])

source("prep_data.R")


####default parameters and baseline run####
# default_params <- list(params_mild,params,params_severe, params_tn_highland_rim, params_mid_south)
default_params <- list(params_mild,params,params_severe, params, params)

params_df <- list(
  list(df_incidence_jhu,init),
  list(df_incidence_jhu,init),
  list(df_incidence_jhu,init),
  # REMOVE REGIONAL FILES FOR NOW
  # list(df_incidence_jhu_tn_highland_rim,init_tn_highland_rim),
  # list(df_incidence_jhu_mid_south,init_mid_south))
  list(df_incidence_jhu,init),
  list(df_incidence_jhu,init))
ls_hosp_calib <- 
  list(
    df_hosp_calib,
    df_hosp_calib,
    df_hosp_calib,
    # REMOVE REGIONAL FILES FOR NOW
    # df_hosp_calib_tn_highland_rim,
    # df_hosp_calib_mid_south
    df_hosp_calib,
    df_hosp_calib
  )

default_res_baseline <- map2(default_params,params_df, ~run_seir(params=.x, df = .y[[1]], initialize_model = .y[[2]]))

mse_case <- map(default_res_baseline,err2)
mse_fatal <- map(default_res_baseline,err2_death)
mse_hosp <- map2(default_res_baseline,ls_hosp_calib,~err2_hosp(run=.x, df_calib = .y))

####default psa parameters and psa runs####
# default_params_psa_fn <- list(params_psa_fn_mild,params_psa_fn,params_psa_fn_severe, params_psa_fn_tn_highland_rim, params_psa_fn_mid_south)
# default_params_nopsa <- list(params_nopsa_mild,params_nopsa,params_nopsa_severe, params_nopsa_tn_highland_rim,params_nopsa_mid_south)
default_params_psa_fn <- list(params_psa_fn_mild,params_psa_fn,params_psa_fn_severe, params_psa_fn, params_psa_fn)
default_params_nopsa <- list(params_nopsa_mild,params_nopsa,params_nopsa_severe, params_nopsa,params_nopsa)

default_params_halton <- 
  map2(default_params_psa_fn,default_params_nopsa,~get_psa_params(
    number_of_halton_draws = number_of_psa_runs, 
    params_psa = .x, 
    params_nop=.y))

default_res_psa <- 
  map2(default_params_halton, params_df, function(x,y) { x %>% 
      map(~({
        .x <- 
          append(.x, 
                 list(
                   pTested  = function(t) ifelse(t<=50,.x[["pTested_per1"]],ifelse(t<=120,.x[["pTested_per2"]],.x[["pTested_per3"]])),
                   pNotSevere = function(t) ifelse(t>=0, .x[["pNotSevere_per1"]],.x[["pNotSevere_per1"]]),
                   cfr = function(t) ifelse(t>=0, .x[["cfr_per1"]],.x[["cfr_per1"]])
                 )
          )
        simulation_function(.x, df_run = y[[1]], init_run = y[[2]])
        }
        ), .progress=TRUE) %>%
      map(~(data.frame(.x))) %>%
      bind_rows(.id = "iteration") %>%
      tbl_df()
  })

####default lift scenario runs####
default_lift_baseline <-
  map2(default_params, params_df,~model_lift2(asymp_R = 1.1, trigger_val = 1000,
                   trigger_y = hospitalized, days_to_asymp = 21, return= 'res',
                   params=.x, df_run = .y[[1]], init_run = .y[[2]]))


default_lift_psa <- map2(1:5,params_df,function(i,y) {
  map(default_params_halton[[i]],function(x) {
    x[["R0"]] = default_lift_baseline[[i]][["R0"]]
    x <- append(x, 
                list(
                  pTested  = function(t) ifelse(t<=50,x[["pTested_per1"]],ifelse(t<=120,x[["pTested_per2"]],x[["pTested_per3"]])),
                  pNotSevere = function(t) ifelse(t>=0,x[["pNotSevere_per1"]],x[["pNotSevere_per1"]]),
                  cfr = function(t) ifelse(t>=0, x[["cfr_per1"]],x[["cfr_per1"]])
                )
    )
    x}) %>%
    map(~(simulation_function(.x, df_run = y[[1]], init_run = y[[2]])), .progress=TRUE) %>%
    map(~(data.frame(.x))) %>%
    bind_rows(.id = "iteration") %>%
    tbl_df()
})

save(default_params,default_res_baseline,
     default_params_nopsa,default_params_halton,
     default_res_psa,default_lift_baseline,default_lift_psa,
     file="preload_data.rda")

