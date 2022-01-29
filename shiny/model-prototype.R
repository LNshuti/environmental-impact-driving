##########################
### PART 0: Preliminaries
##########################

source(here::here("R/manifest.R"))
source(here("R/SEIR.R"))
source(here("R/extract-and-tidy-hopkins-data.R"))
source(here("R/ejection-plots.R"))
source(here("R/plot-theme.R"))
source(here("R/model-lift.R"))
library(glue)
library(here)
library(lubridate)
today <- Sys.Date()
days = 0:150
init <- c(
  S=6.8e6,    # State of TN susceptible population
  E=0,      # Exposed
  I=0,      # Infectious
  
  M=0,      # Mild cases (no hospitalization)
  H=0,      # Hospitalized (Bed)
  D=0,      # Dying  (Bed)
  
  R=0,      # Recovered
  Fa=0,      # Fatality
  
  
  lS=0,     # lag time delay tracking, internal
  
  ObCases = 0
)

###########################
### PART 1: SET PARAMETERS
###########################

param_date <- "2020-06-10"

moderate_params <- read_rds(here(glue("params/baseline-parameters-{param_date}.rds")))
severe_params <- read_rds(here(glue("params/severe-parameters-{param_date}.rds")))
mild_params <- read_rds(here(glue("params/mild-parameters-{param_date}.rds")))

df_incidence_jhu <- read_rds(here(glue("params/incidence-data-{param_date}.rds")))
df_R_calibrate_final <-
  read_rds(here(glue::glue("params/R_t-{param_date}.rds")))
df_hosp_calib <- read_rds(here(glue("params/hospitalization-calibration-{param_date}")))
number_of_psa_runs <- 500


run_model_scenario <- function(calibrated_params) {
  params <- list(
    R0                 = calibrated_params[["R0"]],         
    
    pTested_per1 = calibrated_params[["pTested_per1"]],   
    pTested_per2 = calibrated_params[["pTested_per2"]],  
    
    meanLatency        = calibrated_params[["meanLatency"]],                  
    sdLatency          = 1e-10,  
    
    meanIncubation     = calibrated_params[["meanIncubation"]],                   
    sdIncubation       = 1e-10,
    
    pNotSevere         = calibrated_params[["pNotSevere"]],                
    pNotSevereMin   = pmax(0,pmin(1,calibrated_params[["pNotSevere"]]-.01)),
    pNotSevereMax   = pmin(1,pmax(0,calibrated_params[["pNotSevere"]]+.01)),
    
    cfr                = calibrated_params[["cfr"]],                
    cfrMin             = pmax(0,pmin(1,calibrated_params[["cfr"]] - 0.005)),
    cfrMax             = pmax(0,pmin(1,calibrated_params[["cfr"]] + 0.005)),
    
    meanOnsetToDeath   = calibrated_params[["meanOnsetToDeath"]],                   
    sdOnsetToDeath     = 1e-10, 
    
    meanHospToRecovery = calibrated_params[["meanHospToRecovery"]],                     
    sdHospToRecovery   = 1e-10,
    
    meanMildToRecovery = calibrated_params[["meanMildToRecovery"]],                     
    sdMildToRecovery = 1e-10,
    
    seed               = calibrated_params[["seed"]]                     
  )
  
  params <- 
    append(params, 
           list(
             pTested            = function(t) ifelse(t<=50,params[["pTested_per1"]],params[["pTested_per2"]])
           )
    )
  
  ###########################
  ### PART 2: RUN MODEL
  ###########################
  
  res_baseline <- run_seir(params = params) 
  
  #res_baseline %>% filter(date==Sys.Date())
  
  
  ###################################
  ### PART 3: SENSITIVITY ANALYSES
  ###################################
  betaA <- 2
  betaB <- 2
  
  qbet <- function(x, min, max) (max-min)*qbeta(p=x, shape1=betaA, shape2=betaB)+min
  qgam <- function(x, mu, sd) qgamma(x, shape=mu*mu/sd/sd, rate=mu/sd/sd)
  
  params_uncert <- list(
    meanLatency        = params[["meanLatency"]],                   
    sdLatency          = 1e-10,
    
    meanIncubation     = params[["meanIncubation"]],                   
    sdIncubation       = 1e-10,
    
    pNotSevere      = params[["pNotSevere"]],                  
    pNotSevereMin   = pmax(0,pmin(1,params[["pNotSevere"]]-.01)),
    pNotSevereMax   = pmax(0,pmin(1,params[["pNotSevere"]]+.01)),
    
    cfr                = params[["cfr"]],                   
    cfrMin             = pmax(0,pmin(1,params[["cfr"]] - 0.005)),
    cfrMax             = pmax(0,pmin(1,params[["cfr"]] + 0.005)),
    
    meanOnsetToDeath   = params[["meanOnsetToDeath"]],  
    sdOnsetToDeath     = 1e-10, 
    
    meanHospToRecovery = params[["meanHospToRecovery"]],                       # Radiology Pan
    sdHospToRecovery   = 1e-10,
    
    meanMildToRecovery = params[["meanMildToRecovery"]],                       # WHO, 55k case report
    sdMildToRecovery = 1e-10
  )
  
  params_nopsa  <- 
    
    list(
      min_R = params[["min_R"]],
      max_R = params[["min_R"]],
      pTested            = params[["pTested"]],
      R0                 = params[["R0"]] ,        # This gets replaced with R_t as estimated above.
      seed               = params[["seed"]]       # Seed cases to initialize model, from calibration
      
    )
  
  params_psa_fn <- list(
    meanLatency        = function(x) qgam(x, params_uncert$meanLatency, params_uncert$sdLatency),
    meanInfectious     = function(x) qgam(x, params_uncert$meanIncubation-params_uncert$meanLatency, params_uncert$sdIncubation/2),
    pNotSevere      = function(x) qbet(x, params_uncert$pNotSevereMin, params_uncert$pNotSevereMax),
    cfr                = function(x) qbet(x, params_uncert$cfrMin, params_uncert$cfrMax),
    meanOnsetToDeath   = function(x) qgam(x, params_uncert$meanOnsetToDeath, params_uncert$sdOnsetToDeath),
    meanHospToRecovery = function(x) qgam(x, params_uncert$meanHospToRecovery, params_uncert$sdHospToRecovery),
    meanMildToRecovery = function(x) qgam(x, params_uncert$meanMildToRecovery, params_uncert$sdMildToRecovery)
  )
  
  
  get_psa_params <- function(number_of_halton_draws = 10, params_uncert, params_psa_fn) {
    which_are_fns <- params_psa_fn %>% map_lgl(~is.function(.x)) 
    
    get_params <-  function(x,params_uncert) {
      map2(params_uncert,x,~(
        .x(.y)
      ))
    } 
    
    draws <- randtoolbox::halton(n = number_of_halton_draws, 
                                 dim = length(params_psa_fn[which_are_fns])) %>% as.matrix()
    
    halton_draws <- 
      draws %>% 
      data.frame() %>% 
      magrittr::set_names(names(params_psa_fn[which_are_fns]))  %>% 
      pmap(list) %>% 
      map(~(get_params(.x,params=params_psa_fn[which_are_fns]))) %>% 
      map(~(data.frame(.x))) %>%
      bind_rows() %>% 
      pmap(list)
    
    params_halton <- 
      halton_draws %>% 
      map(~(append(.x,params_nopsa))) 
    
  }
  
  params_halton <- get_psa_params(number_of_halton_draws = number_of_psa_runs , params_uncert = params_uncert, params_psa = params_psa_fn)
  
  simulation_function <- function(params_j) {
    params_j[["meanIncubation"]] <- params_j[["meanInfectious"]] + params_j[["meanLatency"]]
    res<- run_seir(params = params_j) 
    return(res)
  }
  
  df_psa <- 
    params_halton %>% 
    map(~(simulation_function(.x)), .progress=TRUE) %>% 
    map(~(data.frame(.x))) %>% 
    bind_rows(.id = "iteration") %>% 
    tbl_df()
  
  df_res <- 
    res_baseline %>% 
    select(hospitalized,  date) %>% 
    gather(measure, value, -date ) %>% 
    mutate(measure = factor(measure, levels = c("hospitalized"), labels = c("Concurrent Hospitalizations"))) 
  
  out <- list(df_psa = df_psa, df_res = df_res, result = res_baseline)
  return(out)
}

res_moderate <- moderate_params %>% run_model_scenario()
res_mild <- mild_params %>% run_model_scenario()
res_severe <- severe_params %>% run_model_scenario()

#### PLOT

res_all <- 
  list(moderate = res_moderate, mild = res_mild, severe = res_severe)

result <- 
  res_all %>% 
  transpose() %>% 
  pluck("result") %>% 
  bind_rows(.id = "scenario")
  

tmp <- 
  result %>% 
  mutate(iteration = 0) %>% 
  select(iteration, hospitalized, date,scenario) %>% 
  gather(measure, value, -date, -iteration ,-scenario) %>% 
  group_by(scenario,date,measure) %>% 
  mutate_at(vars(value),list(q95=~quantile(.,0.975,na.rm=TRUE), q5 = ~quantile(.,0.025, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(measure = factor(measure, levels = c("hospitalized"), labels = c("Currently Hospitalized"))) %>% 
  filter(date == Sys.Date()) %>% 
  select(scenario,measure,today_value= value)


df_psa <- 
  res_all %>% 
  transpose() %>% 
  pluck("df_psa") %>% 
  bind_rows(.id = "scenario")

df_res <- 
  res_all %>% 
  transpose() %>% 
  pluck("df_res") %>% 
  bind_rows(.id = "scenario") %>% 
  mutate(measure =factor(measure, labels = c("Currently Hospitalized")))

scenario_lut = c("severe" = "Severe Scenario","moderate" = "Moderate Scenario","mild" = "Mild Scenario")


df_psa %>% 
  select(scenario, iteration, hospitalized, date) %>% 
  gather(measure, value, -date, -iteration,-scenario ) %>% 
  group_by(scenario,date,measure) %>% 
  mutate_at(vars(value),list(q95=~quantile(.,0.975,na.rm=TRUE), q5 = ~quantile(.,0.025, na.rm=TRUE))) %>% 
  ungroup() %>% 
  mutate(measure = factor(measure, levels = c("hospitalized"), labels = c("Currently Hospitalized"))) %>% 
  filter(date < "2020-08-01" & date >= Sys.Date() ) %>% 
  left_join(tmp,c("measure","scenario")) %>% 
  group_by(scenario) %>% 
  mutate(days_from_today = as.numeric(date - Sys.Date())) %>% 
  mutate(days_till_end = max(as.numeric(date - Sys.Date()))) %>% 
  mutate(rescale_val = pmin(days_from_today / days_till_end,1)) %>% 
  #mutate(rescale_val = 1) %>% 
  mutate(q95 = today_value + ((q95- today_value ) * rescale_val)) %>% 
  mutate(q5 = today_value - (( today_value - q5 ) * rescale_val)) %>% 
  ggplot(aes(x = date, colour = scenario)) + 
  geom_ribbon(aes(ymin = q5, ymax = q95, colour = scenario, fill = scenario),lwd = 0,alpha = 0.05) + 
  theme_tufte_revised() + 
  facet_wrap(~measure) +
  geom_line(data = df_res %>% filter(date <= "2020-08-01") , aes(y = value,lty=scenario)) + 
  labs(y = "", x = "") +
  ggtitle("") + 
  geom_point(data =
               df_hosp_calib %>%
               filter(date >= "2020-04-22" ) %>%
               select(date,obs_hosp ) %>%
               mutate(date = as.Date(date)) %>%
               mutate(measure = "Currently Hospitalized"),
             aes(y =  obs_hosp),
             colour = "black",
             alpha =0.2
  )  +
  theme(axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 12), 
        strip.text.x = element_text(size=12),axis.title.y = element_text(size=15)) + 
  scale_x_date(limits = as.Date(c("2020-03-01","2020-08-01")),date_labels = "%B %e", breaks = "1 month") +
  scale_colour_manual(name = "",values = c("darkblue","black",scales::muted("red"))) + 
  scale_fill_manual(name="", values = c("darkgrey","darkgrey","darkgrey")) +
  scale_linetype_discrete(name="")+
  #geom_dl(data = df_res %>% filter(date <= "2020-08-01") ,
  #        method = list("last.bumpup",cex=1,hjust=0),aes(y = value,label = scenario_lut[scenario])) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 15), 
        strip.text.x = element_text(size=0),axis.title.y = element_text(size=20),
        legend.position = "top")   +
  guides(fill=FALSE) 
  

data.frame(
  scenario = c("Mild","Moderate","Severe"),
  ifr = c(mild_params[["cfr"]], moderate_params[["cfr"]], severe_params[["cfr"]]),
  hosp = c(1-mild_params[["pNotSevere"]], 1-moderate_params[["pNotSevere"]], 1-severe_params[["pNotSevere"]]),
  infected = c(1- (res_mild$result %>% filter(date==Sys.Date()) %>% pull(S) / init["S"]),
               1- (res_moderate$result %>% filter(date==Sys.Date()) %>% pull(S) / init["S"]),
               1- (res_severe$result %>% filter(date==Sys.Date()) %>% pull(S) / init["S"]))
) %>% 
  gt::gt() %>% 
  fmt_number(columns = vars(ifr,hosp,infected),decimals=3) %>% 
  cols_label(ifr = "Infection Fatality Rate",hosp = "Probability Hospitalized",infected = "Percent of Population Infected to Date")

#https://covid19-projections.com/us-tn (Updated 10 June)
# PCT INFECTED 1.0 (0.70 - 1.4)

# epiforecasts.io R_t = 1.1 (1.1 – 1.2)

