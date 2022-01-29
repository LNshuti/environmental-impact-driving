model_lift <- function(date_of_lift, asymp_R = NULL, trigger_val = 100, trigger_y = new_cases7d, return = "trigger", R_tmp = NULL, days_to_asymp, params = baseline_params) {
  
  yy <- enquo(trigger_y)

  foo <- 
    df_R_baseline_final %>% 
    filter(date == today | date == today + days_to_asymp) %>% 
    mutate(R = ifelse(date == (today + days_to_asymp), asymp_R, R))
  
  tmp_fn <- approxfun(foo$t,foo$R)
  start <- foo %>% filter(date == today ) %>% pull(t)
  end <- foo %>% filter(date == today + days_to_asymp) %>% pull(t)
  
  df_R_lift <- 
    df_R_baseline_final %>% 
    left_join(
      expand.grid(foo$t[1]:foo$t[2]) %>% 
        set_names("t") %>% 
        mutate(R_lift = tmp_fn(t)),
      "t"
    ) %>% 
    mutate(R = ifelse(date>= today & date <= (today + days_to_asymp), R_lift, 
                      ifelse(date > (today + days_to_asymp), asymp_R, R)))
    

  tn_R0_lift        <- splinefun(df_R_lift$t, df_R_lift$R)
  lift_params <- params
  lift_params[["R0"]] <- tn_R0_lift
  res_lift <- run_seir(params = lift_params) 
  
  
  if (return == "trigger") {
    out <- suppressWarnings(get_trigger(df = res_lift, trigger = trigger_val, y = !!yy))
    if (length(out)==0) out <- as.Date("2020-12-31")
    #return(tryCatch(get_trigger(df = res_lift, trigger = trigger_val, y = !!yy),error = function(e) as.Date("2020-12-31"))) else 
    return(out)
  } else {
    {
      return(res_lift)
    }
  }
}

get_trigger <- function(df,trigger = 100, y = new_cases7d) {
  y <- enquo(y)
  trigger_val = trigger
  df %>% 
    #mutate(date = date - testing_lag) %>% 
    filter(date>today) %>% 
    select(date,cases = reported_cases, deaths = fatalities,hospitalized) %>% 
    mutate(geo = "Simulated TN - Baseline") %>% 
    arrange(date) %>% 
    mutate(new_cases = cases - lag(cases))  %>% 
    mutate(new_deaths = deaths - lag(deaths)) %>% 
    mutate(new_hospitalized = hospitalized - lag(hospitalized)) %>% 
    group_by(geo) %>% 
    nest() %>% 
    mutate(avg7 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 7))) %>%
    mutate(avg5 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 5))) %>% 
    mutate(avg3 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 3))) %>% 
    mutate(davg7 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 7))) %>%
    mutate(davg5 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 5))) %>% 
    mutate(davg3 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 3))) %>%   
    mutate(havg7 = map(data,~get_rolling_average(df = .x, x = new_hospitalized,n = 7))) %>%
    mutate(havg5 = map(data,~get_rolling_average(df = .x, x = new_hospitalized,n = 5))) %>% 
    mutate(havg3 = map(data,~get_rolling_average(df = .x, x = new_hospitalized,n = 3))) %>%  
    unnest(cols = c("data","avg7","avg5","avg3","davg7","davg5","davg3","havg7","havg5","havg3")) %>% 
    mutate(peak = as.integer(!!y == max(!!y,na.rm=TRUE))) %>% 
    mutate(peak = ifelse(peak ==1, date, NA)) %>% 
    mutate(date_of_peak = max(peak,na.rm=TRUE)) %>% 
    mutate(date_of_peak = as.Date(peak,origin="1970-01-01")) %>% 
    mutate(date_of_peak = max(date_of_peak, na.rm=TRUE)) %>% 
    filter(date <date_of_peak) %>% 
    mutate(valley = as.integer(!!y == min(!!y,na.rm=TRUE))) %>% 
    mutate(valley = ifelse(valley ==1, date, NA)) %>% 
    mutate(date_of_valley = max(valley,na.rm=TRUE)) %>% 
    mutate(date_of_valley = as.Date(valley,origin="1970-01-01")) %>% 
    mutate(date_of_valley = max(date_of_valley, na.rm=TRUE)) %>% 
    filter(date > date_of_valley & !!y > trigger)  %>% 
    mutate(trigger = as.integer(!!y  == min(!!y,na.rm=TRUE))) %>% 
    mutate(trigger = ifelse(trigger ==1, date, NA)) %>% 
    mutate(date_of_trigger = max(trigger,na.rm=TRUE)) %>% 
    mutate(date_of_trigger = as.Date(trigger,origin="1970-01-01")) %>% 
    mutate(date_of_trigger = max(date_of_trigger, na.rm=TRUE)) %>% 
    filter(row_number()==1) %>% 
    ungroup() %>% 
    filter(date == date_of_valley | date == date_of_trigger) %>% 
    mutate(trigger = trigger_val) %>% 
    pull(date_of_trigger)
}


