covid19_SEIR_parsimonious <- function(t, y, params)
{
  with(as.list(c(y, params)), {
    
    N          <- S+E+I+M+H+D+R
    
    tau_e      <- meanLatency                  # mean time in exposed
    tau_i      <- meanIncubation - meanLatency # mean time in infectious
    
    beta       <- R0(t)/tau_i
    
    exposed    <- if (t < 1) seed else beta*I*S/N
    
    infectious <- E/tau_e
    sick       <- I/tau_i
    
    mRecovery  <- if (t < (meanMildToRecovery+meanIncubation)) 0 else pNotSevere(t)  *      lagderiv(t-meanMildToRecovery, 9)
    hRecovery  <- if (t < (meanHospToRecovery+meanIncubation)) 0 else (1-pNotSevere(t)-cfr(t))*lagderiv(t-meanHospToRecovery, 9)
    dTime      <- if (t < (meanOnsetToDeath+  meanIncubation)) 0 else cfr(t) *         lagderiv(t-meanOnsetToDeath,   9)
    
    list(
      c(
        # Compartment Inbound                    Outbound
        S =                                        - exposed,     #  1
        E =  exposed                               - infectious,  #  2
        I =  infectious                            - sick,        #  3
        M =  pNotSevere(t)*sick                    - mRecovery,   #  4
        H =  (1-pNotSevere(t)-cfr(t))*sick            - hRecovery,   #  5
        D =  cfr(t)*sick                              - dTime,       #  6
        R =  mRecovery+hRecovery,                                 #  7
        Fa =  dTime,                                              #  8
        
        # For tracking lag derivatives
        lS = sick,                                                #  9
        
        ObCases = sick*(1-pNotSevere(t)+pTested(t)*pNotSevere(t))               # 10
        
      ),
      Beds    = H+D,
      Cases   = M+H+D+R+Fa
    )
  })
}

sir_covid <- function(days, init, params) dede(init, days, covid19_SEIR_parsimonious, params)

run_seir <- function(params,
                     initialize_model =init,
                     df = df_incidence_jhu, 
                     testing_lag = 0) { # in rns_run-model.R
  
  run  <- sir_covid(days, initialize_model, params) 
  
  tmp <- 
    df  %>% 
    filter(row_number()==1) %>% 
    rename(report_date = date)
  
  (number_of_initial_cases <- tmp$cases)
  (date_of_initial_cases <- tmp$report_date)
  (days_since_initial_cases = as.numeric(max(df$date,na.rm=TRUE) - date_of_initial_cases))
  (time_of_first_case <- which(run[,"ObCases"] > number_of_initial_cases-.5)[1])
  (time_of_first_death <- which(log(run[,"Fa"])>0)[1])
  #cat(params[["pNotSevere"]] )
  m_death <- exp(log(run[,"Fa"])[time_of_first_death:length(log(run[,"Fa"]))])
  
  trun <- run[time_of_first_case:nrow(run),] %>% 
    tbl_df() %>% 
    mutate(date = as.Date(time - time_of_first_case + 1, origin = date_of_initial_cases)) %>%  
    left_join(df %>% ungroup() %>% select(cases,deaths,date,doy),"date")
  
  (date_of_first_death <- trun %>% filter(deaths>=1) %>% filter(row_number()==1) %>% pull(time))
  
  death_indexed <- 
    trun %>% filter(deaths>=1) %>% #filter(date >= date_of_first_death) %>% 
    mutate(pr_deaths = m_death[1:nrow(.)]) %>% 
    select(date,pr_deaths) 
  trun_final <- 
    trun %>% 
    left_join(death_indexed,"date")
  
  
  int0 <- trun %>% filter(deaths>=1) %>% filter(row_number()==1) %>% pull(date)
  int1 <- trun %>% filter(log(Fa)>0) %>% filter(row_number()==1) %>% pull(date)
  
  trun_final <- 
    trun %>% 
    mutate(fatalities = lead(Fa,pmax(as.numeric(int1-int0),0)))
  
  out <-  
    trun_final %>% select(date,reported_cases = ObCases, hospitalized = Beds, fatalities, everything()) %>% 
    mutate(date = date - testing_lag  ) 
  
  return(out)
}
