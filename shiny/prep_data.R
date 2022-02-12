# ###set up initial inputs and simulation functions
# ###results from default parameters are saved for shiny preloading
# 
# ##########################
# #### PART 0: Preliminaries ####
# ##########################
# # source("./SEIR.R")
# # source("./ejection-plots.R")
# # source("./plot-theme.R")
# # source("./calibrate-model-prep.R")
# # source("./model-lift.R")
# 
# #FIX THE DATE FOR NOW TO PREVENT UPDATE ISSUE FOR PRELOAD DATA
# 
# number_of_psa_runs <- 25
# 
# 
# today <- as.Date("2022-02-02")
# param_date_to_use <- "2022-02-02"
# #when param date is updated, how should maximum date change?
# maximum_date <- today + 60 #"2020-09-01" 
# maximum_x_date <- as.Date(maximum_date)+60
# # today <- Sys.Date()
# days = 0:365
# init <- c(
#   S=6.8e6,    # State of TN susceptible population
#   E=0,      # Exposed
#   I=0,      # Infectious
#   
#   M=0,      # Mild cases (no hospitalization)
#   H=0,      # Hospitalized (Bed)
#   D=0,      # Dying  (Bed)
#   
#   R=0,      # Recovered
#   Fa=0,      # Fatality
#   
#   
#   lS=0,     # lag time delay tracking, internal
#   
#   ObCases = 0
# )
# 
# init_tn_highland_rim <- init
# init_tn_highland_rim['S'] <- 1941325
# 
# init_mid_south <- init
# init_mid_south['S'] <- 1064657
# 
# ###########################
# #### PART 1: SET PARAMETERS ####
# ###########################
# 
# # Statewide Estimates
# 
# df_incidence_jhu <- 
#   read_rds("./df_incidence_jhu.rds")
# df_hosp_calib <-  
#   read_rds("./df_hosp_calib.rds")
# df_R_calibrate_final <-
#   read_rds("./df_R_calibrate_final.rds")
# 
# df_R_baseline_final <-
#   read_rds("./df_R_baseline_final.rds")
# 
# calibrated_params <-
#   read_rds("./calibrated_params.rds")
# calibrated_params[["pTested"]] <- function(t) ifelse(t<=50,calibrated_params[["pTested_per1"]],calibrated_params[["pTested_per2"]])
# 
# calibrated_params_mild <- 
#   read_rds("./calibrated_params_mild.rds")
# calibrated_params_mild[["pTested"]] <- function(t) ifelse(t<=50,calibrated_params_mild[["pTested_per1"]],calibrated_params_mild[["pTested_per2"]])
# 
# calibrated_params_severe <- 
#   read_rds("./calibrated_params_severe.rds")
# calibrated_params_severe[["pTested"]] <- function(t) ifelse(t<=50,calibrated_params_severe[["pTested_per1"]],calibrated_params_severe[["pTested_per2"]])
# 
# 
# # # Regional Estimates
# 
# # Manual calibration of recent R to match state reported data. 
# 
# R0_calibrate        <- splinefun(df_R_calibrate_final$t, df_R_calibrate_final$R)
# calibrated_params[["R0"]] <- R0_calibrate 
# calibrated_params_mild[["R0"]] <- R0_calibrate 
# calibrated_params_severe[["R0"]] <- R0_calibrate 
# 
# # TK ADD REGIONAL R0
# 
# ### USER-DEFINED PARAMETERS (NOTE: DEFAULT IS THE CALIBRATED PARAMETERS)
# 
# params <- list(
#   
#   R0                 = calibrated_params[["R0"]],         # This gets replaced with R_t as estimated above.
#   
#   pTested_per1 = calibrated_params[["pTested_per1"]],   
#   pTested_per2 = calibrated_params[["pTested_per2"]],
#   pTested_per3 = calibrated_params[["pTested_per3"]],
#   
#   meanLatency        = calibrated_params[["meanLatency"]],                  
#   sdLatency          = 0.25,  
#   
#   meanIncubation     = calibrated_params[["meanIncubation"]],                   
#   sdIncubation       = 0.25,
#   
#   pNotSevere_per1         = calibrated_params[["pNotSevere_per1"]],                
#   pNotSevere_per1Min   = pmax(0,pmin(1,calibrated_params[["pNotSevere_per1"]]-.01)),
#   pNotSevere_per1Max   = pmin(1,pmax(0,calibrated_params[["pNotSevere_per1"]]+.01)),
#   
#   pNotSevere_per2         = calibrated_params[["pNotSevere_per2"]],                
#   pNotSevere_per2Min   = pmax(0,pmin(1,calibrated_params[["pNotSevere_per2"]]-.01)),
#   pNotSevere_per2Max   = pmin(1,pmax(0,calibrated_params[["pNotSevere_per2"]]+.01)),
#   
#   pNotSevere_cfr_t    = calibrated_params[["pNotSevere_cfr_t"]],     
#   
#   cfr_per1                = calibrated_params[["cfr_per1"]],                
#   cfrMin_per1             = pmax(0,pmin(1,calibrated_params[["cfr_per1"]] - 0.002)),
#   cfrMax_per1             = pmax(0,pmin(1,calibrated_params[["cfr_per1"]] + 0.002)),
#   
#   cfr_per2                = calibrated_params[["cfr_per2"]],                
#   cfrMin_per2             = pmax(0,pmin(1,calibrated_params[["cfr_per2"]] - 0.002)),
#   cfrMax_per2             = pmax(0,pmin(1,calibrated_params[["cfr_per2"]] + 0.002)),
#   
#   meanOnsetToDeath = calibrated_params[["meanOnsetToDeath"]],
#   sdOnsetToDeath = 3, 
#   
#   meanHospToRecovery = calibrated_params[["meanHospToRecovery"]],                     
#   sdHospToRecovery   = 3,
#   
#   meanMildToRecovery = calibrated_params[["meanMildToRecovery"]],                     
#   sdMildToRecovery = 2,
#   
#   seed               = calibrated_params[["seed"]]                           
# )
# 
# params <- 
#   append(params, 
#          list(
#            pTested  = function(t) ifelse(t<=50,params[["pTested_per1"]],ifelse(t<=120,params[["pTested_per2"]],params[["pTested_per3"]])),
#            pNotSevere = function(t) ifelse(t>=0, params[["pNotSevere_per1"]],params[["pNotSevere_per1"]]),
#            cfr = function(t) ifelse(t>=0, params[["cfr_per1"]],params[["cfr_per1"]])
#          )
#   )
# 
# 
# params_mild <- list(
#   
#   R0                 = calibrated_params_mild[["R0"]],         # This gets replaced with R_t as estimated above.
#   
#   pTested_per1 = calibrated_params_mild[["pTested_per1"]],   
#   pTested_per2 = calibrated_params_mild[["pTested_per2"]],
#   pTested_per3 = calibrated_params_mild[["pTested_per3"]],
#   
#   meanLatency        = calibrated_params_mild[["meanLatency"]],                  
#   sdLatency          = 0.25,  
#   
#   meanIncubation     = calibrated_params_mild[["meanIncubation"]],                   
#   sdIncubation       = 0.25,
#   
#   pNotSevere_per1         = calibrated_params_mild[["pNotSevere_per1"]],                
#   pNotSevere_per1Min   = pmax(0,pmin(1,calibrated_params_mild[["pNotSevere_per1"]]-.01)),
#   pNotSevere_per1Max   = pmin(1,pmax(0,calibrated_params_mild[["pNotSevere_per1"]]+.01)),
#   
#   pNotSevere_per2         = calibrated_params_mild[["pNotSevere_per2"]],                
#   pNotSevere_per2Min   = pmax(0,pmin(1,calibrated_params_mild[["pNotSevere_per2"]]-.01)),
#   pNotSevere_per2Max   = pmin(1,pmax(0,calibrated_params_mild[["pNotSevere_per2"]]+.01)),
#   
#   pNotSevere_cfr_t    = calibrated_params_mild[["pNotSevere_cfr_t"]],     
#   
#   cfr_per1                = calibrated_params_mild[["cfr_per1"]],                
#   cfrMin_per1             = pmax(0,pmin(1,calibrated_params_mild[["cfr_per1"]] - 0.002)),
#   cfrMax_per1             = pmax(0,pmin(1,calibrated_params_mild[["cfr_per1"]] + 0.002)),
#   
#   cfr_per2                = calibrated_params_mild[["cfr_per2"]],                
#   cfrMin_per2             = pmax(0,pmin(1,calibrated_params_mild[["cfr_per2"]] - 0.002)),
#   cfrMax_per2             = pmax(0,pmin(1,calibrated_params_mild[["cfr_per2"]] + 0.002)),
#   
#   meanOnsetToDeath = calibrated_params[["meanOnsetToDeath"]],
#   sdOnsetToDeath = 3, 
#   
#   meanHospToRecovery = calibrated_params_mild[["meanHospToRecovery"]],                     
#   sdHospToRecovery   = 3,
#   
#   meanMildToRecovery = calibrated_params_mild[["meanMildToRecovery"]],                     
#   sdMildToRecovery = 2,
#   
#   seed               = calibrated_params_mild[["seed"]]                           
# )
# 
# params_mild <- 
#   append(params_mild, 
#          list(
#            pTested  = function(t) ifelse(t<=50,params_mild[["pTested_per1"]],ifelse(t<=120,params_mild[["pTested_per2"]],params_mild[["pTested_per3"]])),
#            pNotSevere = function(t) ifelse(t>=0, params_mild[["pNotSevere_per1"]],params_mild[["pNotSevere_per1"]]),
#            cfr = function(t) ifelse(t>=0, params_mild[["cfr_per1"]],params_mild[["cfr_per1"]])
#          )
#   )
# 
# params_severe <- list(
#   
#   R0                 = calibrated_params_severe[["R0"]],         # This gets replaced with R_t as estimated above.
#   
#   pTested_per1 = calibrated_params_severe[["pTested_per1"]],   
#   pTested_per2 = calibrated_params_severe[["pTested_per2"]],
#   pTested_per3 = calibrated_params_severe[["pTested_per3"]],
#   
#   meanLatency        = calibrated_params_severe[["meanLatency"]],                  
#   sdLatency          = 0.25,  
#   
#   meanIncubation     = calibrated_params_severe[["meanIncubation"]],                   
#   sdIncubation       = 0.25,
#   
#   pNotSevere_per1         = calibrated_params_severe[["pNotSevere_per1"]],                
#   pNotSevere_per1Min   = pmax(0,pmin(1,calibrated_params_severe[["pNotSevere_per1"]]-.01)),
#   pNotSevere_per1Max   = pmin(1,pmax(0,calibrated_params_severe[["pNotSevere_per1"]]+.01)),
#   
#   pNotSevere_per2         = calibrated_params_severe[["pNotSevere_per2"]],                
#   pNotSevere_per2Min   = pmax(0,pmin(1,calibrated_params_severe[["pNotSevere_per2"]]-.01)),
#   pNotSevere_per2Max   = pmin(1,pmax(0,calibrated_params_severe[["pNotSevere_per2"]]+.01)),
#   
#   pNotSevere_cfr_t    = calibrated_params_severe[["pNotSevere_cfr_t"]],     
#   
#   cfr_per1                = calibrated_params_severe[["cfr_per1"]],                
#   cfrMin_per1             = pmax(0,pmin(1,calibrated_params_severe[["cfr_per1"]] - 0.002)),
#   cfrMax_per1             = pmax(0,pmin(1,calibrated_params_severe[["cfr_per1"]] + 0.002)),
#   
#   cfr_per2                = calibrated_params_severe[["cfr_per2"]],                
#   cfrMin_per2             = pmax(0,pmin(1,calibrated_params_severe[["cfr_per2"]] - 0.002)),
#   cfrMax_per2             = pmax(0,pmin(1,calibrated_params_severe[["cfr_per2"]] + 0.002)),
#   
#   meanOnsetToDeath = calibrated_params[["meanOnsetToDeath"]],
#   sdOnsetToDeath = 3, 
#   
#   meanHospToRecovery = calibrated_params_severe[["meanHospToRecovery"]],                     
#   sdHospToRecovery   = 3,
#   
#   meanMildToRecovery = calibrated_params_severe[["meanMildToRecovery"]],                     
#   sdMildToRecovery = 2,
#   
#   seed               = calibrated_params_severe[["seed"]]                           
# )
# 
# params_severe <- 
#   append(params_severe, 
#          list(
#            pTested  = function(t) ifelse(t<=50,params_severe[["pTested_per1"]],ifelse(t<=120,params_severe[["pTested_per2"]],params_severe[["pTested_per3"]])),
#            pNotSevere = function(t) ifelse(t>=0, params_severe[["pNotSevere_per1"]],params_severe[["pNotSevere_per1"]]),
#            cfr = function(t) ifelse(t>=0, params_severe[["cfr_per1"]],params_severe[["cfr_per1"]])
#          )
#   )
# 
# #### OUTPUT 1: CALIBRATION CURVES ####
# plot_cali1 <- function(res=res_baseline) {#,df_hosp=df_hosp_calib) {
#   xx1 <- res %>% 
#     filter(date < today & deaths>=1 | cases>=1) %>% 
#     mutate(fatalities = ifelse(deaths==0,NA,fatalities )) %>% 
#     mutate(deaths = ifelse(deaths==0,NA,deaths)) #%>% 
#     #left_join(df_hosp,"date")
#   
#   ll <- xx1 %>% filter(date==as.character(median(date)))
#   ll2 <- bind_rows(ll %>% mutate(ht=min(reported_cases,cases),label="Reported Cases (Total)",grp="Cases",pn=1),
#                    ll %>% mutate(ht=min(deaths,fatalities),label="Reported Deaths (Total)",grp="Death",pn=1)) %>% 
#                    ll %>% mutate(ht=1000,label="Hospitalized (Current)",grp="Hos",pn=2)) %>%
#     select(date,ht,label,grp,pn,lb)
#   
#   xx2 <- bind_rows(xx1 %>% select(date,lns=reported_cases,pts=cases,lb) %>% mutate(grp="Cases",pn=1),
#                    xx1 %>% select(date,lns=fatalities,pts=deaths,lb) %>% mutate(grp="Death",pn=1))
#                    # xx1 %>% select(date,lns=hospitalized,pts=obs_hosp,lb) %>% mutate(grp="Hos",pn=2))
#   
#   xx2 %>% ggplot(aes(x=date,group=grp,color=grp)) +
#     geom_point(aes(y=pts),alpha = 0.25,pch =19) +
#     geom_line(data =   xx2 %>% filter(date>=today-60) , aes(y=lns),lwd=1.5) +
#     scale_color_manual(values=c("black","darkred")) + #,"blue")) +
#     geom_text(data=ll2,
#               aes(label = label,x = date, y = ht,group=NULL),
#               vjust=1.2,
#               hjust=-0.08, 
#               family = "Gill Sans") +
#     facet_grid(lb~pn) + 
#     scale_y_log10(breaks = c(1,10,100,250,500,1000,5000,10000,20000,50000,100000),
#                   labels = comma_format(accuracy = 1)) +
#     scale_x_date(limits = as.Date(c("2020-03-01",paste0(today))), date_labels = "%b%e", breaks = "1 month") + 
#     theme_tufte_revised() + 
#     theme(panel.grid.major = element_line(colour = "lightgrey", linetype=3),
#           legend.position = "none",
#           strip.text.x = element_blank(),
#           axis.text.x = element_text(size = 15), 
#           axis.text.y = element_text(size = 11), 
#           strip.text.y = element_text(size=12),
#           axis.title.y = element_text(size=15)) + 
#     labs(x = "",y="Count\n(log10 scale)") 
#   
# }#;plot_cali1(res_baseline %>% mutate(lb=""))
# # p1_df <- plot_cali1(res_baseline %>% mutate(lb=""))
# 
# #### OUTPUT 2: EJECTION PLOT ####
# df_ejection <- 
#   df_incidence_jhu %>% 
#   group_by(geo) %>% 
#   nest() %>% 
#   mutate(avg7 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 7))) %>%
#   mutate(avg5 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 5))) %>% 
#   mutate(avg3 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 3))) %>% 
#   mutate(davg7 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 7))) %>%
#   mutate(davg5 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 5))) %>% 
#   mutate(davg3 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 3))) %>%   
#   unnest(cols = c("data","avg7","avg5","avg3","davg7","davg5","davg3"))
# 
# plot_ejection2 <- function(df, alpha_series = 1, growth = new_cases7d, max_new = 100, font_size = 2, wrap = FALSE) 
# {
#   yy <- enquo(growth)
#   
#   p <- 
#     df %>% 
#     select(cases, !!yy, geo,date, lb) %>% 
#     na.omit() %>% 
#     ggplot(aes(x = cases, y = !!yy, colour = geo)) + 
#     geom_point(alpha = alpha_series) +  geom_line(alpha = alpha_series,lwd=1.3) + 
#     theme_tufte_revised()  +
#     geom_dl(method =  list("top.bumpup", cex =1, alpha = 1,hjust=0),aes(label= geo)) +
#     theme(legend.position = "none") +
#     scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)),limits=c(10^0,10^6)) +
#     scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                   labels = trans_format("log10", math_format(10^.x)))  +
#     xlab("log10(Confirmed Cases)") + 
#     ylab("log10(Daily Growth)\n7-day moving average") +
#     annotation_logticks(sides = "bl") +
#     theme(axis.text.x = element_text(size = 15), 
#           axis.text.y = element_text(size = 11), 
#           strip.text.x = element_text(size=12),
#           axis.title.y = element_text(size=15)) 
#   
#   if (max_new  == 1e5) {
#     p_f <- p + 
#       geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 500),lty=3) + annotate("text",x=10^0, y=500,label = "+500 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 1000),lty=3) + annotate("text",x=10^0, y=1000,label = "+1,000 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 10000),lty=3) + annotate("text",x=10^0, y=10000,label = "+10,000 new cases daily",hjust = 0,vjust =1,cex = font_size) 
#   } else if (max_new == 1e3) {
#     p_f <- p + 
#       geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 500),lty=3) + annotate("text",x=10^0, y=500,label = "+500 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 1000),lty=3) + annotate("text",x=10^0, y=1000,label = "+1,000 new cases daily",hjust = 0,vjust =1,cex = font_size)
#   } else if (max_new == 500) {
#     p_f <- p + 
#       geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size, family = "Gill Sans") + 
#       geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size, family = "Gill Sans") +
#       geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size, family = "Gill Sans") +
#       geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size, family = "Gill Sans") + 
#       geom_hline(aes(yintercept = 500),lty=3) + annotate("text",x=10^0, y=500,label = "+500 new cases daily",hjust = 0,vjust =1,cex = font_size, family = "Gill Sans") 
#   } else if (max_new == 1e2) {
#     p_f <- p + 
#       geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 100),lty=3) + annotate("text",x=10^0, y=100,label = "+100 new cases daily",hjust = 0,vjust =1,cex = font_size) 
#   } else if (max_new == 50) {
#     p_f <- p + 
#       geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size) +
#       geom_hline(aes(yintercept = 50),lty=3) + annotate("text",x=10^0, y=50,label = "+50 new cases daily",hjust = 0,vjust =1,cex = font_size) 
#   } else if (max_new == 25) {
#     p_f <- p + 
#       geom_hline(aes(yintercept = 10),lty=3) + annotate("text",x=10^0, y=10,label = "+10 new cases daily",hjust = 0,vjust =1,cex = font_size) + 
#       geom_hline(aes(yintercept = 25),lty=3) + annotate("text",x=10^0, y=25,label = "+25 new cases daily",hjust = 0,vjust =1,cex = font_size)
#   }
#   if (wrap) pf_f + facet_wrap(~geo) + facet_grid(.~lb) else p_f + facet_grid(.~lb)
# }
# # 
# # p_calib
# plot_cali2 <- function(res=res_baseline,p2=FALSE,df=res_baseline) {
#   xx1 <- res %>% 
#     filter( date <= Sys.Date()) %>%
#     mutate(date = date - 3) %>%
#     mutate(geo = "Vanderbilt Model") %>%
#     select(geo,date,cases = reported_cases, deaths = fatalities) %>%
#     group_by(geo) %>%
#     arrange(geo,date) %>%
#     mutate(new_cases = cases - lag(cases))  %>%
#     mutate(new_deaths = deaths - lag(deaths)) %>%
#     group_by(geo) %>%
#     nest() %>%
#     mutate(avg7 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 7))) %>%
#     mutate(avg5 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 5))) %>%
#     mutate(davg7 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 7))) %>%
#     mutate(davg5 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 5))) %>%
#     unnest(cols = c("data","avg7","avg5","davg7","davg5")) %>%
#     bind_rows(
#       df_ejection %>%
#         ungroup() %>%
#         filter(geo %in% c("Tennessee")) %>%
#         mutate(geo = glue::glue("Tennessee Reported Cases")) %>%
#         filter(cases>25) %>%
#         filter(date>"2020-03-10" & date < today)
#     ) 
#   if(!p2) {
#     xx1 %>%
#       plot_ejection(max_new = 500, growth = new_cases7d,font_size = 4) +
#       scale_colour_manual(values=c("darkgrey",scales::muted("red"))) 
#   } else {
#     xx2 <- df %>% 
#       filter( date <= Sys.Date()) %>%
#       mutate(date = date ) %>%
#       mutate(geo = "Vanderbilt Model") %>%
#       select(geo,date,cases = reported_cases, deaths = fatalities) %>%
#       group_by(geo) %>%
#       arrange(geo,date) %>%
#       mutate(new_cases = cases - lag(cases))  %>%
#       mutate(new_deaths = deaths - lag(deaths)) %>%
#       group_by(geo) %>%
#       nest() %>%
#       mutate(avg7 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 7))) %>%
#       mutate(avg5 = map(data,~get_rolling_average(df = .x, x = new_cases,n = 5))) %>%
#       mutate(davg7 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 7))) %>%
#       mutate(davg5 = map(data,~get_rolling_average(df = .x, x = new_deaths,n = 5))) %>%
#       unnest(cols = c("data","avg7","avg5","davg7","davg5")) %>%
#       bind_rows(
#         df_ejection %>%
#           ungroup() %>%
#           filter(geo %in% c("Tennessee")) %>%
#           mutate(geo = glue::glue("Tennessee Reported Cases")) %>%
#           filter(cases>25) %>%
#           filter(date>"2020-03-10" & date < today)
#       ) %>% mutate(lb="Default Parameters")
#     
#     xx1 %>% mutate(lb="Customized Parameters") %>% bind_rows(xx2) %>% 
#       plot_ejection2(max_new = 500, growth = new_cases7d,font_size = 4) +
#       scale_colour_manual(values=c("darkgrey",scales::muted("red")))
#   } 
# }
