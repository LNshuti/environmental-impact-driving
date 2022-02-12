#TODO
#test reactivity
#faster preload/default
#param input test type & error message?
#clean up pkg/funcs
#layout cosmetics (inline,font,etc.)
#input params: for text input what range is allowed, for slider input what step is appropriate

# This file requires these:
library(shiny)
library(shinyjs)
library(tidyverse)
library(grid)

# sourced code requires these:
library(directlabels)
library(glue)
library(scales)
library(deSolve)

# server function begins here
shinyServer(function(input, output, session) {
    # params_df <- list(
    #   list(df_incidence_jhu,init),
    #   list(df_incidence_jhu,init),
    #   list(df_incidence_jhu,init),
    #   # REMOVE REGIONAL FILES FOR NOW
    #   # list(df_incidence_jhu_tn_highland_rim,init_tn_highland_rim),
    #   # list(df_incidence_jhu_mid_south,init_mid_south))
    #   list(df_incidence_jhu,init),
    #   list(df_incidence_jhu,init))
    # 
    
    #### CONSTANTS ####
    # TODO: can we coordinate these two with the defaults in the ui
    #    to avoid duplication?
    MPARAMS_TR_PRESPEC <- "1.0"
    MPARAMS_D2R_PRESPEC <- "21"

    T_CUTOFF <- 50
    T_CUTOFF2 <- 120 #pTested now broken to 3 parts, need to add a cutoff
    
    # TODO: tweak these cutoffs
    CALI_CUTOFF_GOOD <- 1.1
    CALI_CUTOFF_FAIR <- 1.25
    
    # TODO: adjust colors once we have decided on a css
    CALI_COLOR_GOOD <- "darkgreen"
    CALI_COLOR_FAIR <- "orange"
    CALI_COLOR_POOR <- "red"
    
    # # placeholder for default params
    # default_params <- list(params_mild,params,params_severe)
    # transform selected params into ranges, 
    # when dynamic panel is not displayed, all inputs are not activated and we need to specify default values
    # p_transform <- function(param) {
    #   list(
    #     rlat=c(max(0, param$meanLatency - param$sdLatency),
    #            param$meanLatency + param$sdLatency),
    #     rinc=c(max(0, param$meanIncubation - param$sdIncubation),
    #            param$meanIncubation + param$sdIncubation),
    #     rph1=c(1 - param$pNotSevere_per1Max, 
    #           1 - param$pNotSevere_per1Min),
    #     rph2=c(1 - param$pNotSevere_per2Max, 
    #            1 - param$pNotSevere_per2Min),
    #     rcfr1=c(param$cfrMin_per1, param$cfrMax_per1),
    #     rcfr2=c(param$cfrMin_per2, param$cfrMax_per2),
    #     ro2d=c(max(0, param$meanOnsetToDeath - param$sdOnsetToDeath),
    #            param$meanOnsetToDeath + param$sdOnsetToDeath),
    #     rh2r=c(max(0, param$meanHospToRecovery - param$sdHospToRecovery),
    #            param$meanHospToRecovery + param$sdHospToRecovery),
    #     rm2r=c(max(0, param$meanMildToRecovery - param$sdMildToRecovery),
    #            param$meanMildToRecovery + param$sdMildToRecovery)
    #   )
    # }
    # default_params_range <- map(default_params,p_transform)
    # 
    # #### Params: reactiveVals ####
    # # All of these are created as reactiveVals because their values
    # #     can be changed from different places
    # mparams_tr    <- reactiveVal(MPARAMS_TR_PRESPEC)
    # mparams_d2r   <- reactiveVal(MPARAMS_D2R_PRESPEC)
    # 
    # mparams_range <- reactiveValues()
    # mparams_rlat <- reactiveVal(default_params_range[[2]]$rlat)
    # mparams_rinc <- reactiveVal(default_params_range[[2]]$rinc)
    # mparams_rph1  <- reactiveVal(default_params_range[[2]]$rph1)
    # mparams_rph2  <- reactiveVal(default_params_range[[2]]$rph2)
    # mparams_rcfr1 <- reactiveVal(default_params_range[[2]]$rcfr1)
    # mparams_rcfr2 <- reactiveVal(default_params_range[[2]]$rcfr2)
    # mparams_ro2d <- reactiveVal(default_params_range[[2]]$ro2d)
    # mparams_rh2r <- reactiveVal(default_params_range[[2]]$rh2r)
    # mparams_rm2r <- reactiveVal(default_params_range[[2]]$rm2r)
    # 
    # 
    # # For params_min and params_nopsa, we first create "prep" versions,
    # #    because one of the list elements is a function
    # #    that depends on other list elements
    # mparams_min_prep   <- reactiveVal(params[c("R0","pTested_per1","pTested_per2","pTested_per3","meanLatency","meanIncubation",
    #                                            "pNotSevere_per1","pNotSevere_per2","cfr_per1","cfr_per2","meanOnsetToDeath","meanHospToRecovery","meanMildToRecovery","seed")])              
    # mparams_nopsa_prep <- reactiveVal(params_nopsa)
    # 
    # #### Dynamic params panel: trigger resetting when users switch scenario/click restore ####
    # which_params <- reactiveVal(2)
    # observeEvent(as.integer(input$scenario)!=which_params() | input$restoreDefaults,{
    #   which_params(as.integer(input$scenario))
    #   callModule(modF, "params_panel",params_list=default_params[[which_params()]])
    # },priority = 15)
    # 
    # #which_params() will update w/o before update is clicked, so need another switch here to prevent rerunning psa 
    # switch_which <- reactiveVal(2)
    # 
    # #### Static params: trigger resetting when "restore defaults" is clicked ####
    # observeEvent(input$restoreDefaults, {
    #   reset("form")
    # },priority = 14)
    # 
    # #### Model results calculated in Shiny using default params ####
    # # We can either preload or calcuate in Shiny as this step runs fast
    # mrun_res_df <- run_seir(params=params) %>%
    #   mutate(lb="Default Parameters") 
    # 
    # ### Model calibration calculated in Shiny using default params: moderate scenario
    # cal_cases_df      <- err2(mrun_res_df) 
    # cal_fatalities_df <- err2_death(mrun_res_df)   
    # # cal_hosp_df <- err2_hosp(mrun_res_df)
    
    ####Calculating middle point of slider bars####
    # output$mlat <- renderText({
    #   paste("Midpoint:",mean(input$rlat),sep="\n\n")
    # })
    # output$minc <- renderText({
    #   paste("Midpoint:",mean(input$rinc),sep="\n\n")
    # })
    # output$ph1 <- renderText({
    #   paste("Midpoint:",mean(input$rph1),sep="\n\n")
    # })
    # output$ph2 <- renderText({
    #   paste("Midpoint:",mean(input$rph2),sep="\n\n")
    # })
    # output$cfr1 <- renderText({
    #   paste("Midpoint:",mean(input$rcfr1),sep="\n\n")
    # })
    # output$cfr2 <- renderText({
    #   paste("Midpoint:",mean(input$rcfr2),sep="\n\n")
    # })
    # output$mo2d <- renderText({
    #   paste("Midpoint:",mean(input$ro2d),sep="\n\n")
    # })
    # output$mh2r <- renderText({
    #   paste("Midpoint:",mean(input$rh2r),sep="\n\n")
    # })
    # output$mm2r <- renderText({
    #   paste("Midpoint:",mean(input$rm2r),sep="\n\n\n")
    # })
    
    
    
    #############################################################
    #### Update model inputs after "update plots" is clicked ####
    #############################################################
    # observeEvent(input$updatePlots, {
    #   
    #   # params affecting calibration
    #   # depending on whether panel is displayed or not, params can take either prerun values or input values
    #   # the issue with input values is rounding error, which can throw calibration warning into poor fit
    #   if(input$show_params) {
    #     ip1 <- list(
    #       R0                 = default_params[[which_params()]][["R0"]],
    #       pTested_per1       = as.numeric(input$pt1),
    #       pTested_per2       = as.numeric(input$pt2),
    #       pTested_per3       = as.numeric(input$pt3),
    #       meanLatency        = mean(input$rlat),
    #       meanIncubation     = mean(input$rinc),
    #       pNotSevere_per1         = 1-mean(input$rph1),
    #       pNotSevere_per2         = 1-mean(input$rph2),
    #       cfr_per1                = mean(input$rcfr1),
    #       cfr_per2                = mean(input$rcfr2),
    #       meanOnsetToDeath   = mean(input$ro2d),
    #       meanHospToRecovery = mean(input$rh2r),
    #       meanMildToRecovery = mean(input$rm2r),
    #       seed               = as.integer(input$tane)
    #     )
    #     mparams_min_prep(ip1)
    #   } else {
    #     mparams_min_prep(default_params[[which_params()]]) 
    #   }
    #   
    #   if(input$show_params) {
    #     ip3  <- mparams_nopsa_prep()
    #     ip3$R0 = default_params[[which_params()]][["R0"]]
    #     ip3$seed = as.integer(input$tane)
    #     mparams_nopsa_prep(ip3)
    #   } else {
    #     mparams_nopsa_prep(default_params_nopsa[[which_params()]])
    #   }
    #   
    #   if(input$show_params) {
    #     mparams_rlat(input$rlat)
    #     mparams_rinc(input$rinc)
    #     mparams_rph1(input$rph1)
    #     mparams_rph2(input$rph2)
    #     mparams_rcfr1(input$rcfr1)
    #     mparams_rcfr2(input$rcfr2)
    #     mparams_ro2d(input$ro2d)
    #     mparams_rh2r(input$rh2r)
    #     mparams_rm2r(input$rm2r)
    #   } else {
    #     mparams_rlat(default_params_range[[which_params()]]$rlat)
    #     mparams_rinc(default_params_range[[which_params()]]$rinc)
    #     mparams_rph1(default_params_range[[which_params()]]$rph1)
    #     mparams_rph2(default_params_range[[which_params()]]$rph2)
    #     mparams_rcfr1(default_params_range[[which_params()]]$rcfr1)
    #     mparams_rcfr2(default_params_range[[which_params()]]$rcfr2)
    #     mparams_ro2d(default_params_range[[which_params()]]$ro2d)
    #     mparams_rh2r(default_params_range[[which_params()]]$rh2r)
    #     mparams_rm2r(default_params_range[[which_params()]]$rm2r)
    #   }
    #   
    #   mparams_tr(input$tr)
    #   mparams_d2r(input$d2r)
    #   
    #   switch_which(which_params())
    #   
    # },priority=10) #highest priority # END of observeEvent()
    # 
    # 
    # #############################################################
    # ####Reactives: These model parameters depend on the values of other params ####
    # #############################################################
    # mparams_min <- reactive({
    #     newlist <- mparams_min_prep() 
    #     newlist$pTested <- function(t) {
    #         ifelse(t <= T_CUTOFF, as.numeric(mparams_min_prep()$pTested_per1),
    #                ifelse(t <= T_CUTOFF2, as.numeric(mparams_min_prep()$pTested_per2),
    #                       as.numeric(mparams_min_prep()$pTested_per3)))
    #     }
    #     
    #     newlist$cfr <- function(t) {
    #       # Note just using a single one for now now, but we can adapt. 
    #       ifelse(t>=0, as.numeric(mparams_min_prep()$cfr_per1),as.numeric(mparams_min_prep()$cfr_per2))
    #     }
    #     newlist$pNotSevere <- function(t) {
    #       ifelse(t>=0, as.numeric(mparams_min_prep()$pNotSevere_per1),as.numeric(mparams_min_prep()$pNotSevere_per2))
    #     }
    #     newlist
    # })
    # 
    # mparams_nopsa <- reactive({
    #     newlist <- mparams_nopsa_prep() 
    #     newlist$pTested <- function(t) {
    #       ifelse(t <= T_CUTOFF, as.numeric(mparams_min_prep()$pTested_per1),
    #              ifelse(t <= T_CUTOFF2, as.numeric(mparams_min_prep()$pTested_per2),
    #                     as.numeric(mparams_min_prep()$pTested_per3)))
    #     }
    #     newlist
    # })
    # 
    # mparams_psa <- reactive({
    #     list(
    #       meanLatency        = function(x) qgam(x, mean(mparams_rlat()), diff(mparams_rlat())/2),
    #       meanInfectious     = function(x) qgam(x, mean(mparams_rinc())-mean(mparams_rlat()), diff(mparams_rinc())/4),
    #       pNotSevere_per1    = function(x) qbet(x, 1-mparams_rph1()[2],1-mparams_rph1()[1]),
    #       pNotSevere_per2    = function(x) qbet(x, 1-mparams_rph2()[2],1-mparams_rph2()[1]),
    #       cfr_per1           = function(x) qbet(x, mparams_rcfr1()[1],mparams_rcfr1()[2]),
    #       cfr_per2           = function(x) qbet(x, mparams_rcfr2()[1],mparams_rcfr2()[2]),
    #       meanOnsetToDeath   = function(x) qgam(x, mean(mparams_ro2d()), diff(mparams_ro2d())/2),
    #       meanHospToRecovery = function(x) qgam(x, mean(mparams_rh2r()), diff(mparams_rh2r())/2),
    #       meanMildToRecovery = function(x) qgam(x, mean(mparams_rm2r()), diff(mparams_rm2r())/2)
    #     )
    # })
    # 
    # mparams_halton <- reactive({
    #   get_psa_params(number_of_halton_draws = number_of_psa_runs,
    #                  params_psa = mparams_psa(),
    #                  params_nop = mparams_nopsa()) %>% 
    #   map(~({
    #       .x <- 
    #         append(.x, 
    #                list(
    #                  pTested  = function(t) ifelse(t<=T_CUTOFF,.x[["pTested_per1"]],ifelse(t<=T_CUTOFF2,.x[["pTested_per2"]],.x[["pTested_per3"]])),
    #                  pNotSevere = function(t) ifelse(t>=0, .x[["pNotSevere_per1"]],.x[["pNotSevere_per1"]]),
    #                  cfr = function(t) ifelse(t>=0, .x[["cfr_per1"]],.x[["cfr_per1"]])
    #                )
    #         );.x}
    #     ), .progress=TRUE) 
    # })
    # 
    # 
    # #############################################################
    # #### Model outputs ####
    # #############################################################
    # #current model params + R remains below 1
    # mrun_res_baseline <- reactive({
    #   run_seir(params = mparams_min(),
    #            df = params_df[[switch_which()]][[1]], initialize_model = params_df[[switch_which()]][[2]]) %>%
    #     mutate(
    #         lb="Customized Parameters"
    #         )
    # })
    # 
    # mrun_df_psa <- reactive({
    #   mparams_halton() %>%
    #     map(~(simulation_function(.x,df_run=params_df[[switch_which()]][[1]],init_run = params_df[[switch_which()]][[2]])), .progress=TRUE) %>%
    #     map(~(data.frame(.x))) %>%
    #     bind_rows(.id = "iteration") %>%
    #     tbl_df()
    # })
    # 
    # #current model params + R rises to user-defined number
    # mrun_lift_res <- reactive({
    #   model_lift2(asymp_R = as.numeric(mparams_tr()), 
    #              trigger_val = 1000, 
    #              trigger_y = hospitalized, 
    #              days_to_asymp = as.integer(mparams_d2r()), 
    #              return= 'res', params= mparams_min(),
    #              df_run=params_df[[switch_which()]][[1]],
    #              init_run = params_df[[switch_which()]][[2]]
    #              )
    # })
    # 
    # mrun_lift_psa <- reactive({
    #   map(mparams_halton(),function(x) {x[["R0"]] = mrun_lift_res()[["R0"]]; x}) %>% 
    #     map(~(simulation_function(.x,df_run=params_df[[switch_which()]][[1]],init_run = params_df[[switch_which()]][[2]])), .progress=TRUE) %>%
    #     map(~(data.frame(.x))) %>%
    #     bind_rows(.id = "iteration") %>%
    #     tbl_df()
    # })
    # 
    # #default model params + R rises to user-defined number
    # mrun_lift_res_df <- reactive({
    #   model_lift2(asymp_R = as.numeric(mparams_tr()), 
    #               trigger_val = 1000, 
    #               trigger_y = hospitalized, 
    #               days_to_asymp = as.integer(mparams_d2r()), 
    #               return= 'res', params=default_params[[switch_which()]],
    #               df_run=params_df[[switch_which()]][[1]],
    #               init_run = params_df[[switch_which()]][[2]])
    # })
    # 
    # mrun_lift_psa_df <-  reactive({
    #   map(default_params_halton[[switch_which()]],function(x) {
    #     x[["R0"]] = mrun_lift_res_df()[["R0"]]
    #     x <- append(x, 
    #                 list(
    #                   pTested  = function(t) ifelse(t<=T_CUTOFF,x[["pTested_per1"]],ifelse(t<=T_CUTOFF2,x[["pTested_per2"]],x[["pTested_per3"]])),
    #                   pNotSevere = function(t) ifelse(t>=0,x[["pNotSevere_per1"]],x[["pNotSevere_per1"]]),
    #                   cfr = function(t) ifelse(t>=0, x[["cfr_per1"]],x[["cfr_per1"]])
    #                 )
    #     )
    #     x}) %>%
    #     map(~(simulation_function(.x,df_run=params_df[[switch_which()]][[1]],init_run = params_df[[switch_which()]][[2]])), .progress=TRUE) %>%
    #     map(~(data.frame(.x))) %>%
    #     bind_rows(.id = "iteration") %>%
    #     tbl_df()
    # })
    # 
    # 
    # 
    # #############################################################
    # #### Calibration Statistics ####
    # #############################################################
    # cal_cases <- reactive({
    #   err2(mrun_res_baseline())    
    # })
    # cal_fatalities <- reactive({
    #   err2_death(mrun_res_baseline())    
    # })
    # cal_hosp <- reactive({
    #   err2_hosp(mrun_res_baseline())    
    # })
    # cal_cases_rel <- reactive({
    #   cal_cases() / cal_cases_df    
    # })
    # cal_fatalities_rel <- reactive({
    #   cal_fatalities() / cal_fatalities_df    
    # })
    # cal_hosp_rel <- reactive({
    #   cal_hosp() / cal_hosp_df    
    # })
    # get_cal_word <- function(relcal) {
    #     if (relcal <= CALI_CUTOFF_GOOD) {
    #         glue::glue("good ({round(pmax(0,(relcal*100-100)),1)} percent error increase)")
    #     } else if (relcal <= CALI_CUTOFF_FAIR) {
    #       glue::glue("fair ({round(pmax(0,(relcal*100-100)),1)} percent error increase)")
    #     } else {
    #       glue::glue("poor ({round(pmax(0,(relcal*100-100)),1)} percent error increase)")
    #     }
    # }
    # cal_cases_word <- reactive({
    #     get_cal_word(cal_cases_rel())
    # })
    # cal_fatalities_word <- reactive({
    #     get_cal_word(cal_fatalities_rel())
    # })
    # cal_hosp_word <- reactive({
    #   get_cal_word(cal_hosp_rel())
    # })
    # get_cal_color <- function(relcal) {
    #     if (relcal <= CALI_CUTOFF_GOOD) {
    #         CALI_COLOR_GOOD    
    #     } else if (relcal <= CALI_CUTOFF_FAIR) {
    #         CALI_COLOR_FAIR    
    #     } else {
    #         CALI_COLOR_POOR    
    #     }
    # }
    # cal_cases_color <- reactive({
    #     get_cal_color(cal_cases_rel())
    # })
    # cal_fatalities_color <- reactive({
    #     get_cal_color(cal_fatalities_rel())
    # })
    # cal_hosp_color <- reactive({
    #   get_cal_color(cal_hosp_rel())
    # })
    # output$cal_preface_text <- renderUI({
    #     if (input$updatePlots) {
    #         shiny::HTML(paste0(tags$h5("Relative fit of customized model, compared to default:")))
    #     } else return(NULL)
    # })
    # output$cal_cases_text <- renderUI({
    #     if (input$updatePlots) {
    #         shiny::HTML(paste0(shiny::tags$span(
    #                 style= paste0("color:", cal_cases_color()), 
    #                 paste0("In terms of cases: ", cal_cases_word())
    #             )
    #         ))
    #     } else return(NULL)
    # })
    # output$cal_fatalities_text <- renderUI({
    #     if (input$updatePlots) {
    #         shiny::HTML(paste0(shiny::tags$span(
    #                 style= paste0("color:", cal_fatalities_color()), 
    #                 paste0("In terms of deaths: ", cal_fatalities_word())
    #             )
    #         ))
    #     } else return(NULL)
    # })
    # output$cal_hosp_text <- renderUI({
    #   if (input$updatePlots) {
    #     shiny::HTML(paste0(shiny::tags$span(
    #       style= paste0("color:", cal_hosp_color()), 
    #       paste0("In terms of hospitalizations: ", cal_hosp_word())
    #     )
    #     ))
    #   } else return(NULL)
    # })
    # 
    # 
    # 
    # 
    # #############################################################
    # #### Plots ####
    # #############################################################
    # output$f1_cali <- renderPlot({
    #   if(input$updatePlots) {
    #     dd <- mrun_res_baseline() %>% mutate(lb="Customized Parameters") %>% 
    #       bind_rows(mrun_res_df %>% mutate(lb="Default Parameters"))
    #     p <- plot_cali1(res=dd)#,df_hosp=ls_hosp_calib[[switch_which()]])
    #   } else {
    #     p <- plot_cali1(res=mrun_res_df %>% mutate(lb=""))#,df_hosp=ls_hosp_calib[[switch_which()]])
    #   }
    #   
    #   p 
    # }, width =1000, height = 1000*5/8)
    # #outputOptions(output, "f1_cali", suspendWhenHidden = FALSE)
    # 
    # # output$f2_cali <- renderPlot({
    # #   if(input$updatePlots) {
    # #     p <- plot_cali2(res=mrun_res_baseline(),p2=TRUE,df=mrun_res_df)
    # #   } else {
    # #     p <- plot_cali2(res=mrun_res_df)
    # #   }
    # #   p
    # # })
    # 
    # output$f5_lift <- renderPlot({
    # 
    #   if(input$updatePlots) {
    #     #customized scenario
    #     lift_new <- list(
    #       tr=mparams_tr(),
    #       lft_res=mrun_lift_res()$res,
    #       lft_psa=mrun_lift_psa(),
    #       base_res=mrun_res_baseline(),
    #       base_psa=mrun_df_psa()
    #     )
    #     #baseline is default, lift scenario with updated tr
    #     lift_old <- list(
    #       tr=mparams_tr(),
    #       lft_res=mrun_lift_res_df()$res,
    #       lft_psa=mrun_lift_psa_df(),
    #       base_res=default_res_baseline[[switch_which()]],
    #       base_psa=default_res_psa[[switch_which()]]
    #     )
    #     p <- plot_lift_psa(l1=lift_new,l2=lift_old,p2=TRUE)
    #     } else {
    #       #everything default
    #       lift_old <- list(
    #         tr=MPARAMS_TR_PRESPEC,
    #         lft_res=default_lift_baseline[[switch_which()]]$res,
    #         lft_psa=default_lift_psa[[switch_which()]],
    #         base_res=default_res_baseline[[switch_which()]],
    #         base_psa=default_res_psa[[switch_which()]]
    #       )
    #       p <- plot_lift_psa(l1=lift_old)
    #   }
    #   print(p)
    #   # grid.text("NOTE: User-customized estimates.", x = 0.65, y = 0.30,
    #   #           gp=gpar(fontsize=15, col="grey"))
    #   # grid.text("NOTE: User-customized estimates.", x = 0.25, y = 0.30,
    #   #           gp=gpar(fontsize=15, col="grey"))
    #   grid.text("NOTE: User-customized estimates.", x = 0.5, y = 0.30,
    #             gp=gpar(fontsize=15, col="grey"))
    # }, width =1000, height = 1000*5/8)
    # #outputOptions(output, "f5_lift", suspendWhenHidden = FALSE)
    # 
    # # output$f6_table <- gt::render_gt(
    # #       expr = gt_tbl
    # #     )
    # 
    # # CMI plot
    # output$f0 <- renderPlot({
    #     # df_pr %>%
    #     # mutate(byregion = "Tennessee")  %>%
    #     ####user defined date or minus 14 days?####
    #     get_case_mix_index_(paste0(input$cmi_dt)) #+
    #     #theme(legend.position="top") +
    #     #ggtitle("All of Tennessee")
    # }) 
    # 
    # # DEBUGGER
    # output$p1 <- renderPrint({
    #   mparams_min() %>% print()
    #   #mrun_res_baseline() %>% head() %>% print()
    # })
    # 
    # output$p2 <- renderPrint({
    #   # mparams_halton()[[1]] %>% print()
    #   # mrun_res_baseline() %>% head() %>% print()
    #   # mrun_df_psa() %>% head() %>% print()
    #   # mparams_nopsa() %>% print()
    #   # mrun_lift_res_df()$res %>% head() %>% print()
    #   # mrun_lift_psa_df() %>% head() %>% print()
    #   # default_lift_psa[[switch_which()]] %>% head() %>% print()
    #   mrun_df_psa() %>% head() %>% print()
    #   default_res_psa[[switch_which()]] %>% head() %>% print()
    # })
    # 
    # output$test1 <- renderPrint({
    #   #mparams_min()$pTested  
    #   input$show_params
    # })
    # 
    # 
    
})
