library(shinydashboard)
library(shinyjs)
library(shinybusy)
library(readr)
library(purrr)
library(stringr)
library(shinyBS)
options(readr.default_locale=readr::locale(tz="America/Chicago"))
# library(reactlog)
# 
# # tell shiny to log all reactivity
# options(shiny.reactlog = TRUE)
# load("preload_params.rda")
source("dynamic_ui.R")
max_cmi_date <- Sys.Date()-5
source("model-description.R")
# tags$head(
#   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
# ),

# dbHeader <- dashboardHeader()
# dbHeader$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
#                                            tags$img(src='www/chem-logo.png',height='60',width='200'))

ui <- navbarPage(theme = "custom.css",
                 selected = "COVID-19 Vital Signs",
                title=div(img(src='chem-logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 55)),
                useShinyjs(),
                
                #indicator when shiny is busy
                #alternative progress bar will not display if the app is just loading
                div(add_busy_bar(color = "red", height = "6px")),
                
                #### COVID VITAL SIGNS Page####
                
                tabPanel("COVID-19 Vital Signs",
                         theme = "custom.css",
                         #img(src = "chem-logo.png", style = "float:right; padding-right:25px", height = "25%", width = "25%"),
                         br(),
                         br(),
                         p("Welcome to the Vanderbilt Center for Health Economic Modeling COVID-19 Dashboard. This interactive website provides regularly-updated analyses
                           of COVID-19 in Tennessee. We also provide a customizable projection model to allow users to explore different future scenarios 
                           based on the Susceptible-Exposed-Infected-Recovered (SEIR) model adapted to SARS-CoV-2/COVID-19."),
      
                         br(),
                         p("The tabs below show descriptive output on SARS-CoV-2 transmission across Tennessee. It is updated weekly on Monday afternoons."),
                         br(),
                         fluidRow(
                           tags$style(type = "text/css",
                                      "label { font-size: 14px; font-family: 'Gill Sans', 'Gill Sans MT', Calibri, sans-serif; }"
                           ),
                           tags$head(tags$style(type="text/css",
                                                ".shiny-text-output {white-space: pre-wrap;
                                                 font-size: 15px;
                                                 text-align: right;
                                                 font-family: 'Gill Sans', 'Gill Sans MT', Calibri, sans-serif;}"
                           )),
                           
                           #####results section#####
                           tabBox(
                             id="out1",
                             width=8,
                             tabPanel("Case Mix Index",
                                      br(),
                                      p("The COVID Case Mix Index (CMI) adjusts the number of daily cases by a key factor—age—which
                                        has been consistently identified as a risk factor for poor outcomes after infection, 
                                        including increasing risk of both hospitalization and death. Our index 
                                        incorporates data compiled by the Centers for Disease Control and Prevention (CDC)."),
                                      p("Each index is based on a rolling 7 day average. To facilitate comparisons across indices, each index has also been rescaled 
                                        to have a value of 1.0 two weeks prior to the most recently shown date. Thus, values above 1.0 indicate a 
                                        rise in the number of cases in the last two weeks, a rise in the risk profile of new cases, 
                                        or both. Moreover, time periods when the COVID CMI exceeds the reported new cases 
                                        (i.e., regions shaded in red ) indicate periods where there are a relatively 
                                        large number of infected high-risk individuals—while time periods where the COVID 
                                        CMI adjusted new cases are lower than the reported new cases (i.e., regions shaded in blue) 
                                        indicate periods with a relatively larger share of infections among lower-risk individuals."),
                                      
                                      dateInput("cmi_dt","Index Date",value=max_cmi_date-21,min=as.Date("2020-03-25"),max=max_cmi_date),
                                      plotOutput("f0",width = "900px", height ="1000px"),
                                      # img(src="01_case-mix-index.png",
                                      #     height="100%", width="100%"),
                                      p("Note: Vertical dotted line denotes index date. Both indices are rescaled to have a value of 1.0 on this date.")
                                      
                             ),
                             tabPanel("Geographic Heatmaps",
                                      img(src="map-new-covid-cases-TN.png",
                                          height="150%", width="150%"),
                                      img(src="anim_tn-cases-by-zip_TN.gif",
                                          height = "100%",width = "100%"),
                                      br(),
                                      img(src="map-new-covid-cases-tn_highland_rim.png",
                                          height="100%", width="100%"),
                                      img(src="map-new-covid-cases-mid_south.png",
                                          height="100%", width="100%"),
                                      img(src="map-new-covid-cases-southeast_hamilton.png",
                                          height="100%", width="100%"),
                                      
                                      img(src="map-new-covid-cases-knox_east.png",
                                          height="100%", width="100%"),
                                      img(src="map-new-covid-cases-upper_cumberland.png",
                                          height="100%", width="100%"),
                                      img(src="map-new-covid-cases-south_central.png",
                                          height="100%", width="100%"),
                                      
                                      img(src="map-new-covid-cases-watch_7.png",
                                          height="100%", width="100%"),
                                      img(src="map-new-covid-cases-northeast.png",
                                          height="100%", width="100%")
                                      
                             ),
                             tabPanel("Age Heatmaps",
                                      img(src = "age-heatmap-byregion.png",
                                          height = "100%",width = "100%")
                            ),
                             tabPanel("Reproduction Number",
                                      img(src="01_Rt_TN.png",
                                          height="100%", width="100%"),
                                      br(),
                                      img(src="01_Rt_davidson.png",
                                          height="100%", width="100%"),
                                      img(src="01_Rt_shelby.png",
                                          height="100%", width="100%"),
                                      img(src = "01_Rt_coalition.png",
                                          height = "100%",width = "100%")
                             ),
                             tabPanel("Mobility",
                                      h5("Mobility into regions across Tennessee compared to the same time period in 2019."),
                                      img(src="in-mobility_coalition.png",
                                          height="100%", width="100%"),
                                      h5("Mobility into regions of Tennessee most affected and least affected by COVID-19."),
                                      img(src="in-mobility_affected.png",
                                          height="100%", width="100%"),
                                      img(src="in-mobility_affected_diff.png",
                                          height="100%", width="100%")
                             )
                             
                           )
                           
                         )
                ),
                 ####Model Page####
                 tabPanel("Model",
                          theme = "custom.css",
                          #img(src = "chem-logo.png", style = "float:right; padding-right:25px", height = "15%", width = "15%"),
                          fluidRow(
                            tags$style(type = "text/css",
                                       "label { font-size: 12px; font-family: 'Gill Sans', 'Gill Sans MT', Calibri, sans-serif; }"
                            ),
                            tags$head(tags$style(type="text/css",
                                                 ".shiny-text-output {white-space: pre-wrap;
                                                 font-size: 13px;
                                                 text-align: right;
                                                 font-family: 'Gill Sans', 'Gill Sans MT', Calibri, sans-serif;}"
                            )),
                            
                            #####parameter section#####
                            box(title="Parameters",
                                width=4,
                                actionButton("updatePlots", "Update Plots"),
                                actionButton("restoreDefaults", "Restore Defaults"),
                                br(),br(),
                                HTML('<p>Click "Update Plots" to refresh the output.<br/>
                                Click "Restore defaults" to reset parameters.<br/> <br/>
                                Note: Model runtime is up to 2 minutes when plots are updated.</p>'),
                                
                                h4("Key Model Parameters"),
                                div(id="form",
                                  textInput("tr","Assumed future effective reproduction number (R)","1.1"),
                                  textInput("d2r","Days to reach assumed effective reproduction number","21")
                                ),
                                tags$div(
                                  id="scenario", class="form-group shiny-input-radiogroup shiny-input-container",
                                  tags$label(class="control-label", `for`="scenario", "Disease Parameter Scenarios:"),
                                  tags$div(class="shiny-options-group",
                                           tags$div(class="radio",
                                                    tags$label(
                                                      tags$input(type="radio", name="scenario", value="1",id="el1",
                                                                 tags$span(HTML("High Asymptomatic/Mild Scenario"))))),
                                           tags$div(class="radio",
                                                    tags$label(
                                                      tags$input(type="radio", name="scenario", value="2",checked="checked",id="el2",
                                                                 tags$span(HTML("Medium Asymptomatic/Mild Scenario"))))),
                                           tags$div(class="radio",
                                                    tags$label(
                                                      tags$input(type="radio", name="scenario", value="3",id="el3",
                                                                 tags$span(HTML("Low Asymptomatic/Mild Scenario")))))
                                  )
                                ),
                                bsTooltip("el1", "Scenario assumes a very high fraction of mild and asymptomatic cases are not detected and reported via testing.","top"),
                                bsTooltip("el2", "Scenario assumes some mild and asymptomatic cases are detected and reported via testing.","top"),
                                bsTooltip("el3", "Scenario assumes most mild and asymptomatic cases are detected and reported via testing.","top"),
                                # REMOVE REGIONAL FILES FOR NOW
                                # radioButtons("scenario","Disease Severity Assumptions",
                                #              choiceNames=c("Mild Disease Assumptions","Moderate Disease Assumptions","Severe Disease Assumptions","Nashville Region (Moderate Assumptions)","Memphis Region (Moderate Assumptions)"),
                                #              choiceValues = 1:5, selected = 2),
                                h4("Advanced Model Settings"),
                                checkboxInput("show_params","Show advanced model parameters",FALSE),
                                conditionalPanel(condition = "input.show_params",
                                  modUI("params_panel")
                                )
                                
                            ),
                            #####results section#####
                            tabBox(
                              id="out1",
                              width=8,
                              # tabPanel("Animation",
                              #          br(),
                              #          img(src="anim_tn-simulated.gif",
                              #              height="50%", width="60%")
                              # ),
                              tabPanel("Projections",
                                       br(),
                                       p("This section provides model projections and uncertainty ranges based on default and user-defined parameters and scenarios for the trajectory of the effective reproduction number (R) moving forward."),
                                       plotOutput("f5_lift")
                                       # br(),
                                       # br(),
                                       # br(),
                                       # br(),
                                       # br(),
                                       # br(),
                                       # br(),
                                       # br(),
                                       # br(),
                                       # gt::gt_output(outputId = "f6_table")
                                       # #DEBUGGER
                                       # verbatimTextOutput("p2")
                              ),
                              tabPanel("Calibration",
                                       br(),
                                       p("This section compares model fit (line) to reported data on cases (total and per day) and deaths (total). Calibration statistics are calculated based on the default and customized parameters to give users a sense of how the model fits historic data on the transmission, reporting and health consequences of SARS-CoV-2/COVID-19 in Tennessee. "),
                                       br(),
                                       p("Note that default calibration of model parameters is set to achieve the lowest mean-squared error for the last 21 days of reported cases and the last 14 days of hospitalizations.
                                         Therefore, the model trades off some error in the early days of the pandemic for improved projections based on data over the last 2-3 weeks"),
                                       htmlOutput('cal_preface_text'),
                                       htmlOutput('cal_cases_text'),
                                       htmlOutput('cal_fatalities_text'),
                                       # htmlOutput('cal_hosp_text'),
                                       br(),
                                       plotOutput("f1_cali")
                                      # plotOutput("f2_cali")
                                       # #DEBUGGER
                                       # verbatimTextOutput("test1"),
                                       # verbatimTextOutput("p1")
                              )
                              
                            )
                            
                          )
                 ),
               
                
                ####Intro Page####
                tabPanel("About",
                         theme = "custom.css",
                         #img(src = "chem-logo.png", style = "float:right; padding-right:25px", height = "15%", width = "15%"),
                         br(),
                         br(),
                         #lapply(model_description,FUN=p),
                         lapply(transpose(str_split(model_description,"\n\n")),FUN=p),
                         br(),
                         br(),
                         img(src="vu-covid-model-diagram.png",
                             height="50%", width="50%"),
                         br()
                         
                ),
                 tabPanel("Team",
                          theme = "custom.css",
                          #img(src = "chem-logo.png", style = "float:right; padding-right:25px", height = "15%", width = "15%"),
                          h3("Modeling Team"),
                          tags$ul(
                            tags$li("John A. Graves, Ph.D.,",
                                    tags$em("Associate Professor and Director of the Vanderbilt Center for Health Economic Modeling, Department of Health Policy and Department of Medicine, Vanderbilt University School of Medicine and Vanderbilt University Medical Center")
                            ),
                            tags$li("Shawn Garbett, MA,",
                                    tags$em("Assistant in Biostatistics, Vanderbilt University School of Medicine and Vanderbilt University Medical Center, Department of Biostatistics")
                            ),
                            tags$li("Melinda Buntin, Ph.D.,",
                                    tags$em("Mike Curb Chair and Chair, Department of Health Policy, Vanderbilt University School of Medicine and Vanderbilt University Medical Center")
                            ),
                            tags$li("Melissa McPheeters, Ph.D.,",
                                    tags$em("Research Professor, Department of Health Policy, Vanderbilt University School of Medicine and Vanderbilt University Medical Center")
                            ),
                            
                            tags$li("Peter Rebeiro, Ph.D.,",
                                    tags$em("Assistant Professor, Vanderbilt University School of Medicine and Vanderbilt University Medical Center, Department of Medicine and Department of Biostatistics")
                            ),
                            #ORDER?
                            tags$li("Leonce Nshuti, MS,",
                                    tags$em("Applications Developer, Department of Health Policy, Vanderbilt University Medical Center")
                            ),
                            tags$li("Zilu Zhou, MPH,",
                                    tags$em("Sr Statistical Analyst, Department of Health Policy, Vanderbilt University Medical Center")
                            ),
                            tags$li("Laurie Samuels, Ph.D.,",
                                    tags$em("Assistant Professor, Department of Biostatistics and Department of Health Policy, Vanderbilt University Medical Center")
                            )
                          )
                 )
                 
                 # ####Other####
                 # navbarMenu("More",
                 #            tabPanel("References",
                 #                     h2("...")
                 #            ),
                 #            tabPanel("Data",
                 #                     h2("...")
                 #            ),
                 #            tabPanel("Version",
                 #                     # update version number each time a new version is pushed
                 #                     #    major.minor.patch
                 #                     h2("Preload data using 06/01 parameters")
                 #            )
                 # )
)

