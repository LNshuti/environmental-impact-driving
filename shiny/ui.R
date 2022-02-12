library(shinydashboard)
library(shinyjs)
library(shinybusy)
library(readr)
library(purrr)
library(stringr)
library(shinyBS)
options(readr.default_locale=readr::locale(tz="America/Chicago"))
library(reactlog)
# 
# # tell shiny to log all reactivity
options(shiny.reactlog = TRUE)
# load("preload_params.rda")
source("dynamic_ui.R")
max_cmi_date <- Sys.Date()-5


# dbHeader <- dashboardHeader()
# dbHeader$children[[2]]$children <-
#   tags$a(href='https://one.ai/'#,
#          #tags$img(src='www/chem-logo.png',height='60',width='200')
#          )

ui <- navbarPage(theme = "custom.css",
                 selected = "COVID-19 Vital Signs",
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
                         p("Welcome to ONE CIRCLE"),
      
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
                                      
                                      p("Note: Vertical dotted line denotes index date. Both indices are rescaled to have a value of 1.0 on this date.")
                                      
                             )#,
                            #  tabPanel("Geographic Heatmaps",
                            #           img(src="map-new-covid-cases-TN.png",
                            #               height="150%", width="150%"),
                            #           img(src="anim_tn-cases-by-zip_TN.gif",
                            #               height = "100%",width = "100%"),
                            #           br()
                            #           
                            #  ),
                            #  tabPanel("Age Heatmaps",
                            #           img(src = "age-heatmap-byregion.png",
                            #               height = "100%",width = "100%")
                            # )
                             
                           )
                           
                         )
                ),
                ####Intro Page####
                tabPanel("About",
                         theme = "custom.css",
                         #img(src = "chem-logo.png", style = "float:right; padding-right:25px", height = "15%", width = "15%"),
                         br()
                         
                ),
                 tabPanel("Team",
                          theme = "custom.css",
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
                 

)

