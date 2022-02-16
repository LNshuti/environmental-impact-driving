
#' # Mother Consent Form (MC): only in screening
#' 
## ----  echo=FALSE, results='hide', eval = TRUE--------------------------------------------------------------------------------
###################
# MC Form
###################

grep("lat", names(BaseDat))
grep("mother_consent_complete", names(BaseDat))

names(BaseDat)[5:31]

new.names=c(lat="latscr",
            lon="lonscr", 
            chw_id_screening="chwidscr",
            chw_screening_name="name_chwscr",
            location_of_screening="loc_scr",
            full_name_of_parent="name_p",
            chw_inform="cons_sigscrchw",
            participant_sign_screen="cons_sigscrp",  
            date_of_consent_screen="dateconsentscr",
            screen_consent_check="scrConsent_complete",
            address_of_guardian="address_guard",  
            in_what_city_does_the_pati="city", 
            center_2="phc_kad",
            center="phc_kan",
            center_3="phc_zar", 
            level_of_education_of_pare="educ_p", 
            does_the_guardian_or_paren="reled_p", 
            marital_status_of_parent_o="marital_status", 
            number_of_children_in_hous="num_chil")

#Rename old var names to new var names, the labels will stay with the new variable names
 repindx <- names(BaseDat) %in% names(new.names)
 names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]


#' 
#' 
#' # Epilepsy_screening_questionnaire Form (EPSCR) (only in screening)
#' 
## ----  echo=FALSE, results='hide', eval = TRUE--------------------------------------------------------------------------------
###################
#EPSCR Form
###################
 grep("now", names(BaseDat))
 grep("epilepsy_screening_questionnaire_complete", names(BaseDat))
 
 names(BaseDat)[32:81]
########## rename

new.names=c(now="now",
            date="date",
            name_of_the_child="name_child",
            date_of_birth="dob",
            age="age",
            gender="gender",
            child_currently_at_school="school",
            if_yes_what_grade="grade",
            ever_had_a_seizure="epeval_q1",
            a_has_she_he_had_two_more="epeval_q1a",
            b_has_she_he_had_any_episo="epeval_q1b",
            c_has_she_he_have_these_ep="epeval_q1c",
            has_your_child_ever_had_an="epeval_q2",
            clusters_of_head_drops="epeval_q3",
            a_does_your_child_also_hav="epeval_q3a",
            b_do_these_episodes_result="epeval_q3b",
            do_any_of_the_following_ha="epeval_q4",
            side_shaking="epeval_q4a",
            does_his_her_eyes_or_head="epeval_q5",
            side_jerking="epeval_q5a",
            does_he_she_have_difficult="epeval_q6",
            side_body="epeval_q6a",
            difficulty_talking="epeval_q7",
            appears_confused="epeval_q7a",
            can_he_she_answer_question="epeval_q8",
            does_his_her_eyes_always_m="epeval_q9",
            a_is_it_the_same_side_each="epeval_q9a",
            side_eyes="epeval_q9b",
            does_his_her_head_always_m="epeval_q10",
            a_is_it_the_same_direction="epeval_q10a",
            does_he_she_describe_any_u="epeval_q11",
            does_he_she_remember_any_u="epeval_q12",
            does_he_she_lose_contact_w="epeval_q13",
            during_an_episode_do_any_o="epeval_q14",
            does_he_she_complain_of_an="epeval_q15",
            just_before_the_episode_or="epeval_q16",
            does_your_child_have_isola="epeval_q17",
            does_your_child_have_episo="epeval_q18",
            vscreen_positive="screen_positive",
            age_of_onset="onset_age",
            does_your_child_suffer_fro="other_condition",
            which_one___1="condition1",
            which_one___2="condition2",
            which_one___3="condition3",
            which_one___4="condition4",
            which_one___5="condition5",
            which_one___6="condition6",
            which_one___7="condition7",
            which_one___8="condition8")

# Rename old var names to new var names
repindx <- names(BaseDat) %in% names(new.names)
names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]



#' 
#' 
#' # Assent_and_physical_exam form (AAP) (for a kid, either in screening or follow up [one time])
#' 
## ----  echo=FALSE, results='hide', eval = TRUE--------------------------------------------------------------------------------
###################
# AAP Form
###################

####variables common in screening and follow up###

intersect(names(datKano)[22:102], names(BaseDat)[82:160])

####variables in follow up AAP but not in screening 
setdiff(names(datKano)[22:102], names(BaseDat)[82:160] )
#"child_present", "enroll_today", "weight_w0" , "what_is_the_final_diagnosi_w1"

####variables in screening but not in follow up 
setdiff( names(BaseDat)[82:160],names(datKano)[22:102])
#"weight"                        "what_is_the_final_diagnosi_w0"

setdiff(names(datKaduna)[22:102], names(BaseDat)[82:160] )##same as datKano
setdiff(names(BaseDat)[82:160],names(datKaduna)[22:102] )
# "weight"                        "what_is_the_final_diagnosi_w0"

setdiff(names(datZaria)[22:102], names(BaseDat)[82:160] )  ##same as datKano
setdiff(names(BaseDat)[82:160],names(datZaria)[22:102] )
# "weight"                        "what_is_the_final_diagnosi_w0"
#


################## rename

new.names=c(
###common variables in screening and follow up####
date_assent_exam="dateassent", 
assent_7_12_w0="assent_712_scr", 
assent_13_17_w0="assent_1317_scr",
assent_check_w0="assent_chk_scr",

temperature_in_c_w0="temp_w0",

ears_nose_mouth_and_throat_w0="enmt_w0",
cardiovascular_w0="heart_w0",
respiratory_w0="resp_w0",
gastrointestinal_w0="gi_w0",

skin_hair_and_nails_w0="skin_w0",

psychiatric_mental_health_w0="psych_w0",

hematological_w0="blood_w0",
which_skin_disorder_w0="skin_disorder_w0",
blood_disorder_w0___1="blood_disorder1",
blood_disorder_w0___2="blood_disorder2",
blood_disorder_w0___3="blood_disorder3",
blood_disorder_w0___4="blood_disorder4",
blood_disorder_w0___5="blood_disorder5",
blood_disorder_w0___6="blood_disorder6",
blood_disorder_w0___0="blood_disorder0",


is_there_weakness_present_w0="weakness_w0",
what_type_of_global_weakne_w0="which_weak_w0",
is_there_tremor_present_w0="tremor",

is_the_child_walking_norma_w0="gait",
develop_delay_w0___0="dev_del0",
develop_delay_w0___1="dev_del1",
develop_delay_w0___2="dev_del2",
develop_delay_w0___3="dev_del3",
develop_delay_w0___4="dev_del4",
develop_delay_w0___5="dev_del5",
seizures_first_month_w0="seize_1stmo",
has_the_child_had_regressi_w0="dev_regress",
brain_imaging_w0___0="imaging0",
brain_imaging_w0___1="imaging1",
brain_imaging_w0___2="imaging2",
brain_imaging_w0___3="imaging3",
ever_had_eeg_w0="eeg",
diagnosis_of_seizures_w0="diagsieze",
if_not_seizure_then_what_i_w0="not_seizure",
what_type_of_seizure_w0="siezeclass",
what_type_of_focal_seizure_w0="typefocal",
aura_present_w0="aura",
what_type_of_generalized_s_w0="typegeneralized",

what_type_of_generalized_n_w0="typegennomoto",
does_the_child_has_altered_w0="awareness",
is_there_consiousness_impa_w0="consciousness",
has_the_child_been_treated_w0="treatedprevious",
when_was_the_last_time_the_w0="whentreated",
what_drug_did_the_child_re_w0="aedpast",
does_this_child_has_infant_w0="spasms",
does_this_child_have_a_dia_w0="diagepi")

# Rename for each dataset
repindx <- names(BaseDat) %in% names(new.names)
names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]



#specific to screening
names_scr <- c(
weight="Weight",
what_is_the_final_diagnosi_w0="diagfinal_w0")

repindx <- names(BaseDat) %in% names(names_scr)
names(BaseDat)[repindx] <- names_scr[match(names(BaseDat)[repindx], names(names_scr))]


#specific to follow up
names_follow <- c(
child_present="aap",
enroll_today="ev0",

what_is_the_final_diagnosi_w1="diagfinal_w0"
)
 
repindx <- names(datKano) %in% names(names_follow )
names(datKano)[repindx] <- names_follow[match(names(datKano)[repindx], names(names_follow ))]

repindx <- names(datKaduna) %in% names(names_follow )
names(datKaduna)[repindx] <- names_follow[match(names(datKaduna)[repindx], names(names_follow))]

repindx <- names(datZaria) %in% names(names_follow )
names(datZaria)[repindx] <- names_follow[match(names(datZaria)[repindx], names(names_follow))]


#' 
#' # Enroll_consent_assent_and_visit_0 form (EV0) (For a kid, either in screening or follow up [one time])
#' 
## ----  echo=FALSE, results='hide', eval = TRUE--------------------------------------------------------------------------------
###################
# EV0 Form
###################

####variables common in screening and follow up###

intersect(names(datKano)[103:140], names(BaseDat)[161:197])

####variables in follow up but not in screening 
setdiff(names(datKano)[103:140], names(BaseDat)[161:197] )
#[1] "chw_id_assent"   "guardian_name"   "aed_v0_w0"   "step_1_dosing_carb_0_w0"
#[5] "step_1_dosing_valp_0_w0" "step_1_dosing_phen_0_w0"

####variables in screening but not in follow up 
setdiff( names(BaseDat)[161:197],names(datKano)[103:140])
#"baseline_freq_seizures" "aed_v0_w02"             "step_1_dosing_carb_0"   "step_1_dosing_valp_0"   "step_1_dosing_phen_0"  

setdiff(names(datKaduna)[103:140], names(BaseDat)[161:197] )##same as datKano
setdiff(names(BaseDat)[161:197],names(datKaduna)[103:140] )

setdiff(names(datZaria)[103:140], names(BaseDat)[161:197] )  ##same as datKano
setdiff(names(BaseDat)[161:197],names(datZaria)[103:140] )



############rename
new.names=c(
  
 chw_enroll_consent_w0="cons_sigenrollchw",                    
 enroll_consent_w0="cons_sigenrollp",
 date_of_consent_w0="dateconsentenroll",
 enroll_assent_w0="asnt_sigenrollc" ,           
 chw_enroll_assent_w0="asnt_sigenrollchw",                     
 date_of_assent_w0="dateassentenroll",
 assent_enroll_check_w0="asnt_chk_enroll"   ,
 adhd_hi_q1_w0="adhd_hiq1_w0",    
 adhd_hi_q2_w0="adhd_hiq2_w0", 
 adhd_hi_q3_w0="adhd_hiq3_w0", 
 adhd_hi_q4_w0="adhd_hiq4_w0", 
 adhd_hi_q5_w0="adhd_hiq5_w0", 
 adhd_hi_q6_w0="adhd_hiq6_w0", 
 adhd_hi_q7_w0="adhd_hiq7_w0", 
 adhd_hi_q8_w0="adhd_hiq8_w0", 
 adhd_hi_q9_w0="adhd_hiq9_w0",
 adhd_ia_q1_w0="adhd_iaq1_w0",
 adhd_ia_q2_w0="adhd_iaq2_w0", 
 adhd_ia_q3_w0="adhd_iaq3_w0", 
 adhd_ia_q4_w0="adhd_iaq4_w0",
 adhd_ia_q5_w0="adhd_iaq5_w0", 
 adhd_ia_q6_w0="adhd_iaq6_w0", 
 adhd_ia_q7_w0="adhd_iaq7_w0", 
 adhd_ia_q8_w0="adhd_iaq8_w0", 
 adhd_ia_q9_w0="adhd_iaq9_w0", 
 adhd_hi_raw_w0="adhd_hiraw_w0", 
 adhd_ia_raw_w0="adhd_israw_w0",
 adhd_tot_raw_w0="adhdtotraw_w0",
 
 verify_type_w0="verifyaed1", 
 verify_type_3_w0="verifyaed3", 
 verify_type_2_w0="verifyaed2")


# Rename for each dataset
repindx <- names(BaseDat) %in% names(new.names)
names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]

           
###specific to screening
names_scr <- c(
 baseline_freq_seizures="basefreq", 
 aed_v0_w02="aedv0" 
 
)

repindx <- names(BaseDat) %in% names(names_scr)
names(BaseDat)[repindx] <- names_scr[match(names(BaseDat)[repindx], names(names_scr))]



###specific to follow up
names_follow <- c(
    
    guardian_name="name_guardian"
    )

repindx <- names(datKano) %in% names(names_follow)
names(datKano)[repindx] <- names_follow[match(names(datKano)[repindx], names(names_follow))]

repindx <- names(datKaduna) %in% names(names_follow)
names(datKaduna)[repindx] <- names_follow[match(names(datKaduna)[repindx], names(names_follow))]

repindx <- names(datZaria) %in% names(names_follow)
names(datZaria)[repindx] <- names_follow[match(names(datZaria)[repindx], names(names_follow))]


#' 
#' 
#' 
#' # QOILE 31 (in visit 0 [in either screening or follow up] and follow up month 2 and month 24[in follow up], repeated)
#' 
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#Qolie31>17, Form
###################


####variables common in screening and follow up###

intersect(names(datKano)[409:441], names(BaseDat)[198:230])

####variables in follow up but not in screening 
setdiff(names(datKano)[409:441], names(BaseDat)[198:230] )
#"qolie_31_17_v0_m2_m24_complete"

####variables in screening but not in follow up 
setdiff( names(BaseDat)[198:230],names(datKano)[409:441])
#"qolie_31_17_v0_complete"




###################rename
new.names=c(q_date2="dateqolie31", 
            quality_rating="qolie31_q1",
    full_of_pep="qolie31_q2",
    nervous_person="qolie31_q3",
    felt_down="qolie31_q4",
    felt_calm="qolie31_q5",
    lot_of_energy="qolie31_q6",
    downhearted="qolie31_q7",
    worn_out="qolie31_q8",
    happy_person="qolie31_q9",
    feel_tired="qolie31_q10",
    worried_about_seize="qolie31_q11",
    difficulty_reasoning="qolie31_q12",
    limited_social="qolie31_q13",
    qol_number="qolie31_q14",
    fourwks_memory="qolie31_q15",
    fourwks_remembering_told="qolie31_q16",
    fourwks_reading="qolie31_q17",
    fourwks_1_task="qolie31_q18",
    leisure_time="qolie31_q19",
    driving="qolie31_q20",
    fearful_seize="qolie31_q21",
    hurting_yourself="qolie31_q22",
    social_problems="qolie31_q23",
    medications_bad="qolie31_q24",
    seizures="qolie31_q25",
    memory_difficulties="qolie31_q26",
    work_limits="qolie31_q27",
    social_limits="qolie31_q28",
    physical_effects_aed="qolie31_q29",
    mental_effects_aed="qolie31_q30",
    health_rating_sco="qolie31_q31")

# Rename for each dataset
repindx <- names(BaseDat) %in% names(new.names)
names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]



#' 
#' 
#' # QOILE 48 (in visit 0 [screening] and follow up month 2 and month 24 [follow up])
#' 
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#Qolie 48 11-17, Form
###################

####variables common in screening and follow up###

intersect(names(datKano)[442:493], names(BaseDat)[231:282])

####variables in follow up but not in screening 
setdiff(names(datKano)[442:493], names(BaseDat)[231:282] )
#"qolie_48_1117_v0_m2_m24_complete"

####variables in screening but not in follow up 
setdiff( names(BaseDat)[231:282],names(datKano)[442:493])
#"qolie_48_1117_v0_complete"



#################### rename
new.names <- c(
    q_date3="dateqolie48",
    health="qolie48_1",
    one_yr_ago="qolie48_2",
    heavy_activities="qolie48_3",
    moderate_activities="qolie48_4",
    light_activities="qolie48_5",
    other_daily_activities="qolie48_6",
    do_fewer_things="qolie48_7",
    limit_activities="qolie48_8",
    difficulty_perform="qolie48_9",
    skip_school="qolie48_10",
    trouble_in_school="qolie48_11",
    trouble_out_of_scho="qolie48_12",
    concentration_activity="qolie48_13",
    concentration_reading="qolie48_14",
    difficulty_thinking="qolie48_15",
    solving_problems="qolie48_16",
    remembering_previous="qolie48_17",
    understanding_teachers="qolie48_18",
    understanding_read="qolie48_19",
    someone_available="qolie48_20",
    someone_confide="qolie48_21",
    someone_talk_to="qolie48_22",
    someone_accepted="qolie48_23",
    aed_limits_social="qolie48_24",
    feel_alone="qolie48_25",
    miss_classes="qolie48_26",
    aed_as_excuse="qolie48_27",
    feel_embarrassed="qolie48_28",
    limits_performance="qolie48_29",
    limits_from_seize="qolie48_30",
    limit_independ="qolie48_31",
    limit_social_li="qolie48_32",
    limit_sport="qolie48_33",
    looks="qolie48_34",
    limits_set="qolie48_35",
    lees_than_per="qolie48_36",
    employment="qolie48_37",
    dating="qolie48_38",
    blame_afraid="qolie48_39",
    blame_opinions="qolie48_40",
    mentally_unstable="qolie48_41",
    good_or_bad="qolie48_42",
    how_fair="qolie48_43",
    happy_or_sad="qolie48_44",
    bad_or_good_felt="qolie48_45",
    starting_new="qolie48_46",
    worry_sieze="qolie48_47",
    fear_dying="qolie48_48",
    hurting_yourself_during="qolie48_49")


# Rename for each dataset
repindx <- names(BaseDat) %in% names(new.names)
names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]



#' 
#' 
#' # Next step form (in screening only, one time)
#' 
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#NSF Form
###################

names(BaseDat)[283:287]

new.names <- c(
  child_present="aap",  
  enroll_today="ev0"  
  )

repindx <- names(BaseDat) %in% names(new.names)
names(BaseDat)[repindx] <- new.names[match(names(BaseDat)[repindx], names(new.names))]



#' 
#' # Data landing form (DLP, in follow up only, one time)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#DLP Form
###################

names(datKano)[1:21]


###############rename: not do
new.names <- c(
  center="phc_kan",
  center_2 ="phc_kad",
  center_3="phc_zar",
  chw_id="chw_name",
  name_of_the_child ="name_child",
  weight ="Weight",
  date_of_birth ="dob",
  age_w0="age",
  gender="gender",                     
  full_name_of_parent="name_p",
  
  
  what_is_the_final_diagnosi_w0="diagfinal0")

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' 
#' # Follow up care form (FCF, in follow up repeated at each visit w1, 1,2,4,6,9,12,15,18,24 months)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#FCF Form
###################

grep("lat3",names(datKano))
grep("follow_up_care_form_complete",names(datKano))

names(datKano)[141:274]

  
###############rename
new.names=c(lat3 = "latfu",
lon3 = "lonfu",
date_fu_v2 = "datefu",
chw_id_fu_v2 = "chwidfu",
name_of_chew_v2 = "name_chwfu",
age_fu_v2 = "agefu",
new_med_v2 = "newmed",

temperature_in_c_v2 = "temp_v2",
diagnosis_of_seizures_v2 = "confirmseize",
confirm_seizure_class_v2 = "seizeclass",

ears_nose_mouth_and_throat_v2 = "enmt_v2",
cardiovascular_v2 = "heart_v2",
respiratory_v2 = "resp_v2",
gastrointestinal_v2 = "gi_v2",

skin_hair_and_nails_v2 = "skin_v2",

psychiatric_mental_health_v2 = "psych_v2",

which_skin_finding_v2 = "skin_disorder_v2",
hematological_disorder_v2___1 = "anemia",
hematological_disorder_v2___2 = "sicklecell",
hematological_disorder_v2___3 = "nhl",
hematological_disorder_v2___4 = "leukemia",
hematological_disorder_v2___5 = "hl",
hematological_disorder_v2___6 = "multimyeloma",





is_there_weakness_present_v2 = "weakness",
type_global_weakness_v2 = "which_weak",


developmental_delays_v2___0 = "nodelays",
developmental_delays_v2___1 = "motor",
developmental_delays_v2___2 = "speech",
developmental_delays_v2___3 = "cognitive",
developmental_delays_v2___4 = "social",
developmental_delays_v2___5 = "other",

experienced_seiz_v2 = "expseiz",




drug_treatment_v2 = "treatment",
aed_number = "aednumber",
what_drug_v2 = "aed",
freq_carb_v2 = "freqseiz_carb",

md_communication_carb_v2 = "mdcomm_carb",

step_1_carb_v2 = "step1carb",
step_2_carb_v2 = "step2carb",
step_3_carb_v2 = "step3carb",
freq_valp_v2 = "freqseiz_valp",

md_communication_valp_v2 = "mdcomm_valp",


step_1_valp_v2 = "step1valp",
step_2_valp_v2 = "step2valp",
step_3_valp_v2 = "step3valp",
freq_phen_v2 = "freqseiz_phen",

md_communication_phen_v2 = "mdcomm_phen",

step_1_phen_v2 = "step1phen",
step_2_phen_v2 = "step2phen",
step_3_phen_v2 = "step3phen",
med_1_changes_v2 = "deltamed",
changes_v2 = "medchange",

how_many_euc_v2 = "number_seiz",


most_recent_prolong_euc_v2 = "last_pseiz",

combined_aed_euc_v2 = "combinedaed",

dosing_am_euc_v2 = "doseam",
dosing_afternoon_euc_v2 = "dosenoon",
dosing_evening_euc_v2 = "dosepm",
anti_epileptic_drug_2_euc_v2 = "aed2",
dosing_am_euc_2_v2 = "doseam2",
dosing_noon_euc_2_v2 = "dosenoon2",
dosing_pm_euc_2_v2 = "dosepm2",
anti_epileptic_drug_3_euc_v2 = "aed3",
dosing_am_euc_3_v2 = "doseam3",
dosing_noon_euc_3_v2 = "dosenoon3",
dosing_pm_euc_3_v2 = "dosepm3",

which_side_effects_euc_v2___1 = "whicheffect_1",
which_side_effects_euc_v2___2 = "whicheffect_2",
which_side_effects_euc_v2___3 = "whicheffect_3",
which_side_effects_euc_v2___4 = "whicheffect_4",
which_side_effects_euc_v2___5 = "whicheffect_5",
which_side_effects_euc_v2___6 = "whicheffect_6",
which_side_effects_euc_v2___7 = "whicheffect_7",
which_side_effects_euc_v2___8 = "whicheffect_8",
which_side_effects_euc_v2___9 = "whicheffect_9",
which_side_effects_euc_v2___10 = "whicheffect_10",
which_side_effects_euc_v2___11 = "whicheffect_11",
which_side_effects_euc_v2___12 = "whicheffect_12",
which_side_effects_euc_v2___13 = "whicheffect_13",
which_side_effects_euc_v2___14 = "whicheffect_14",
which_side_effects_euc_v2___15 = "whicheffect_15",
seizures_after_aed_euc_v2 = "seizaftaed",
frequency_change_euc_v2 = "deltafreq",
new_abnormalities_euc_v2 = "newabs",
#specific for Kano
added_question_for_fup = "qolie",

#specific for Kaduna
added_question_for_fup_kaduna="qolie",

#specific for Zaria
added_question_for_fup_zaria="qolie"
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]


repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]


repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' 
#' 
#' 
#' # Supplemental Visit/CRF (SV, in follow up, repeated  when needed )
#' 
#' Use this form after tirst two drugs failed.
#' 
#' 
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#SV Form
###################

grep("date_sup",names(datKano))
grep("supplemental_visitcrf_complete",names(datKano))

names(datKano)[275:370]


##############rename##########

new.names=c(date_sup="datesup",

name_of_chew_sup="name_chwsup",
weight_sup="weightsup",
temperature_sup="tempsup",
diagnosis_seizures_sup="",
confirm_seize_class="confirmseizesup",
eyes_sup="eyessup",
ears_nose_mouth_throat_sup="enmtsup",
cardiovascular_sup="heartsup",
respiratory_sup="respsup",
gastrointestinal_sup="gisup",
musculoeskeletal_sup="mssksup",
skin_hair_nails_sup="skinsup",
neurological_sup="neurosup",
psych_health_sup="psychsup",
hormones_sup="endosup",
blood_sup="bloodsup",
which_skin_sup="skin_disordersup",
abnormal_blood_sup___1="blood_disordersup1",
abnormal_blood_sup___2="blood_disordersup2",
abnormal_blood_sup___3="blood_disordersup3",
abnormal_blood_sup___4="blood_disordersup4",
abnormal_blood_sup___5="blood_disordersup5",
abnormal_blood_sup___6="blood_disordersup6",
abnormal_blood_sup___7="blood_disordersup7",
working_memory_sup="memorysup",
spontanoeous_speech_sup="spon_speechsup",
comprehemsion_sup="comprehendsup",
naming_sup="namingsup",
repetition_sup="repititionsup",
reading_optional_sup="readingsup",
affect_sup="affectsup",
right_arm_sup="rtarmsup",
left_arm_sup="lftarmsup",
right_leg_sup="rtlegsup",
left_leg_sup="lftlegsup",
right_arm2_sup="rtarm2sup",
left_arm2_sup="lftarm2sup",
right_leg2_sup="rtleg2sup",
left_leg2_sup="lftleg2sup",
right_arm3_sup="rtarm3sup",
left_arm3_sup="lftarm3sup",
right_leg3_sup="rtleg3sup",
left_leg3_sup="lftleg3sup",
weakness_sup="weaknesssup",
type_weakness_sup="which_weaksup",
tremor_sup="tremorsup",
type_tremor_sup="which_tremorsup",
gait_sup="gaitsup",
develop_delay_sup___0="dev_delsup1",
develop_delay_sup___1="dev_delsup2",
develop_delay_sup___2="dev_delsup3",
develop_delay_sup___3="dev_delsup4",
develop_delay_sup___4="dev_delsup5",
develop_delay_sup___5="dev_delsup6",


status_epilepticus="prolong_seiz",
how_many_status="number_pseiz",

two_drugs_non="numberaed",


what_side_effects___1="whicheffect1",
what_side_effects___2="whicheffect2",
what_side_effects___3="whicheffect3",
what_side_effects___4="whicheffect4",
what_side_effects___5="whicheffect5",
what_side_effects___6="whicheffect6",
what_side_effects___7="whicheffect7",
what_side_effects___8="whicheffect8",
what_side_effects___9="whicheffect9",
what_side_effects___10="whicheffect10",

dx_tests_non="dxtests",
what_dxtests___1="whichdxtest1",
what_dxtests___2="whichdxtest2",
what_dxtests___3="whichdxtest3",
what_dxtests___4="whichdxtest4",
complete_and_schedule_non="visitcomplete")

## Rename old var names to new var names

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' 
#' 
#' # Child care questionaire (CCQ,in follow up W1, M12 M24)
#' 
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#CCQ Form
###################

grep("q_date",names(datKano))
grep("child_care_questionnaire_w1_m12_m24_complete",names(datKano))

names(datKano)[371:384]


  
###############rename
new.names <- c(
  q_date ="dateccare",
  
  name_of_your_phc="phc_name",
  name_of_your_phc_kaduna="phc_name",
  name_of_your_pch_zaria="phc_name"
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]


repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' # 23 Questionaire (in follow up W1, M24, repeated)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#23Q Form
###################

grep("q_date1",names(datKano))
grep("questionnaire_w1_m24_complete",names(datKano))

names(datKano)[385:408]



###############rename
new.names <- c(
  q_date1="date23q"
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]



#' 
#' # ADHD age 5-10 (in follow up M2, M24 repeated)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#ADHD Form
###################

grep("q_date4",names(datKano))
grep("adhd_age_510_m2_m24_complete",names(datKano))

names(datKano)[494:516]

###############rename
new.names <- c(
  q_date4 = "dateadhd"
  
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]



#' 
#' # Blineded 1 month (in follow up)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#BMD1 Form
###################

grep("date_bm1",names(datKano))
grep("blinded_1_month_complete",names(datKano))

names(datKano)[517:576]


###############rename
new.names <- c(
date_bm1 = "datebm1",
md_bm1 = "namemdbm1",
place_bm1 = "citybm1",
age_bm1 = "agebm1",
age_bm1_mo = "agemobm1",
weight_bm1 = "wtbm1",
ep_dx_bm1 = "epdxbm1",
other_dx_bm1 = "otherdxbm1",
seizure_class_bm1 = "seizeclassbm1",

seizures_last_visit_bm1 = "last_seizbm1",
prolonged_bm1 = "prolong_seizbm1",
number_prolonged_bm1 = "number_pseizbm1",
on_aed_bm1 = "treatmentbm1",
aed_bm1 = "aedbm1",
other_aed_bm1 = "otheraedbm1",
aed_am_bm1 = "doseambm1",
aed_noon_bm1 = "dosenoonbm1",
aed_pm_bm1 = "dosepmbm1",
side_effects_bm1 = "sideffectbm1",
which_side_effects_bm1___1 = "concentrationbm1",
which_side_effects_bm1___2 = "fatiguebm1",
which_side_effects_bm1___3 = "se_visionbm1",
which_side_effects_bm1___4 = "se_speechbm1",
which_side_effects_bm1___5 = "coordinationbm1",
which_side_effects_bm1___6 = "dizzinessbm1",
which_side_effects_bm1___7 = "nauseabm1",
which_side_effects_bm1___8 = "depressionbm1",
which_side_effects_bm1___9 = "rashbm1",
which_side_effects_bm1___10 = "bruisingbm1",
which_side_effects_bm1___11 = "sedationbm1",
which_side_effects_bm1___12 = "hyperbm1",
which_side_effects_bm1___13 = "drowsybm1",
which_side_effects_bm1___14 = "mtremorsbm1",
which_side_effects_bm1___15 = "vomitingbm1",
sz_reduction_bm1 = "szdecreasebm1",
sz_freq_pct_bm1 = "pctdecreasebm1",
new_neurodeficit_bm1 = "newneurodefbm1",
which_deficit_bm1___1 = "reflexesbm1",
which_deficit_bm1___2 = "mutebm1",
which_deficit_bm1___3 = "desensbm1",
which_deficit_bm1___4 = "balancebm1",
which_deficit_bm1___5 = "memorybm1",
which_deficit_bm1___6 = "defi_visionbm1",
which_deficit_bm1___7 = "walkingbm1",
which_deficit_bm1___8 = "weaknessbm1",
which_delays_bm1___1 = "nonebm1",
which_delays_bm1___2 = "motorbm1",
which_delays_bm1___3 = "delay_speechbm1",
which_delays_bm1___4 = "cognitivebm1",
which_delays_bm1___5 = "socialbm1",
which_delays_bm1___6 = "otherbm1",
dx_tests_bm1 = "dxtestsbm1",
which_tests_bm1___1 = "bloodbm1",
which_tests_bm1___2 = "imagingbm1",
which_tests_bm1___3 = "eegbm1",
which_tests_bm1___4 = "hospitalizationsbm1",
final_comments_bm1 = "commentsbm1",
blinded_1_month_complete = "completebm1"
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' # Blineded 6 month (in follow up)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#BMD6 Form
###################

grep("date_bm6",names(datKano))
grep("blinded_6_month_complete",names(datKano))

names(datKano)[577:635]


###############rename
new.names <- c(
  date_bm6 = "datebm6",
md_bm6 = "namemdbm6",
place_bm6 = "citybm6",
age_bm6 = "agebm6",
weight_bm6 = "wtbm6",
ep_dx_bm6 = "epdxbm6",
other_dx_bm6 = "otherdxbm6",
seizure_class_bm6 = "seizeclassbm6",

seizures_last_visit_bm6 = "last_seizbm6",
prolonged_bm6 = "prolong_seizbm6",
number_prolonged_bm6 = "number_pseizbm6",
on_aed_bm6 = "treatmentbm6",
aed_bm6 = "aedbm6",
other_aed_bm6 = "otheraedbm6",
aed_am_bm6 = "doseambm6",
aed_noon_bm6 = "dosenoonbm6",
aed_pm_bm6 = "dosepmbm6",
side_effects_bm6 = "sideffectbm6",
which_side_effects_bm6___1 = "concentrationbm6",
which_side_effects_bm6___2 = "fatiguebm6",
which_side_effects_bm6___3 = "se_visionbm6",
which_side_effects_bm6___4 = "se_speechbm6",
which_side_effects_bm6___5 = "coordinationbm6",
which_side_effects_bm6___6 = "dizzinessbm6",
which_side_effects_bm6___7 = "nauseabm6",
which_side_effects_bm6___8 = "depressionbm6",
which_side_effects_bm6___9 = "rashbm6",
which_side_effects_bm6___10 = "bruisingbm6",
which_side_effects_bm6___11 = "sedationbm6",
which_side_effects_bm6___12 = "hyperbm6",
which_side_effects_bm6___13 = "drowsybm6",
which_side_effects_bm6___14 = "mtremorsbm6",
which_side_effects_bm6___15 = "vomitingbm6",
sz_reduction_bm6 = "szdecreasebm6",
sz_freq_pct_bm6 = "pctdecreasebm6",
new_neurodeficit_bm6 = "newneurodefbm6",
which_deficit_bm6___1 = "reflexesbm6",
which_deficit_bm6___2 = "mutebm6",
which_deficit_bm6___3 = "desensbm6",
which_deficit_bm6___4 = "balancebm6",
which_deficit_bm6___5 = "memorybm6",
which_deficit_bm6___6 = "defi_visionbm6",
which_deficit_bm6___7 = "walkingbm6",
which_deficit_bm6___8 = "weaknessbm6",
which_delays_bm6___1 = "nonebm6",
which_delays_bm6___2 = "motorbm6",
which_delays_bm6___3 = "delay_speechbm6",
which_delays_bm6___4 = "cognitivebm6",
which_delays_bm6___5 = "socialbm6",
which_delays_bm6___6 = "otherbm6",
dx_tests_bm6 = "dxtestsbm6",
which_tests_bm6___1 = "bloodbm6",
which_tests_bm6___2 = "imagingbm6",
which_tests_bm6___3 = "eegbm6",
which_tests_bm6___4 = "hospitalizationsbm6",
final_comments_bm6 = "commentsbm6",
blinded_6_month_complete = "completebm6"
  
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]



#' # Blineded 12 month (in follow up)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#BMD12 Form
###################

grep("date_bm12",names(datKano))
grep("blinded_12_month_complete",names(datKano))

names(datKano)[636:694]


###############rename
new.names <- c(
  date_bm12 = "datebm12",
md_bm12 = "namemdbm12",
place_bm12 = "citybm12",
age_bm12 = "agebm12",
weight_bm12 = "wtbm12",
ep_dx_bm12 = "epdxbm12",
other_dx_bm12 = "otherdxbm12",
seizure_class_bm12 = "seizeclassbm12",

seizures_last_visit_bm12 = "last_seizbm12",
prolonged_bm12 = "prolong_seizbm12",
number_prolonged_bm12 = "number_pseizbm12",
on_aed_bm12 = "treatmentbm12",
aed_bm12 = "aedbm12",
other_aed_bm12 = "otheraedbm12",
aed_am_bm12 = "doseambm12",
aed_noon_bm12 = "dosenoonbm12",
aed_pm_bm12 = "dosepmbm12",
side_effects_bm12 = "sideffectbm12",
which_side_effects_bm12___1 = "concentrationbm12",
which_side_effects_bm12___2 = "fatiguebm12",
which_side_effects_bm12___3 = "se_visionbm12",
which_side_effects_bm12___4 = "se_speechbm12",
which_side_effects_bm12___5 = "coordinationbm12",
which_side_effects_bm12___6 = "dizzinessbm12",
which_side_effects_bm12___7 = "nauseabm12",
which_side_effects_bm12___8 = "depressionbm12",
which_side_effects_bm12___9 = "rashbm12",
which_side_effects_bm12___10 = "bruisingbm12",
which_side_effects_bm12___11 = "sedationbm12",
which_side_effects_bm12___12 = "hyperbm12",
which_side_effects_bm12___13 = "drowsybm12",
which_side_effects_bm12___14 = "mtremorsbm12",
which_side_effects_bm12___15 = "vomitingbm12",
sz_reduction_bm12 = "szdecreasebm12",
sz_freq_pct_bm12 = "pctdecreasebm12",
new_neurodeficit_bm12 = "newneurodefbm12",
which_deficit_bm12___1 = "reflexesbm12",
which_deficit_bm12___2 = "mutebm12",
which_deficit_bm12___3 = "desensbm12",
which_deficit_bm12___4 = "balancebm12",
which_deficit_bm12___5 = "memorybm12",
which_deficit_bm12___6 = "defi_visionbm12",
which_deficit_bm12___7 = "walkingbm12",
which_deficit_bm12___8 = "weaknessbm12",
which_delays_bm12___1 = "nonebm12",
which_delays_bm12___2 = "motorbm12",
which_delays_bm12___3 = "delay_speechbm12",
which_delays_bm12___4 = "cognitivebm12",
which_delays_bm12___5 = "socialbm12",
which_delays_bm12___6 = "otherbm12",
dx_tests_bm12 = "dxtestsbm12",
which_tests_bm12___1 = "bloodbm12",
which_tests_bm12___2 = "imagingbm12",
which_tests_bm12___3 = "eegbm12",
which_tests_bm12___4 = "hospitalizationsbm12",
final_comments_bm12 = "commentsbm12",
blinded_12_month_complete = "completebm12"
  
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' # Blineded 18 month (in follow up)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#BMD18 Form
###################

grep("date_bm18",names(datKano))

grep("blinded_18_month_complete",names(datKano))

names(datKano)[695:753]


###############rename
new.names <- c(
  date_bm18 = "datebm18",
md_bm18 = "namemdbm18",
place_bm18 = "citybm18",
age_bm18 = "agebm18",
weight_bm18 = "wtbm18",
ep_dx_bm18 = "epdxbm18",
other_dx_bm18 = "otherdxbm18",
seizure_class_bm18 = "seizeclassbm18",

seizures_last_visit_bm18 = "last_seizbm18",
prolonged_bm18 = "prolong_seizbm18",
number_prolonged_bm18 = "number_pseizbm18",
on_aed_bm18 = "treatmentbm18",
aed_bm18 = "aedbm18",
other_aed_bm18 = "otheraedbm18",
aed_am_bm18 = "doseambm18",
aed_noon_bm18 = "dosenoonbm18",
aed_pm_bm18 = "dosepmbm18",
side_effects_bm18 = "sideffectbm18",
which_side_effects_bm18___1 = "concentrationbm18",
which_side_effects_bm18___2 = "fatiguebm18",
which_side_effects_bm18___3 = "se_visionbm18",
which_side_effects_bm18___4 = "se_speechbm18",
which_side_effects_bm18___5 = "coordinationbm18",
which_side_effects_bm18___6 = "dizzinessbm18",
which_side_effects_bm18___7 = "nauseabm18",
which_side_effects_bm18___8 = "depressionbm18",
which_side_effects_bm18___9 = "rashbm18",
which_side_effects_bm18___10 = "bruisingbm18",
which_side_effects_bm18___11 = "sedationbm18",
which_side_effects_bm18___12 = "hyperbm18",
which_side_effects_bm18___13 = "drowsybm18",
which_side_effects_bm18___14 = "mtremorsbm18",
which_side_effects_bm18___15 = "vomitingbm18",
sz_reduction_bm18 = "szdecreasebm18",
sz_freq_pct_bm18 = "pctdecreasebm18",
new_neurodeficit_bm18 = "newneurodefbm18",
which_deficit_bm18___1 = "reflexesbm18",
which_deficit_bm18___2 = "mutebm18",
which_deficit_bm18___3 = "desensbm18",
which_deficit_bm18___4 = "balancebm18",
which_deficit_bm18___5 = "memorybm18",
which_deficit_bm18___6 = "defi_visionbm18",
which_deficit_bm18___7 = "walkingbm18",
which_deficit_bm18___8 = "weaknessbm18",
which_delays_bm18___1 = "nonebm18",
which_delays_bm18___2 = "motorbm18",
which_delays_bm18___3 = "delay_speechbm18",
which_delays_bm18___4 = "cognitivebm18",
which_delays_bm18___5 = "socialbm18",
which_delays_bm18___6 = "otherbm18",
dx_tests_bm18 = "dxtestsbm18",
which_tests_bm18___1 = "bloodbm18",
which_tests_bm18___2 = "imagingbm18",
which_tests_bm18___3 = "eegbm18",
which_tests_bm18___4 = "hospitalizationsbm18",
final_comments_bm18 = "commentsbm18",
blinded_18_month_complete = "completebm18"
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' 
#' # Blineded 24 month (in follow up)
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#BMD24 Form
###################

grep("date_bm24",names(datKano))
grep("blinded_24_month_complete",names(datKano))

names(datKano)[754:812]



###############rename
new.names <- c(
  date_bm24 = "datebm24",
md_bm24 = "namemdbm24",
place_bm24 = "citybm24",
age_bm24 = "agebm24",
weight_bm24 = "wtbm24",
ep_dx_bm24 = "epdxbm24",
other_dx_bm24 = "otherdxbm24",
seizure_class_bm24 = "seizeclassbm24",

seizures_last_visit_bm24 = "last_seizbm24",
prolonged_bm24 = "prolong_seizbm24",
number_prolonged_bm24 = "number_pseizbm24",
on_aed_bm24 = "treatmentbm24",
aed_bm24 = "aedbm24",
other_aed_bm24 = "otheraedbm24",
aed_am_bm24 = "doseambm24",
aed_noon_bm24 = "dosenoonbm24",
aed_pm_bm24 = "dosepmbm24",
side_effects_bm24 = "sideffectbm24",
which_side_effects_bm24___1 = "concentrationbm24",
which_side_effects_bm24___2 = "fatiguebm24",
which_side_effects_bm24___3 = "se_visionbm24",
which_side_effects_bm24___4 = "se_speechbm24",
which_side_effects_bm24___5 = "coordinationbm24",
which_side_effects_bm24___6 = "dizzinessbm24",
which_side_effects_bm24___7 = "nauseabm24",
which_side_effects_bm24___8 = "depressionbm24",
which_side_effects_bm24___9 = "rashbm24",
which_side_effects_bm24___10 = "bruisingbm24",
which_side_effects_bm24___11 = "sedationbm24",
which_side_effects_bm24___12 = "hyperbm24",
which_side_effects_bm24___13 = "drowsybm24",
which_side_effects_bm24___14 = "mtremorsbm24",
which_side_effects_bm24___15 = "vomitingbm24",
sz_reduction_bm24 = "szdecreasebm24",
sz_freq_pct_bm24 = "pctdecreasebm24",
new_neurodeficit_bm24 = "newneurodefbm24",
which_deficit_bm24___1 = "reflexesbm24",
which_deficit_bm24___2 = "mutebm24",
which_deficit_bm24___3 = "desensbm24",
which_deficit_bm24___4 = "balancebm24",
which_deficit_bm24___5 = "memorybm24",
which_deficit_bm24___6 = "defi_visionbm24",
which_deficit_bm24___7 = "walkingbm24",
which_deficit_bm24___8 = "weaknessbm24",
which_delays_bm24___1 = "nonebm24",
which_delays_bm24___2 = "motorbm24",
which_delays_bm24___3 = "delay_speechbm24",
which_delays_bm24___4 = "cognitivebm24",
which_delays_bm24___5 = "socialbm24",
which_delays_bm24___6 = "otherbm24",
dx_tests_bm24 = "dxtestsbm24",
which_tests_bm24___1 = "bloodbm24",
which_tests_bm24___2 = "imagingbm24",
which_tests_bm24___3 = "eegbm24",
which_tests_bm24___4 = "hospitalizationsbm24",
final_comments_bm24 = "commentsbm24",
blinded_24_month_complete = "completebm24"
)

repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


#' # Quantitative survey (in follow up,repeated w1, m12, m24 )
#' 
## ----echo=FALSE, results='hide'-----------------------------------------------------------------------------------------------
###################
#QS Form
###################

grep("date_qs",names(datKano))
grep("quantitative_survey_complete",names(datKano))

names(datKano)[813:904]


###############rename
new.names=c(date_qs = "dateqs",
age_qs = "ageqs",
age_mo_qs = "agemoqs",
uncomfortable_qs = "uncomfortableqs",
inferior_qs = "inferiorqs",
avoid_you_qs = "avoidyouqs",
perceived_stigma_sco_qs = "perceivedstigmascoqs",
possessed_qs = "possessedqs",
witch_qs = "witchqs",
never_marry_qs = "nevermarryqs",
good_job_qs = "goodjobqs",
no_job_qs = "nojobqs",
never_play_qs = "neverplayqs",
avoided_qs = "avoidedqs",
bad_treatment_qs = "badtreatmentqs",
bad_family_qs = "badfamilyqs",
tot_en_stigma_score_qs = "totenstigmascoreqs",
medical_qs = "medicalqs",
knowledge_qs = "knowledgeqs",
refers_qs = "refersqs",
good_tests_qs = "goodtestsqs",
best_effort_qs = "besteffortqs",
responsive_qs = "responsiveqs",
knows_me_qs = "knowsmeqs",
considers_preferences_qs = "considersprefsqs",
avoids_assumptions_qs = "avoidsassumptqs",
tailors_treatment_qs = "tailorstreatqs",
treats_me_indiv_qs = "treatsmeindivqs",
considers_whole_qs = "considerswholeqs",
concerned_comfort_qs = "concerncomfortqs",
expresses_concern_qs = "expressconcernqs",
is_empathetic_qs = "isempatheticqs",
offers_help_qs = "offershelpqs",
is_reassuring_qs = "isreassuringqs",
is_hopeful_qs = "ishopefulqs",
puts_my_interest_qs = "putsmyinterestqs",
health_concerns_serious_qs = "healthconcernsqs",
correct_diagnosis_qs = "correctdiagnosisqs",
achieves_outcomes_qs = "achievesoutcomesqs",
uses_prev_services_qs = "usesprevservicesqs",
listens_actively_qs = "listensactivelyqs",
acknowledges_concerns_qs = "acknowconcernsqs",
explains_diagnosis_qs = "explaindiagnosisqs",
answers_my_questions_qs = "answerquestionsqs",
comms_directly_qs = "commsdirectlyqs",
is_sensitive_qs = "issensitiveqs",
is_relaxed_and_calm_qs = "isrelaxedncalmqs",
provides_trea_opts_qs = "providetreaoptsqs",
treats_as_equal_qs = "treatsasequalqs",
trusts_patients_qs = "trustspatientsqs",
is_open_qs = "isopenqs",
is_flexible_qs = "isflexibleqs",
admits_mistakes_qs = "admitsmistakesqs",
honors_commitment_qs = "honorscommitmentqs",
is_respectful_qs = "isrespectfulqs",
non_judgmental_qs = "nonjudgmentalqs",
appropriate_train_qs = "appropriatetrainqs",
appropriate_age_qs = "appropriateageqs",
preffered_gender_qs = "prefferedgenderqs",
recommended_by_others_qs = "recommendedsqs",
professional_appearance_qs = "profappearanceqs",
courteous_staff_qs = "courteousstaffqs",
staff_forwards_qs = "staffforwardsqs",
clearly_explains_qs = "clearexplainqs",
staff_provides_access_qs = "staffprovsaccessqs",
on_call_arrange_qs = "oncallarrangeqs",
common_disea_qs = "commondiseaqs",
treatable_disease_qs = "treatablediseaseqs",
not_psychoge_qs = "notpsychogeqs",
not_heredita_qs = "nothereditaqs",
not_contagious_qs = "notcontagiousqs",
not_evil_forces_qs = "notevilforcesqs",
not_magic_qs = "notmagicqs",
not_evil_eye_qs = "notevileyeqs",
brain_malformation_qs = "brainmalformqs",
caused_by_meningitis_qs = "causedbymeningqs",
caused_by_trauma_qs = "causedbytraumaqs",
caused_by_stroke_qs = "causedbystrokeqs",
know_aed_qs = "knowaedqs",
know_seizure_type_qs = "knowseizuretypeqs",
meets_approval_qs = "meetsapprovalqs",
appealing_qs = "appealingqs",
welcome_program_qs = "welcomeprogramqs",
seems_fitting_qs = "seemsfittingqs",
seems_suitable_qs = "seemssuitableqs",
seems_applicable_qs = "seemsapplicableqs",
seems_good_match_qs = "seemsgoodmatchqs",
seems_implement_qs = "seemsimplementqs",
seems_possible_qs = "seemspossibleqs",
seems_doable_qs = "seemsdoableqs",
seems_easy_qs = "seemseasyqs"
)
repindx <- names(datKano) %in% names(new.names)
names(datKano)[repindx] <- new.names[match(names(datKano)[repindx], names(new.names))]

repindx <- names(datKaduna) %in% names(new.names)
names(datKaduna)[repindx] <- new.names[match(names(datKaduna)[repindx], names(new.names))]

repindx <- names(datZaria) %in% names(new.names)
names(datZaria)[repindx] <- new.names[match(names(datZaria)[repindx], names(new.names))]


