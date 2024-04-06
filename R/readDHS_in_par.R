# Read in data, merge, and append to list function ####
readDHS_in_par <- function(directory = NULL, survey_recode = NULL, fw = NULL, 
                           #hr = NULL, 
                           male = FALSE) {
  
  # Error messages
  #   No directory
  if(is.null(directory)) {
    stop("You must specify a directory")
  }
  #   No individual recode
  if(is.null(survey_recode)) {
    stop("You must specify an survey value")
  }
  # No fieldworker code
  if(is.null(fw)) {
    warning("You must specify a fieldworker character value")
  }
  # # No fieldworker code
  # if(is.null(hr)) {
  #   stop("You must specify a Household Record character value")
  # }
  
  # Make sure it's upper case
  survey_recode <- str_to_upper(survey_recode)
  try({fw <- str_to_upper(fw)})
  
  # Read in the data
  survey.df <- read.dta(paste0(directory, survey_recode, "DT\\", survey_recode, "FL.DTA"), convert.factors = FALSE)
  try({fw.df <- read.dta(paste0(directory, fw, "DT\\", fw, "FL.DTA"), convert.factors = FALSE)})
  
  indicList <- c()
  valueName <- c()
  
  for (indic in 1:16) {
    
    # indicator
    question <- paste0("mm", indic)
    
    for (i in 1:20) {
      # iter_char
      if (i <10) {iter_char <- paste0("0", i)} else {iter_char <- as.character(i)}
      # valuename
      valueName[i] <- paste0(question, "_", iter_char)
    }
    
    indicList <- c(indicList, valueName)
    
  }
  
  # mmidx
  for (i in 1:20) {
    # iter_char
    if (i <10) {iter_char <- paste0("0", i)} else {iter_char <- as.character(i)}
    # valuename
    valueName[i] <- paste0("mmidx", "_", iter_char)
  }
  
  mm_indics <- c(valueName, indicList)
  
  
  
  # Merge the data
  #   Female Questionnaire merge
  if(!male) {
    # list the variables to keep
    keepvars <- c("v000", # type of place of residence
                  "v001", # cluster number 
                  "v002", # household number
                  "v003", # respondent's line number
                  "v004", # ultimate area unit
                  "v005", # women's individual sample weight
                  "v006", # Month of interview
                  "v007", # year of interview
                  "v008", # date of interview (cmc)
                  "v009", # respondent's month of birth
                  "v010", # respondent's year of birth
                  "v011", # date of birth (cmc)
                  "v012", # age
                  "v013", # age in 5-year groups
                  "v014", # completeness of age information
                  "v040", # cluster altitude in meters
                  "v102", # de facto type of place of residence
                  "v104", # years lived in place of residence
                  "v105", # type of place of previous residence
                  "v105a", # region of previous residence
                  "v106", # highest education level
                  "v107", # highest year of education
                  

                  
                  "v525", # age at first sex
                  "v527", # Time since last sexual intercourse
                  "v528", # time since last sexual intercourse in days up to 31
                  "v529", # computed time since last intercourse. This is computed from v527, with duration exceeding the interval since the last birth recoded as "before last birth".
                  "v531", # age at first sex imputed
                  "v532", # V532 Flag variable for inconsistencies found in editing the responses for V525.
                  "v535", # whether the respondent has ever been married or living with a man
                  "v536", # Recent sexual activity gives the sexual activity of the respondents during the last four weeks coded as follows 0: never had intercourse
                  "v537", # Months of abstinence. Postpartum or not postpartum abstinence. among women sexually active
                  "v501", # union status

                  "v028", # interviewer classification
                  "v030", # field supervisor
                  "v045a", # language of questionnaire
                  "v045b", # interview language 
                  "v045c", # native language of the respondent
                  "v046", # translator used

                  
                  
                  "v155", # literacy
                  "v190", # wealth index combined
                  "v191", # wealth index fator score combined
                  "v190a", # wealth index for urban/rural
                  "v191a", # wealth index factor score urban/rural
                  "v208", #!!!
                  "v301", # knowledge of any method
                  "v302", # ever use any method
                  "v302a", # ever used anything or tried to delay pregnancy
                  # MFP methods / messages
                  "v305_01",
                  "v305_02",
                  "v305_03",
                  "v305_04",
                  "v305_05",
                  "v305_06",
                  "v305_07",
                  "v305_08",
                  "v305_09",
                  "v305_10",
                  "v305_11",
                  "v305_13",
                  "v305_14",
                  "v305_16",
                  "v305_17",
                  "v305_18",
                  # MFP currently used
                  
                  "v312",
                  "v313",
                  "v318", # completeness of (FP) information
                  "v815a", # presence of children <10 during sexual activity section
                  "v815b", # presence of male adults during sexual activity section
                  "v815c", # presence of female adults during sexual activity section
                  "d122a", # interview interrupted: husband's presence
                  "d122b", # interview interrupted: other male's presence
                  "d122c", # interview interrupted: adult female's presence
                  
                  "v021", # cluster ID
                  "v801", # time interview started (hhmm - 24 hour clock)
                  "v802", # time interview ended (hhmm - 24 hour clock)
                  "v803", # length of interview in minutes
                  "v008", # date of interview (cmc)
                  "v008a", # date of interview century day code (cdc)
                  "b3_01",
                  
                  "b5_01", # Child alive
                  "b5_02",
                  "b5_03",
                  "b5_04",
                  "b5_05",
                  "b5_06",
                  "b5_07",
                  "b5_08",
                  "b5_09",
                  "b5_10",
                  "b5_11",
                  "b5_12",
                  "b5_13",
                  "b5_14",
                  "b5_15",
                  "b5_16",
                  "b5_17",
                  "b5_18",
                  "b5_19",
                  "b5_20",
                  mm_indics,
                  # DHS cleaning section
                  "b19_01",
                  # ??? 
                  "m1_1",
                  # Type of provider
                  "m2a_1",
                  "m2b_1",
                  "m2c_1",
                  "m2d_1",
                  "m2e_1",
                  "m2f_1",
                  "m2g_1",
                  "m2h_1",
                  "m2i_1",
                  "m2j_1",
                  "m2k_1",
                  "m2l_1",
                  "m2m_1",
                  "m2b_1",
                  "m2c_1",
                  # something about months pregnant when first visit
                  "m13_1",
                  # ANC Visits
                  "m14_1",
                  # something about supplements
                  "m45_1",
                  "m60_1",
                  "m42c_1",
                  "m42d_1",
                  "m42e_1",
                  "m43_1",
                  "v006", # Month of HH interview
                  "v007", # Year of interview
                  "v016", # day of interview
                  
                  # postnatal care
                  "m50_1",
                  "m51_1",
                  "m52_1",
                  "bidx_01",
                  "m62_1",
                  "m63_1",
                  "m64_1",
                  
                  "m66_1",
                  "m67_1",
                  "m68_1",
                  "m70_1",
                  "m71_1",
                  "m72_1",
                  "m74_1",
                  "m75_1",
                  "m76_1",
                  "mmc1", # number of occurances in the maternal mortality section
                  "mmc2", # Number of births preceding respondents birth
                  "mmc3", # no longer part of the questionnaire, but used to be the actual respondent of the mm section, if not the respondent
                  "mmc4", # not in the DHS anymore
                  "mmc5" # cutoff age for this section. In most countries, age of 12 is the cutoff, but can be 10, 13 or 15
                  
    )
    # # get subset
    # survey.df <- survey.df[names(survey.df) %in% keepvars]
    
    # special instructions for Mozambique !!!
    if (exists("fw.df") &&  fw.df$fw000[1] == "MZ7") {
      survey.df$v028 <- survey.df$v028 + 1000
    }
    
    if (exists("fw.df")) {
      survey_merge.df <- merge(survey.df, fw.df, by.x = "v028", by.y = "fw101", all.x = TRUE) 
    } else {survey_merge.df <- survey.df}
    
  }
  
  if(male) {
    # list the variables to keep
    keepvars <- c("mv525", # age at first sex
                  "mv527", # Time since last sexual relations 1, days, 2 weeks, 3 months, 4 years
                  "mv528", # time since last sexual intercourse in days up to 31
                  "mv529", # computed time since last intercourse. This is computed from v527, with durations exceeding the interval since the last birth recoded as "before last birth".
                  "mv531", # age at first sex imputed
                  "mv532", # V532 Flag variable for inconsistencies found in editing the responses for V525.
                  "mv535", # whether the respondent has ever been married or living with a man
                  "mv536", # Recent sexual activity gives the sexual activity of the respondents during the last four weeks coded as follows 0: never had intercourse
                  "mv501", # union status
                  "mv012", # age
                  "mv028", # interviewer classification
                  "mv030", # field supervisor
                  "mv045a", # language of questionnaire
                  "mv045b", # native language of respondent
                  "mv045c", # translator used
                  "mv046", # translator used
                  "mv101", # region
                  "mv102", # type of place of residence
                  "mv106", # highest education level
                  "mv107", # highest year of education
                  "mv155", # literacy
                  "mv190", # wealth index combined
                  "mv191", # wealth index factor score combined
                  "mv190a", # wealth index for urban/rural
                  "mv191a", # wealth index factor score urban/rural
                  "mv301", # knowledge of any method
                  "mv000",
                  "mv801", # time interview started (hhmm - 24 hour clock)
                  "mv802", # time interview ended (hhmm - 24 hour clock)
                  "mv803", # length of interview in minutes
                  "mv008", # date of interview (cmc)
                  "mv008a", # date of interview century day code (cdc)
                  "mv021" # cluster ID
    )
    # get subset
    survey.df <- survey.df[names(survey.df) %in% keepvars]
    
    # special instructions for Mozambique !!!
    if (exists("fw.df") &&  fw.df$fw000[1] == "MZ7") {
      survey.df$mv028 <- survey.df$mv028 + 1000
    }
    
    if (exists("fw.df")) {
      survey_merge.df <- merge(survey.df, fw.df, by.x = "mv028", by.y = "fw101", all.x = TRUE)
    } else {survey_merge.df <- survey.df}
  }
  
  # throw warning if it looks like something happened to the merge
  if(nrow(survey.df) != nrow(survey_merge.df)) {
    warning(paste0(survey_merge.df$fw000[1], ": WARNING! There are ", nrow(survey.df), " in the original and ", nrow(survey_merge.df), " in the merged survey dataset!"))
  }
  
  # name the round for ease - it's the same between male and female surveys
  survey_merge.df$svy_round <- survey_merge.df$fw000[1]
  
  rm(survey.df)
  
  if (exists("fw.df")) {rm(fw.df)}
  
  # append to the appropriate lists
  return(survey_merge.df)
}