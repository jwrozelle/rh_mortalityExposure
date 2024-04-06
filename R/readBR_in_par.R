

# Read in data, merge, and append to list function ####
readBR_in_par <- function(directory = NULL, survey_recode = NULL) {
  
  # Error messages
  #   No directory
  if(is.null(directory)) {
    stop("You must specify a directory")
  }
  #   No individual recode
  if(is.null(survey_recode)) {
    stop("You must specify an survey value")
  }
  
  # # No fieldworker code
  # if(is.null(hr)) {
  #   stop("You must specify a Household Record character value")
  # }
  
  # Make sure it's upper case
  survey_recode <- str_to_upper(survey_recode)
  
  # Read in the data
  kr.df <- read.dta(paste0(directory, survey_recode, "DT\\", survey_recode, "FL.DTA"), convert.factors = FALSE)
  
  return(kr.df)
}





