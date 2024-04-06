normalizeAgeAtBirth <- function(inputData) {
  inputData$age_r_atCBirth <- inputData$age_r - (inputData$age/12)
  inputData$age_r_atCBirth_normalized <- (inputData$age_r - mean(inputData$age_r)) / sd(inputData$age_r)
  
  return(inputData)
}