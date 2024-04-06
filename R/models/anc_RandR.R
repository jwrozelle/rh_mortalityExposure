anc_RandR <- function(inputData = inputData, outcomeVar = NULL, deathExposure = "prec_neoDeath", bordRestricted = F) {
  
  
  # Remove the bord variable if bordRestricted = T
  if (bordRestricted) {
    model.formula <- paste0(
      outcomeVar, " ~ ",
      deathExposure, " + ",
      "age_r_atCBirth_normalized + 
      rural +
      married + 
      wealthIndex + 
      v133 + 
      (1|v001)"
    )
  } else {
    model.formula <- paste0(
      outcomeVar, " ~ ",
      deathExposure, " + ",
      "bord +
      age_r_atCBirth_normalized + 
      rural +
      married + 
      wealthIndex + 
      v133 + 
      (1|v001)"
    )
  }
  
  
  
  model <- lme4::glmer(
    model.formula,
    family=binomial, 
    data=inputData, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(model)
}