

feverDay_fit <- function(KRdata) {
  
  # filter by outcome completeness
  KRdata <- filter(KRdata, !is.na(ch_fev_care_day))
  
  # calculate necessary variables
  KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    ch_fev_care_day ~ u5_death_any + wealthIndex + age_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}


feverDaySimple_fit <- function(KRdata) {
  
  # filter by outcome completeness
  KRdata <- filter(KRdata, !is.na(ch_fev_care_day))
  
  # calculate necessary variables
  KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    ch_fev_care_day ~ u5_death_any + wealthIndex + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}



