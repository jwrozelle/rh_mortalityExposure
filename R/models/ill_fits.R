



# KRdata <- df_list$MW7


ill_everu5 <- function(KRdata) {
  
  # calculate necessary variables
  #   date of interview
  KRdata$age_r_atCBirth <- KRdata$age_r - (KRdata$age/12)
  
  KRdata$age_r_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    any_care ~ u5_death_any + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}

ill_countu5 <- function(KRdata) {
  # calculate necessary variables
  KRdata$age_r_normalized <- (KRdata$age_r - mean(KRdata$age_r, na.rm=T)) / sd(KRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    any_care ~ u5_death_count + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}













