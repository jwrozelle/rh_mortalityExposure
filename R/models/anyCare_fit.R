KRdata.bkp <- KRdata

KRdata$onlyU5_any <- NA
KRdata$onlyU5_any <- ifelse(KRdata$u5_death_any == 1 & KRdata$inf_death_any == 0, TRUE, FALSE)


anyCare_everu5 <- function(KRdata) {
  
  # filter by outcome completeness
  KRdata <- filter(KRdata, !is.na(ch_fev_care))
  
  KRdata$onlyU5_any <- NA
  KRdata$onlyU5_any <- ifelse(KRdata$u5_death_any == 1 & KRdata$inf_death_any == 0, TRUE, FALSE)
  
  # calculate necessary variables
  KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    any_care ~ onlyU5_any + wealthIndex + age_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}


anyCare_countu5 <- function(KRdata) {
  
  # filter by outcome completeness
  KRdata <- filter(KRdata, !is.na(ch_fev_care))
  
  # calculate necessary variables
  KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    any_care ~ u5_death_count + wealthIndex + age_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}


# anyCareSimple_fit <- function(KRdata) {
#   
#   # filter by outcome completeness
#   KRdata <- filter(KRdata, !is.na(ch_fev_care))
#   
#   # calculate necessary variables
#   KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
#   
#   # fit the model
#   modelFit <- lme4::glmer(
#     any_care ~ u5_death_any + wealthIndex + age_normalized + (1|v001), 
#     family=binomial, 
#     data=KRdata, 
#     control = glmerControl(optimizer="bobyqa", 
#                            optCtrl = list(maxfun=2e6))
#   )
#   
#   return(modelFit)
#   
# }





























