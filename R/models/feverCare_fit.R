




feverCare_fit <- function(KRdata) {
  
  # filter by outcome completeness
  KRdata <- filter(KRdata, !is.na(ch_fev_care))
  
  # calculate necessary variables
  KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    ch_fev_care ~ u5_death_any + wealthIndex + age_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}


feverCareSimple_fit <- function(KRdata) {
  
  # filter by outcome completeness
  KRdata <- filter(KRdata, !is.na(ch_fev_care))
  
  # calculate necessary variables
  KRdata$age_normalized <- (KRdata$age_r - mean(KRdata$age_r)) / sd(KRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    ch_fev_care ~ u5_death_any + wealthIndex + age_normalized + (1|v001), 
    family=binomial, 
    data=KRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}


pncNB_everu5 <- function(IRdata) {
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r)) / sd(IRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ u5_death_any + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}

pncNB_countu5 <- function(IRdata) {
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ u5_death_count + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_everInf <- function(IRdata) {
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ inf_death_any + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_countInf <- function(IRdata) {
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ inf_death_count + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_everNeo <- function(IRdata) {
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ neo_death_any + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_countNeo <- function(IRdata) {
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ neo_death_count + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_everB4Dur <- function(IRdata) {
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # set missing to zero
  IRdata$sd_del_b4Dur_ever <- ifelse(IRdata$sd_del_b4Dur_ever %in% 1, 1, 0)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ sd_del_b4Dur_ever + wealthIndex + age_r_atCBirth_normalized + rural + edLevel_r + everUnion + (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}



