

# IRdata <- df_list$MW7


anc4_int_everu5 <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r)) / sd(IRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      prec_u5Death * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}

anc4_int_countu5 <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      u5_death_count * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

anc4_int_everInf <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      prec_infDeath * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural + 
      v133 + # Education in single years
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

anc4_int_countInf <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      inf_death_count * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

anc4_int_everNeo <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      prec_neoDeath * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

anc4_int_countNeo <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      neo_death_count * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

anc4_int_everB4Dur <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  IRdata$sd_del_b4Dur_ever <- ifelse(IRdata$sd_del_b4Dur_ever %in% 1, 1, 0)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_anc_4vs ~ 
      sd_del_b4Dur_ever * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}











