

# BRdata <- dfBR_list$MW7


fbd_int_everu5 <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r)) / sd(BRdata$age_r)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      prec_u5Death * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}

fbd_int_countu5 <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r, na.rm=T)) / sd(BRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      u5_death_count * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

fbd_int_everInf <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r, na.rm=T)) / sd(BRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      prec_infDeath * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

fbd_int_countInf <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r, na.rm=T)) / sd(BRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      inf_death_count * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

fbd_int_everNeo <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r, na.rm=T)) / sd(BRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      prec_neoDeath * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

fbd_int_countNeo <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r, na.rm=T)) / sd(BRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      neo_death_count * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

fbd_int_everB4Dur <- function(BRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # BRdata <- filter(BRdata, bord >= 2)
  
  # restrict to specific bord number
  if (!is.null(restrictBord)) {
    BRdata <- filter(BRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  BRdata$age_r_atCBirth <- BRdata$age_r - (BRdata$age/12)
  
  BRdata$age_r_atCBirth_normalized <- (BRdata$age_r - mean(BRdata$age_r, na.rm=T)) / sd(BRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_del_fbd ~ 
      sd_del_b4Dur_ever * v133 + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      # edLevel_r +
      everUnion +
      (1|v001), 
    family=binomial, 
    data=BRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}











