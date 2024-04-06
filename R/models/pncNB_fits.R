

# IRdata <- df_list$MW7


pncNB_everu5 <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm=T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      prec_u5Death + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}

pncNB_countu5 <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm = T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      u5_death_count + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_everInf <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm = T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      prec_infDeath + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_countInf <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm = T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      inf_death_count + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_everNeo <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm = T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      prec_neoDeath + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_countNeo <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm = T)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      neo_death_count + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

pncNB_everB4Dur <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  # IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r_atCBirth - mean(IRdata$age_r_atCBirth, na.rm = T)) / sd(IRdata$age_r_atCBirth, na.rm = T)
  
  IRdata$sd_del_b4Dur_ever <- ifelse(IRdata$sd_del_b4Dur_ever %in% 1, 1, 0)
  
  # fit the model
  modelFit <- lme4::glmer(
    rh_pnc_nb_2days ~ 
      sd_del_b4Dur_ever + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion + bord + 
      (1|v001), 
    family=binomial, 
    data=IRdata, 
    control = glmerControl(optimizer="bobyqa", 
                           optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}













