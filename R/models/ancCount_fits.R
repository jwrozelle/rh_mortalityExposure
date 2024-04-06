

# IRdata <- df_list$MW7


ancCount_everu5 <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r)) / sd(IRdata$age_r)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      prec_u5Death + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
  
}

ancCount_countu5 <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      u5_death_count + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

ancCount_everInf <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      prec_infDeath + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural + 
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

ancCount_countInf <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      inf_death_count + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

ancCount_everNeo <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      prec_neoDeath + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

ancCount_countNeo <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      neo_death_count + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}

ancCount_everB4Dur <- function(IRdata, restrictBord = NULL) {
  
  # filter to second or later birth order
  IRdata <- filter(IRdata, bord >= 2)
  
  if (!is.null(restrictBord)) {
    IRdata <- filter(IRdata, bord %in% restrictBord)
  }
  
  # recode 98 to missing ANC visits
  IRdata$ancVisits <- NA
  IRdata$ancVisits <- ifelse(IRdata$m14_1 == 98, NA, IRdata$m14)
  
  # calculate necessary variables
  #   date of interview
  IRdata$age_r_atCBirth <- IRdata$age_r - (IRdata$age/12)
  
  IRdata$age_r_atCBirth_normalized <- (IRdata$age_r - mean(IRdata$age_r, na.rm=T)) / sd(IRdata$age_r, na.rm=T)
  
  IRdata$sd_del_b4Dur_ever <- ifelse(IRdata$sd_del_b4Dur_ever %in% 1, 1, 0)
  
  # fit the model
  modelFit <- glm(
    ancVisits ~ 
      sd_del_b4Dur_ever + 
      wealthIndex +
      age_r_atCBirth_normalized +
      rural +
      v133 + # Education in single years
      # edLevel_r +
      everUnion, #+
    # (1|v001), 
    family=poisson(link = log), 
    data=IRdata, 
    # control = glmerControl(optimizer="bobyqa", 
    #                        optCtrl = list(maxfun=2e6))
  )
  
  return(modelFit)
}











