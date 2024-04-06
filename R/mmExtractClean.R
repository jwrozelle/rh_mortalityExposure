
# IRdata <- df_list$MW7

# BRdata <- dfBR_list$MW7

# IRdata <- df_list$ET7

mmExtractClean <- function(IRdata) {
  
  require(lubridate)
  
  valueName <- c()
  indicList <- c()
  question <- c()
  
  # generate list of mm variables
  for (indic in 1:16) {
    
    # indicator
    question[indic] <- paste0("mm", indic)
    
    for (i in 1:20) {
      # iter_char
      if (i <10) {iter_char <- paste0("0", i)} else {iter_char <- as.character(i)}
      # valuename
      valueName[i] <- paste0(question[indic], "_", iter_char)
    }
    
    indicList <- c(indicList, valueName)
    
  }
  
  # mmidx
  for (i in 1:20) {
    # iter_char
    if (i <10) {iter_char <- paste0("0", i)} else {iter_char <- as.character(i)}
    # valuename
    valueName[i] <- paste0("mmidx", "_", iter_char)
  }
  
  mm_indics <- c(valueName, indicList)
  
  rm(indicList, valueName)
  
  # the variables we want for all responents
  respondentLevelVars <- c("femaleID", "v000", "v001", "v002", "v003", "b19_01", "interview_date")
  
  mm_indics <- c(mm_indics, respondentLevelVars)
  
  # subset the mm indicators from the IRdata
  mmDataWide.df <- IRdata[names(IRdata) %in% mm_indics]
  
  # prepare question names without indices
  noIndexNames <- c("mmidx", 
                    # pull out mmidx and respondent level variables to get the length of noIndexnames
                    question[1:(floor((length(names(mmDataWide.df))-length(respondentLevelVars)-1)/20))], 
                    respondentLevelVars)
  
  
  # make a vector of individual level sibling variables
  siblingVar.list <- list()
  sibling.list <- list()
  
  for (sibling in 1:20) {
    # sibling <- 1
    
    # get a vector of variables we want to keep for each sibling
    siblingVars <- c(mm_indics[
      seq(from= sibling, 
          to = length(names(mmDataWide.df))-length(respondentLevelVars), 
          by = 20)], 
      respondentLevelVars)
    
    sibling.list[[sibling]] <- mmDataWide.df[,siblingVars]
    
    # rename without the index number
    names(sibling.list[[sibling]]) <- noIndexNames
  }
  
  sibling.df <- iotools::fdrbind(sibling.list)
  
  rm(sibling.list)
  
  # Final listing of all siblings!
  sibling.df <- subset(sibling.df, !is.na(mmidx))
  
  # subset to just sisters !!!
  sibling.df <- subset(sibling.df, mm1 == 2)
  
  
  ## Process sibling.df
  #   Respondent date of interview
  sibling.df$int_dateR <- NA
  sibling.df$int_dateR <- as.Date("1970-01-01") + sibling.df$interview_date
  
  # sibling's data of birth
  sibling.df$sib_dobR <- NA
  sibling.df$sib_dobR <- as.Date("1899-12-15") %m+% months(sibling.df$mm4)
  
  # sibling's date of death
  sibling.df$sib_dodR <- NA
  sibling.df$sib_dodR <- as.Date("1899-12-15") %m+% months(sibling.df$mm8)
  
  # living sibling's age
  sibling.df$sibAge_current <- NA
  sibling.df$sibAge_current <- sibling.df$mm3
  sibling.df$sibAge_current <- ifelse(sibling.df$mm3 == 98, NA, sibling.df$sibAge_current)
  
  # sibling age at death
  sibling.df$sibAge_at_death <- NA
  sibling.df$sibAge_at_death <- sibling.df$mm7
  sibling.df$sibAge_at_death <- ifelse(sibling.df$mm7 == 98, NA, sibling.df$sibAge_at_death)
  
  # sibling age constant !!!
  sibling.df$ageUse <- NA
  sibling.df$ageUse <- ifelse(sibling.df$mm2 == 1, sibling.df$sibAge_current, sibling.df$ageUse)
  sibling.df$ageUse <- ifelse(sibling.df$mm2 == 0, sibling.df$sibAge_at_death, sibling.df$ageUse)
  sibling.df$ageUse <- ifelse(is.na(sibling.df$ageUse) & 
                                sibling.df$mm2 ==0 & # dead
                                !is.na(sibling.df$sib_dobR) & # missing date of birth
                                !is.na(sibling.df$sib_dodR), # missing date of death
                              floor(time_length(difftime(sibling.df$sib_dodR, sibling.df$sib_dobR), "years")),
                              sibling.df$ageUse
                              )
  sibling.df$ageUse <- ifelse(is.na(sibling.df$ageUse) & 
                                sibling.df$mm2 ==1 & # alive
                                !is.na(sibling.df$sib_dobR) & # missing data of birth
                                !is.na(sibling.df$int_dateR), # missing interview date
                              floor(time_length(difftime(sibling.df$int_dateR, sibling.df$sib_dobR), "years")),
                              sibling.df$ageUse
  )
  
  # sister at risk of maternal death
  sibling.df$sisMatRisk <- NA
  sibling.df$sisMatRisk <- ifelse(sibling.df$ageUse > 15, 1, 0)
  
  # sister at risk of maternal death
  sibling.df$sisMatRisk_known <- NA
  sibling.df$sisMatRisk_known <- ifelse(sibling.df$ageUse > 15, 1, 0)
  sibling.df$sisMatRisk_known <- ifelse(
    sibling.df$mm2 == 0 & sibling.df$mm7 == 98, # unknown age at death for dead sibling
    NA, # Set to NA
    sibling.df$sisMatRisk_known) 
  sibling.df$sisMatRisk_known <- ifelse(
    sibling.df$mm2 == 1 & sibling.df$mm3 == 98, # unknown current age for living sibling
    NA, # set to NA
    sibling.df$sisMatRisk_known) 
  
  
  # sister
  sibling.df$sister <- NA
  sibling.df$sister <- ifelse(sibling.df$mm1 == 2, 1, 0)
  
  # sister death
  sibling.df$sisDied <- NA
  sibling.df$sisDied <- ifelse(sibling.df$mm1 == 2 & sibling.df$mm2 == 0, 1, 0)
  sibling.df$sisDied <- ifelse(sibling.df$mm1 == 1, NA, sibling.df$sisDied)
  
  # sibling died while pregnant
  sibling.df$sisDied_whilePreg <- NA
  sibling.df$sisDied_whilePreg <- ifelse(sibling.df$mm9== 2, 1, 0)
  
  # sibling died during delivery
  sibling.df$sisDied_Del_during <- NA
  sibling.df$sisDied_Del_during <- ifelse(sibling.df$mm9== 3, 1, 0)
  
  # sibling died "since delivery"
  sibling.df$sisDied_Del_since <- NA
  sibling.df$sisDied_Del_since <- ifelse(sibling.df$mm9== 4, 1, 0)
  
  # sibling died 6 weeks after delivery
  sibling.df$sisDied_Del_6wks <- NA
  sibling.df$sisDied_Del_6wks <- ifelse(sibling.df$mm9== 5, 1, 0)
  
  # sibling died 2 months after delivery
  sibling.df$sisDied_Del_2mnth <- NA
  sibling.df$sisDied_Del_2mnth <- ifelse(sibling.df$mm9== 6, 1, 0)
  
  # sibling who died while pregnant or during delivery
  sibling.df$sisDied_Del_b4Dur <- NA
  sibling.df$sisDied_Del_b4Dur <- ifelse(sibling.df$sisDied_Del_during == 1 | sibling.df$sisDied_whilePreg == 1, 1, 0)
  
  # sibling died after delivery
  sibling.df$sisDied_Del_in2mnths <- NA
  sibling.df$sisDied_Del_in2mnths <- ifelse(sibling.df$sisDied_Del_2mnth == 1 | sibling.df$sisDied_Del_6wks == 1 | sibling.df$sisDied_Del_since == 1, 1, 0)
  
  # sibling died during pregnancy or within two months after
  sibling.df$sisDied_nearBirth <- NA
  sibling.df$sisDied_nearBirth <- ifelse(sibling.df$sisDied_Del_in2mnths == 1 | sibling.df$sisDied_Del_b4Dur == 1, 1, 0)
  
  return(sibling.df)
  
}



# sibData <- mmExtractClean(IRdata)


# group this for merge to IR dataset
sibatIRlvl <- function(sibData) {
  
  IRsibData <- sibData %>% 
    group_by(femaleID) %>%
    summarise(
      # has a sister
      sister_count = sum(sister, na.rm=T),
      sisMatRisk_inclusive = ifelse(sum(sisMatRisk, na.rm = T)>0, 1, 0),
      sisMatRisk_known = ifelse(sum(sisMatRisk_known, na.rm = T)>0, 1, 0),
      # sister died
      sd_count=sum(sisDied, na.rm = T),
      sd_ever = ifelse(sum(sisDied, na.rm = T)>0, 1, 0),
      # While pregnant
      sd_whilePreg_count = sum(sisDied_whilePreg, na.rm = T),
      sd_whilePreg_ever = ifelse(sum(sisDied_whilePreg, na.rm = T)>0, 1, 0),
      sd_whilePreg_ever_2yrs = ifelse(sum(sisDied_whilePreg, na.rm = T)>0 & sum(mm6 <=2)>0, 1, 0),
      # during delivery
      sd_del_during_count = sum(sisDied_Del_during, na.rm = T),
      sd_del_during_ever = ifelse(sum(sisDied_Del_during, na.rm = T)>0, 1,0),
      sd_del_during_ever_2yrs = ifelse(sum(sisDied_Del_during, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # 6 weeks after delivery
      sd_del_6wks_count = sum(sisDied_Del_6wks, na.rm = T),
      sd_del_6wks_ever = ifelse(sum(sisDied_Del_6wks, na.rm = T)>0,1,0),
      sd_del_6wks_ever_2yrs = ifelse(sum(sisDied_Del_6wks, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # 2 months after delivery
      sd_del_2mnth_count = sum(sisDied_Del_2mnth, na.rm = T),
      sd_del_2mnth_ever = ifelse(sum(sisDied_Del_2mnth, na.rm = T)>0,1,0),
      sd_del_2mnth_ever_2yrs = ifelse(sum(sisDied_Del_2mnth, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # before or during delivery
      sd_del_b4Dur_count = sum(sisDied_Del_b4Dur, na.rm = T),
      sd_del_b4Dur_ever = ifelse(sum(sisDied_Del_b4Dur, na.rm = T)>0,1,0),
      sd_del_b4Dur_ever_2yrs = ifelse(sum(sisDied_Del_b4Dur, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # sibling died after delivery
      sd_del_in2mnths_count = sum(sisDied_Del_in2mnths, na.rm = T),
      sd_del_in2mnths_ever = ifelse(sum(sisDied_Del_in2mnths, na.rm = T)>0,1,0),
      sd_del_in2mnths_ever_2yrs = ifelse(sum(sisDied_Del_in2mnths, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0),
      # sibling died during pregnancy or within two months after
      sd_nearBirth_count = sum(sisDied_nearBirth, na.rm = T),
      sd_nearBirth_ever = ifelse(sum(sisDied_nearBirth, na.rm = T)>0,1,0),
      sd_nearBirth_ever_2yrs = ifelse(sum(sisDied_nearBirth, na.rm = T)>0 & sum(mm6 <=2)>0, 1,0)
    )
  
  # add country level info
  IRsibData$v000 <- sibData$v000[1]
  
  return(IRsibData)
  
}

# sibatIRlvl.df <- sibatIRlvl(sibData)



addSib2BR <- function(BRdata, sibData, by = "femaleID") {
  
  # the long duplicate merge
  mergedLong.df <- merge(BRdata, sibData, by = by, all.x = T, all.y = F)
  
  # sibling died at all
  mergedLong.df$prev_sisDied <- NA
  mergedLong.df$prev_sisDied <- ifelse(mergedLong.df$sisDied == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # sibling died, unrelated to pregnancy
  mergedLong.df$prev_sisDied_nonMat <- NA
  mergedLong.df$prev_sisDied_nonMat <- ifelse(mergedLong.df$sisDied == 1 &  # this will code 
                                         mergedLong.df$sisDied_Del_2mnth != 1 &
                                         mergedLong.df$sisDied_Del_6wks != 1 & 
                                         mergedLong.df$sisDied_Del_b4Dur != 1 &
                                         !is.na(mergedLong.df$sib_dodR) &
                                         mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                       1, 0)
  
  # sibling died while pregnant
  mergedLong.df$prev_sisDied_whilePreg <- NA
  mergedLong.df$prev_sisDied_whilePreg <- ifelse(mergedLong.df$sisDied_whilePreg == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # sibling died while pregnant
  mergedLong.df$prev_sisDied_whilePreg <- NA
  mergedLong.df$prev_sisDied_whilePreg <- ifelse(mergedLong.df$sisDied_whilePreg == 1 &  # this will code 
                                              !is.na(mergedLong.df$sib_dodR) &
                                              mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                            1, 0)
  
  # sibling died during delivery
  mergedLong.df$prev_sisDied_Del_during <- NA
  mergedLong.df$prev_sisDied_Del_during <- ifelse(mergedLong.df$sisDied_Del_during == 1 &  # this will code 
                                                    !is.na(mergedLong.df$sib_dodR) &
                                                    mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                  1, 0)
  
  # sibling died "since delivery"
  mergedLong.df$prev_sisDied_Del_since <- NA
  mergedLong.df$prev_sisDied_Del_since <- ifelse(mergedLong.df$sisDied_Del_since == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # sibling died 6 weeks after delivery
  mergedLong.df$prev_sisDied_Del_6wks <- NA
  mergedLong.df$prev_sisDied_Del_6wks <- ifelse(mergedLong.df$sisDied_Del_6wks == 1 &  # this will code 
                                                  !is.na(mergedLong.df$sib_dodR) &
                                                  mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                1, 0)
  
  # sibling died 2 months after delivery
  mergedLong.df$prev_sisDied_Del_2mnth <- NA
  mergedLong.df$prev_sisDied_Del_2mnth <- ifelse(mergedLong.df$prev_sisDied_Del_2mnth == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # before or during delivery
  mergedLong.df$prev_sisDied_Del_b4Dur <- NA
  mergedLong.df$prev_sisDied_Del_b4Dur <- ifelse(mergedLong.df$sisDied_Del_b4Dur == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  # before or during delivery 6 months prior to birthday!!!
  mergedLong.df$prev6m_sisDied_Del_b4Dur <- NA
  mergedLong.df$prev6m_sisDied_Del_b4Dur <- ifelse(mergedLong.df$sisDied_Del_b4Dur == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$c_dobR - mergedLong.df$sib_dodR >= (30.437*6), 
                                                 1, 0)
  
  # sibling died after delivery
  mergedLong.df$prev_sisDied_Del_in2mnths <- NA
  mergedLong.df$prev_sisDied_Del_in2mnths <- ifelse(mergedLong.df$sisDied_Del_in2mnths == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  # sibling died during pregnancy or within two months after
  mergedLong.df$prev_sisDied_nearBirth <- NA
  mergedLong.df$prev_sisDied_nearBirth <- ifelse(mergedLong.df$sisDied_nearBirth == 1 &  # this will code 
                                                   !is.na(mergedLong.df$sib_dodR) &
                                                   mergedLong.df$sib_dodR < mergedLong.df$c_dobR, 
                                                 1, 0)
  
  
  # group back to child level
  BRsibData.df <- mergedLong.df %>%
    dplyr::group_by(childID) %>%
    dplyr::summarise(
      
      motherHasSister = ifelse(sum(!is.na(mm1), na.rm = T) > 0, 1, 0),
      
      # While pregnant
      prev_sd_count = sum(prev_sisDied, na.rm = T),
      prev_sd_ever = ifelse(sum(prev_sisDied, na.rm = T)>0, 1, 0),
      
      # nonMaternal death
      prev_sd_nonMat_count = sum(prev_sisDied_nonMat, na.rm = T),
      prev_sd_nonMat_ever = ifelse(sum(prev_sisDied_nonMat, na.rm = T)>0,1,0),
      
      # While pregnant
      prev_sd_whilePreg_count = sum(prev_sisDied_whilePreg, na.rm = T),
      prev_sd_whilePreg_ever = ifelse(sum(prev_sisDied_whilePreg, na.rm = T)>0, 1, 0),
      # during delivery
      prev_sd_del_during_count = sum(prev_sisDied_Del_during, na.rm = T),
      prev_sd_del_during_ever = ifelse(sum(prev_sisDied_Del_during, na.rm = T)>0, 1,0),
      # 6 weeks after delivery
      prev_sd_del_6wks_count = sum(prev_sisDied_Del_6wks, na.rm = T),
      prev_sd_del_6wks_ever = ifelse(sum(prev_sisDied_Del_6wks, na.rm = T)>0,1,0),
      # 2 months after delivery
      prev_sd_del_2mnth_count = sum(prev_sisDied_Del_2mnth, na.rm = T),
      prev_sd_del_2mnth_ever = ifelse(sum(prev_sisDied_Del_2mnth, na.rm = T)>0,1,0),
      # before or during delivery
      prev_sd_del_b4Dur_count = sum(prev_sisDied_Del_b4Dur, na.rm = T),
      prev_sd_del_b4Dur_ever = ifelse(sum(prev_sisDied_Del_b4Dur, na.rm = T)>0,1,0),
      # before or during delivery
      prev6m_sd_del_b4Dur_count = sum(prev6m_sisDied_Del_b4Dur, na.rm = T),
      prev6m_sd_del_b4Dur_any = ifelse(sum(prev6m_sisDied_Del_b4Dur, na.rm = T)>0,1,0),
      # sibling died after delivery
      prev_sd_del_in2mnths_count = sum(prev_sisDied_Del_in2mnths, na.rm = T),
      prev_sd_del_in2mnths_ever = ifelse(sum(prev_sisDied_Del_in2mnths, na.rm = T)>0,1,0),
      # sibling died during pregnancy or within two months after
      prev_sd_nearBirth_count = sum(prev_sisDied_nearBirth, na.rm = T),
      prev_sd_nearBirth_ever = ifelse(sum(prev_sisDied_nearBirth, na.rm = T)>0,1,0)
    )
  
  
  BRsibData.df <- merge(BRdata, BRsibData.df, by = "childID", all.x = T, all.y = F)
  
  
  
  return(BRsibData.df)
  
}





