
# BRdata <- dfBR_list$MW7
# IRdata <- df_list$MW7



BRatIRlevel <- function(BRdata) {
  
  BR_grouped <- BRdata %>% 
    dplyr::group_by(femaleID) %>%
    dplyr::summarise(
      neo_death_count = sum(neonatal_death, na.rm = T),
      neo_death_any = ifelse(sum(neonatal_death, na.rm = T) %in% 1:30, 1, 0),
      inf_death_count = sum(infant_death, na.rm = T),
      inf_death_any = ifelse(sum(infant_death, na.rm = T) %in% 1:30, 1, 0),
      u5_death_count = sum(U5_death, na.rm = T),
      u5_death_any = ifelse(sum(U5_death, na.rm = T) %in% 1:30, 1, 0),
      death_28dTo11m_count = sum(death_28dTo11m, na.rm = T), 
      death_28dTo11m_any = ifelse(sum(death_28dTo11m, na.rm = T) %in% 1:30, 1, 0),
      death_12mTo59m_count = sum(death_12mTo59m, na.rm = T), 
      death_12mTo59m_any = ifelse(sum(death_12mTo59m, na.rm = T) %in% 1:30, 1, 0)
      )
  
  
  BR_grouped$v000 <- BRdata$v000[1]
  
  return(BR_grouped)
  
}













