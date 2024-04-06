makefwdf <- function(dataset) {
  fw_ID <- unique(na.omit(dataset$fw_ID))
  fw.df <- as.data.frame(fw_ID)
  for (i in 1:length(fw.df$fw_ID)) {
    fw_subset.df <- subset(dataset, fw_ID == fw.df$fw_ID[i])
    # print(i)
    fw.df$interviews[i] <- nrow(fw_subset.df)
    if (!is.null(fw_subset.df$age_fw)) {
      fw.df$interviews_inc[i] <- nrow(subset(fw_subset.df, everUnion == 0 & !is.na(everSex) & !is.na(age_fw)))
    } else {
      fw.df$interviews_inc[i] <- NA
    }

    fw.df$clusters[i] <- length(unique(fw_subset.df$cluster_ID))
    
    # fieldworker age
    if (!is.null(fw_subset.df$age_fw[1])) {
      fw.df$age_fw[i] <- fw_subset.df$age_fw[1]
    } else {
      fw.df$age_fw[i] <- NA
    }
    
    # fieldworker age
    if (!is.null(fw_subset.df$survey_experience[1])) {
      fw.df$survey_experience[i] <- fw_subset.df$survey_experience[1]
    } else {
      fw.df$survey_experience[i] <- NA
    }
    
    # dhs experience
    if (!is.null(fw_subset.df$dhs_experience[1])) {
      fw.df$dhs_experience[i] <- fw_subset.df$dhs_experience[1]
    } else {
      fw.df$dhs_experience[i] <- NA
    }

    
    fw.df$collday_first[i] <- min(fw_subset.df$intDay, na.rm = TRUE)
    fw.df$collday_last[i] <- max(fw_subset.df$intDay, na.rm = TRUE)
  }
  
  fw.df$gt_3_clusters <- ifelse(fw.df$clusters >= 3, 1, 0)
  
  return(fw.df)
}