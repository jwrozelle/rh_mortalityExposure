
# dataset <- df_list[[1]]


addInterviewCount <- function(dataset) {
  # make the fieldworker dataset
  
  try({fw.df <- makefwdf(dataset)[, c("fw_ID","interviews", "interviews_inc", "clusters", "gt_3_clusters", "collday_first", "collday_last")]})
  
  if (exists("fw.df")) {
    fulldataset <- base::merge(dataset, fw.df, by = "fw_ID", all.x=TRUE, all.y=FALSE)
    # fulldataset$interviews50 <- fulldataset$interviews / 50
  } else {
    fulldataset <- dataset
    warning(paste0("Fieldworker data not produced for ", dataset$v000[1]))
  }
  
  return(fulldataset)
}

