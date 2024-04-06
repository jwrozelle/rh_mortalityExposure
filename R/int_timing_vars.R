int_timing_vars <- function(dataset) {
  # get vector of unique fieldworker IDs
  fw_ID.vec <- unique(na.omit(dataset$fw_ID))
  # # put this vector in a dataframe
  # fw.df <- as.data.frame(fw_ID)
  # create empty variables
  dataset$intDay_num <- NA
  dataset$intDay_num <- dataset$intDay - dataset$collday_first
  
  # create empty datataset for fieldworker interview number
  # fw_intN.df <- data.frame(matrix(ncol = 2, nrow=0))
  # colnames(fw_intN.df) <- c("uuid", "fw_intN")
  
  # fw_IntOfDay.df.list <- list()
  
  # # create empty dataset for nth interview of the day for a fieldworker
  # fw_IntOfDay.df <- data.frame(matrix(ncol = 2, nrow=0))
  # colnames(fw_IntOfDay.df) <- c("uuid", "fw_IntOfDay")
  
  fw_ID.list <- lapply(fw_ID.vec, function(fw_ID.i) {
    
    
    fw_subset.df <- subset(dataset, fw_ID == fw_ID.i)
    fw_subset.df <- fw_subset.df[order(fw_subset.df$intDay, fw_subset.df$startTime),]
    # create a new variable based on the interview number
    fw_subset.df$fw_intN <- seq_along(fw_subset.df[,1])
    fw_intN.df <- fw_subset.df[c("uuid", "fw_intN")]
    
    fw_intDay <- unique(na.omit(fw_subset.df$intDay))
    # int_day.df <- as.data.frame(intDay)
    
    fw_IntOfDay.list <- lapply(fw_intDay, function(inter) {
      fw_day_subset.df <- subset(fw_subset.df, intDay == inter)
      # order by startime within the day
      fw_day_subset.df <- fw_day_subset.df[order(fw_day_subset.df$startTime),]
      # add a new variable for interviw of the day
      fw_day_subset.df$fw_IntOfDay <- seq_along(fw_day_subset.df[,1])
      # extract variables needed
      return(fw_day_subset.df[c("uuid", "fw_IntOfDay")])
      
    })
    
    fw_IntOfDay.df <- iotools::fdrbind(fw_IntOfDay.list)
    # fw_IntOfDay.df <- do.call(rbind, fw_IntOfDay.list)
    
    return(list(fw_intN = fw_intN.df, fw_IntOfDay = fw_IntOfDay.df))
    
  })
  
  # get the first nested element in the list
  fw_intN.list <- lapply(fw_ID.list, function(x) {return(x$fw_intN)})
  # use do.call to bind it into a dataframe
  
  fw_intN.df <- iotools::fdrbind(fw_intN.list)
  # fw_intN.df <- do.call(rbind, fw_intN.list)
  
  # get the second nested element in the list
  fw_IntOfDay.list <- lapply(fw_ID.list, "[[", 2)
  # use do.call to bind it into a dataframe
  fw_IntOfDay.df <- iotools::fdrbind(fw_IntOfDay.list)
  # fw_IntOfDay.df <- do.call(rbind, fw_IntOfDay.list)
  
  
  # merge in interview numbers
  dataset <- merge(dataset, fw_intN.df, by = "uuid", all.x=TRUE, all.y=FALSE)
  # merge in interview N of day numbers
  dataset <- merge(dataset, fw_IntOfDay.df, by = "uuid", all.x=TRUE, all.y=FALSE)
  
  
  # quantile variables
  dataset$svy_phase3 <- cut(
    dataset$intDay, 
    breaks=quantile(
      dataset$intDay, 
      probs = seq(0,1,1/3)), 
    na.rm=TRUE, 
    labels = FALSE
  )
  # quantile variables
  dataset$svy_phase4 <- cut(
    dataset$intDay, 
    breaks=quantile(
      dataset$intDay, 
      probs = seq(0,1,1/4)), 
    na.rm=TRUE, 
    labels = FALSE
  )
  
  
  return(dataset)
  
}