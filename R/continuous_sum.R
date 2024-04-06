

continuous_sum <- function(dataset, variable, useNA = TRUE, by.var = NULL, roundby = 2) {
  
  if (is.null(by.var)) {
    c.mean <- mean(dataset[[variable]], na.rm=TRUE)
    c.25 <- quantile(dataset[[variable]], probs = 0.25, na.rm = TRUE)
    c.75 <- quantile(dataset[[variable]], probs = 0.75, na.rm = TRUE)
    
    result <- paste0(round(c.mean, roundby), " (", round(c.25, roundby), ", ", round(c.75, roundby), ")")
    
    if (useNA) {
      c.missing <- sum(is.na(dataset[[variable]]))
      result <- paste0(result, "\nMissing= ", c.missing)
    }
    return(result)
  }
  
  if (!is.null(by.var)) {
    lvls <- names(table(dataset[[by.var]]))
    row <- c()
    for (i in 1:length(lvls)) {
      dataset_f <- subset(dataset, dataset[[by.var]] == lvls[i])
      c.mean <- mean(dataset_f[[variable]], na.rm=TRUE)
      c.25 <- quantile(dataset_f[[variable]], probs = 0.25, na.rm = TRUE)
      c.75 <- quantile(dataset_f[[variable]], probs = 0.75, na.rm = TRUE)
      
      result <- paste0(round(c.mean, roundby), " (", round(c.25, roundby), ", ", round(c.75, roundby), ")")
      
      if (useNA) {
        c.missing <- sum(is.na(dataset_f[[variable]]))
        result <- paste0(result, "\nMissing= ", c.missing)
      }
      
      row <- c(row, result)
    }
    return(row)
  }
  
}

# dataset <- df_list$BJ7
# variable <- "anyCMortality"
# useNA = T

cat_sum <- function(dataset, variable, useNA = TRUE, by.var = NULL, roundby = 2) {
  
  if (is.null(dataset[[variable]])) {
    return("-")
  }
  
  if (is.null(by.var)) {
    # create a single row table with variable breakdown
    if (!useNA) {
      var.table <- table(dataset[[variable]])
      # get the names of the variable levels
      varlvls <- names(var.table) |> as.character()
    } else {
      var.table <- table(dataset[[variable]], useNA = "ifany")
      # get the names of the variable levels
      varlvls <- names(var.table) |> as.character()
      # Add the missing label if it exists
      varlvls[is.na(varlvls)] <- "Missing"
    } 

    # get the denominator
    denom <- sum(var.table)
    # create empty summary matrix
    sum.mat <- matrix(nrow = length(varlvls), ncol = 1, dimnames = list(c(varlvls), c()))
    # for each level of the variable, extract the information
    for (i in 1:length(varlvls)) {
      # lvl.num <- var.table[[varlvls[[i]]]]
      lvl.num <- var.table[[i]]
      # calculate the percentage
      pct <- paste0(round(lvl.num / denom*100, roundby), "%")
      # calculate the result with n (%)
      txt_result <- paste0(lvl.num, " (", pct, ")")
      sum.mat[varlvls[[i]], 1] <- txt_result
      
    } 
    
    return(sum.mat)
  } else {
    # create a table with variable breakdown
    if (!useNA) {
      var.table <- table(dataset[[variable]], dataset[[by.var]])
      # create the varlvl vector with names of levels
      varlvls <- names(var.table[,1])
    } else {
      var.table <- table(dataset[[variable]], dataset[[by.var]], useNA = "ifany")
      varlvls <- names(var.table[,1])
      # Add the missing label if it exists
      varlvls[is.na(varlvls)] <- "Missing"

    } 
    
    # get the names of the "by" levels
    bylvls <- names(var.table[1,])
    
    # create empty summary matrix
    sum.mat <- matrix(nrow = length(varlvls), ncol = length(bylvls), dimnames = list(c(varlvls), c(bylvls)))
    
    for (i in 1:length(bylvls)) {
      denom <- sum(var.table[,i])
      
      for (j in 1:length(varlvls)) {
        lvl.num <- var.table[[j,i]]
        # calculate the percentage
        pct <- paste0(round(lvl.num / denom*100, roundby), "%")
        # calculate the result with n (%)
        txt_result <- paste0(lvl.num, " (", pct, ")")
        sum.mat[j, i] <- txt_result
        
      }
      
    }
    return(sum.mat)
  }
  
}

# summaryList <- anyCMortalitySum.list
# prefix <- "anyCMortality::"

# Create a summary section
summarySection <- function (summaryList, prefix = NULL) {
  if (is.null(prefix)) {
    stop("prefix name required")
  }
  
  # bind each of the matrices in the list to a df
  rbound.list <- lapply(summaryList, function(x){
    as.data.frame(rbind(x))
  })
  
  # 
  for (i in 2:length(rbound.list)) {
    df1 <- rbound.list[[i-1]]
    df2 <- rbound.list[[i]]
    # Set level to ensure things get lined up with the appropriate label when r binding
    df1$lvl <- row.names(df1)
    df2$lvl <- row.names(df2)
    if (i == 2){
      sum.df <- merge(df1, df2, by = "lvl", all = TRUE, suffixes = c(paste0(".",names(rbound.list[i-1])), paste0(".",names(rbound.list[i]))))
    } else {
      sum.df <- merge(sum.df, df2, by = "lvl", all = TRUE, suffixes = c(paste0(".",names(rbound.list[i-1])), paste0(".",names(rbound.list[i]))))
    }
  }

  # rename column names to country names (skipping "lvl")
  names(sum.df)[2:ncol(sum.df)] <- names(rbound.list)
  
  
  # set rownames
  for (row in 1:nrow(sum.df)) {
    rownames(sum.df)[row] <- (paste0(prefix, ".", sum.df[["lvl"]][[row]]))
  }
  
  # Remove the "lvl" column
  sum.df <- sum.df[!names(sum.df) %in% "lvl"]
  
  return(sum.df)
}
