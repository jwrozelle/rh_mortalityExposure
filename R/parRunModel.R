
# modelFunction <- anc4_everu5
# dataList <- df_list
# n.cores <- 10

# deparse(substitute(my_object))

parRunModel.lme4 <- function(dataList, modelFunction, n.cores = 1) {
  
  if (n.cores > 1) {
    
    modelCode <- modelFunction
    data.list <- dataList
    
      cl <- makeCluster(n.cores)
      ## Step 2: Load the necessary R package(s)
      ## N.B. length(cl) is the number of child processes
      ##      in the cluster 
      par.setup <- parLapplyLB( cl, 1:length(cl),
                                function(xx) {
                                  require(lme4)
                                })
      ## Step 3: Distribute the necessary R objects 
      clusterExport( cl, c('modelCode', 'data.list') )
      ## Step 4: Do the computation
      ### Individual Recode
      model.list <- parLapplyLB(cl, dataList,
                             function(x) {
                               model.obj <- try({modelCode(x)})
                               
                               if(!exists("model.obj")) {
                                 model.obj <- NA
                               }
                               return(model.obj)
                             }
      )
      
      
      ## Step 5: Remember to stop the cluster!
      parallel::stopCluster(cl)
      
    
    
  } else { # if not parallel
    model.list <- lapply(dataList, function(x) {
      
      try({model.obj <- modelFunction(x)})
      
      if(!exists("model.obj")) {
        model.obj <- NA
      }
      
      return(model.obj)
      
    })
    
  }
 
  return(model.list) 
}


# make table

modelListTables <- function(model.list) {
  # output the tables
  for (i in 1:length(model.list)) {
    svy.name <- names(model.list)[i]
    x <- model.list[[i]]
    if ("glmerMod" %in% class(x)) {
      cat("\n", svy.name)
      stargazer(x, type = "html")
    } else {
      cat("\n", svy.name, " is not yet properly coded")
    }
  }
}













