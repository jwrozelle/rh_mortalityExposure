

# model and create table

modelAndTable <- function(dataList, modelFunction, outputType = "html") {
  model.list <- lapply(dataList, function(x) {
    try({model.obj <- modelFunction(x)})
    
    if(!exists("model.obj")) {
      model.obj <- NA
    }
    
    return(model.obj)
    
  })
  
  star.list <- list()
  
  # output the tables
  for (i in 1:length(model.list)) {
    svy.name <- names(model.list)[i]
    x <- model.list[[i]]
    if (!is.na(x)) {
      cat("\n", svy.name)
      star.list[[i]] <- stargazer(x, type = outputType)
    } else {
      star.list[[i]] <- cat("\n", svy.name, " is not yet properly coded")
    }
  }
  
  return(list(modelList = model.list, tableList = star.list))
}
















