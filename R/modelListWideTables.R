
modelListWideTable <- function(
    listOfModels, 
    type = "html", 
    star.cutoffs = c(0.1, 0.05, 0.01, 0.001), 
    star.char = c("+", "*", "**", "***"),
    notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001")
    ) {
  
  models2render <- list()
  keepCount <- 0
  for (model_fit in listOfModels) {
    if ("glmerMod" %in% class(model_fit) || "glm" %in% class(model_fit)) {
      keepCount <- keepCount + 1
      
      models2render[[keepCount]] <- model_fit
      
    }
  }
  
  
  
  if(length(models2render) > 0) {
    # short names so stargazer doesn't error out
    names(models2render) <- paste0("mod", 1:length(models2render))
    
    if (is.null(star.cutoffs)) {
      stargazer(models2render, 
                type = type,
      )
    } else {
      stargazer(models2render,
                type = type,
                star.cutoffs = star.cutoffs,
                star.char = star.char,
                notes = notes,
                notes.append = F
                )
    }
  } else {
    "Data has not been fully processed yet"
  }
}





