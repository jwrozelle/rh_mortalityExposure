cleanBR_addedChildVars <- function(BRdata) {
  
  require(stringr)
  
  # CMC date of birth for first child
  BRdata$first_dob_CMC_b3 <- extractByRC(
    BRdata, 
    idx = "v224", # first birth index
    varName = "b3", 
    zeroPad_numDigits = 2, 
    output.class = "integer") 
  
  BRdata$first_c_dobR <- extractByID(
    BRdata,
    varName = "c_dobR",
    parentID = "femaleID",
    childID = "childID",
    index = "bord",
    sep = "_",
    indexNumber = 1
  )
  
  return(BRdata)
  
}