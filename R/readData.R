library(stringr)
library(foreign)
library(parallel)


source("R/readDHS_in_par.R")
source("R/readKR_in_par.R")
source("R/readBR_in_par.R")

data_dir <- Sys.getenv("DATA_DIRECTORY")

# data_dir <- "I:\\DHS\\InterviewerEffect"


# List of datanames ####
# This assumes you have downloaded and extracted all malawi data

dataList <- list(c("MWIR7A", "MWFW7A")) # Malawi (2015-16)
dataList_m <- list(c("MWMR7A", "MWFW7A"))

# child data_list
dataList_kr <- c(
  "MWKR7A"
)

dataList_br <- c(
  "MWBR7A"
)


# Use parallel computing to read in the data ####

## Individual Recode ###########################################################

## Step 1: Create a cluster of child processes 

cl <- makeCluster(n.cores)
## Step 2: Load the necessary R package(s)
## N.B. length(cl) is the number of child processes
##      in the cluster 
par.setup <- parLapplyLB( cl, 1:length(cl),
                          function(xx) {
                            require(stringr)
                            require(foreign)
                          })
## Step 3: Distribute the necessary R objects 
clusterExport( cl, c('readDHS_in_par', 'dataList', 'dataList_m', 'data_dir') )
## Step 4: Do the computation
### Individual Recode
df_list <- parLapplyLB(cl, dataList,
                       function(x) {
                         # do this to each dataframe in the list
                         dataset <- readDHS_in_par(data_dir, x[1], x[2])
                         # for each dataframe in the list, return the following
                         return(dataset)
                       }
)

for (i in 1:length(dfKR_list)) {
  names(df_list)[i] <- df_list[[i]]$v000[1]
}

## Step 5: Remember to stop the cluster!
stopCluster(cl)


## Child Recode ################################################################

## Step 1: Create a cluster of child processes 

cl <- makeCluster(n.cores)
## Step 2: Load the necessary R package(s)
## N.B. length(cl) is the number of child processes
##      in the cluster 
par.setup <- parLapplyLB( cl, 1:length(cl),
                          function(xx) {
                            require(stringr)
                            require(foreign)
                          })
## Step 3: Distribute the necessary R objects 
clusterExport( cl, c('readKR_in_par', 'dataList_kr', 'data_dir') )
## Step 4: Do the computation
### Individual Recode
dfKR_list <- parLapplyLB(cl, dataList_kr,
                         function(x) {
                           # do this to each dataframe in the list
                           dataset <- readKR_in_par(data_dir, x)
                           # for each dataframe in the list, return the following
                           return(dataset)
                         }
)

## Step 5: Remember to stop the cluster!
stopCluster(cl)

for (i in 1:length(dfKR_list)) {
  names(dfKR_list)[i] <- dfKR_list[[i]]$v000[1]
}


save(list=c("df_list", "dfKR_list"), file="data/ie_dfList.Rdata")



## Child Recode ################################################################

## Step 1: Create a cluster of child processes 

cl <- makeCluster(n.cores)
## Step 2: Load the necessary R package(s)
## N.B. length(cl) is the number of child processes
##      in the cluster 
par.setup <- parLapplyLB( cl, 1:length(cl),
                          function(xx) {
                            require(stringr)
                            require(foreign)
                          })
## Step 3: Distribute the necessary R objects 
clusterExport( cl, c('readBR_in_par', 'dataList_br', 'data_dir') )
## Step 4: Do the computation
### Individual Recode
dfBR_list <- parLapplyLB(cl, dataList_br,
                         function(x) {
                           # do this to each dataframe in the list
                           dataset <- readBR_in_par(data_dir, x)
                           # for each dataframe in the list, return the following
                           return(dataset)
                         }
)

# ### Men's Recode
# df_list_m <- parLapplyLB(cl, dataList_m,
#                          function(x) {
#                            # do this to each dataframe in the list
#                            dataset <- readDHS_in_par(data_dir, x[1], x[2], male = TRUE)
#                            # for each dataframe in the list, return the following
#                            return(dataset)
#                          }
# )


## Step 5: Remember to stop the cluster!
stopCluster(cl)

# Name them according to 3 digit survey code
for (i in 1:length(dfBR_list)) {
  names(dfBR_list)[i] <- dfBR_list[[i]]$v000[1]
}


save(list=c("df_list", "dfKR_list", "dfBR_list"), file="data/ie_dfList.Rdata")














