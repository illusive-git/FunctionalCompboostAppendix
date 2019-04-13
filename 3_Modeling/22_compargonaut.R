# ======================================================================================= #

library(purrr)
library(data.table)
library(dplyr)
library(ggplot2)
library(compboost)
library(lubridate)
library(reshape2)
library(pspline)
library(urltools)
library(bda)
library(hexbin)
library(RColorBrewer)
library(parallel)
library(mltools)


# ======================================================================================= #

# Parameters
MAX_CPC = 0.5
TARGET = "TRANSACTION_REV" # "SESSIONS", "TRANSACTION_REV", "TRANSACTION"
DATASET = "FULL" # "FULL" "NO_PRODUCT"
CORES = 10L

# ======================================================================================= #

# set up local cluster 

# set up colour
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

# ======================================================================================= #
options(stringsAsFactors = TRUE)

# get custom losses
# source("x_custom_loss.R")

# get tie_fighter
source("x_tie_fighter.R")

# get filter_by_grid
source("x_filter_by_grid.R")

# get step function from vector
source("x_stepf.R")

# Preparators
source("x_preparators.R")
if(TARGET == "SESSIONS"){
    dis = fread("~/data/SESSION_FINAL.csv")
}
if(TARGET == "TRANSACTION_REV"){
    dis = fread("~/data/TRANSCAT_REV_FINAL.csv")
}

gc()

# ======================================================================================= #

# Filtering
dis %>%
#   filter(Category %in% c("TRAVEL","BEAUTY_AND_FITNESS","HEALTHCARE","AUTOMOTIVE")) %>%
    filter(ga.CPC < MAX_CPC) ->
    dis_1

# Feature Engineering
dis_1 %>%
    mutate(female_ratio_cat = as.numeric(cut_number(female_ratio, 3)),
           meanRev_cat = as.numeric(cut_number(meanRev,3))) ->
    dis_1

rm(dis)
gc()

# Construct a grid of all unique combinations
key_list = expand.grid(ga.viewId = unique(dis_1$ga.viewId)) 

# Cast data.table
dis_1 = as.data.table(dis_1)
var_list = colnames(dis_1)


# ======================================================================================= #
setwd("plots/")

# Now put build lists, that contain all the values corresponding to the grid values
# i.e. list objects k belong to grid combination in key_list[k,]

runner  = function(i){
    # Build container to return everything
    i_result = list()

    # Filter data according to grid elements key_list[i,]
    dis_1 %>%
      filter_by_grid(target = ., v_grid = key_list, v_row = i) %>%
      tie_fighter(tie.var = "ga.CPC") %>% # FIXME add weights for CPC?
      arrange(`ga.CPC`) %>%
      # cumulate
      mutate(ga.sessions = cumsum(ga.sessions),
             ga.transactionRevenue = cumsum(ga.transactionRevenue)) ->
      dis_c
      
       
    # Test if number of rows sufficient and enough unique values for spline creation
    if(nrow(dis_c) > 20){
        # Report number of rows in each variable combination
        print(paste("key_list:",i,
                    "| Website:", suffix_extract(domain(dis_c$Website[1]))[3], 
                    "| Campaign: <", substr(key_list$ga.campaign[i],1,20),
                    "> | Rows:", nrow(dis_c),
                    "| NAs.", sum(is.na(dis_c))
            )
        )


        i_result[["grid"]] = matrix(dis_c$ga.CPC, nrow = 1)
        i_result[["value"]] = matrix(dis_c$ga.sessions, nrow = 1)    

        # Now concatenate all the other variables accordingly
        for(var_i in var_list){
            # mean for numerics - be careful what the current level is!!!
            if(is.numeric(dis_c[1,var_i])){
                i_result[[var_i]] = mean(dis_c[[var_i]], na.rm = TRUE)
            } else{
            # else use most frequent value
                i_result[[var_i]] = sort(table(dis_c[[var_i]]), decreasing = TRUE)[1]

            }
        }
        # Save used variables
        i_result[["legend"]] = paste(as.character(lapply(key_list[i,], as.character)), collapse = " ")
        i_result[["n_data_points"]] = nrow(dis_c)

    } else{
        return(NULL)
    }
    return(i_result)
}


gc()
   #################          ##########          ##########          ##########          ################# 
  #                                                                                                        #
 #                                                                                                          #
# Parallel execute                                                                                           #
     result_list = mclapply(X = 1:nrow(key_list), FUN = runner, mc.cores = getOption("mc.cores", CORES))      #
# Collect from Forks                                                                                         #
 #                                                                                                          #
  #                                                                                                        #
   #################          ##########          ##########          ##########          ################# 


# remove NULLS
result_list = result_list[-which(sapply(result_list, is.null))]


# invert list
var_list = names(result_list[[1]])

cp_grid = list() 
cp_target = list()
for(i in 1:length(result_list)){
    cp_target[[i]] = result_list[[i]]$value / max(result_list[[i]]$value)
    cp_grid[[i]] = result_list[[i]]$grid
}


result_list_t = list()
for(i_var in var_list){
    collector = list()
    for(i_obs in 1:length(result_list)){
        collector = c(collector, result_list[[i_obs]][[i_var]])
    }
    if(is.null(names(collector))){
        result_list_t[[i_var]] = unlist(collector)
    } else {
        result_list_t[[i_var]] = names(collector)
    
    }
}


# Add f and t
result_list_t$value = NULL
result_list_t$grid = NULL

cp_data = as.data.frame(result_list_t)

cp_data2 = cp_data[,c("Category","account_level","uniqueProducts","meanBasketSize","meanBasketVariety",
                      "female_ratio","X18.24","X25.34","X35.44","X45.54","X55.64","X65.")]

cp_dat = as.data.table(cp_data2)
cp_data_oh = mltools::one_hot(dt = cp_dat, cols = c("Category","account_level"))

cp_data = as.data.frame(cp_data_oh)
cp_data$ONE =1 

rm(cp_dat)
rm(cp_data2)
rm(cp_data_oh)
rm(result_list_t)
rm(dis_1)
rm(result_list)
gc()
# =======================================================================================

library(compboost)

cp_data$Category_unknown = NULL
cp_data$Category_UNSPECIFIED = NULL

poly_names = c("ONE",
               names(cp_data)[grepl("Category",names(cp_data))],
               names(cp_data)[grepl("account_level",names(cp_data))]) 

cp_response = ResponseFDALong$new(target = "sessions", response = cp_target, grid = cp_grid) 
cp = Compboost$new(data = cp_data, target = cp_response, loss = LossQuadratic$new())

for(v in poly_names){
    cp$addBaselearner(feature = v, id = "P", BaselearnerPolynomial, intercept = FALSE)
}

cp$addBaselearner(feature = "uniqueProducts", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "meanBasketSize", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "female_ratio", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "X18.24", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "X25.34", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "X35.44", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "X45.54", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "X55.64", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "X65.", id = "S", BaselearnerPSpline, df = 4)
cp$addBaselearner(feature = "meanBasketVariety", id = "S", BaselearnerPSpline, df = 4)

cp$train(300)

table(cp$getSelectedBaselearner())

saveRDS(object = cp$getEstimatedCoef(), file = "compboost_model.RDS")


var_plot_list = c("female_ratio_S","X18.24_S","X25.34_S","X35.44_S","X45.54_S","X55.64_S","X65._S","meanBasketVariety_S")

for(v in var_plot_list){
    pdf(paste0(v,".pdf"))
    tryCatch({cp$plot(v)}, error = function(e){return(NULL)})
    dev.off()
}

