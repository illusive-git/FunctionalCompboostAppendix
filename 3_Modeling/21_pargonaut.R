# ======================================================================================= #

library(purrr)
library(data.table)
library(dplyr)
library(ggplot2)
library(FDboost)
library(lubridate)
library(reshape2)
library(pspline)
library(urltools)
library(bda)
library(hexbin)
library(RColorBrewer)
library(parallel)

# ======================================================================================= #

# Parameters
MAX_CPC = 10
TARGET = "SESSIONS" # "SESSIONS", "TRANSACTION_REV", "TRANSACTION"
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
source("x_custom_loss.R")

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
#   filter(ga.deviceCategory != "tablet") %>%
    filter(ga.CPC < MAX_CPC) ->
    dis_1

# Feature Engineering
dis_1 %>%
    mutate(female_ratio_cat = as.numeric(cut_number(female_ratio, 3))) ->
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

        
        # Smooth targets
        if(TARGET == "SESSIONS"){
            i_result[["smoother"]] = stepf(input_x = dis_c$ga.CPC, input_y = dis_c$ga.sessions, fun = mean) 
        }
        if(TARGET == "TRANSACTION_REV"){
            i_result[["smoother"]] = smooth.spline(x = dis_c$`ga.CPC`, y = dis_c$normalized_revenue, cv = FALSE)
         }
        

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
result_list_t$f = fun_mat
result_list_t$t = fun_grid

result_list = result_list_t

# cut out smoother objects
smoothers = result_list$smoother
result_list$smoother = NULL

rm(result_list_t)
rm(dis_1)
gc()


# ======================================================================================= #

pdf("fun_data.pdf", width = 7, height = 4)

plot(fun_grid, fun_mat[1,], type = "l", ylim = c(min(fun_mat),max(fun_mat)), col = 1, 
  sub = paste("Dataset:", DATASET),
  xlab = "CPC", ylab = TARGET)

for(p in 2:nrow(fun_mat)){
    lines(fun_grid, fun_mat[p,], col = p)
}

dev.off()





# ======================================================================================= #

# Model Call
# Small model Website level
FD_call_1 = list(formula = f ~ 1 +bolsc(Category, df = 2), 
    timeformula = ~bbs(t, df = 4), data = result_list, family = Gaussian())
# Large Model WEbsite level
FD_call_2 = list(formula = f ~ 1 +bolsc(Category, df = 2) + bbsc(uniqueProducts, df = 4) + bbsc(meanBasketSize, df = 4) +
    bbsc(female_ratio, df = 4) + bbsc(X18.24, df = 4)+ bbsc(X25.34, df = 4)+ bbsc(X35.44, df = 4)+ bbsc(X45.54, df = 4)+ 
    bbsc(X55.64, df = 4)+ bbsc(X65., df = 4)+ bbsc(meanBasketVariety, df = 4)+ bolsc(account_level, df = 2) ,   
    timeformula = ~bbs(t, df = 4), data = result_list, family = Gaussian())
# Device specific model

FD_mod = do.call(FDboost, FD_call_1)
FD_mod = do.call(FDboost, FD_call_2)

# Selected variables
selected(FD_mod)

# Cross validate mstop
folds_sof = cv(weights = model.weights(FD_mod), type = "kfold", B = 2)

start_time = Sys.time()
cvm_sof = cvrisk(FD_mod, folds = folds_sof, grid = c(300), papply = lapply)
end_time = Sys.time()

end_time - start_time
mstop(cvm_sof)


#fd_model_val = applyFolds(FD_mod, folds = cv(rep(1, length(unique(FD_mod$id))), B = 3), papply = mclapply, grid = c(1,5,10))
#mstop(fd_model_val)
cvm_sof = 300

# Final Model call
FD_call_2$control = boost_control(mstop = 300)
FD_cv = do.call(FDboost, FD_call_2)

sel = selected(FD_cv)
table(names(FD_cv$baselearner)[sel])

# ======================================================================================= #


# Predict & Plot
setwd("plots/")

var_selected = list()
# build new data of all combinations
for(l in 1:length(FD_cv$baselearner)){
    var_selected[[l]] = FD_cv$baselearner[[l]]$get_names()[1]
}

input_df = lapply(var_selected, function(x){a = unlist(unique(result_list[[x]])); return(a)})
names(input_df) = var_selected
input_df = input_df[-which(sapply(input_df, is.null))]

griddata = as.list(expand.grid(input_df))
griddata$t = fun_grid

prediction = predict(FD_cv, griddata)

# Scale to positivity
# prediction = prediction - abs(min(prediction)) 

# sum up
# prediction = t(apply(X = prediction, MARGIN = 1, FUN = cumsum))

# 
pdf("fd_model_pred.pdf", width = 7, height = 4)

plot(fun_grid, prediction[1,], type = "l", ylim = c(min(prediction),max(prediction)), col = 1, 
  sub = paste("Dataset:", DATASET),
  xlab = "CPC", ylab = TARGET, main = "Predictions")

for(p in 2:nrow(prediction)){
    lines(fun_grid, prediction[p,], col = p)
}
# closer label
# title(ylab = "Transaction Revenue", mgp = c(1,1,0))

# legend
legend("bottomright", legend = unlist(input_df), fill = 1:nrow(prediction), cex = 0.75) 

dev.off()


FD_cv_coef = coef(FD_cv)
capture.output(FD_cv_coef,file = "FD_cv_coefs.txt")

pdf("FD_cv.pdf", width = 7, height = 4)
plot(FD_cv, ask = FALSE, pers = FALSE, xlab = "max CPC")
dev.off()

pdf("FD_cv_pred.pdf", width = 7, height = 4)
plotPredicted(FD_cv, ask = FALSE)
dev.off()


pdf("FD_cv_res.pdf", width = 7, height = 4)
plotResiduals(FD_cv, ask = FALSE)
dev.off()


print("Done")


