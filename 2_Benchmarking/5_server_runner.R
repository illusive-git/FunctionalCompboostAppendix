# Benchmarking

library(FDboost)
library(compboost)
library(microbenchmark)
library(peakRAM)
library(dplyr)
library(pushoverr)

set_pushover_user(user = "uff9siau93c4sa8xstqvd9defm66gm")
set_pushover_app(token = "a1iggrizczf3ikfi41hus5fym4svii")

## -------------------------------------- 
# Create simuation data
set.seed(-753)

# simulation parameters
t_min = -1
t_max = 1
obs = c(10^2,10^3)
t_length_min = c(10,300)
t_length_max = c(50,400)
true_predictors = c(10,100)
noise_predictors = c(2,20)
knots_pred = c(10,20)
iters = c(10000)
draws_per_para_set = 5
measurement_repeats = 5

# Define Grid
para_set = expand.grid(obs = obs, t_length_min = t_length_min, t_length_max = t_length_max, 
  true_predictors = true_predictors,noise_predictors = noise_predictors, knots_pred = knots_pred,
  iters = iters)
para_set %>% 
  filter(t_length_max - t_length_min < 101) %>% 
  filter(t_length_min < t_length_max) %>% 
  filter(true_predictors / noise_predictors == 5) ->
  para_set


# Functions to randomly draw from
f1 = function(grid, para){ sqrt(abs(para*10))*grid^2}
f2 = function(grid, para){ sin(grid)*para}
f3 = function(grid, para){ 1/(abs(grid *para)+100)}
f4 = function(grid, para){ (1-exp(grid))*para}
f5 = function(grid, para){ para*(grid)^4-para*grid^3}
f6 = function(grid, para){ -para*(grid)^4+para*grid^3}
f7 = function(grid, para){ sqrt(abs(para*10*grid)) }
f8 = function(grid, para){ para^2*grid}
f9 = function(grid, para){ para*grid - sqrt(abs(para*grid)+2)}
f10 = function(grid, para){ para*grid}

f_list = list(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10)

# Prepeare File
write.table(x = "obs;t_length_min;t_length_max;true_predictors;noise_predictors;knots_pred;peakRAMFdB;peakRAMCpB;T_CpB_mean;T_CpB_min; T_CpB_max;T_CpB_med;T_FDb_mean;T_FDb_min; T_FDb_max;T_FDb_med;data_draws;para_runner", 
  file = "results_runner.txt", append = TRUE, row.names = TRUE, col.names = FALSE)


for(para_runner in 1:nrow(para_set)){
    
    for(data_draws in 1:draws_per_para_set){
        # Build data
        df_train = data.frame(matrix(rnorm(para_set$obs[para_runner]*para_set$true_predictors[para_runner]),nrow = para_set$obs[para_runner]))
        
        fun_list = list()
        grid_list = list()
        id_list = list()
        
        # Construct empty grids
        for(i in 1:para_set$obs[para_runner]){
          grid_list = append(grid_list, list(sort(runif(round(runif(1,para_set$t_length_min[para_runner], 
            para_set$t_length_max[para_runner])), t_min, t_max))))
          fun_list = append(fun_list, list(rep(0,length(tail(grid_list,1)[[1]]))))
          id_list = append(id_list, list(rep(i, length(tail(grid_list,1)[[1]]))))
        }
        
        
        # runs through the variables
        for(k in 1:ncol(df_train)){
          # i runs through the observations
          for(i in 1:nrow(df_train)){
            # generate grid data
            # Select a random function
            selected_function = round(runif(1,1,10))
            names(df_train)[k] = paste0("varN",k,"f", selected_function)
            # Generate Data from function for all rows
            temp = f_list[[selected_function]](grid = grid_list[[i]], para = df_train[i,k])
            fun_list[[i]] = fun_list[[i]] + temp
          }
        }
        
        # Now add random nois variables
        df_noise = data.frame(matrix(rnorm(para_set$obs[para_runner]*para_set$noise_predictors[para_runner]),
          nrow = para_set$obs[para_runner]))
        df_train = cbind.data.frame(df_train, df_noise)
        
        
        fdb_list = list(fun_mat = unlist(fun_list),
                        time = unlist(grid_list),
                        id  = unlist(id_list)
        )
        fdb_list = append(fdb_list,as.list(df_train))
        
        
        cp_grid = lapply(grid_list, function(x){matrix(unlist(x),nrow=1)})
        cp_fun = lapply(fun_list, function(x){matrix(unlist(x),nrow=1)})
        
        # ------------------------------------------------------------------------------------------------------------------------------- #
        # BENCHMARKING
    
        mb_obj = microbenchmark(
          "FDb" = {
            bols_list = lapply(names(df_train), function(x){
              paste0("bbs(",x,", lambda = 10, knots = ", para_set$knots_pred[para_runner],", degree = 3)" )
              })
            formula = as.formula(paste("fun_mat", paste(bols_list, collapse=" + "), sep=" ~ "))
            fdb_model = FDboost(formula = formula, 
                              timeformula = ~bbs(time,  knots = 18, degree = 3, lambda = 10), data = fdb_list, id = ~id,
                              control = boost_control(mstop = para_set$iters[para_runner] , nu = 0.3), offset = "scalar")
            
          },
          "CpB" = {
            response_test = ResponseFDALong$new(target = "target", cp_fun, grid = cp_grid)
            cpd_model = boostSplines(data = df_train,target = response_test,loss = LossQuadratic$new(),learning_rate = 0.3, 
              iterations = para_set$iters[para_runner],
              degree = 3,n_knots = para_set$knots_pred[para_runner], 
              penalty = 10,time_spline_pars = list(n_knots = 18, degree = 3, penalty = 10),trace = 0)
          
          }
        , times = measurement_repeats)
        
        
        
        
        
        
        # RAM TEST

        peakRAM_obj = peakRAM(
          {
            bols_list = lapply(names(df_train), function(x){
              paste0("bbs(",x,", lambda = 10, knots = ", para_set$knots_pred[para_runner],", degree = 3)" )
            })
            formula = as.formula(paste("fun_mat", paste(bols_list, collapse=" + "), sep=" ~ "))
            fdb_model = FDboost(formula = formula, 
              timeformula = ~bbs(time,  knots = 18, degree = 3, lambda = 10), data = fdb_list, id = ~id,
              control = boost_control(mstop = para_set$iters[para_runner], nu = 0.3), offset = "scalar")
            
          },
          {
            response_test = ResponseFDALong$new(target = "target", cp_fun, grid = cp_grid)
            boostSplines(data = df_train,target = response_test,loss = LossQuadratic$new(),learning_rate = 0.3, 
              iterations =para_set$iters[para_runner] ,
              degree = 3,n_knots = para_set$knots_pred[para_runner], 
              penalty = 10,time_spline_pars = list(n_knots = 18, degree = 3, penalty = 10),trace = 0)
            
          }
        )
        
        
        # change format of microbenchmark
        mb_df = data.frame(time = mb_obj$time, exp = mb_obj$expr)
          
        current_result_df = data.frame(para_set[para_runner,])
        current_result_df$peakRamFDb = peakRAM_obj$Peak_RAM_Used_MiB[1]
        current_result_df$peakRamCpB = peakRAM_obj$Peak_RAM_Used_MiB[2]
        current_result_df$T_CpB_mean = mb_df %>% filter(exp == "CpB") %>% summarise(T_CpB_mean = mean(time))
        current_result_df$T_CpB_min = mb_df %>% filter(exp == "CpB") %>% summarise(T_CpB_min = min(time))
        current_result_df$T_CpB_max = mb_df %>% filter(exp == "CpB") %>% summarise(T_CpB_max = max(time))
        current_result_df$T_CpB_med = mb_df %>% filter(exp == "CpB") %>% summarise(T_CpB_med = median(time))
        current_result_df$T_FDb_mean = mb_df %>% filter(exp == "FDb") %>% summarise(T_FDb_mean = mean(time))
        current_result_df$T_FDb_min = mb_df %>% filter(exp == "FDb") %>% summarise(T_FDb_min = min(time))
        current_result_df$T_FDb_max = mb_df %>% filter(exp == "FDb") %>% summarise(T_FDb_max = max(time))
        current_result_df$T_FDb_med = mb_df %>% filter(exp == "FDb") %>% summarise(T_FDb_med = median(time))
        current_result_df$data_draws = data_draws
        current_result_df$para_runner = para_runner
        
        # pushover talk
       
        
        write.table(x = current_result_df,file = "results_runner.txt", append = TRUE,sep = ";",row.names = FALSE, col.names = FALSE)
        
        rm(df_train)
        rm(current_result_df)
        rm(str)
        rm(df_noise)
        
        gc()
        print(paste("para_runner:",para_runner,"| data draw:", data_draws))
    }
        str = paste("WE ARE AT ", round(para_runner * 100 / nrow(para_set),2), "%. Continuing") 
        pushover(str)

}














