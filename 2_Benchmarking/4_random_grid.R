# Benchmarking

library(FDboost)
library(compboost)
library(microbenchmark)
library(peakRAM)
library(dplyr)

## -------------------------------------- 
# Create simuation data
set.seed(-753)

# simulation parameters
t_min = -1
t_max = 1
obs = c(50,500,50000)
t_length_min = c(10,50,300)
t_length_max = c(50,150,400)
true_predictors = c(10,25,100)
noise_predictors = c(2,5,10)

# Define Grid
para_set = expand.grid(obs = obs, t_length_min = t_length_min, t_length_max = t_length_max, 
                       true_predictors = true_predictors,noise_predictors = noise_predictors)
para_set %>% 
  filter(t_length_max - t_length_min < 101) %>% 
  filter(t_length_min < t_length_max) %>% 
  filter(true_predictors > noise_predictors) ->
  para_set

for(para_runner in 1:nrow(para_set)){
    
    # Build data
    df_train = data.frame(matrix(rnorm(para_set$obs[para_runner]*para_set$true_predictors[para_runner]),nrow = para_set$obs[para_runner]))
    fun_mat = matrix(0, ncol = ncol(grid_test),nrow = nrow(df_train))
    
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
    
    # Function plots
    i = 1
    plot(grid_test, f1(para = 1,grid = grid_test[1,]), type = "l", ylim = c(-5,5), lwd = 2, main = "Parameter = 1")
    for(f in f_list){
      i = i + 1
      curve(f(grid = x,para = 1),add = TRUE, col = i, lwd = 2)
    }
    
    i = 1
    plot(grid_test, f1(para = -4,grid = grid_test[1,]), type = "l", ylim = c(-5,5), lwd = 2, main = "Parameter = -4")
    for(f in f_list){
      i = i + 1
      curve(f(grid = x,para = -4),add = TRUE, col = i, lwd = 2)
    }
    
    
    fun_list = list()
    grid_list = list()
    id_list = list()
    # Construct empty grids
    for(i in 1:obs){
      grid_list = append(grid_list, list(sort(runif(round(runif(1,t_length_min,t_length_max)), t_min, t_max))))
      fun_list = append(fun_list, list(rep(0,length(tail(grid_list,1)[[1]]))))
      id_list = append(id_list, list(rep(i, length(tail(grid_list,1)[[1]]))))
    }
    #
    
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
        if(any(is.na(temp))){
          print(paste("i=",i,"and k=",k,"and f=",selected_function))
        }
        fun_list[[i]] = fun_list[[i]] + temp
      }
    }
    
    
    plot(x = grid_list[[1]], y = fun_list[[1]], xlim = c(min(unlist(grid_list)),max(unlist(grid_list))), 
         ylim = c(min(unlist(fun_list)),max(unlist(fun_list))), type = "l",
         xlab = "Grid", ylab = expression(paste('Y'[i],'(Grid)')), main = "Observed Functions")
    for(i in 1:nrow(df_train)){
      lines(x = grid_list[[i]], y = fun_list[[i]], col = i)
    }
    
    
    # Now add random nois variables
    df_noise = data.frame(matrix(rnorm(obs*noise_predictors),nrow = obs))
    
    
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
    
    names(df_train)[selected(fdb_model)]
    
    
    
    microbenchmark(list = list(
      "FDb" = {
        bols_list = lapply(names(df_train), function(x){paste0("bbs(",x,", lambda = 10, knots = 18, degree = 3)" )})
        formula = as.formula(paste("fun_mat", paste(bols_list, collapse=" + "), sep=" ~ "))
        fdb_model = FDboost(formula = formula, 
                          timeformula = ~bbs(time,  knots = 14, degree = 3, lambda = 10), data = fdb_list, id = ~id,
                          control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")
        
      },
      "CpB" = {
        response_test = ResponseFDALong$new(target = "target", cp_fun, grid = cp_grid)
        boostSplines(data = df_train,target = response_test,loss = LossQuadratic$new(),learning_rate = 0.3, iterations = 100,
                     degree = 3,n_knots = 14, penalty = 10,time_spline_pars = list(n_knots = 18, degree = 3, penalty = 10),trace = 0)
      
      }
    ), times = 10)
    
    
    
    
    
    
    # RAM TEST
    bols_list = lapply(names(df_train), function(x){paste0("bbs(",x,", lambda = 10, knots = 18, degree = 3)" )})
    formula = as.formula(paste("fun_mat", paste(bols_list, collapse=" + "), sep=" ~ "))
    response_test = ResponseFDALong$new(target = "target", cp_fun, grid = cp_grid)
    peakRAM(
      {
        fdb_model = FDboost(formula = formula, 
                            timeformula = ~bbs(time,  knots = 14, degree = 3, lambda = 10), data = fdb_list, id = ~id,
                            control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")
      },
      {
        boostSplines(data = df_train,target = response_test,loss = LossQuadratic$new(),learning_rate = 0.3, iterations = 100,
                     degree = 3,n_knots = 14, penalty = 10,time_spline_pars = list(n_knots = 18, degree = 3, penalty = 10),trace = 0)
        
      }
    )
      
    
    

}














