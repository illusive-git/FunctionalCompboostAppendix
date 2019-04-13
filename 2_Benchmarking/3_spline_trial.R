# Benchmarking

library(FDboost)
library(compboost)
library(funData)

## -------------------------------------- 
# Create simuation data
set.seed(-753)

# simulation parameters
obs = 500
t_length = 100
t_min = -1
t_max = 1
true_predictors = 20
noise_predictors = 10


# Build data
df_train = data.frame(matrix(rnorm(obs*true_predictors),nrow = obs))
grid_test = matrix(seq(t_min, t_max, length.out = t_length), ncol = t_length)
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



for(k in 1:ncol(df_train)){
  for(i in 1:nrow(df_train)){
    # Select a random function
    selected_function = round(runif(1,1,10))
    names(df_train)[k] = paste0("varN",k,"f", selected_function)
    # Generate Data from function for all rows
    temp = f_list[[selected_function]](grid = grid_test[1,], para = df_train[i,k])
    if(any(is.na(temp))){
      print(paste("i=",i,"and k=",k,"and f=",selected_function))
  }
    fun_mat[i,] = fun_mat[i,] + temp
  }
}


plot(x = grid_test[1,], y = fun_mat[1,], xlim = c(min(grid_test),max(grid_test)), ylim = c(min(fun_mat),max(fun_mat)), type = "l",
     xlab = "Grid", ylab = expression(paste('Y'[i],'(Grid)')), main = "Observed Functions")
for(i in 1:nrow(df_train)){
  lines(x = grid_test[1,], y = fun_mat[i,], col = i)
}


# Now add random nois variables
df_noise = data.frame(matrix(rnorm(obs*noise_predictors),nrow = obs))


df_train = cbind.data.frame(df_train, df_noise)


fdb_list = list(fun_mat = fun_mat,
                t = grid_test[1,]
)
fdb_list = append(fdb_list,as.list(df_train))



## -------------------------------------- 
# Run Stuff

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

cbd = boostSplines(data = df_train, target = response_test,learning_rate = 0.3,loss = LossQuadratic$new(),
             time_spline_pars = list(n_knots = 14, degree = 3, penalty = 10),iterations = 10)


bols_list = lapply(names(df_train), function(x){paste0("bbs(",x,", lambda = 10, knots = 14, degree = 3)" )})
formula = as.formula(paste("fun_mat", paste(bols_list, collapse=" + "), sep=" ~ "))

fdb_model = FDboost(formula = formula, 
                    timeformula = ~bbs(t,  knots = 3, degree = 3, lambda = 10), data = fdb_list, 
                    control = boost_control(mstop = 10, nu = 0.3), offset = "scalar")


names(df_train)[selected(fdb_model)]
cbd$getSelectedBaselearner()






