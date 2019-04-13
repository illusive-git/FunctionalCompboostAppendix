# ----------------------------------------------------------------------------------------------------------------------------------------------- #

library(compboost)
library(FDboost)
set.seed(-753)

## -------------------------------------- 
obs = 1000
t_length = 100
df_train = data.frame(a = rnorm(obs), b = rexp(obs), ONE = 1)
grid_test = matrix(seq(-2,2, length.out = t_length), ncol = t_length)
fun_mat = matrix(NA, ncol = ncol(grid_test),nrow = nrow(df_train))
for(i in 1:nrow(df_train)){
  fun_mat[i,] =  df_train$a[i]*grid_test[1,]^2 - 3*df_train$a[i]*grid_test[1,]
                 + df_train$b[i]*grid_test[1,]^2 
                 + rnorm(t_length,0,1)
  - grid_test[1,]
}
plot(x = grid_test[1,], y = fun_mat[1,], xlim = c(min(grid_test),max(grid_test)), ylim = c(min(fun_mat),max(fun_mat)), type = "l")
for(i in 1:nrow(df_train)){
  lines(x = grid_test[1,], y = fun_mat[i,], col = i)
}

fdb_list = list(fun_mat = fun_mat,
                t = grid_test[1,],
                a = df_train$a,
                b = df_train$b,
                ONE = rep(1,length(df_train$a)))

## -------------------------------------- 
obs = 500
t_length = 100
df_train = data.frame(a = rnorm(obs), b = runif(obs,0,1), ONE = 1)
grid_test = matrix(seq(-2,2, length.out = t_length), ncol = t_length)
fun_mat = matrix(NA, ncol = ncol(grid_test),nrow = nrow(df_train))
fun_inf = function(x){
  exp(x) - x^5
}
plot(sort(df_train$b),fun_inf(sort(df_train$b)))

for(i in 1:nrow(df_train)){
  fun_mat[i,] =  (df_train$a[i] +1)^2*sin(grid_test[1,]*4)*15 + fun_inf(df_train$b[i]*grid_test[1,])*5
}
plot(x = grid_test[1,], y = fun_mat[1,], xlim = c(min(grid_test),max(grid_test)), ylim = c(min(fun_mat),max(fun_mat)), type = "l")
for(i in 1:nrow(df_train)){
  lines(x = grid_test[1,], y = fun_mat[i,], col = i)
}

fdb_list = list(fun_mat = fun_mat,
                t = grid_test[1,],
                a = df_train$a,
                b = df_train$b,
                ONE = rep(1,length(df_train$a)))

obs = 500
t_length = 100
df_train = data.frame(a = rnorm(obs), ONE = 1)
grid_test = matrix(seq(-2,2, length.out = t_length), ncol = t_length)
fun_mat = matrix(NA, ncol = ncol(grid_test),nrow = nrow(df_train))
fun_inf = function(x){
  exp(x) - x^5
}
plot(sort(df_train$b),fun_inf(sort(df_train$b)))

for(i in 1:nrow(df_train)){
  fun_mat[i,] =  df_train$a[i]*sin(grid_test[1,]*4)*15
}
plot(x = grid_test[1,], y = fun_mat[1,], xlim = c(min(grid_test),max(grid_test)), ylim = c(min(fun_mat),max(fun_mat)), type = "l")
for(i in 1:nrow(df_train)){
  lines(x = grid_test[1,], y = fun_mat[i,], col = i)
}

fdb_list = list(fun_mat = fun_mat,
                t = grid_test[1,],
                a = df_train$a,
                b = df_train$b,
                ONE = rep(1,length(df_train$a)))

## -------------------------------------- 
# Test_FDA_spline_low_1

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

comp_test = Compboost$new(data = df_train, target = response_test, loss = LossQuadratic$new(),
  time_spline_pars = list(n_knots = 3, degree = 3, differences = 2, penalty = 10), learning_rate = 0.3)

comp_test$addBaselearner(feature = "a",id = "P1",bl_factory = BaselearnerPSpline, penalty = 0, n_knots = 4, degree = 3)
comp_test$train(1, trace = 0) 

fdb_model = FDboost(formula = fun_mat ~ bbs(a, lambda = 10, knots = 4, degree = 3), 
  timeformula = ~bbs(t,  knots = 3, degree = 3, lambda = 10), data = fdb_list, 
   control = boost_control(mstop = 1, nu = 0.3), offset = "scalar")

fdb_model_A = FDboost(formula = fun_mat ~ bbs(a, knots = 4, degree = 3) %A% bbs(t,  knots = 3, degree = 3), 
  timeformula = ~bbs(t,  knots = 3, degree = 3), data = fdb_list, 
   control = boost_control(mstop = 1, nu = 0.3), offset = "scalar")

fdb_model_direct = FDboost(formula = fun_mat ~ bbs(a, knots = 4, degree = 3, lambda= 10) 
  %O% bbs(t,  knots = 3, degree = 3, lambda = 10),  timeformula = ~bbs(t,  knots = 10, degree = 3), data = fdb_list, 
   control = boost_control(mstop = 1, nu = 0.3), offset = "scalar")

est_comp_sort = data.frame(fdb = matrix(matrix(unlist(fdb_model$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           fdb_A = matrix(matrix(unlist(fdb_model_A$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           fdb_model_direct = matrix(matrix(unlist(fdb_model_direct$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           cbd = comp_test$model$getEstimatedParameter()[[1]])

par(mfrow = c(1,1))
plot(est_comp_sort$fdb, type = "b", ylim = c(min(est_comp_sort[,1:4]),max(est_comp_sort[,1:4])), col = "red", pch = 3,
  main = "Beta Parameters 1 iters", ylab = expression(paste(beta," value")))
points(est_comp_sort$cbd, type = "b", col = "blue", pch = 5)
points(est_comp_sort$fdb_A, type = "b", col = "green", pch = 0)
points(est_comp_sort$fdb_model_direct, type = "b", col = "pink", pch = 0)
#legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))

par(mfrow = c(1,1))
plot(est_comp_sort$fdb, type = "p", ylim = c(-60 , 60), col = "red", pch = 3,
  main = "Beta Parameters 1 iters", ylab = expression(paste(beta," value")))
points(est_comp_sort$cbd, type = "p", col = "blue", pch = 5)
#legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))


plot( (est_comp_sort$fdb - est_comp_sort$cbd)/abs(est_comp_sort$fdb))


par(mfrow = c(1,1))
plot(sort(est_comp_sort$fdb), type = "p", ylim = c(min(est_comp_sort[,1:2]),max(est_comp_sort[,1:2])), col = "red", pch = 3)
points(sort(est_comp_sort$cbd), type = "p", col = "blue", pch = 5)
legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))

# Errors
fdb_predict_error = (matrix(fdb_model$predict(), ncol = t_length) - fun_mat)
cpb_predict_error = (matrix(comp_test$predict(), byrow = TRUE, ncol = 100) - fun_mat)

plot(colMeans(fdb_predict_error), type = "b", col = "blue", pch = 3, main ="Residuals - 1 Iteration", ylab = "Mean Residual", xlab = "t")
points(colMeans(cpb_predict_error), type = "b", col = "red", pch = 5)
legend("bottomright",legend = c("FDb","CPb"), fill = c("red","blue"))


## -------------------------------------- 
# Test_FDA_spline_low_1

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

comp_test = Compboost$new(data = df_test, target = response_test, loss = LossQuadratic$new(),
  time_spline_pars = list(n_knots = 3, degree = 3, differences = 2, penalty = 10), learning_rate = 0.3)

comp_test$addBaselearner(feature = "a",id = "P1",bl_factory = BaselearnerPSpline, penalty = 0, n_knots = 4, degree = 3)
comp_test$train(100, trace = 0) 

fdb_model = FDboost(formula = fun_mat ~ bbs(a, lambda = 10, knots = 4, degree = 3), 
  timeformula = ~bbs(t,  knots = 3, degree = 3, lambda = 10), data = fdb_list, 
   control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")

fdb_model_A = FDboost(formula = fun_mat ~ bbs(a, knots = 4, degree = 3) %A% bbs(t,  knots = 3, degree = 3), 
  timeformula = ~bbs(t,  knots = 3, degree = 3), data = fdb_list, 
   control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")

fdb_model_direct = FDboost(formula = fun_mat ~ bbs(a, knots = 4, degree = 3, lambda= 10) 
  %O% bbs(t,  knots = 3, degree = 3, lambda = 10),  timeformula = ~bbs(t,  knots = 10, degree = 3), data = fdb_list, 
   control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")

est_comp_sort = data.frame(fdb = matrix(matrix(unlist(fdb_model$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           fdb_A = matrix(matrix(unlist(fdb_model_A$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           fdb_model_direct = matrix(matrix(unlist(fdb_model_direct$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           cbd = comp_test$model$getEstimatedParameter()[[1]])

par(mfrow = c(1,1))
plot(est_comp_sort$fdb, type = "b", ylim = c(min(est_comp_sort[,1:4]),max(est_comp_sort[,1:4])), col = "red", pch = 3,
  main = "Beta Parameters 1 iters", ylab = expression(paste(beta," value")))
points(est_comp_sort$cbd, type = "b", col = "blue", pch = 5)
points(est_comp_sort$fdb_A, type = "b", col = "green", pch = 0)
points(est_comp_sort$fdb_model_direct, type = "b", col = "pink", pch = 0)
#legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))

par(mfrow = c(1,1))
plot(est_comp_sort$fdb, type = "p", ylim = c(-60 , 60), col = "red", pch = 3,
  main = "Beta Parameters 1 iters", ylab = expression(paste(beta," value")))
points(est_comp_sort$cbd, type = "p", col = "blue", pch = 5)
#legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))


plot( (est_comp_sort$fdb - est_comp_sort$cbd)/abs(est_comp_sort$fdb))


par(mfrow = c(1,1))
plot(sort(est_comp_sort$fdb), type = "p", ylim = c(min(est_comp_sort[,1:2]),max(est_comp_sort[,1:2])), col = "red", pch = 3)
points(sort(est_comp_sort$cbd), type = "p", col = "blue", pch = 5)
legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))

# Errors
fdb_predict_error = (matrix(fdb_model$predict(), ncol = t_length) - fun_mat)
cpb_predict_error = (matrix(comp_test$predict(), byrow = TRUE, ncol = 100) - fun_mat)

plot(colMeans(fdb_predict_error), type = "b", col = "blue", pch = 3, main ="Residuals - 1 Iteration", ylab = "Mean Residual", xlab = "t")
points(colMeans(cpb_predict_error), type = "b", col = "red", pch = 5)
legend("bottomright",legend = c("FDb","CPb"), fill = c("red","blue"))
