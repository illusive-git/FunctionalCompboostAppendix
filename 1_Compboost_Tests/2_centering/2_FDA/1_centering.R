library(compboost)
library(FDboost)
library(Matrix)
set.seed(-753)

source("~/Documents/3_LMU/2_Master/1_Code/4_tensors/baselearner_minus.R")
source("../Documents/GitHub/master_tex/1_Code/4_support/baselearner_minus.R")

## -------------------------------------- 
obs = 500
t_length = 100
df_train = data.frame(a = rnorm(obs), b = rexp(obs), ONE = 1)
grid_test = matrix(seq(-2,2, length.out = t_length), ncol = t_length)
fun_mat = matrix(NA, ncol = ncol(grid_test),nrow = nrow(df_train))
for(i in 1:nrow(df_train)){
  fun_mat[i,] =  grid_test[1,]^4 - sqrt(abs(df_train$a[i]*10))*grid_test[1,]^2
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
# Test_FDA_spline_low_1

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

comp_test = Compboost$new(data = df_train, target = response_test, loss = LossQuadratic$new(),
  time_spline_pars = list(n_knots = 20, degree = 3, differences = 2, penalty = 10), learning_rate = 0.3)
comp_test$addBaselearner(feature = "a",id = "spline",bl_factory = BaselearnerPSpline, df = 4, n_knots = 15, degree = 3)
comp_test$addBaselearner(feature = "a",id = "bols",bl_factory = BaselearnerPolynomial, intercept = TRUE)
comp_test$CenterBaselearner(bl_target = "a_spline", bl_center = "a_bols", keep = FALSE)
comp_test$removeBaselearner("a_bols")
comp_test$train(50, trace = 0) 

fdb_model = FDboost(formula = fun_mat ~ bbs(a, lambda = 212.1214, knots = 15, degree = 3) %-% bols(a), 
  timeformula = ~bbs(t,  knots = 20, degree = 3, lambda = 10), data = fdb_list, 
   control = boost_control(mstop = 50, nu = 0.3), offset = "scalar")



est_comp_sort = data.frame(fdb_model = matrix(matrix(unlist(fdb_model$coef()), ncol = dim(extract(fdb_model)[[1]][[1]])[2]
                                   ,byrow = TRUE),ncol = 1),
                           cbd = comp_test$model$getEstimatedParameter()[[1]])

par(mfrow = c(1,1))
plot(est_comp_sort$fdb, type = "b", ylim = c(min(est_comp_sort[,1:2]),max(est_comp_sort[,1:2])), col = "red", pch = 3,
  main = "Beta Parameters 10 iters", ylab = expression(paste(beta," value")))
points(est_comp_sort$cbd, type = "b", col = "blue", pch = 5)
legend("bottom",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))


plot( (est_comp_sort$fdb - est_comp_sort$cbd)/abs(est_comp_sort$fdb), ylim = c(-1,1))

par(mfrow = c(1,1))
plot(sort(est_comp_sort$fdb), type = "p", ylim = c(min(est_comp_sort[,1:2]),max(est_comp_sort[,1:2])), col = "red", pch = 3)
points(sort(est_comp_sort$cbd), type = "p", col = "blue", pch = 5)
legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))

# Errors
fdb_predict_error = (matrix(fdb_model$predict(), ncol = t_length) - fun_mat)
cpb_predict_error = (matrix(comp_test$predict(), byrow = TRUE, ncol = 100) - fun_mat)

plot(colMeans(fdb_predict_error), type = "b", col = "blue", pch = 3, main ="Residuals - 10 Iteration", ylab = "Mean Residual", xlab = "t")
points(colMeans(cpb_predict_error), type = "b", col = "red", pch = 5)
legend("bottom",legend = c("FDb","CPb"), fill = c("red","blue"))


