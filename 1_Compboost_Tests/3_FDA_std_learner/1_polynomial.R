
# ----------------------------------------------------------------------------------------------------------------------------------------------- #

library(compboost)
library(FDboost)
set.seed(-753)

obs = 50
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

# Test Data set
test_obs = 50
df_test = data.frame(a = rnorm(obs), b = rexp(obs), ONE = 1)
fun_mat_test = matrix(NA, ncol = ncol(grid_test),nrow = nrow(df_test))
for(i in 1:nrow(df_test)){
  fun_mat_test[i,] =  df_test$a[i]*grid_test[1,]^2 - 3*df_test$a[i]*grid_test[1,]
                 + df_test$b[i]*grid_test[1,]^2 
                 + rnorm(t_length,0,1)
  - grid_test[1,]
}
fdb_list_test = list(fun_mat = fun_mat,
                t = grid_test[1,],
                a = df_test$a,
                b = df_test$b,
                ONE = rep(1,length(df_test$a)))









## -------------------------------------- 
# Test_FDA_poly_

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

comp_test = Compboost$new(data = df_train, target = response_test, loss = LossQuadratic$new(),
  time_spline_pars = list(degree = 3, n_knots = 25), learning_rate = 0.3)
comp_test$addBaselearner(feature = "b",id = "P1",bl_factory = BaselearnerPolynomial,
                         degree = 1, intercept = TRUE)
comp_test$train(10, trace = 0) 

fdb_model = FDboost(formula = fun_mat ~ bols(b, lambda = 0, intercept = TRUE),
                    timeformula = ~bbs(t, lambda = 0, knots = 25, degree = 3, differences = 2), data = fdb_list, 
                    control = boost_control(mstop = 10, nu = 0.3), offset = "scalar")

table(comp_test$getSelectedBaselearner())
table(selected(fdb_model))

est_comp = data.frame(FDb_1 = fdb_model$coef()[[1]],
                      CPb_1 = comp_test$model$getEstimatedParameter()[[1]])


temp_coef_mat_1 = matrix(fdb_model$coef()[[1]], nrow = 2)
temp_coef_mat_2 = matrix(comp_test$model$getEstimatedParameter()[[1]], nrow = 2)

est_comp_sort = data.frame(fdb = c(temp_coef_mat_1[1,],temp_coef_mat_1[2,]),
                           cbd = c(temp_coef_mat_2[1,],temp_coef_mat_2[2,]))

par(mfrow = c(1,1))
plot(est_comp_sort$fdb, type = "p", col = "red", pch = 3,
  main = "Beta Parameters - 1 iterations", ylab = expression(paste(beta," value")), ylim = c(-0.5,1))
points(est_comp_sort$cbd, type = "p", col = "blue", pch = 5)
legend("bottomleft",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))

plot( (est_comp$FDb_1 - est_comp$CPb_1) /abs(est_comp$FDb_1), main = "Relative Differences in beta parameters - 1000 iterations",
  ylab = expression(paste("rel(FD ",beta," value - CP ",beta," value)")),
  type = "b")

# Errors
fdb_predict = (matrix(fdb_model$predict(), ncol = t_length) - fun_mat)
cpb_predict = (matrix(comp_test$predict(), byrow = TRUE, ncol = 100) - fun_mat)

plot(colMeans(cpb_predict), type = "b", col = "blue", pch = 3, main ="Residuals - 1000 Iteration", ylab = "Mean Residual", xlab = "t")
points(colMeans(fdb_predict), type = "b", col = "red", pch = 5)
legend("bottomright",legend = c("FDb","CPb"), fill = c("red","blue"))

plot(colMeans(fdb_predict), type = "b", col = "red", pch = 5, main ="Residuals - 1000 Iteration", ylab = "Mean Residual", xlab = "t")
points(colMeans(cpb_predict), type = "b", col = "blue", pch = 3)

# Test Data Set
test_predict = fdb_model$predict(newdata = fdb_list_test)
fdb_predict_test = (matrix(test_predict, ncol = t_length) - fun_mat_test)
plot(colMeans(fdb_predict_test), type = "b", col = "blue", pch = 3, main ="Residuals - 1000 Iteration", ylab = "Mean Residual", xlab = "t")


