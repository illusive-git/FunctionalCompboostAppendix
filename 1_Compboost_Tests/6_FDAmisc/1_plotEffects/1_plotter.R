# ----------------------------------------------------------------------------------------------------------------------------------------------- #

library(compboost)
library(FDboost)
set.seed(-753)

## -------------------------------------- 
obs = 500
t_length = 100
df_train = data.frame(a = rnorm(obs), ONE = 1)
grid_test = matrix(seq(-2,2, length.out = t_length), ncol = t_length)
fun_mat = matrix(NA, ncol = ncol(grid_test),nrow = nrow(df_train))

for(i in 1:nrow(df_train)){
  fun_mat[i,] =  grid_test[1,]^4 - sqrt(abs(df_train$a[i]*10))*grid_test[1,]^2 - 5*sin(grid_test[1,])
}
plot(x = grid_test[1,], y = fun_mat[1,], xlim = c(min(grid_test),max(grid_test)), ylim = c(min(fun_mat),max(fun_mat)), type = "l",
  xlab = "Grid", ylab = expression(paste('Y'[i],'(Grid)')), main = "Observed Functions")
for(i in 1:nrow(df_train)){
  lines(x = grid_test[1,], y = fun_mat[i,], col = i)
}

fdb_list = list(fun_mat = fun_mat,
  t = grid_test[1,],
  a = df_train$a,
  b = df_train$b,
  ONE = rep(1,length(df_train$a)))


## -------------------------------------- 
# Plot Polynomial Addtivive Effects

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

comp_test = Compboost$new(data = df_train, target = response_test, loss = LossQuadratic$new(),
  time_spline_pars = list(n_knots = 3, degree = 3, differences = 2, penalty = 10), learning_rate = 0.3)

comp_test$addBaselearner(feature = "a",id = "P1",bl_factory = BaselearnerPolynomial)
comp_test$train(100, trace = 0) 

fdb_model = FDboost(formula = fun_mat ~ bols(a), 
  timeformula = ~bbs(t,  knots = 3, degree = 3, lambda = 10), data = fdb_list, 
  control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")

comp_test$plot("a_P1")

plot(fdb_model)


## -------------------------------------- 
# Plot Spline Addtivive Effects

response_test = ResponseFDA$new(target = "test", fun_mat, grid = grid_test)

comp_test = Compboost$new(data = df_train, target = response_test, loss = LossQuadratic$new(),
  time_spline_pars = list(n_knots = 3, degree = 3, differences = 2, penalty = 10), learning_rate = 0.3)

comp_test$addBaselearner(feature = "a",id = "P1",bl_factory = BaselearnerPSpline, penalty = 10, n_knots = 4, degree = 3)
comp_test$train(100, trace = 0) 

fdb_model = FDboost(formula = fun_mat ~ bbs(a, lambda = 10, knots = 4, degree = 3), 
  timeformula = ~bbs(t,  knots = 3, degree = 3, lambda = 10), data = fdb_list, 
  control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")

comp_test$plot("a_P1")

plot(fdb_model)



