# LONG MODEL

# TESTS
library(FDboost)
library(compboost)
library(microbenchmark)

# ____________________________________________________________________________________________________________________________________ #


# Data Creation
set.seed(-753)
obs = 100
t_length_min = 100
t_length_max = 300

grid_list = list()
grid_list_FDb = list()
for(i in 1:obs){
  grid_list[[i]] = matrix(sort(runif(round(runif(1, t_length_min, t_length_max)),-2,2)), nrow = 1)
  grid_list_FDb[[i]] = as.vector(grid_list[[i]])
}

unlist(lapply(grid_list, length))

df_test_long = data.frame(a = rnorm(length(grid_list)), b = rexp(length(grid_list)))

fun_list = list()
for(i in 1:length(grid_list)){
  fun_list[[i]] = as.matrix((100*sqrt(abs(df_test_long$a[i]*grid_list[[i]] - 0.01*df_test_long$b[i]^2)) 
    + 0.01 * abs(df_test_long$b[i]*grid_list[[i]] +10)))
  -grid_list[[i]]
}

plot(x = grid_list[[i]], y = fun_list[[i]], xlim = c(min(unlist(grid_list)), max(unlist(grid_list))), 
  ylim = c(min(unlist(fun_list)),max(unlist(fun_list))), type = "l")
for(i in 1:nrow(df_test_long)){
  lines(x = grid_list[[i]], y = fun_list[[i]], col = i)
}

fdb_long = list(fun = unlist(fun_list),
  t = unlist(grid_list),
  a = df_test_long$a,
  b = df_test_long$b,
  ONE = rep(1, length(df_test_long$a)),
  id = rep(1:length(fun_list), unlist(lapply(fun_list, length))) )



# ____________________________________________________________________________________________________________________________________ #

# Intercept Model


response_test = ResponseFDALong$new(target = "test", response = fun_list, grid = grid_list)
comp_test = Compboost$new(data = df_test_long, target = response_test, loss = LossQuadratic$new(), learning_rate = 0.3)
comp_test$addBaselearner(feature = "a",id = "P1",bl_factory = BaselearnerPSpline, degree = 3, n_knots = 20, differences = 2, penalty = 10)
comp_test$train(iteration = 100) 

fdb_model = FDboost(formula = fun ~ bbs(a, knots = 20, degree = 3, lambda = 10),
  timeformula = ~bbs(t, lambda = 0, knots = 25, degree = 3, differences = 2), data = fdb_long, id = ~id,
  control = boost_control(mstop = 100, nu = 0.3), offset = "scalar")
fdb_model$coef()[[1]]
comp_test$model$getEstimatedParameter()[[1]]


est_comp = data.frame(FDb_1 = fdb_model$coef()[[1]],
  CPb_1 = comp_test$model$getEstimatedParameter()[[1]])
est_comp

par(mfrow = c(1,1))
plot(est_comp$FDb_1, type = "b", pch = 5, ylim = c(min(est_comp[,1:2]),max(est_comp[,1:2])), col = "red", main = "Parameterrs")
points(est_comp$CPb_1, type = "b", pch = 3, col = "blue")
legend("top",legend = c("FDb","CPb"), fill = c("red","blue"))
par(mfrow = c(1,1))



comp_test$plot("a_P1")

plot(fdb_model)








