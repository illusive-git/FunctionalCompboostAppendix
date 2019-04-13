#
library(compboost)
library(FDboost)

# ----------------------------
# Creating Data
set.seed(-753)
obs = 1000
df_train = data.frame(
  Age = round(runif(n = obs, 18, 99)),
  Height = rnorm(obs, 165, 10)
)
fun_age = function(x){ ( 0.00001*x^4 - x^2 + 1/x) / 10000 }
fun_height = function(x){( sin(x) )}
df_train$target = fun_age(df_train$Age) + fun_height(df_train$Height) + rnorm(100,0,0.1)

# INSIDE MODELS

# one iteration
cp = Compboost$new(data = df_train, target = "target",loss = LossQuadratic$new(), learning_rate = 0.3)
cp$addBaselearner(feature = "Age", id = "spline", bl_factory = BaselearnerPSpline, degree = 3, n_knots = 14, differences = 2, df = 4)
cp$train(1000)

mb = mboost(formula = target ~ bbs(Age, degree = 3, knots = 14, differences = 2, df = 4), 
  data = df_train, control = boost_control(mstop = 1000, nu = 0.3))


plot(coef(mb)[[1]], col = "red", pch = 5, main = "Beta Parameters - 1 Iteration", ylab = expression(paste(beta,"-value")))
points(cp$getEstimatedCoef()[[1]], col = "blue", pch = 3)
legend("topright", legend = c("FDboost","Compboost"), col = c("red","blue"),pch = c(5,3))

# Differences
fdb_predict = mb$predict()
cpb_predict = cp$predict()

mean( (fdb_predict - cpb_predict)^2)





