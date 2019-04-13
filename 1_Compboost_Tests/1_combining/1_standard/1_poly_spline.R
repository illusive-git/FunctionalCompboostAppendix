#
library(compboost)
library(FDboost)

# ----------------------------
# Creating Data
set.seed(-753)
obs = 10000
df_train = data.frame(
  Age = round(runif(n = obs, 18, 99)),
  Height = rnorm(obs, 165, 10)
)
fun_age = function(x){ ( 0.00001*x^4 - x^2 + 1/x) / 10000 }
fun_height = function(x){( sin(x) )}
df_train$target = fun_age(df_train$Age) + fun_height(df_train$Height) + rnorm(100,0,0.1)

par(mfrow = c(1,2))
grid_Age = seq(min(df_train$Age),max(df_train$Age), by =0.01)
grid_Height = seq(min(df_train$Height),max(df_train$Height), by =0.01)
plot(grid_Age,fun_age(grid_Age), type = "l", main = "Additive effect of Age")
plot(grid_Height,fun_height(grid_Height),type = "l", main = "Additive effect of Height")
par(mfrow = c(1,1))

# OUTSIDE MODELS
# Baselearner 2
data_source_1 = InMemoryData$new(as.matrix(df_train$Age), "my_variable_2")
data_target_1 = InMemoryData$new()
linear_factory_1 = BaselearnerPolynomial$new(data_source_1, data_target_1, list(degree = 1, intercept = TRUE))
cp_Age = linear_factory_1$getData()

# time spline
data_source_2 = InMemoryData$new(as.matrix(df_train$Height), "my_variable_2")
data_target_2 = InMemoryData$new()
spline_factory_2 = BaselearnerPSpline$new(data_source_2, data_target_2, list(degree = 3, n_knots = 14, differences = 2, penalty = 0))
cp_Height = spline_factory_2$getData()

mb_Age = with(df_train, extract(bols(Age, intercept = TRUE), "design"))
mb_Height = as.matrix(with(df_train, extract(bbs(Height, degree = 3, knots = 14, differences = 2, lambda = 0), "design")))

all(mb_Age == cp_Age)
all(mb_Height == cp_Height)


mb_combined = as.matrix(with(df_train, extract(bols(Age, intercept = TRUE)
  %X% bbs(Height, degree = 3, knots = 14, differences = 2, lambda = 0), "design")))

combined = BaselearnerCombined$new(linear_factory_1, spline_factory_2, "test")

cp_mat = combined$getData()


all(mb_combined == cp_mat)

# INSIDE MODELS

# one iteration
cp = Compboost$new(data = df_train, target = "target",loss = LossQuadratic$new(), learning_rate = 0.3)
cp$addBaselearner(feature = "Age", id = "ols", bl_factory = BaselearnerPolynomial, intercept = TRUE, degree = 1)
cp$addBaselearner(feature = "Height", id = "spline", bl_factory = BaselearnerPSpline, degree = 3, n_knots = 21, differences = 2, penalty = 0)
cp$CombineBaselearners(bl1 = "Age_ols", bl2 = "Height_spline", keep = FALSE)
cp$train(1)

mb = mboost(formula = target ~ bols(Age, intercept = TRUE, lambda = 0) %X% bbs(Height, degree = 3, knots = 21, differences = 2, lambda = 0), 
  data = df_train, control = boost_control(mstop = 1, nu = 0.3))


plot(coef(mb)[[1]], col = "red", pch = 5, main = "Beta Parameters - 1 Iteration", ylab = expression(paste(beta,"-value")), ylim = c(-5,5))
points(cp$getEstimatedCoef()[[1]], col = "blue", pch = 3)
legend("topright", legend = c("FDboost","Compboost"), col = c("red","blue"),pch = c(5,3))

# Differences
fdb_predict = mb$predict()
cpb_predict = cp$predict()

mean( (fdb_predict - cpb_predict)^2)



# ten iterations
cp = Compboost$new(data = df_train, target = "target",loss = LossQuadratic$new(), learning_rate = 0.3)
cp$addBaselearner(feature = "Age", id = "ols", bl_factory = BaselearnerPolynomial, intercept = TRUE, degree = 1)
cp$addBaselearner(feature = "Height", id = "spline", bl_factory = BaselearnerPSpline, degree = 3, n_knots = 14, differences = 2, penalty = 0)
cp$CombineBaselearners(bl1 = "Age_ols", bl2 = "Height_spline", keep = FALSE)
cp$train(1000)

mb = mboost(formula = target ~ bols(Age, intercept = TRUE, lambda = 0) %X% bbs(Height, degree = 3, knots = 14, differences = 2, lambda = 0), 
  data = df_train, control = boost_control(mstop = 1000, nu = 0.3))


plot(coef(mb)[[1]], col = "red", pch = 5, main = "Beta Parameters - 1000 Iteration", ylab = expression(paste(beta,"-value")))
points(cp$getEstimatedCoef()[[1]], col = "blue", pch = 3)
legend("topright", legend = c("FDboost","Compboost"), col = c("red","blue"),pch = c(5,3))


# Differences
fdb_predict = mb$predict()
cpb_predict = cp$predict()

mean( (fdb_predict - cpb_predict)^2)







