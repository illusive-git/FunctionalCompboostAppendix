# TESTS
library(mboost)
library(compboost)
library(Matrix)
# mboost Case
source("~/Documents/3_LMU/2_Master/1_Code/4_tensors/baselearner_minus.R")


# Compboost Case
data("cars")
df_train = cars


# ------------------------------------------------------------------------------------------------------------------------ #
# Define Baslearners

# OUTSIDE MODELS
# Speed Spline
data_source_1 = InMemoryData$new(as.matrix(df_train$speed), "my_variable_2")
data_target_1 = InMemoryData$new()
speed_4_factory = BaselearnerPSpline$new(data_source_1, data_target_1, list(n_knots = 20, degree = 3, df = 4))
speed_4 = speed_4_factory$getData()

# Speed OLS
data_source_2 = InMemoryData$new(as.matrix(df_train$speed), "my_variable_2")
data_target_2 = InMemoryData$new()
speed_bols_factory = BaselearnerPolynomial$new(data_source_2, data_target_2, list(degree = 1, intercept = TRUE))
speed_bols = speed_bols_factory$getData()

centered = BaselearnerCentered$new(speed_4_factory, speed_bols_factory, "Test")

dim(centered$getData())
dim(mb_mat)
diff = mb_mat-centered$getData()
sum(diff)




# ------------------------------------------------------------------------------------------------------------------------ #
# Define Baslearners

# Hand version by Almond
mb_minus = mboost(formula = dist ~ bbs(speed, degree = 3, knots = 20, differences = 2, df = 4) %-% bols(speed, df = 2)  
  + bols(speed), data = df_train, control = boost_control(mstop = 100, nu = 0.3))
extract(mb_minus,what = "lambda")
mb_mat_minus = extract(mb_minus)[[1]]

# Mboost internal version
mb_center = mboost(formula = dist ~ bbs(speed, degree = 3, knots = 20, differences = 2, df = 2, center = TRUE) + bols(speed), data = df_train,
  control = boost_control(mstop = 100, nu = 0.3))
extract(mb_center,what = "lambda")
mb_mat_center = extract(mb_center)[[1]]

# Compboost Version
cb = Compboost$new(data = cars, target = "dist",loss = LossQuadratic$new(), learning_rate = 0.3)
cb$addBaselearner(feature = "speed", id = "spline", bl_factory = BaselearnerPSpline,  degree = 3, n_knots = 20, df = 4, differences = 2)
cb$addBaselearner(feature = "speed", id = "poly", bl_factory = BaselearnerPolynomial,  degree = 1)
cb$CenterBaselearner(bl_target = "speed_spline", bl_center = "speed_poly", keep = FALSE)
cb$train(100, trace = 0)


cb_eff = cb$getFeatEffectData(blearner_name = "speed_spline_|_speed_poly")

table(selected(mb_minus))
table(selected(mb_center))
table(cb$getSelectedBaselearner())


plot(cb_eff$feature, cb_eff$effect,type = "l", col = "blue", main = "Effect of Centered Learner", xlab = "speed", ylab = "Additive Effect",
  ylim = c(-10,17))
points(sort(cars$speed), predict(mb_center, which = 1)[order(cars$speed)], t = "b", col="black")
points(sort(cars$speed), predict(mb_minus, which = 1)[order(cars$speed)], t = "b", col="red")
legend("top",legend = c("mboost %-%","mboost center","compboost"), fill = c("black","red","blue"))



plot(mb_center, which = 1)
points(sort(cars$speed), predict(mb_minus, which = 1)[order(cars$speed)], t = "b", col="red")







