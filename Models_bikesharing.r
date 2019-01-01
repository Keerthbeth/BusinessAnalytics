library(pacman)
p_load(dygraphs, ggplot2, MASS, ggplot2, gbm, xgboost, plyr,zoo, rattle, caret,dplyr,xlsx)

data= read.csv("E:/EBAC/6 Data Analytics/Assignment 1 Barry/Data_final.csv")


######### Defining the Metrics Function ########################

Profit <- function(x,y)
{
  x=data.frame(x)
  y=data.frame(y)
  #z= sqrt(mean((x - y)^2))
  return(sum(3*pmin(x,y)- 2*y))
}

rmse <- function(x,y)
{
  sqrt (mean((x - y)^2, na.rm=TRUE))
}


######### Spilting into test & train  ###########################
traindata= data[22:365,-1]
testdata= data[366:730,-1]


traindata= traindata[,c("season","holiday","AR","workingday","weathersit","atemp","hum","windspeed","cnt_lag_1","cnt_lag_2","cnt_lag_3","cnt_lag_4","cnt_lag_5","r_cnt_mov_avg_7","r_cnt_mov_avg_2","r_cnt_mov_avg_21","cnt_lag_6","cnt_lag_7","cnt_lag_8","weekly_trend", "cnt_tom")]
testdata= testdata[,c("season","holiday","AR","workingday","weathersit","atemp","hum","windspeed","cnt_lag_1","cnt_lag_2","cnt_lag_3","cnt_lag_4","cnt_lag_5","r_cnt_mov_avg_7","r_cnt_mov_avg_2","r_cnt_mov_avg_21","cnt_lag_6","cnt_lag_7","cnt_lag_8","weekly_trend", "cnt_tom")]

cols <- c("season","holiday","workingday","weathersit")
traindata[cols] <- lapply(traindata[cols], factor)
testdata[cols] <- lapply(testdata[cols], factor)


######### Neural Network ####################################

Neural= function(x,y)
{
  crs$dataset <- x
  testdata_NN=y
  
  library(nnet, quietly=TRUE)
  
  #============================================================
  # Rattle timestamp: 2018-08-30 15:52:32 x86_64-w64-mingw32 
  
  # Note the user selections. 
  
  # The following variable selections have been noted.
  
  crs$input     <- c("season", "holiday", "AR","workingday", "weathersit", "atemp",
                     "hum", "windspeed", "cnt_lag_1", "cnt_lag_2", "cnt_lag_3",
                     "cnt_lag_4", "cnt_lag_5", 
                     #"r_cnt_mov_avg_7",
                     # "r_cnt_mov_avg_2", 
                     "r_cnt_mov_avg_21", 
                     "cnt_lag_6", "cnt_lag_7", "cnt_lag_8", "weekly_trend")
  
  crs$numeric   <- c("atemp", "hum", "windspeed", "cnt_lag_1", "cnt_lag_2",
                     "cnt_lag_3", "cnt_lag_4", "cnt_lag_5",
                     #"r_cnt_mov_avg_7",
                     # "r_cnt_mov_avg_2",
                     "r_cnt_mov_avg_21", "AR",
                     "cnt_lag_6", "cnt_lag_7", "cnt_lag_8", "weekly_trend")
  
  crs$categoric <- c("season", "holiday", "workingday", "weathersit")
  
  crs$target    <- "cnt_tom"
  crs$risk      <- NULL
  crs$ident     <- NULL
  crs$ignore    <- NULL
  crs$weights   <- NULL
  
  #============================================================
  # Rattle timestamp: 2018-08-30 15:52:36 x86_64-w64-mingw32 
  
  # Neural Network 
  
  # Build a neural network model using the nnet package.
  
  library(nnet, quietly=TRUE)
  
  # Build the NNet model.
  
  set.seed(199)
  crs$nnet <- nnet(cnt_tom ~ .,
                   data=crs$dataset[,c(crs$input, crs$target)],
                   size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)
  pred_NN= predict(crs$nnet, newdata= y[, crs$input])
  pred_NN
}

pred_NN=Neural(traindata, testdata)
pred_trainNN= Neural(traindata, traindata)

Profit(testdata$cnt_tom, pred_NN)
rmse(testdata$cnt_tom, pred_NN)
plot(testdata$cnt_tom, type='l')
lines(pred_NN, col='blue')

plot(testdata$cnt_tom, pred_NN, xlab='Actual Values', ylab='Predicted Values')


######### Linear Regression #######################

fm1= cnt_tom ~.
summary(ols <- lm(fm1, data = traindata))

#Removing variables with less p values
fm2 = cnt_tom~season+holiday+weathersit+r_cnt_mov_avg_21 +AR
ols <- lm(cnt_tom~season+holiday+weathersit+r_cnt_mov_avg_21 +AR, data=traindata)
summary(ols)
pred_Linear= predict(ols, testdata)
pred_trainLinear= predict(ols, traindata)
Profit(testdata$cnt_tom, pred_Linear)
rmse(testdata$cnt_tom, pred_Linear)
plot(testdata$cnt_tom, pred_Linear, xlab='Actual Values', ylab='Predicted Values')

######### Robust Regression #######################

rr.bisquare <- rlm(fm2, traindata, psi=psi.bisquare,  wt.method = c("inv.var", "case"))
summary(rr.bisquare)
pred_Robust =predict(rr.bisquare, testdata)
pred_trainRobust =predict(rr.bisquare, traindata)
Profit(testdata$cnt_tom, pred_Robust)
rmse(testdata$cnt_tom, pred_Robust)
plot(testdata$cnt_tom, pred_Robust, xlab='Actual Values', ylab='Predicted Values')

######### XGboost ###################################

Training= traindata
Test= testdata

labels <- Training$cnt_tom
ts_label <- Test$cnt_tom
new_tr <- model.matrix(~.+0,data = Training[-21]) 
new_ts <- model.matrix(~.+0,data = Test[-21])
new_tr <- as.matrix(new_tr)
dtrain <- xgb.DMatrix(data = new_tr,label = labels)
dtest <- xgb.DMatrix(data = new_ts,label=ts_label)
set.seed(2)
xgbFit = xgboost(data = new_tr, label= Training$cnt_tom,
                 nrounds= 1000, objective = "reg:linear", eval_metric = "rmse",
                 booster='gblinear', nthread=4, gamma=0.01, subsample=0.5, alpha=1,
                 num_parallel_tree=10, verbose = FALSE)

pred_xg <- predict(xgbFit, new_ts)
pred_trainxg= predict(xgbFit, new_tr)
rmse(Test$cnt_tom,pred_xg)
Profit(Test$cnt_tom, pred_xg)

######### Ensemble ################################

pred_ensem= (pred_NN+ pred_Linear+pred_Robust+ pred_xg)/4
rmse(Test$cnt_tom,pred_ensem)
Profit(Test$cnt_tom, pred_ensem)

pred_ensem_1= (0.5*pred_NN+ 0.0*pred_Linear+0.5*pred_Robust+ 0.0*pred_xg)
rmse(Test$cnt_tom,pred_ensem_1)
Profit(Test$cnt_tom, pred_ensem_1)


######### Stacked Model##############################

train_stack= as.data.frame(cbind(pred_trainNN, pred_trainRobust, pred_trainxg, pred_trainLinear,traindata$cnt_tom))
test_stack =  as.data.frame(cbind(pred_NN, pred_Robust, pred_xg, pred_Linear,testdata$cnt_tom))
colnames(train_stack) <- c("NN", "Robust", "Xgboost", "Linear", "cnt_tom")
colnames(test_stack) <- c("NN", "Robust", "Xgboost", "Linear", "cnt_tom")


profit <- function(data, lev = NULL, model = NULL) {
  profit <- Profit(data$obs,data$pred)
  c(Profit = profit)
}
#-----------------------------------------------------------------------------------------------------------------------------

xgb_tree_grid_1 = expand.grid(
  nrounds = 1000,
  #alpha = c(0,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1),
  #lambda = c(0,0.025,0.05,0.075,0.1,0.25,0.5,0.75,1)
  max_depth = c(1,2,3,4),
  eta = c(0.01,0.05, 0.001,0.005, 0.0001,0.0005),
  gamma = 0,
  colsample_bytree = .7,
  min_child_weight = 1,
  subsample = c(.8, 1)
  #size = c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
  #parameter = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  #link = c('log','sqrt','identity')
  #sigma = c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,2,5,10,23)
  #degree = c(1,2,3,4,5),
  #scale = c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1)
  #C = c(0.1,0.5,1,5,10,100,500,1000,1500,3000,6000,9000,10000,12000,15000,20000,23000,30000)
  #fraction = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
  #decay = c(0.01,0.02,0.05,0.075,0.1,0.2,0.5,0.75,1)
)

# pack the training control parameters
xgb_tree_trcontrol_1 = trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  allowParallel = TRUE,
  summaryFunction = profit
)

library(caret)
# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_tree_model = train(
  x = as.matrix(test_stack[,1:4]),
  y = test_stack$cnt_tom,
  trControl = xgb_tree_trcontrol_1,
  tuneGrid = xgb_tree_grid_1,
  method = "xgbTree",
  metric = "Profit",
  maximize = TRUE
)

plot(xgb_tree_model)
xgb_tree_model$bestTune

pred_stack<-caret::predict.train(xgb_tree_model,test_stack[,1:4])
Profit(testdata$cnt_tom,pred_stack)
rmse(testdata$cnt_tom, pred_stack)
plot(testdata$cnt_tom,pred_stack)

write.csv(y, 'old prediction.csv')
Final = cbind(as.numeric(testdata$cnt_tom), as.numeric(pred_stack))
write.csv(Final, 'new prediction.csv')
rmse(testdata$cnt_tom,preds)
Expenditure= 2*preds

