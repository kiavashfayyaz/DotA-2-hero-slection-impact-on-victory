install.packages("dplyr")
library(dplyr)
install.packages("class")
library(class)
install.packages("neuralnet")
library(neuralnet)
install.packages("h2o")
library(h2o)
install.packages("ggplot2")
library(ggplot2)
install.packages("factoextra")
library(factoextra)
install.packages("cluster")
library(cluster)

mymatch <- match[c('match_id', 'radiant_win')]

## data prepration and matching
players %>%
  + count(match_id) %>%
  + filter(n>1)
spread(mymatch, player_slot, hero_id)
mymatch %>%
  + group_by(match_id) %>%
  + summarise_each(funs(mean(., na.rm=TRUE)))
mymatch <- mymatch %>%
  + left_join(matchfilt, by = "match_id")

radiant.hero <- dummy(mymatch$`0`, sep = "_") + dummy(mymatch$`1`, sep = "_") + dummy(mymatch$`2`, sep = "_")
  + dummy(mymatch$`3`, sep = "_") + dummy(mymatch$`4`, sep = "_")

dire.hero <- dummy(mymatch$`128`, sep = "_") + dummy(mymatch$`129`, sep = "_") + dummy(mymatch$`130`, sep = "_")
  + dummy(mymatch$`131`, sep = "_") + dummy(mymatch$`132`, sep = "_")

hero.dummy <- radiant.hero + (dire.hero * -1)
hero_names <- within(hero_names, rm("name"))
hero_names1 <- lapply(hero_names[2:11], function(x) -1 * x)

for (i in 1:10){
  if(i<6){
    role.namefix <- hero_names
    x <- i - 1
    names(role.namefix) <- paste0("slot", as.character(x), "_", names(role.namefix))
    y <- paste0("slot", "x", "_hero_id")
    match.role <- mymatch %>% left_join(hero_names, c(as.character(x) = y))
  } 
  if(i>5){
    role.namefix <- hero_names1
    x <- i + 122
    names(role.namefix) <- paste0("slot", as.character(x), "_", names(role.namefix))
    y <- paste0("slot", "x", "_hero_id")
    match.role <- mymatch %>% left_join(hero_names, c(as.character(x) = y))
  }
}

rad_role = match.role[,1]
dire_role = match.role[,1]

for (i in 1:10) {
  x <- match.role[,(i+12)] + match.role[,(i+22)] + match.role[,(i+32)] + match.role[,(i+42)] + match.role[,(i+52)]
  x1 <- match.role[,i+62] + match.role[,i+72] + match.role[,i+82] + match.role[,i+92] + match.role[,i+102]
  rad_role <- cbind(rad_role,x)
  dire_role <- cbind(dire_role, x1)
}

match.role.agg <- rad_role %>% left_join(dire_role, by = "match_id")
match.role.agg <- match.role.agg %>% left_join(matchfilt, by = "match_id")
role_data <- match.role.agg[,-1]
role_data$radiant_win <- as.integer(as.logical(role_data$radiant_win))

for (i in 1:10) {
  x <- role_data[,i] + role_data[,(i+10)]
  role_data_agg[,i] <- x
}

#comparing the distribution of different roles when their team is winning/losing
plot_role <- function(x){
  ggplot() + geom_density(data = role_data_agg[role_data_agg[,11]==1,], aes(role_data_agg[role_data_agg[,11]==1,x], colour= "Radiant win")) + 
    geom_density(data = role_data_agg[role_data_agg[,11]==0,], aes(role_data_agg[role_data_agg[,11]==0,x], colour= "Dire win")) + 
    scale_colour_manual("", breaks = c("Radiant win","Dire win" ), values=c("red","blue")) + labs(x=colnames(role_data_agg)[x])
}
grid.arrange(plot_role(1), plot_role(2),plot_role(3),plot_role(4),plot_role(5),plot_role(6),plot_role(7),plot_role(8),plot_role(9),plot_role(10),
             nrow= 5,ncol=2)

#clustering heros based on their roles (can we achieve position 1 to 5 based on this data?)
set.seed(120)
p.data <- hero_names[,5:13]
row.names(p.data) <- hero_names[,3]
fviz_nbclust(p.data, kmeans, method = "gap_stat")

km.res <- kmeans(p.data, 6, nstart = 30) # 6 cluster
km.res$centers
p6 <- fviz_cluster(km.res, data = p.data, labelsize = 10) +
  theme(axis.title.x = element_text(face="bold", size=30), axis.text.x  = element_text(face="bold", size=25)) +
  theme(axis.title.y = element_text(face="bold", size=30), axis.text.y  = element_text(face="bold", size=25)) +
  labs(title = '6 clusters') + theme(title = element_text(size=40, face="bold")) +
  theme(legend.title = element_text(size=30, face="bold"), legend.text = element_text(size=25, face="bold"))

km.res <- kmeans(p.data, 5, nstart = 30) # 5 cluster
km.res$centers
p5 <- fviz_cluster(km.res, data = p.data, labelsize = 10) +
  theme(axis.title.x = element_text(face="bold", size=30), axis.text.x  = element_text(face="bold", size=25)) +
  theme(axis.title.y = element_text(face="bold", size=30), axis.text.y  = element_text(face="bold", size=25)) +
  labs(title = '5 clusters') + theme(title = element_text(size=40, face="bold")) +
  theme(legend.title = element_text(size=30, face="bold"), legend.text = element_text(size=25, face="bold")) +
  scale_colour_discrete(breaks=c("1", "2", "3", "4" , "5"), labels=c("sup", "nuker", "carry", "escape-carry", "dis-init"))


#logistic regression for outcome prediction
logistic_reg <- function(x){
  train_log <- x[1:40000,]
  test_log <- x[40001:50000,]
  model_log <- glm(radiant_win ~., family = binomial(link="logit"), data = train_log)
  summary(model_log)
  
  fitted_log <- predict(model_log, newdata = test_log, type = "response")
  fitted_log <- ifelse(fitted_log > .5,1,0)
  misclasError <- mean(fitted_log != test_log$radiant_win, na.rm = TRUE)
  print(paste('Accuracy on test data',1-misclasError))
  
  fitted_log <- predict(model_log, newdata = train_log, type = "response")
  fitted_log <- ifelse(fitted_log > .5,1,0)
  misclasError <- mean(fitted_log != train_log$radiant_win, na.rm = TRUE)
  print(paste('Accuracy on train data',1-misclasError))
} 

logistic_reg(role_data)
logistic_reg(role_data_agg)

#neural network (low speed)
model_nn <- function(x){
  train_role <- x[1:40000,]
  test_role <- x[40001:50000,]
  n <- names(train_role)
  f <- as.formula(paste("radiant_win ~", paste(n[!n %in% "radiant_win"], collapse = " + ")))
  train_role <- na.omit(train_role)
  nn_class_role <- neuralnet(f, data= train_role, hidden = c(5), linear.output = FALSE)
  plot(nn_class_role)
  test_role <- na.omit(test_role)
  pr.nn_classRole <- compute(nn_class_role,test_role[,1:20])
  x <- ifelse(pr.nn_classRole$net.result>.5,1,0)
  eror_nn <- mean(x != test_role$radiant_win, na.rm = TRUE)
  print(paste('Accuracy',1-eror_nn))
}

model_nn(role_data_agg)

hero.dummy <- cbind(hero.dummy, role_data[,21])
colnames(hero.dummy)[112] <- "radiant_win"
hero.dummy <- data.frame(hero.dummy)

logistic_reg(hero.dummy)

#h20 package for deeplearning on hero data/ finding best model
h2o.init()
h2oframHERO <- as.h2o(hero.dummy)
output <- "radiant_win"
split <- h2o.splitFrame( h2oframHERO, c(0.6, 0.2), seed = 1234 )
train <- h2o.assign( split[[1]], "train" ) # 60%
valid <- h2o.assign( split[[2]], "valid" ) # 20%
test  <- h2o.assign( split[[3]], "test" )  # 20%

hyper_hero <- list(hidden = list(c(50,20,10), c(100,50,100)))

nn_modelgrid <- h2o.grid(
  algorithm = "deeplearning",x = names(train)[1:111],y= output, training_frame = train, 
  validation_frame = valid, activation = "RectifierWithDropout", input_dropout_ratio = 0.2, sparse = TRUE, epochs = 1000,
  stopping_metric = "misclassification", hyper_params = hyper_hero
)

sorted_nn <- h2o.getGrid(nn_modelgrid, sort_by = stopping_metric, decreasing = TRUE)
best_nn_model <- h2o.getModel(sorted_nn@model_ids[[1]])
pred_test_Hero <- h2o.predict(best_nn_model, test)
pred_test_Hero <- ifelse(pred_test_Hero$predict > 0.5, 1, 0)
test_error <- mean(pred_test_Hero != test$radiant_win, na.rm = TRUE)
print(paste('Accuracy',1- test_error))
plot(best_nn_model)

# KNN classification
accuracy <- rep(0,10)
k_neigh <- rep(0,10)

for (i in 1:10) {
  x <- i * 5
  pred <- knn(train_hero[,1:111], test_hero[,1:111], train_hero[,112],k = x)
  accuracy[i] <- mean(pred == test_hero[,112])
  k_neigh[i] <- x
}
plot(k_neigh, accuracy, type = 'b')

