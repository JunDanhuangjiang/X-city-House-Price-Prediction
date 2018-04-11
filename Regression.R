library(ISLR)
library(leaps)
library(tree)
library(randomForest)
library(gbm)
library(caret)
HouseData<-read.csv(file.choose(),na.strings = c("NA",""))
# next step is feature selection and Na elimination, I deleted over 8000 data points
# which is 7325 Na in "effective_build_year", 822 Na in "lot_size", 658 Na in "Site_coverage"
# 2 in "fully_complete" and 1 in "market_building". The rest data set size is 62716
temp2 = Hou_Data
summary(is.na(temp2))
na=is.na(temp2$effective_build_year) # numerical feature we can't leave Na as a class remaining in this feature 
Na_posi = which(na)
temp2 = temp2[-Na_posi,]

# string extract
x<-temp2$lot_size # most important feature for model building
x<-matrix(x)
col1<-str_sub(x, 1,6) # 6 characters
col2<-str_extract(x, "\\d+") # number before decimal point
mode(col1)
col1<-as.numeric(col1) 
col1<-matrix(col1)
col2<-as.numeric(col2)
col2<-matrix(col2)
na<-is.na(col1)
sum(na)
p<-which(na) # numbers like 678 in 150-th row will become NA when extract
p[1]
col1[p[1],]
col2[p[1],]
col1[p,]<-col2[p,]
na<-is.na(col1)
sum(na)
temp2$lot_size <- col1 # give all the transfered lot_size values to temp

# feature transfer by"2016 - built_year"
b=(temp2$effective_build_year)
b=matrix(b)
b=as.numeric(b)
for (i in (1:length(b))){
  
  b[i]=2016-b[i]
}
temp2$effective_build_year=b
# feature transfer "20" to "0.2"
j=temp2$site_coverage
j=as.numeric(j)
col2=str_extract(j, "\\d+")
col2=as.numeric(col2)
j=col2*0.01
temp2$site_coverage=j
# Creating new a feature based on GPS 
Geo.Trans = function(x,y){
  
  Tra = atan(y/x)* (x^2 + y^2)^0.5
  
  return(Tra*10000)
} 
g = Geo.Trans(-113.4712,53.49639)
for (i in c(1:length(temp2$lat))) {
  g[i] = Geo.Trans(temp2$lon[i],temp2$lat[i])
}
# normalization for the new feature
x = range(g)
min = x[1]
max = x[2]
out = vector(length = length(Hou_Data$lat))
j = max - min
for (i in (1:length(g))) {
  out[i] = (1-0)*(g[i] - min)/j 
}

range(out)
hist(out)
Hou_Data = data.frame(Hou_Data,out)

# clustering based on lot_size feature

x= data.frame(temp2$lot_size,y=temp2$assessed_value)
colnames(x) <- c("size", "value")
ssratio = vector(length = 8)
for(k in 1:length(ssratio)) {
  fit = kmeans(x, k, nstart = 25)  # Tries numerous random starts
  ssratio[k] = fit$betweenss/fit$totss
}
plot(ssratio)
(cl <- kmeans(x, 3)) # choose cluster size 5
plot(x, col = cl$cluster)
cluster_vector=cl$cluster
cluster_vector=as.factor(cluster_vector)

posi=which(cluster_vector==1)
class1=temp2[posi,]
posi=which(cluster_vector==2)
class2=temp2[posi,]
posi=which(cluster_vector==3)
class3=temp2[posi,]
summary(class1$lot_size)
quantile(class1$lot_size,c(0.93))
posi=which(class1$lot_size<1351.2) # manually reclass to get a gaussian distribution of
                                   # lot_size.This class mainly focus on normal residence house

hist(class1$lot_size[posi],xlab = "lot_size(<1351.2)") # visualize
plot(log10(class1$lot_size),log10(class1$assessed_value))

# based on class1, we re-cluster the rest points
posi = row.names(class1)
posi = as.numeric(posi)
class2_5 = Hou_Data[-posi,] # all these row.name is inherited from original dataset
posi = which(is.na(class2_5$market_building_class))
class2_5 = class2_5[-posi,]
j = class2_5$assessed_value
j = as.numeric(as.character(class2_5$assessed_value))
class2_5$assessed_value = j

summary(class2_5$property_type)
posi = which(class2_5$property_type=="AGRICULTURE"|class2_5$property_type=="INDUSTRIAL")
class2 = class2_5[posi,] # class2 is data mainly focusing on AGRICULTURE and INDUSTRIAL area
class3 = class2_5[-posi,] #  class3 is data mainly focusing on very large lot_size residence house

# normalization to minimize the effect for large-value data
# y= (ymax-ymin)*(x-xmin)/(xmax-xmin) + ymin, where ymin = 0, ymax = 1
x = range(outliner$tot_gross_area_description)
min = x[1]
max = x[2]
out = vector(length = length(outliner$tot_gross_area_description))
j = max - min
for (i in (1:length(outliner$tot_gross_area_description))) {
  out[i] = (1-0)*(outliner$tot_gross_area_description[i] - min)/j 
}
range(out)
outliner$tot_gross_area_description = out
# Normalizing the net_area and tot_gross as well.




# fitting model for class1
c1=data.frame(size=class1$lot_size,tot=class1$tot_gross_area_description,
             net=class1$net_area,build_year=class1$effective_build_year,
             out= class1$out,assessed_value=class1$assessed_value)

range(c1$size)
range(c1$net)

# Normalization for class
x = range(c1$size)
min = x[1]
max = x[2]
out = vector(length = length(c1$size))
j = max - min
for (i in (1:length(c1$size))) {
  out[i] = (1-0)*(c1$size[i] - min)/j 
}
range(out)
c1$size = out

x = range(c1$net)
min = x[1]
max = x[2]
out = vector(length = length(c1$net))
j = max - min
for (i in (1:length(c1$net))) {
  out[i] = (1-0)*(c1$net[i] - min)/j 
}
range(out)
c1$net = out

x = range(c1$tot)
min = x[1]
max = x[2]
out = vector(length = length(c1$tot))
j = max - min
for (i in (1:length(c1$tot))) {
  out[i] = (1-0)*(c1$tot[i] - min)/j 
}
range(out)
c1$tot = out
View(c1[1:10,])


# PCA for visualize
pr.out=prcomp(c1, scale=TRUE)
biplot(pr.out, scale=0)
pr.out$rotation
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$rotation
pr.var=pr.out$sdev^2 
pr.var/sum(pr.var) # PVE

# Fitting Regression Trees


set.seed(1)
Train<-sample(nrow(c1),nrow(c1)*0.9)
Test<-c1[-Train,]
Train<-c1[Train,]
# split Train and test for class1 because it's too large
set.seed(1)
train<-sample(nrow(Train),nrow(Train)*0.5)
test = sample(nrow(Test),nrow(Test)*0.5)
# This below only for class1 rf test
Test1<-Test[test,]
Test2<-Test[-test,]
Train1 = Train[train,]
Train2 = Train[-train,]
set.seed(1)
tree.house=tree(Train$assessed_value~.,Train)
summary(tree.house)
plot(tree.house)
text(tree.house,pretty=0)
yhat=predict(tree.house,newdata=Test)
j = (yhat-Test$assessed_value)^2 
mean(j)  # class1: 1.5e10 class2: 2.4e13 class3: 3.4e13
plot(yhat,Test$assessed_value) 
abline(0,1)
cv.house=cv.tree(tree.house)
cv.house
plot(cv.house$size,cv.house$dev,type='b')
prune.house=prune.tree(tree.house,best=7)
plot(prune.house)
text(prune.house,pretty=0)
yhat=predict(prune.house,newdata=Test)
j = (yhat-Test$assessed_value)^2 
mean(j)  # 1.6e10 

# Boosting
# since boosting might be overfitting if the # of new trees are too large,
# performing CV to select the optimal #
folds= createFolds(y=Train$size,k=5)
CVerror=0
ref=matrix(rep(0),1,5)
set.seed(2)
for(j in (1:5)){  # j is from 1-10, 10 numbers
  for(i in 1:5){  
    traindata=Train[-folds[[i]],]  
    validationdata=Train[folds[[i]],]  
    
    boost.house = gbm(traindata$assessed_value~.,data=traindata,distribution="gaussian",
                      n.trees=j*1000,interaction.depth=1) 
    
    yhat.boost=predict(boost.house,newdata=validationdata,n.trees=j*1000)
    
    CVerror = CVerror + mean((yhat.boost - validationdata$assessed_value)^2)
    
  }  
  ref[1,j]=CVerror/5
  CVerror=0
}

which.min(ref)
ref[5] # class1:1.5e10  class2: 1.3e13

#boost.house=gbm(Train$assessed_value~.,data=Train,distribution="gaussian",n.trees=1000,interaction.depth=3)
#summary(boost.house)
#boost.house
#yhat.boost=predict(boost.house,newdata=Test,n.trees=1000)
#j = mean((yhat.boost-Test$assessed_value)^2)  # class1:2e10 class2: 4.5e13, 
# class3: 3.0e13
#range(j)
#j = (exp(yhat.boost) - exp(Test$assessed_value))^2
#mean(j) # MSE:4e10,  for class2

# bagging and randomforest

set.seed(2)
bag.house=randomForest(Train1$assessed_value~.,data=Train1,mtry=5,importance=TRUE)
bag.house
yhat.bag = predict(bag.house,newdata=Test1)
plot(yhat.bag, Test1$assessed_value)
abline(0,1)
mean((yhat.bag-Test1$assessed_value)^2) #class1: (5.8e9 + 8.2e9)/2

rf.house=randomForest(Train1$assessed_value~.,data=Train1,importance=TRUE)
rf.house
yhat.rf = predict(rf.house,newdata=Test1)
j = (yhat.rf-Test1$assessed_value)^2  # class1:(5.6e9+7.9e9)/2 class2: 2.5e13 
                                      # class3: 3.0e13
mean(j)
rf.house$importance

# class 1 is too large, I have to seperate it into two parts for rf, otherwise there will be an error
set.seed(2)
bag.house=randomForest(Train2$assessed_value~.,data=Train2,mtry=5,importance=TRUE)
bag.house
yhat.bag = predict(bag.house,newdata=Test2)
plot(yhat.bag, Test2$assessed_value)
abline(0,1)
mean((yhat.bag-Test2$assessed_value)^2) #class1: 7e9

rf.house=randomForest(Train2$assessed_value~.,data=Train2,importance=TRUE)
rf.house
yhat.rf = predict(rf.house,newdata=Test2)
j = (yhat.rf-Test2$assessed_value)^2  # class1:(5.6e9+7.9e9)/2 class2: 2.5e13 
                                      # class3: 3.0e13
mean(j)
rf.house$importance




# fitting model for class2
c2=data.frame(neigh = class2$neighbourhood ,size=class2$lot_size,tot=class2$tot_gross_area_description,
              landuse = class2$landuse_description,net=class2$net_area,build_year=class2$effective_build_year,
              out= class2$out,assessed_value=class2$assessed_value)

range(c2$size)
range(c2$net)
c2$neigh = as.numeric(c2$neigh) 
c2$landuse = as.numeric(c2$landuse)
c2$landuse[1:10] # normalization for this land information 

# Normalization for class
x = range(c2$size)
min = x[1]
max = x[2]
out = vector(length = length(c2$size))
j = max - min
for (i in (1:length(c2$size))) {
  out[i] = (1-0)*(c2$size[i] - min)/j 
}
range(out)
c2$size = out

x = range(c2$net)
min = x[1]
max = x[2]
out = vector(length = length(c2$net))
j = max - min
for (i in (1:length(c2$net))) {
  out[i] = (1-0)*(c2$net[i] - min)/j 
}
range(out)
c2$net = out

x = range(c2$tot)
min = x[1]
max = x[2]
out = vector(length = length(c2$tot))
j = max - min
for (i in (1:length(c2$tot))) {
  out[i] = (1-0)*(c2$tot[i] - min)/j 
}
range(out)
c2$tot = out

x = range(c2$neigh)
min = x[1]
max = x[2]
out = vector(length = length(c2$neigh))
j = max - min
for (i in (1:length(c2$neigh))) {
  out[i] = (1-0)*(c2$neigh[i] - min)/j 
}
range(out)
c2$neigh = out

x = range(c2$landuse)
min = x[1]
max = x[2]
out = vector(length = length(c2$landuse))
j = max - min
for (i in (1:length(c2$landuse))) {
  out[i] = (1-0)*(c2$landuse[i] - min)/j 
}
range(out)
c2$landuse = out
View(c2[1:10,])

# PCA for visualize
pr.out=prcomp(c2, scale=TRUE)
biplot(pr.out, scale=0)
pr.out$rotation
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$rotation
pr.var=pr.out$sdev^2 
pr.var/sum(pr.var) # PVE

# Fitting Regression Trees


set.seed(1)
Train<-sample(nrow(c2),nrow(c2)*0.9)
Test<-c2[-Train,]
Train<-c2[Train,]

set.seed(1)
tree.house=tree(Train$assessed_value~.,Train)
summary(tree.house)
plot(tree.house)
text(tree.house,pretty=0)
yhat=predict(tree.house,newdata=Test)
j = (yhat-Test$assessed_value)^2 
mean(j)  # class1: 1.5e10 class2: 2.4e13 class3: 3.4e13
plot(yhat,Test$assessed_value) 
abline(0,1)
cv.house=cv.tree(tree.house)
cv.house
plot(cv.house$size,cv.house$dev,type='b')
prune.house=prune.tree(tree.house,best=7)
plot(prune.house)
text(prune.house,pretty=0)
yhat=predict(prune.house,newdata=Test)
j = (yhat-Test$assessed_value)^2 
mean(j)  # 6.5e10
plot(yhat,Test$assessed_value)
abline(0,1)


# Boosting
# since boosting might be overfitting if the # of new trees are too large,
# performing CV to select the optimal #
folds= createFolds(y=Train$size,k=5)
CVerror=0
ref=matrix(rep(0),1,5)
set.seed(2)
for(j in (1:5)){  # j is from 1-10, 10 numbers
  for(i in 1:5){  
    traindata=Train[-folds[[i]],]  
    validationdata=Train[folds[[i]],]  
    
    boost.house = gbm(traindata$assessed_value~.,data=traindata,distribution="gaussian",
                      n.trees=j*1000,interaction.depth=1) 
    
    yhat.boost=predict(boost.house,newdata=validationdata,n.trees=j*1000)
    
    CVerror = CVerror + mean((yhat.boost - validationdata$assessed_value)^2)
    
  }  
  ref[1,j]=CVerror/5
  CVerror=0
}

which.min(ref)
ref[5] # class1:1.5e10  class2: 1.3e13

# bagging and randomforest
set.seed(2)
bag.house=randomForest(Train1$assessed_value~.,data=Train1,mtry=5,importance=TRUE)
bag.house
yhat.bag = predict(bag.house,newdata=Test1)
plot(yhat.bag, Test1$assessed_value)
abline(0,1)
mean((yhat.bag-Test1$assessed_value)^2) #class1: 7e9

rf.house=randomForest(Train1$assessed_value~.,data=Train1,importance=TRUE)
rf.house
yhat.rf = predict(rf.house,newdata=Test1)
j = (yhat.rf-Test1$assessed_value)^2  # class1:(5.6e9+7.9e9)/2 class2: 2.5e13 
                                      # class3: 3.0e13
mean(j)
rf.house$importance

# class 1 is too large, I have to seperate it into two parts for rf, otherwise there will be an error
set.seed(2)
bag.house=randomForest(Train2$assessed_value~.,data=Train2,mtry=5,importance=TRUE)
bag.house
yhat.bag = predict(bag.house,newdata=Test2)
plot(yhat.bag, Test2$assessed_value)
abline(0,1)
mean((yhat.bag-Test2$assessed_value)^2) #class1: 7e9

rf.house=randomForest(Train2$assessed_value~.,data=Train2,importance=TRUE)
rf.house
yhat.rf = predict(rf.house,newdata=Test2)
j = (yhat.rf-Test2$assessed_value)^2  # class1:(5.6e9+7.9e9)/2 class2: 2.5e13 
                                      # class3: 3.0e13
mean(j)
rf.house$importance


# fitting model for class3
c3=data.frame(neigh = class3$neighbourhood,landuse = class3$landuse_description,
              property=class3$property_type,size=class3$lot_size,
              tot=class3$tot_gross_area_description,net=class3$net_area,
              build_year=class3$effective_build_year,out= class3$out,
              assessed_value=class3$assessed_value)

range(c3$size)
range(c3$net)
c3$neigh = as.numeric(c3$neigh) 
c3$landuse = as.numeric(c3$landuse)
c3$landuse[1:10] # normalization for this land information 

# Normalization for class
x = range(c3$size)
min = x[1]
max = x[2]
out = vector(length = length(c3$size))
j = max - min
for (i in (1:length(c3$size))) {
  out[i] = (1-0)*(c3$size[i] - min)/j 
}
range(out)
c3$size = out

x = range(c3$net)
min = x[1]
max = x[2]
out = vector(length = length(c3$net))
j = max - min
for (i in (1:length(c3$net))) {
  out[i] = (1-0)*(c3$net[i] - min)/j 
}
range(out)
c3$net = out

x = range(c3$tot)
min = x[1]
max = x[2]
out = vector(length = length(c3$tot))
j = max - min
for (i in (1:length(c3$tot))) {
  out[i] = (1-0)*(c3$tot[i] - min)/j 
}
range(out)
c3$tot = out

x = range(c3$neigh)
min = x[1]
max = x[2]
out = vector(length = length(c3$neigh))
j = max - min
for (i in (1:length(c3$neigh))) {
  out[i] = (1-0)*(c3$neigh[i] - min)/j 
}
range(out)
c3$neigh = out

x = range(c3$landuse)
min = x[1]
max = x[2]
out = vector(length = length(c3$landuse))
j = max - min
for (i in (1:length(c3$landuse))) {
  out[i] = (1-0)*(c3$landuse[i] - min)/j 
}
range(out)
c3$landuse = out
View(c3[1:10,])

# PCA for visualize
pr.out=prcomp(c3, scale=TRUE)
biplot(pr.out, scale=0)
pr.out$rotation
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$rotation
pr.var=pr.out$sdev^2 
pr.var/sum(pr.var) # PVE

# Fitting Regression Trees


set.seed(1)
Train<-sample(nrow(c3),nrow(c3)*0.9)
Test<-c3[-Train,]
Train<-c3[Train,]

set.seed(1)
tree.house=tree(Train$assessed_value~.,Train)
summary(tree.house)
plot(tree.house)
text(tree.house,pretty=0)
yhat=predict(tree.house,newdata=Test)
j = (yhat-Test$assessed_value)^2 
mean(j)  # class1: 1.5e10 class2: 2.4e13 class3: 3.4e13
plot(yhat,Test$assessed_value) 
abline(0,1)
cv.house=cv.tree(tree.house)
cv.house
plot(cv.house$size,cv.house$dev,type='b')
prune.house=prune.tree(tree.house,best=7)
plot(prune.house)
text(prune.house,pretty=0)
j= data.frame(size=Test$size,out=Test$out,assessed_value=Test$assessed_value)
yhat=predict(prune.house,newdata=Test)
j = (yhat-Test$assessed_value)^2 
mean(j)  # 6.5e10
plot(yhat,Test[-posi,]$assessed_value)
abline(0,1)
posi = which(yhat>4000000) # take 2 outliner from Test, MSE= 2e10

# Boosting
# since boosting might be overfitting if the # of new trees are too large,
# performing CV to select the optimal #
folds= createFolds(y=Train$size,k=5)
CVerror=0
ref=matrix(rep(0),1,5)
set.seed(2)
for(j in (1:5)){  # j is from 1-10, 10 numbers
  for(i in 1:5){  
    traindata=Train[-folds[[i]],]  
    validationdata=Train[folds[[i]],]  
    
    boost.house = gbm(traindata$assessed_value~.,data=traindata,distribution="gaussian",
                      n.trees=j*1000,interaction.depth=1) 
    
    yhat.boost=predict(boost.house,newdata=validationdata,n.trees=j*1000)
    
    CVerror = CVerror + mean((yhat.boost - validationdata$assessed_value)^2)
    
  }  
  ref[1,j]=CVerror/5
  CVerror=0
}

which.min(ref)
ref[5] # class1:1.5e10  class2: 1.3e13

# bagging and randomforest
set.seed(2)
bag.house=randomForest(Train1$assessed_value~.,data=Train1,mtry=5,importance=TRUE)
bag.house
yhat.bag = predict(bag.house,newdata=Test1)
plot(yhat.bag, Test1$assessed_value)
abline(0,1)
mean((yhat.bag-Test1$assessed_value)^2) #class1: 7e9

rf.house=randomForest(Train1$assessed_value~.,data=Train1,importance=TRUE)
rf.house
yhat.rf = predict(rf.house,newdata=Test1)
j = (yhat.rf-Test1$assessed_value)^2  # class1:(5.6e9+7.9e9)/2 class2: 2.5e13 
                                      # class3: 3.0e13
mean(j)
rf.house$importance

# class 1 is too large, I have to seperate it into two parts for rf, otherwise there will be an error
set.seed(2)
bag.house=randomForest(Train2$assessed_value~.,data=Train2,mtry=5,importance=TRUE)
bag.house
yhat.bag = predict(bag.house,newdata=Test2)
plot(yhat.bag, Test2$assessed_value)
abline(0,1)
mean((yhat.bag-Test2$assessed_value)^2) #class1: 7e9

rf.house=randomForest(Train2$assessed_value~.,data=Train2,importance=TRUE)
rf.house
yhat.rf = predict(rf.house,newdata=Test2)
j = (yhat.rf-Test2$assessed_value)^2  # class1:(5.6e9+7.9e9)/2 class2: 2.5e13 
                                      # class3: 3.0e13
mean(j)
rf.house$importance






write.csv(c1, file = "class1.csv")
write.csv(c2, file = "class2.csv")
write.csv(c3, file = "class3.csv")
