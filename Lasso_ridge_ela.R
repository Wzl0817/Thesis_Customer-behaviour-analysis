bc=read.csv("C:/Users/12866/Desktop/my_data.csv")
library(glmnet)
bc <- na.omit(bc)
y=as.matrix(bc[,23])
x=as.matrix(bc[,-23])

#f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1) 
#这里alpha=1为LASSO回归，如果等于0就是岭回归
#print(f1)#把f1结果输出

f1.cvfit=cv.glmnet(x,y,family="binomial", nlambda=100, alpha=1)
#f1.cvfit$lambda.min#求出最小值
#f1.cvfit$lambda.1se#求出最小值一个标准误的λ值
f1.coef2<-coef(f1.cvfit$glmnet.fit,s=f1.cvfit$lambda.min,exact = F)
f1.coef1<-coef(f1.cvfit$glmnet.fit,s=f1.cvfit$lambda.1se,exact = F)
f1c1_indexs=f1.coef1@i  #index
f1c1_values=f1.coef1@x  #数值
f1c1_names=f1.coef1@Dimnames[[1]][f1.coef1@i+1] #选择出的列名
f1c2_indexs=f1.coef2@i  #index
f1c2_values=f1.coef2@x  #数值
f1c2_names=f1.coef2@Dimnames[[1]][f1.coef2@i+1] #选择出的列名
length(f1c1_names)-1
length(f1c2_names)-1


f2.cvfit=cv.glmnet(x,y,family="binomial", nlambda=100, alpha=0)
f2.coef2<-coef(f2.cvfit$glmnet.fit,s=f2.cvfit$lambda.min,exact = F)
f2.coef1<-coef(f2.cvfit$glmnet.fit,s=f2.cvfit$lambda.1se,exact = F)
f2c1_indexs=f2.coef1@i  #index
f2c1_values=f2.coef1@x  #数值
f2c1_names=f2.coef1@Dimnames[[1]][f2.coef1@i+1] #选择出的列名
f2c2_indexs=f2.coef2@i  #index
f2c2_values=f2.coef2@x  #数值
f2c2_names=f2.coef2@Dimnames[[1]][f2.coef2@i+1] #选择出的列名
length(f2c1_names)-1
length(f2c2_names)-1


f3.cvfit=cv.glmnet(x,y,family="binomial", nlambda=100, alpha=0.5)
f3.coef2<-coef(f3.cvfit$glmnet.fit,s=f3.cvfit$lambda.min,exact = F)
f3.coef1<-coef(f3.cvfit$glmnet.fit,s=f3.cvfit$lambda.1se,exact = F)
f3c1_indexs=f3.coef1@i  #index
f3c1_values=f3.coef1@x  #数值
f3c1_names=f3.coef1@Dimnames[[1]][f3.coef1@i+1] #选择出的列名
f3c2_indexs=f3.coef2@i  #index
f3c2_values=f3.coef2@x  #数值
f3c2_names=f3.coef2@Dimnames[[1]][f3.coef2@i+1] #选择出的列名
length(f3c1_names)-1
length(f3c2_names)-1


