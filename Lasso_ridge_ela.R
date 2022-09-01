bc=read.csv("C:/Users/12866/Desktop/my_data.csv")
library(glmnet)
bc <- na.omit(bc)
y=as.matrix(bc[,23])
x=as.matrix(bc[,-23])

#f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1) 
#����alpha=1ΪLASSO�ع飬�������0������ع�
#print(f1)#��f1������

f1.cvfit=cv.glmnet(x,y,family="binomial", nlambda=100, alpha=1)
#f1.cvfit$lambda.min#�����Сֵ
#f1.cvfit$lambda.1se#�����Сֵһ����׼��Ħ�ֵ
f1.coef2<-coef(f1.cvfit$glmnet.fit,s=f1.cvfit$lambda.min,exact = F)
f1.coef1<-coef(f1.cvfit$glmnet.fit,s=f1.cvfit$lambda.1se,exact = F)
f1c1_indexs=f1.coef1@i  #index
f1c1_values=f1.coef1@x  #��ֵ
f1c1_names=f1.coef1@Dimnames[[1]][f1.coef1@i+1] #ѡ���������
f1c2_indexs=f1.coef2@i  #index
f1c2_values=f1.coef2@x  #��ֵ
f1c2_names=f1.coef2@Dimnames[[1]][f1.coef2@i+1] #ѡ���������
length(f1c1_names)-1
length(f1c2_names)-1


f2.cvfit=cv.glmnet(x,y,family="binomial", nlambda=100, alpha=0)
f2.coef2<-coef(f2.cvfit$glmnet.fit,s=f2.cvfit$lambda.min,exact = F)
f2.coef1<-coef(f2.cvfit$glmnet.fit,s=f2.cvfit$lambda.1se,exact = F)
f2c1_indexs=f2.coef1@i  #index
f2c1_values=f2.coef1@x  #��ֵ
f2c1_names=f2.coef1@Dimnames[[1]][f2.coef1@i+1] #ѡ���������
f2c2_indexs=f2.coef2@i  #index
f2c2_values=f2.coef2@x  #��ֵ
f2c2_names=f2.coef2@Dimnames[[1]][f2.coef2@i+1] #ѡ���������
length(f2c1_names)-1
length(f2c2_names)-1


f3.cvfit=cv.glmnet(x,y,family="binomial", nlambda=100, alpha=0.5)
f3.coef2<-coef(f3.cvfit$glmnet.fit,s=f3.cvfit$lambda.min,exact = F)
f3.coef1<-coef(f3.cvfit$glmnet.fit,s=f3.cvfit$lambda.1se,exact = F)
f3c1_indexs=f3.coef1@i  #index
f3c1_values=f3.coef1@x  #��ֵ
f3c1_names=f3.coef1@Dimnames[[1]][f3.coef1@i+1] #ѡ���������
f3c2_indexs=f3.coef2@i  #index
f3c2_values=f3.coef2@x  #��ֵ
f3c2_names=f3.coef2@Dimnames[[1]][f3.coef2@i+1] #ѡ���������
length(f3c1_names)-1
length(f3c2_names)-1

