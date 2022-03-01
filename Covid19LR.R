#安装包
#install.packages('ggplot2')
#install.packages('car')
#install.packages("carData")
library(car)

#当前路径
setwd("/Users/liunz/code/mba/covid19")
#getwd()

#取消科学计数法
options(scipen=200)

#读取csv
data <- read.csv('COVID19.csv', fileEncoding = 'gbk',header=T)
names(data) = c("province","x1","x2","x3","x4","x5","x6","Y")
head(data)  # 展示前6行

#数据探查
dim(data)
colnames(data)
str(data)
summary(data)
#table(data$地区)

#含有缺失值的行数
#sum(complete.cases(data))         #is.na(saledata)
#sum(!complete.cases(data))
#mean(!complete.cases(data))       #1/201数字，缺失值比例
#data[!complete.cases(data),]  #筛选出缺失值的数值


#直方图
par(family='STKaiti') # mac电脑显示中文的问题，windows电脑不用加这个代码
par(family="ArialUnicodeMS") # mac电脑显示中文的问题
hist(data$Y,xlab="累计确诊人数",ylab="样本数量",main=NULL)

#定义变量
x1<-data$x1
x2<-data$x2
x3<-data$x3
x4<-data$x4
x5<-data$x5
x6<-data$x6
y<-data$Y

par(mfrow = c(2,3))
hist(x1,xlab="累计武汉滞留时间",ylab="样本数量",main=NULL)
hist(x2,xlab="风险携带者比例",ylab="样本数量",main=NULL)
hist(x3,xlab="武汉62家定点医院到访比例",ylab="样本数量",main=NULL)
hist(x4,xlab="华南海鲜市场到访比例",ylab="样本数量",main=NULL)
hist(x5,xlab="机场到访比例",ylab="样本数量",main=NULL)
hist(x6,xlab="火车站到访比例",ylab="样本数量",main=NULL)


#描述分析
N=sapply(data,length)
MU=sapply(data,mean)
SD=sapply(data,sd)
MIN=sapply(data,min)
MED=sapply(data,median)
MAX=sapply(data,max)
result=cbind(N,MU,SD,MIN,MED,MAX)
result

#箱线图
#data$cat = as.factor((data$Y > median(data$Y))*1)
#levels(data$cat) = c("低","高")
#data$cat = factor(data$cat,levels=c("高","低"))
#boxplot(data$x1~cat,data,xlab="累计确诊人数",ylab = "累计武汉滞留时间")
#boxplot(data$x2~cat,data,xlab="累计确诊人数",ylab = "数量")
#boxplot(data$x3~cat,data,xlab="累计确诊人数",ylab = "数量")
#par(mfrow = c(1,3))
#boxplot(data$x4~cat,data,xlab="累计确诊人数",ylab = "数量")
#boxplot(data$x5~cat,data,xlab="累计确诊人数",ylab = "数量")
#boxplot(data$x6~cat,data,xlab="累计确诊人数",ylab = "数量")


#异常值处理函数
f <- function(xdata, outdata){
  
  #计算下四分位数、上四分位数和四分位距
  QL <- quantile(xdata, probs = 0.25)
  QU <- quantile(xdata, probs = 0.75)
  QU_QL <- QU-QL
  QL;QU;QU_QL
  
  which(xdata > QU + 1.5*QU_QL)
  xdata[which(xdata > QU + 1.5*QU_QL)]
  summary(xdata)
  
  
  # 用上四分位数的1.5倍四分位距或下四分位数的1.5倍四分位距替换
  outdata <- xdata
  out_imp02 <- QU + 1.5*QU_QL
  outdata[which(outdata > QU + 1.5*QU_QL)] <- out_imp02
  
  # 对比替换前后的数据概览
  summary(outdata)
  
  
}

#将自变量进行异常值处理
x1<-f(data$x1)
x2<-f(data$x2)
x3<-f(data$x3)
x4<-f(data$x4)
x5<-f(data$x5)
x6<-data$x6
y<-f(data$Y)


par(family='STKaiti') # mac电脑显示中文的问题
par(family="ArialUnicodeMS") # mac电脑显示中文的问题
par(mfrow = c(2,3))

boxplot(data$Y,xlab="累计确诊人数",ylab = "累计武汉滞留时间")
boxplot(y,xlab="累计确诊人数",ylab = "累计武汉滞留时间")


boxplot(data$x1,xlab="累计确诊人数",ylab = "累计武汉滞留时间")
boxplot(x1,xlab="累计确诊人数",ylab = "累计武汉滞留时间")

boxplot(data$x2,xlab="累计确诊人数",ylab = "风险携带者比例")
boxplot(x2,xlab="累计确诊人数",ylab = "风险携带者比例")

boxplot(data$x3,xlab="累计确诊人数",ylab = "武汉62家定点医院到访比例")
boxplot(x3,xlab="累计确诊人数",ylab = "武汉62家定点医院到访比例")

boxplot(data$x4,xlab="累计确诊人数",ylab = "华南海鲜市场到访比例")
boxplot(x4,xlab="累计确诊人数",ylab = "华南海鲜市场到访比例")

boxplot(data$x5,xlab="累计确诊人数",ylab = "机场到访比例")
boxplot(x5,xlab="累计确诊人数",ylab = "机场到访比例间")

boxplot(data$x6,xlab="累计确诊人数",ylab = "火车站到访比例")
boxplot(x6,xlab="累计确诊人数",ylab = "火车站到访比例")



#直方图，暂时废弃不用
# #par(mfrow = c(2,3))
# hist(x1,xlab="累计武汉滞留时间",ylab="样本数量",main=NULL)
# hist(x2,xlab="风险携带者比例",ylab="样本数量",main=NULL)
# hist(x3,xlab="武汉62家定点医院到访比例",ylab="样本数量",main=NULL)
# hist(x4,xlab="华南海鲜市场到访比例",ylab="样本数量",main=NULL)
# hist(x5,xlab="机场到访比例",ylab="样本数量",main=NULL)
# hist(x6,xlab="火车站到访比例",ylab="样本数量",main=NULL)

#建立模型
#x1 = data$累计武汉滞留时间
#x2 = data$风险携带者比例
#x3 = data$武汉62家定点医院到访比例
#x4 = data$华南海鲜市场到访比例
#x5 = data$机场到访比例
#x6 = data$火车站到访比例
#y=data$累计确诊人数
#y = data$Y

#建立线性回归模型
model <- lm(y ~x1+x2+x3+x4+x5+x6, data)
#查看是否有共线性
vif(model)
#模型摘要
summary(model)
plot(fitted(model),resid(model),cex=1.2,pch=21,col="red",bg="orange",xlab="Fitted value",ylab="Residuals")



#建立对数线性回归模型
#model <- lm(y ~x1+x2+x3+x4+x5+x6, data)
model_log <- lm(log(y) ~ log(x1+1)+log(x2+1)+log(x3+1)+log(x4+1)+log(x5+1)+log(x6+1), data)
#查看是否有共线性
vif(model_log)
#模型摘要
summary(model_log)
plot(fitted(model_log),resid(model_log),cex=1.2,pch=21,col="red",bg="orange",xlab="Fitted value",ylab="Residuals")

#描述aic,bic
ss=length(data[,1])
summary(model_log)
plot(model_log,which=1)
aic=step(model_log,trace=F)
bic=step(model_log,k=log(ss),trace=F)
summary((aic))
summary((bic))
#(fitted(model_log),resid(model_log),cex=1.2,pch=21,col="red",bg="orange",xlab="Fitted value",ylab="Residuals")


#模型预测
#a <- data.frame(x = 222)
#result <-  predict(model,a)
#print(result)



