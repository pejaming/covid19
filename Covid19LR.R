#安装包
#install.packages('ggplot2')

#当前路径
getwd()
#setwd("/Users/liunz/code/BA/covid19")

data <- read.csv('COVID19.csv', fileEncoding = 'gbk')
head(data)  # 展示前6行

dim(data)

colnames(data)
str(data)
table(data$地区)

#par(family='STKaiti') # mac电脑显示中文的问题，windows电脑不用加这个代码
#hist(data$累计确诊人数, xlab='车辆现价(万元)', ylab='频数', main='车辆现价直方图', breaks=seq(30,70,10))  # 设置间隔点

par(family='STKaiti') # mac电脑显示中文的问题，windows电脑不用加这个代码
boxplot(data$累计确诊人数~data$地区 , col='red', xlab='', ylab='万元')  # boxplot(y~x)，y为连续变量，x为离散变量


x = data$累计武汉滞留时间
model <- lm(data$累计确诊人数 ~ x, data)
#model <- lm(data$累计确诊人数 ~ data$地区+data$累计武汉滞留时间+data$机场到访比例, data)

summary(model)

a <- data.frame(x = 222)
result <-  predict(model,a)
print(result)