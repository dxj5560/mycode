# 邓煊洁 应统17 2017310901 statcomp L3
# dplyr的使用、vector与as.vector等的比较

NM <- c("z3", "l4", "w5", "z6")
ID <- c(901, 902, 903, 904)
score <- c(90, 80, 80, 100)
stu <- data.frame(NM, ID, score)

# 使用dplyr包
library(dplyr)
# 选分数大于80的NM，ID变量
subset(stu, score >= 80, select = c(NM,ID))
# 看数据框的维度
dim(stu)
# 简单看一下数据框
str(stu)
# 看数据框的1-2个变量名
names(stu)[1:2]
# 选择数据框ID到select的变量
select(stu, ID:score)
# 删除数据框score这一变量
select(stu, -score)
# 选择数据框以re结尾的变量
select(stu, ends_with("re"))
# 选择数据库以N开始的变量
select(stu, starts_with("N"))
# 只保留分数大于80的观测
filter(stu, score > 80)
# 看数据框各变量的数据特征
summary(stu)
# 看数据框score变量的数据特征
summary(stu$score)
# 只保留数据框分数大于80并且学号大于903的观测
filter(stu, score > 80 & ID > 903)
# 将观测按score的升序排列
arrange(stu, score)
# 将观测按score的降序排列
arrange(stu, desc(score))
# 看数据框最后两个观测的姓名
tail(select(stu, NM), 2)
# 给变量重命名 注意新变量在左边
stu <- rename(stu, sc = score)
# 重新生成一个变量
sc <- score
mutate(stu, sc.n = sc - mean(sc))
# 重新生成一个按分数分割的数据框（不同的分数一个组）
scs <- group_by(stu, sc)
# 返回每一分数ID的最大值
summarise(scs, ID = max(ID))
# 重新生成一个按学号分割的数据框（处在同一分位点区间的观测一个组）
quan <- quantile(stu$ID, seq(0, 1, 0.5), na.rm = TRUE)
stu <- mutate(stu, ID.quan = cut(ID, quan))
IDs <- group_by(stu, ID.quan)
# 看不同ID区间分数的均值
summarise(IDs, sc = mean(sc))
# 出现问题：由于分数分割时分位点是901.0 902.5 904.0 而由于精度的问题导致第一个同学
# 没有被分在任意区间，该位置为缺失值
stu <- select(stu, -ID.quan) 
# 一步完成上面的过程
mutate(stu, ID.quan = cut(ID, quan)) %>% group_by(ID.quan) %>% 
  summarise(sc = mean(sc, na.rm = TRUE))

# 生成数据类型的方法
# vector    as.vector是将一个数组强制转换成简单向量   c输入每个数据直接相连
A <- matrix(runif(10), 2, 5)
v1 <- c(runif(10))
v2 <- as.vector(A)
# matrix    matrix输入数据和维度  as.matrix转成一列矩阵
m1 <- matrix(1:12, 3, 4)
m2 <- as.matrix(v2,5)
is.matrix(m2)
# array    array输入数据和维度  as.array将已有数据结构转换为数组
a1 <- array(1:24, 2:4)
a2 <- as.array(m1)
# data frame  data.frame连接各向量     as.data.frame将已有数据结构转换为数据框
NM <- c("z3", "l4", "w5", "z6")
ID <- c(901, 902, 903, 904)
score <- c(90, 80, 80, 100)
d1 <- data.frame(NM, ID, score)
d2 <- as.data.frame(m1)
# list list连接各数据结构  as.list将已有数据结构转换为清单(每个数据不同层面)
l1 <- list(v1, m1, a1, d1)
l2 <- as.list(v1)