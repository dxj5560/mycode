# 邓煊洁 应统17 2017310901 statcomp L2
# 矩阵的20条性质验证——格式修改

A <- matrix(c(-5, 10, -2, 6, 2, 17, 7, -6, 10), 3, 3)
B <- matrix(c(2, 0, 1, 9, 3, 4, 1, 0, 5), 3, 3, byrow=TRUE)
C <- matrix(seq(1, 18, 2), 3, 3)
A.solve <- solve(A)

# 1 验证矩阵加法交换律
identical(A + B, B + A)

# 2 验证矩阵加法结合律
identical((A + B) + C, A + (B + C))

# 3 验证矩阵的乘法不适合交换律
identical(A %*% B, B %*% A)

# 4 验证乘法结合律  
identical((A %*% B) %*% C, A %*% (B %*% C))

# 5 验证乘法对加法的分配律 
identical(A %*% (B + C), A %*% B + A %*% C)

# 6 验证秩的大小关系
qr(A + B)$rank <= qr(A)$rank + qr(B)$rank
# 可以发现秩依次为3,3,3

# 7 验证矩阵相乘转置
identical(t(A %*% B), t(B) %*% t(A))

# 8 验证矩阵相加转置 
identical(t(A + B), t(A) + t(B))

# 9 验证矩阵的逆的定义
all.equal(A %*% D, diag(3))

# 10 验证A的逆的逆是A
all.equal(solve(A.solve), A)

# 11 验证（kA）^-1=(k^-1)*((A)^-1)
identical(solve(2*A), 0.5*A.solve)

# 12 验证 (AB)^-1=(B^-1)*(A^-1)
all.equal(solve(A %*% B), solve(B) %*% solve(A))

# 13 验证t(A)^-1=t(A^-1)
all.equal(solve(t(A)), t(A.solve))

# 14 验证相似矩阵行列式相等 从书上找到两个相似的矩阵
E = matrix(c(2, 1, 1, 1, 2, 1, 1, 1, 2), 3, 3)
F = matrix(c(4, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
all.equal(det(E), det(F))

# 15 验证相似矩阵特征值相等
all.equal(eigen(E)$values, eigen(F)$values)

# 16 验证相似矩阵秩相等
all.equal(qr(E)$rank, qr(F)$rank)

# 17 矩阵特征值的连乘等于矩阵的行列式
all.equal(prod(eigen(E)$values), det(E))

# 18 验证特征值的连加是A的迹（A对角线上元素的和）
all.equal(sum(eigen(E)$values), sum(diag(E)))

# 19 验证若λ是E的特征值 则f(λ)是f(E)的特征值
all.equal(eigen(2*E)$values, 2*eigen(E)$values)

# 20 验证矩阵乘积的行列式 
det(A %*% B) == det(A)*det(B)
# 这里显示不相等，但是用all.equal能判断出二者相等
all.equal(det(A %*% B), det(A)*det(B))
# 而当用identical判断时,二者结果并不相等
identical(det(A %*% B), det(A)*det(B))
# identical用来比较二者精确相等，而all.equal是近似相等

# apply 与tapply的应用
G=array(1:24,2:4)
apply(G,1:2,function(x) mean(x)+2)
tag=rep(1:6,4)
tapply(G,tag,function(x) mean(x)+2)
# 可以知道apply与tapply都可以用来批量处理数据，前者用位置来划分，后者可以自定义标签来划分。