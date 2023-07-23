library(ggplot2)

# 箱线图---------------------------------------------------------
## 生成数据
set.seed(8)
y <- rnorm(200)
group <- sample(LETTERS[1:3], size = 200,
                replace = TRUE)
df <- data.frame(y, group)
# 绘制带散点分布的箱线图
ggplot(df, aes(x = group, y = y, colour = group)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter()

# 密度分布图--------------------------------------------------------
## 生成数据
set.seed(5)
x <- c(rnorm(200, mean = -2, 1.5),
       rnorm(200, mean = 0, sd = 1),
       rnorm(200, mean = 2, 1.5))
group <- c(rep("A", 200), rep("B", 200), rep("C", 200))
df <- data.frame(x, group)

# 绘制分组密度分布图
cols <- c("#F76D5E", "#FFFFBF", "#72D8FF")
ggplot(df, aes(x = x, fill = group)) +
  geom_density(alpha = 0.7) + 
  scale_fill_manual(values = cols)

# 热图-------------------------------------------------------------------
## 生成数据
library(reshape)
set.seed(8)
m <- matrix(round(rnorm(200), 2), 10, 10)
colnames(m) <- paste("Col", 1:10)
rownames(m) <- paste("Row", 1:10)
df <- melt(m)
colnames(df) <- c("x", "y", "value")
## 绘制热图
ggplot(df, aes(x = x, y = y, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) 

