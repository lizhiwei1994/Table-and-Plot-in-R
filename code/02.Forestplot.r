# 加载R包
library(forestplot) 
# 读取数据
fp <- read.csv("data/forestplot.csv", header = F) # 注意设置不读取第一行为标题行

# forestplot包绘制简单森林图  -----------------------------------------------------
forestplot( # 如果绘图窗口太小，这个代码会报错
  labeltext = as.matrix(fp[,1:2]),#设置文本列
  mean = fp$V3, #均值
  lower = fp$V4,#95%下限
  upper = fp$V5 #95%上限
)

# forestplot包绘制复杂森林图 ----------------------------------------------------
tiff('result/forestplot_forestplot.tiff',width = 2100, height = 3000, res = 300)
# pdf('result/forestplot_forestplot.pdf',width = 7, height = 10)
forestplot(
  labeltext = as.matrix(fp[,1:2]),#设置文本列#
  mean = fp$V3, #绘图用的均值#
  lower = fp$V4,
  upper = fp$V5,
  is.summary = c(T,rep(c(T,F,F,F,F,F,F,F),3),T,F,F,F,F,F,F), #某行是否加粗显示#
  zero = 0, 
  col = fpColors(box='blue', #均值点的颜色#
                 summary='#8B008B', #加粗行的颜色#
                 lines = 'black', #95%CI线的颜色#
                 zero = 'skyblue'), #无效线的颜色#
  boxsize = 0.5,  #均值点图形的大小#
  lineheight = unit(8,'mm'), #行距#
  colgap = unit(2,'mm'), #列宽#
  lwd.zero = 1,# 无效线宽度
  lwd.ci = 1, # 95%CI线宽度
  xlab = 'The estimate', #x轴的标题#
  lwd.xaxis = 1, #x 轴的刻度#
  lty.ci = 1,# 95%CI线类型
  ci.vertices = T, #是否在95%CI两头添加箭头#
  ci.vertices.height = 0.15, #箭头的长短#
  graph.pos = 2, #森林图画在第几列#
  fn.ci_norm = fpDrawNormalCI, #均值点的形状，Normal是方形，Circel是圆形，Diamod是菱形，Ponit是点#
  txt_gp = fpTxtGp(
    ticks = gpar(cex = 1), #坐标轴上刻度的大小#
    xlab = gpar(cex = 1),  #x轴名称的大小#
    label = gpar(cex = 1)),  #开始用labeltext（）函数设置的文本字体的大小#
  hrzl_lines=list(  "1"  = gpar(lty=1,lwd=1, col="black",fill="black"), #在表的第1行上边加一条黑线#
                    "2"  = gpar(lty=1,lwd=1, col="black",fill="black"),#在表的第2行上边加一条黑线#
                    "33" = gpar(lty=1,lwd=1, col="black",fill="black"),#在表的第33行上边加一条黑线
                    #在表的第5行上边加一条宽度为60，线型两端为butt，颜色为#898988，透明度为55的线段，线段分布在表格的2-3列
                    "5"  = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "8"  = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "13" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "16" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "21" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "24" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "29" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "32" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855")
  )
)
dev.off()

# 不同颜色的森林图 -----------------------------------------

fn <- local({
  i = 0
  no_lines <- sum(!is.na(fp$V3))
  b_clrs = colorRampPalette(colors=c(rep('blue', 8), rep('red', 4), rep('blue', 4)))(no_lines)
  l_clrs = colorRampPalette(colors=c(rep('blue', 8), rep('red', 4), rep('blue', 4)))(no_lines)
  
  function(..., clr.line, clr.marker){
    i <<- i + 1
    fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
  }
})

forestplot(
  labeltext = as.matrix(fp[,1:2]),#设置文本列#
  mean = fp$V3, #绘图用的均值#
  lower = fp$V4,
  upper = fp$V5,
  is.summary = c(T,rep(c(T,F,F,F,F,F,F,F),3),T,F,F,F,F,F,F), #某行是否加粗显示#
  zero = 0, 
  col = fpColors(box='blue', #均值点的颜色#
                 summary='#8B008B', #加粗行的颜色#
                 lines = 'black', #95%CI线的颜色#
                 zero = 'skyblue'), #无效线的颜色#
  boxsize = 0.5,  #均值点图形的大小#
  lineheight = unit(8,'mm'), #行距#
  colgap = unit(2,'mm'), #列宽#
  lwd.zero = 1,# 无效线宽度
  lwd.ci = 1, # 95%CI线宽度
  xlab = 'The estimate', #x轴的标题#
  lwd.xaxis = 1, #x 轴的刻度#
  lty.ci = 1,# 95%CI线类型
  ci.vertices = T, #是否在95%CI两头添加箭头#
  ci.vertices.height = 0.15, #箭头的长短#
  graph.pos = 2, #森林图画在第几列#
  fn.ci_norm = fn, #均值点的形状，Normal是方形，Circel是圆形，Diamod是菱形，Ponit是点#
  txt_gp = fpTxtGp(
    ticks = gpar(cex = 1), #坐标轴上刻度的大小#
    xlab = gpar(cex = 1),  #x轴名称的大小#
    label = gpar(cex = 1)),  #开始用labeltext（）函数设置的文本字体的大小#
  hrzl_lines=list(  "1"  = gpar(lty=1,lwd=1, col="black",fill="black"), #在表的第1行上边加一条黑线#
                    "2"  = gpar(lty=1,lwd=1, col="black",fill="black"),#在表的第2行上边加一条黑线#
                    "33" = gpar(lty=1,lwd=1, col="black",fill="black"),#在表的第33行上边加一条黑线
                    #在表的第5行上边加一条宽度为60，线型两端为butt，颜色为#898988，透明度为55的线段，线段分布在表格的2-3列
                    "5"  = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "8"  = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "13" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "16" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "21" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "24" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "29" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855"),
                    "32" = gpar(lty=1,lwd=60,lineend="butt",columns=c(2:3),col="#89898855")
  )
  
)

# 使用ggplot2包绘制森林图 ----------------------------------

library(ggplot2)
dat = read.csv("data/forestplot_ggplot.csv")

ggplot(dat, aes(x = Disease, y = RR, shape = group, fill = group, colour = group, group = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", size = 0.5) +
  geom_point(position = position_dodge(.7), size = 2) +
  geom_errorbar(aes(ymin = RR_95_LOW, ymax = RR_95_UP), position = position_dodge(.7), width = 0.3, size = 0.6) +
  xlab("")+
  scale_y_continuous("RR and 95% CI", labels = scales::number_format(accuracy = 0.01))+
  scale_shape_manual(values = c(16, 23, 24, 15), guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#ea4a50", "#325788", "dodgerblue", "coral1"), guide = guide_legend(title = NULL)) +
  scale_colour_manual(values = c("#ea4a50", "#325788", "dodgerblue", "coral1"), guide = guide_legend(title = NULL)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "top") + # top
  theme(legend.key = element_blank()) +
  theme(legend.background = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 10, color = "black")) +
  theme(legend.key.height = unit(0.3, "cm")) +
  theme(axis.text.y = element_text(size = 12, angle = 0, color = "black")) +
  theme(axis.text.x = element_text(size = 12, color = "black")) 

# ## 有更多分类变量？ ------------------------------------------

library(tidyverse)
dat1.1 = dat %>% mutate(country = 'China')
dat1.2 = dat %>% mutate(country = 'USA')
dat2 = bind_rows(dat1.1, dat1.2)

ggplot(dat2, aes(x = Disease, y = RR, shape = group, fill = group, colour = group, group = group)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", size = 0.5) +
  geom_point(position = position_dodge(.7), size = 2) +
  geom_errorbar(aes(ymin = RR_95_LOW, ymax = RR_95_UP), position = position_dodge(.7), width = 0.3, size = 0.6) +
  scale_y_continuous("Percentage Change (%) and 95% CI", labels = scales::number_format(accuracy = 0.01))+
  scale_shape_manual(values = c(16, 23, 24, 15), guide = guide_legend(title = NULL)) +
  scale_fill_manual(values = c("#ea4a50", "#325788", "dodgerblue", "coral1"), guide = guide_legend(title = NULL)) +
  scale_colour_manual(values = c("#ea4a50", "#325788", "dodgerblue", "coral1"), guide = guide_legend(title = NULL)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "top") + 
  theme(legend.key = element_blank()) +
  theme(legend.background = element_rect(fill = "white", colour = "black")) +
  theme(legend.text = element_text(size = 10, color = "black")) +
  theme(legend.key.height = unit(0.3, "cm")) +
  theme(axis.text.y = element_text(size = 12, angle = 0, color = "black")) +
  theme(axis.text.x = element_text(size = 12, color = "black")) +
  xlab("") +
  facet_wrap(vars(country))

# 使用ggfp包简化森林图绘制 -----------------------------
# devtools::install_github('lizhiwei1994/ggfp')
library(ggfp)
gg_fp(
  data=dat2, x_axis=Disease, point=RR, low=RR_95_LOW, up=RR_95_UP, group_var=group,
  facet_var=country, point_shape=c(16, 16, 16, 16),
  facet_color = c('#d9d9d9', '#d9d9d9'), group_color = c("#ea4a50", "#325788", "dodgerblue", "coral1"))

# forestploter绘制森林图---------------------------------------------------------------------------
library(forestploter)
library(grid)
# 读取数据
fp <- read.csv("data/forestplot.csv") 
# 变量设置
names(fp) <- c("Disease", "RR and 95% CI", 'point', 'low', 'up')
fp$` ` <- paste(rep(" ", 20), collapse = " ")
# 自定义主题
tm <- forest_theme(base_size = 10, # 字体大小
                   refline_col = "red", # 参考线颜色
                   arrow_type = "closed") # 箭头形状
# 绘制森林图
p <- forest(fp[,c(1,6,2)], # 文本和森林图列
            est = fp$point, # 点估计值
            lower = fp$low, # 95% CI下限
            upper = fp$up,  # 95% CI上限
            sizes = 0.7, # 点大小
            ci_column = 2, # 森林图绘制列
            ref_line = 1, # 参考线值
            # xlim = c(0, 4), # x轴范围
            # ticks_at = c(0.5, 1, 2, 3), # x轴显示数值
            theme = tm)
plot(p)
# 增加三线表形式
g <- add_border(p, part = "header", row = 1, where = "top")
g <- add_border(g, part = "header", row = 1, where = "bottom")
g <- add_border(g, part = "body", row = 31, where = "bottom")
plot(g)
# 编辑森林图颜色
## 1-31行显示绿色
g <- edit_plot(g, row = c(1:31), col = 2, which = 'ci', gp = gpar(col = 'green'))
## 第3行显示紫色
g <- edit_plot(g, row = 3, col = 2,  which = 'ci', gp = gpar(col = 'purple'))
plot(g)
# 加粗标题字体
g <- edit_plot(g, row = c(1, 9, 17, 25), gp = gpar(fontface = "bold"))
plot(g)
# 保存图片
p_wh = get_wh(g)
pdf('result/forestploter_forestplot.pdf', width = p_wh[1], height = p_wh[2])
plot(g)
dev.off()
