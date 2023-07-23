# ����R��
library(forestplot) 
# ��ȡ����
fp <- read.csv("data/forestplot.csv", header = F) # ע�����ò���ȡ��һ��Ϊ������

# forestplot�����Ƽ�ɭ��ͼ  -----------------------------------------------------
forestplot( # �����ͼ����̫С���������ᱨ��
  labeltext = as.matrix(fp[,1:2]),#�����ı���
  mean = fp$V3, #��ֵ
  lower = fp$V4,#95%����
  upper = fp$V5 #95%����
)

# forestplot�����Ƹ���ɭ��ͼ ----------------------------------------------------
tiff('result/forestplot_forestplot.tiff',width = 2100, height = 3000, res = 300)
# pdf('result/forestplot_forestplot.pdf',width = 7, height = 10)
forestplot(
  labeltext = as.matrix(fp[,1:2]),#�����ı���#
  mean = fp$V3, #��ͼ�õľ�ֵ#
  lower = fp$V4,
  upper = fp$V5,
  is.summary = c(T,rep(c(T,F,F,F,F,F,F,F),3),T,F,F,F,F,F,F), #ĳ���Ƿ�Ӵ���ʾ#
  zero = 0, 
  col = fpColors(box='blue', #��ֵ�����ɫ#
                 summary='#8B008B', #�Ӵ��е���ɫ#
                 lines = 'black', #95%CI�ߵ���ɫ#
                 zero = 'skyblue'), #��Ч�ߵ���ɫ#
  boxsize = 0.5,  #��ֵ��ͼ�εĴ�С#
  lineheight = unit(8,'mm'), #�о�#
  colgap = unit(2,'mm'), #�п�#
  lwd.zero = 1,# ��Ч�߿��
  lwd.ci = 1, # 95%CI�߿��
  xlab = 'The estimate', #x��ı���#
  lwd.xaxis = 1, #x ��Ŀ̶�#
  lty.ci = 1,# 95%CI������
  ci.vertices = T, #�Ƿ���95%CI��ͷ��Ӽ�ͷ#
  ci.vertices.height = 0.15, #��ͷ�ĳ���#
  graph.pos = 2, #ɭ��ͼ���ڵڼ���#
  fn.ci_norm = fpDrawNormalCI, #��ֵ�����״��Normal�Ƿ��Σ�Circel��Բ�Σ�Diamod�����Σ�Ponit�ǵ�#
  txt_gp = fpTxtGp(
    ticks = gpar(cex = 1), #�������Ͽ̶ȵĴ�С#
    xlab = gpar(cex = 1),  #x�����ƵĴ�С#
    label = gpar(cex = 1)),  #��ʼ��labeltext�����������õ��ı�����Ĵ�С#
  hrzl_lines=list(  "1"  = gpar(lty=1,lwd=1, col="black",fill="black"), #�ڱ�ĵ�1���ϱ߼�һ������#
                    "2"  = gpar(lty=1,lwd=1, col="black",fill="black"),#�ڱ�ĵ�2���ϱ߼�һ������#
                    "33" = gpar(lty=1,lwd=1, col="black",fill="black"),#�ڱ�ĵ�33���ϱ߼�һ������
                    #�ڱ�ĵ�5���ϱ߼�һ�����Ϊ60����������Ϊbutt����ɫΪ#898988��͸����Ϊ55���߶Σ��߶ηֲ��ڱ���2-3��
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

# ��ͬ��ɫ��ɭ��ͼ -----------------------------------------

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
  labeltext = as.matrix(fp[,1:2]),#�����ı���#
  mean = fp$V3, #��ͼ�õľ�ֵ#
  lower = fp$V4,
  upper = fp$V5,
  is.summary = c(T,rep(c(T,F,F,F,F,F,F,F),3),T,F,F,F,F,F,F), #ĳ���Ƿ�Ӵ���ʾ#
  zero = 0, 
  col = fpColors(box='blue', #��ֵ�����ɫ#
                 summary='#8B008B', #�Ӵ��е���ɫ#
                 lines = 'black', #95%CI�ߵ���ɫ#
                 zero = 'skyblue'), #��Ч�ߵ���ɫ#
  boxsize = 0.5,  #��ֵ��ͼ�εĴ�С#
  lineheight = unit(8,'mm'), #�о�#
  colgap = unit(2,'mm'), #�п�#
  lwd.zero = 1,# ��Ч�߿��
  lwd.ci = 1, # 95%CI�߿��
  xlab = 'The estimate', #x��ı���#
  lwd.xaxis = 1, #x ��Ŀ̶�#
  lty.ci = 1,# 95%CI������
  ci.vertices = T, #�Ƿ���95%CI��ͷ��Ӽ�ͷ#
  ci.vertices.height = 0.15, #��ͷ�ĳ���#
  graph.pos = 2, #ɭ��ͼ���ڵڼ���#
  fn.ci_norm = fn, #��ֵ�����״��Normal�Ƿ��Σ�Circel��Բ�Σ�Diamod�����Σ�Ponit�ǵ�#
  txt_gp = fpTxtGp(
    ticks = gpar(cex = 1), #�������Ͽ̶ȵĴ�С#
    xlab = gpar(cex = 1),  #x�����ƵĴ�С#
    label = gpar(cex = 1)),  #��ʼ��labeltext�����������õ��ı�����Ĵ�С#
  hrzl_lines=list(  "1"  = gpar(lty=1,lwd=1, col="black",fill="black"), #�ڱ�ĵ�1���ϱ߼�һ������#
                    "2"  = gpar(lty=1,lwd=1, col="black",fill="black"),#�ڱ�ĵ�2���ϱ߼�һ������#
                    "33" = gpar(lty=1,lwd=1, col="black",fill="black"),#�ڱ�ĵ�33���ϱ߼�һ������
                    #�ڱ�ĵ�5���ϱ߼�һ�����Ϊ60����������Ϊbutt����ɫΪ#898988��͸����Ϊ55���߶Σ��߶ηֲ��ڱ���2-3��
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

# ʹ��ggplot2������ɭ��ͼ ----------------------------------

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

# ## �и����������� ------------------------------------------

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

# ʹ��ggfp����ɭ��ͼ���� -----------------------------
# devtools::install_github('lizhiwei1994/ggfp')
library(ggfp)
gg_fp(
  data=dat2, x_axis=Disease, point=RR, low=RR_95_LOW, up=RR_95_UP, group_var=group,
  facet_var=country, point_shape=c(16, 16, 16, 16),
  facet_color = c('#d9d9d9', '#d9d9d9'), group_color = c("#ea4a50", "#325788", "dodgerblue", "coral1"))

# forestploter����ɭ��ͼ---------------------------------------------------------------------------
library(forestploter)
library(grid)
# ��ȡ����
fp <- read.csv("data/forestplot.csv") 
# ��������
names(fp) <- c("Disease", "RR and 95% CI", 'point', 'low', 'up')
fp$` ` <- paste(rep(" ", 20), collapse = " ")
# �Զ�������
tm <- forest_theme(base_size = 10, # �����С
                   refline_col = "red", # �ο�����ɫ
                   arrow_type = "closed") # ��ͷ��״
# ����ɭ��ͼ
p <- forest(fp[,c(1,6,2)], # �ı���ɭ��ͼ��
            est = fp$point, # �����ֵ
            lower = fp$low, # 95% CI����
            upper = fp$up,  # 95% CI����
            sizes = 0.7, # ���С
            ci_column = 2, # ɭ��ͼ������
            ref_line = 1, # �ο���ֵ
            # xlim = c(0, 4), # x�᷶Χ
            # ticks_at = c(0.5, 1, 2, 3), # x����ʾ��ֵ
            theme = tm)
plot(p)
# �������߱���ʽ
g <- add_border(p, part = "header", row = 1, where = "top")
g <- add_border(g, part = "header", row = 1, where = "bottom")
g <- add_border(g, part = "body", row = 31, where = "bottom")
plot(g)
# �༭ɭ��ͼ��ɫ
## 1-31����ʾ��ɫ
g <- edit_plot(g, row = c(1:31), col = 2, which = 'ci', gp = gpar(col = 'green'))
## ��3����ʾ��ɫ
g <- edit_plot(g, row = 3, col = 2,  which = 'ci', gp = gpar(col = 'purple'))
plot(g)
# �Ӵֱ�������
g <- edit_plot(g, row = c(1, 9, 17, 25), gp = gpar(fontface = "bold"))
plot(g)
# ����ͼƬ
p_wh = get_wh(g)
pdf('result/forestploter_forestplot.pdf', width = p_wh[1], height = p_wh[2])
plot(g)
dev.off()
