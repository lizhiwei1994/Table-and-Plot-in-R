# tableone package --------------------------------------------------------

library(tableone) 
# 读取数据
dat = read.csv('data/table1_data.csv')
# 划分连续型变量和分类变量
listVars <- c("Age", "Cholesterol", "SystolicBP", 
              "BMI", "Smoking", "Education")
catVars <- c("Smoking","Education")
# 制作基线表
table1 <- CreateTableOne(vars = listVars, 
                         data = dat, 
                         factorVars = catVars, 
                         strata = c("Gender"))
# 导出基线表
a <- print(table1, quote = TRUE, noSpaces = TRUE)
a <- as.data.frame(a)
write.csv(a, 'result/result_tableone.csv')

# arsenal package ---------------------------------------------------------

library(arsenal)
# 读取数据
dat = read.csv('data/table1_data.csv')
# 制作基线表
tab1 <- tableby(Gender ~ Age + Cholesterol + SystolicBP + 
                  BMI + Smoking+ Education, data = dat)
# 查看基线表
sum_tab1 <- summary(tab1, text=TRUE)
sum_tab1

# tableby.control() function
# 对基线表进行更精细的设置
mycontrols  <- tableby.control(numeric.stats = c("meansd"))

tab1 <- tableby(Gender ~ Age + Cholesterol + SystolicBP + 
                  BMI + Smoking+ Education, data = dat,
                control = mycontrols)
# 导出基线表
sum_tab1 <- summary(tab1, text=TRUE)
sum_tab1
sum_tab1_out <- summary(sum_tab1, text=NULL)
sum_tab1_out <- as.data.frame(summary(tab1, text=NULL))

write.csv(sum_tab1_out, 'result/tableby_tableone.csv')

# gtsummary package ------------------------------------------------------------

library(gtsummary)
library(readr)
# 读取数据
dat = read_csv('data/table1_data.csv')
# 制作基线表(精简版)
dat %>% 
  tbl_summary(by = Gender)
# 制作基线表(详细版)
tab_gt = 
dat %>% 
  tbl_summary(by = Gender, # group by
              
              statistic = list(all_continuous() ~ '{mean} ± {sd}'), # stats for numeric
              
              digits = list(all_continuous() ~ 2, # continuous digits
                            all_categorical() ~ c(0, 2)),  # categorical count and percent digits
              
               type = list(Smoking ~ 'categorical'), # <---二分类变量请注意这里;dichotomous vars to show
              
              missing = 'no') %>% # missing value to show
  
  modify_header(all_stat_cols() ~ '**{level}**<br>N = {n}') %>% # group level to show
  
  modify_spanning_header(all_stat_cols() ~ '**Gender**') %>% # main header for group var
  
  bold_labels() %>% 
  
  add_overall(col_label = '**Overall**<br>N = {N}') %>% # set overall label
  
  add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3))
# 保存到docx文件
tab_gt %>% 
  as_flex_table() %>%
  flextable::save_as_docx(path = 'result/gtsummary_tableone.docx')

