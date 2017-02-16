library(ggplot2)
source("ggforestplot.R")
dat <- read.table("testdata.csv",header=T,sep=",",quote="")

q <- ggforestplot(dat, summary_name = "Summary", range=c(93,100), v_line=99 ,col_titles=c("Group","FN","TP"), stat_lable="Sensitivity (%)", colour_lines = "blue", pt_shape=15, summary_shape=9,pad = 0.5, x_step_delta=1, x_lab_pos =99.4)
(q)

