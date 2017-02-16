ggforestplot <- function(data, summary_name = NULL, range=NULL, v_line=NULL,col_titles=NULL, stat_lable=NULL, colour_lines = colour_lines, pt_shape=19, summary_shape=23, pad=0, x_step_delta=1, x_lab_pos = NULL){
  
  # Current version 0.2 17 February 2017
  # First version January 2017, John W Pickering.  Creative Commons 3.0
  # data:  data frame with minimum set of columns with titles 
  #   "test":  The test statistic
  #   "lower":  The lower bound of the test statistic (eg lower 95%CI)
  #   "upper": The upper bound of the test statistic
  #   "subgroups": The name of the subgroup to which each row of data belongs
  #    
  # summary_name: Name of the row with Summary data.  Can be NULL.
  # range: minimum and maxium values for arrows. of the form c(min, max)
  # v_line: where a vertical line (dashed) will be placed on the plot
  # col_titles: Names of the columns with addtional data
  # stat_lable:  Lable for the x axis
  # colour_lines:  colour of the horizontal lines
  # pt_shape:  Shape for the point estimates (default is filled circle)
  # summary_shape:  Overall summary shape (default is diamond)
  # pad is a number to shift the labels for the groups to the left if they are long. The units are the same units used for the numeric outcomes
  # x_step_delta:  The steps for the lables of the x axis
  # x_lab_pos: An approximate position of the x axis label.  
  
  dat <- data
  
  # Set bounds for each horizontal line segment
  dat$x1 <- dat$lower
  dat$x2 <- dat$upper
  
  dat$arrow_low<-FALSE
  dat$arrow_high<-FALSE
  
  if (!is.null(range)){
    dat$arrow_low[dat$lower<range[1]]<-TRUE
    dat$x1[dat$arrow_low==TRUE]<-range[1]
    dat$arrow_high[dat$upper>range[2]]<-TRUE
    dat$x2[dat$arrow_high==TRUE]<-range[2]
  }
  
  #Split the dataset into the subgroups for the graph
  df<-split(dat,dat$subgroup)
  # Order the same as in the input data file
  sg_name_dat<-unique(dat$subgroup)  #Subgroup names in order from dat
  df<-df[sg_name_dat] 
  sg_name <- names(df) #Subgroup names
  if (!is.null(summary_name)){
    sg_name <- sg_name[!(sg_name==summary_name)] #ie exclude the "Summary" data which will be added at the end
  }
  n_sg <- length(sg_name)
  
  n_col_titles = length(col_titles)
  n_dat <- dim(dat)[1] + n_sg + 1 #This will be the "height" of the graph
  n_d<-n_dat 
  
  #Initiate the plot
  g <-ggplot() + 
    theme(legend.position = "none",
          panel.background = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.x	= element_line(size=1),
          axis.title.y = element_blank(),
          axis.text.x=element_text(size=12)
    )
  #Subgroups
  x_range <- range[2]-range[1]
  x_delta <- x_range/4        #guess
  
  
  if(n_col_titles>0){
    #  max(nchar(as.character(dat$Group))) # Maximum number of characters in the Group names
    # x_lab_start <- seq((range[1]-n_col_titles*x_delta),range[1]-x_delta,x_delta )
    
    x_lab_start <-c(range[1]-(n_col_titles)*x_delta-pad ,seq((range[1]-(n_col_titles-1)*x_delta),range[1]-x_delta,x_delta ))
  }
  
  subtitle <- vector(length = n_sg)
  
  for (i in 1:n_sg){
    sub_name <- sg_name[i]
    df_i <- df[[sub_name]]
    l <- dim(df_i)[1]
    y_pos <- seq((n_d-1),(n_d-l),-1)
    subtitle[i]<-max(y_pos)+1
    df_i$y_pos <- y_pos
    
    # Add test lables to the left of the data points
    
    if (n_col_titles>0){
      for (q in 1:n_col_titles){
        temp_labs <- df_i[,col_titles[q]]
        g <- g + annotate("text",x=x_lab_start[q],y=y_pos,label=temp_labs, hjust=0) 
      }
    }
    
    # Apply points and lines
    
    for (j in 1:l) {
      dd <- df_i[j,c("x1","x2","arrow_low","arrow_high","y_pos")]
      if(dd$arrow_low==TRUE & dd$arrow_high==TRUE){
        g <- g + geom_segment(data=dd, aes(x=x1,xend=x2,y=y_pos, yend=y_pos), arrow=arrow(length = unit(0.125, "inches"), ends="both"),colour=colour_lines)}
      if(dd$arrow_low==TRUE & dd$arrow_high==FALSE){
        g <- g + geom_segment(data=dd, aes(x=x1, xend=x2,y=y_pos, yend=y_pos), arrow=arrow(length = unit(0.125, "inches"), ends="first"),colour=colour_lines)}
      if(dd$arrow_low==FALSE & dd$arrow_high==TRUE){
        g <- g + geom_segment(data=dd, aes(x=x1,xend=x2,y=y_pos, yend=y_pos), arrow=arrow(length = unit(0.125, "inches"), ends="last"),colour=colour_lines)}
      if(dd$arrow_low==FALSE & dd$arrow_high==FALSE){
        g <- g+ geom_segment(data=dd, aes(x=x1,xend=x2,y=y_pos, yend=y_pos), colour=colour_lines)}
    }
    
    g <- g + 
      geom_point(data=df_i, aes(x=test,y=y_pos, size=n),shape=pt_shape)
    
    n_d <- n_d -1 - l
  }  
  
  if (n_col_titles>0){ 
    for (k in 1:n_sg){
      g <- g + annotate("text",x=x_lab_start[1],y=subtitle[k],label=sg_name[k], hjust=0, fontface = "bold")
    }
  }
  
  
  #Overall
  if (!is.null(summary_name)){
    summary_df<-df$Summary
    g <- g + annotate("text",x=x_lab_start[1],y=1,label="Summary", hjust=0, fontface = "bold") +
      geom_point(data=summary_df, aes(x=test,y=1, size=n),shape=summary_shape) +
      geom_segment(data=summary_df, aes(x=lower,xend=upper,y=1, yend=1), colour=colour_lines) 
  }
  
  # Horizontal line
  if (!is.null(v_line)){
    g <- g + geom_vline(xintercept=v_line,linetype="dashed")
  }
  
  # labels
  if (n_col_titles>0){
    yy <- n_dat+1.5
    for (q in 1:n_col_titles){
      temp_lab <- col_titles[q]
      g <- g + annotate("text",x=x_lab_start[q],y=yy,label=temp_lab, hjust=0, fontface = "bold", size=5) 
    }
  }
  
  # g <- g + theme(axis.title.x=element_text(size=14, face="bold", hjust=1- x_range/(2*(range[2]-x_lab_start[1]))))
  g <- g   + xlim(x_lab_start[1],range[2]) + scale_x_continuous(breaks=seq(range[1],range[2],x_step_delta))  + xlab(stat_lable)
  w <- (range[2]-x_lab_pos)/(range[2]-x_lab_start[1])
  g <- g + theme(axis.title.x=element_text(size=14, face="bold", hjust=1-w, margin = margin(t=7, r=0, b=0, l=0)))
  
  # g <- g + annotate("text",x=x_lab_pos, y=-1,label=stat_lable, fontface = "bold", size=5)
  
  return(g)
}



