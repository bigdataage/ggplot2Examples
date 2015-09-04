






AllResults_g  <-  "Figures_H3K27me3_All"
system( paste("mkdir ",  AllResults_g,  sep = "  ",  collapse = NULL) )










#################################################################### Start ##########################################################################################################################################
#################################################################### Start ##########################################################################################################################################
#################################################################### Start ##########################################################################################################################################
#################################################################### Start ##########################################################################################################################################
#################################################################### Start ##########################################################################################################################################
library("reshape2")
library("ggplot2") 
library("grid")
library("Cairo")
library("RColorBrewer")
library("gplots")  
library("stats")
library("KernSmooth")
library("psych")
library("minerva")



pTheme_1 <- function( textSize=12 ) {  
    theme(  
        line  = element_line(colour="black", size=1.0,   linetype=1,      lineend=NULL),                                                          ## all line elements.  局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
        rect  = element_rect(colour="black", size=1.0,   linetype=1,      fill="transparent" ),                                                   ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
        text  = element_text(family="sans",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),    ## all text elements.  "sans" for a sans-sans font. 所有文本相关属性.
        title = element_text(family="sans",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=15,   angle=NULL, lineheight=NULL),    ## all title elements: plot, axes, legends. hjust:水平对齐的方向.  所有标题属性.

        axis.title        = element_text(family="sans", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
        axis.title.x      = element_text(family="sans", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=-15,  angle=NULL, lineheight=NULL),      ## x axis label (element_text; inherits from axis.title)
        axis.title.y      = element_text(family="sans", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=15,   angle=NULL, lineheight=NULL),      ## y axis label (element_text; inherits from axis.title)
        axis.text         = element_text(family="sans", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
        axis.text.x       = element_text(family="sans", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## x axis tick labels (element_text; inherits from axis.text)
        axis.text.y       = element_text(family="sans", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## y axis tick labels (element_text; inherits from axis.text)
        axis.ticks        = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                   ## tick marks along axes (element_line; inherits from line). 坐标轴刻度线.
        axis.ticks.x      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                   ## x axis tick marks (element_line; inherits from axis.ticks)
        axis.ticks.y      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                   ## y axis tick marks (element_line; inherits from axis.ticks)
        axis.ticks.length = grid::unit(1.5, "mm", data=NULL),                                                                                                   ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm.  刻度线长度
        axis.ticks.margin = grid::unit(1.0, "mm", data=NULL),  	                                                                                                ## space between tick mark and tick label (unit),  ‘"mm"’ Millimetres.  10 mm = 1 cm. 刻度线和刻度标签之间的间距.                                                                           
        axis.line         = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                        ## lines along axes (element_line; inherits from line). 坐标轴线
        axis.line.x       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                        ## line along x axis (element_line; inherits from axis.line)
        axis.line.y       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),	                                                        ## line along y axis (element_line; inherits from axis.line)
  
        legend.background    = element_rect(colour="transparent", size=1, linetype=1, fill="transparent" ), 	      ## background of legend (element_rect; inherits from rect)
        legend.margin        = grid::unit(0.5, "mm", data=NULL), 	                                              ## extra space added around legend (unit). linetype=1指的是矩形边框的类型.
        legend.key           = element_rect(colour="transparent", size=2, linetype=1, fill="transparent" ), 	      ## background underneath legend keys. 图例符号. size=1指的是矩形边框的大小.
        legend.key.size      = grid::unit(5, "mm", data=NULL) , 	                                              ## size of legend keys (unit; inherits from legend.key.size)
        legend.key.height    = grid::unit(7, "mm", data=NULL) , 	                                              ## key background height (unit; inherits from legend.key.size)
        legend.key.width     = grid::unit(5, "mm", data=NULL) ,                                                       ## key background width (unit; inherits from legend.key.size)
        legend.text          = element_text(family="sans", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
        legend.text.align    = 0, 	                ## alignment of legend labels (number from 0 (left) to 1 (right))
        legend.title         = element_blank(),   	## title of legend (element_text; inherits from title)
        legend.title.align   = 0, 	                ## alignment of legend title (number from 0 (left) to 1 (right))
        legend.position      = "right", 	        ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
        legend.direction     = "vertical",        	## layout of items in legends  ("horizontal" or "vertical")   图例排列方向
        legend.justification = "center",      	        ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)  图例居中方式
        legend.box           = NULL, 	                ## arrangement of multiple legends ("horizontal" or "vertical")  多图例的排列方式
        legend.box.just      = NULL, 	                ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")  多图例的居中方式
  
        panel.background   = element_rect(colour="transparent", size=0.0, linetype=1, fill="transparent" ),   	## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
        panel.border       = element_rect(colour="black", size=0.3, linetype=1, fill=NA ), 	                ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)
        panel.margin       = grid::unit(2, "mm", data=NULL) , 	                                                ## margin around facet panels (unit)  分面绘图区之间的边距
        panel.grid         = element_blank(), 	                                                                ## grid lines (element_line; inherits from line)  绘图区网格线
        panel.grid.major   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	## major grid lines (element_line; inherits from panel.grid)  主网格线
        panel.grid.minor   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## minor grid lines (element_line; inherits from panel.grid)  次网格线
        panel.grid.major.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	## vertical major grid lines (element_line; inherits from panel.grid.major)
        panel.grid.major.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal major grid lines (element_line; inherits from panel.grid.major)
        panel.grid.minor.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## vertical minor grid lines (element_line; inherits from panel.grid.minor)
        panel.grid.minor.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal minor grid lines (element_line; inherits from panel.grid.minor)
  
        plot.background	= element_rect(colour="transparent", size=NULL, linetype=NULL, fill="transparent" ),                                              ## background of the entire plot (element_rect; inherits from rect)  整个图形的背景
        plot.title      = element_text(family="sans", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	  ## plot title (text appearance) (element_text; inherits from title)  图形标题
        plot.margin     = grid::unit(c(8, 8, 8, 8), "mm", data=NULL), 	                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
  
        strip.background = element_rect(colour=NULL, size=NULL, linetype=NULL, fill=NULL ), 	                                                        ## background of facet labels (element_rect; inherits from rect)  分面标签背景
        strip.text       = element_text(family="sans", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	## facet labels (element_text; inherits from text)
        strip.text.x     = element_text(family="sans", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	## facet labels along horizontal direction (element_text; inherits from strip.text)
        strip.text.y     = element_text(family="sans", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	## facet labels along vertical direction (element_text; inherits from strip.text) 
    ) 
} 






pTheme_2 <- function( textSize=12 ) {  
  theme(  
    line  = element_line(colour="black", size=1.0,   linetype=1,      lineend=NULL),                                                          ## all line elements.  局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
    rect  = element_rect(colour="black", size=1.0,   linetype=1,      fill="transparent" ),                                                   ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
    text  = element_text(family="sans",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),    ## all text elements.  "sans" for a sans-sans font. 所有文本相关属性.
    title = element_text(family="sans",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=15,   angle=NULL, lineheight=NULL),    ## all title elements: plot, axes, legends. hjust:水平对齐的方向.  所有标题属性.
    
    axis.title        = element_text(family="sans", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
    axis.title.x      = element_text(family="sans", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=-15,  angle=NULL, lineheight=NULL),      ## x axis label (element_text; inherits from axis.title)
    axis.title.y      = element_text(family="sans", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=15,   angle=NULL, lineheight=NULL),      ## y axis label (element_text; inherits from axis.title)
    axis.text         = element_text(family="sans", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
    axis.text.x       = element_text(family="sans", face=NULL, colour="black", size=textSize-2,  hjust=1, vjust=1, angle=45, lineheight=NULL),      ## x axis tick labels (element_text; inherits from axis.text)
    axis.text.y       = element_text(family="sans", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),      ## y axis tick labels (element_text; inherits from axis.text)
    axis.ticks        = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                   ## tick marks along axes (element_line; inherits from line). 坐标轴刻度线.
    axis.ticks.x      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                   ## x axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.y      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                   ## y axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.length = grid::unit(1.5, "mm", data=NULL),                                                                                                   ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm.  刻度线长度
    axis.ticks.margin = grid::unit(1.0, "mm", data=NULL),                                                                                                    ## space between tick mark and tick label (unit),  ‘"mm"’ Millimetres.  10 mm = 1 cm. 刻度线和刻度标签之间的间距.                                                                           
    axis.line         = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                        ## lines along axes (element_line; inherits from line). 坐标轴线
    axis.line.x       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                        ## line along x axis (element_line; inherits from axis.line)
    axis.line.y       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),	
    
    legend.background    = element_rect(colour="transparent", size=1, linetype=1, fill="transparent" ), 	      ## background of legend (element_rect; inherits from rect)
    legend.margin        = grid::unit(0.5, "mm", data=NULL), 	                                              ## extra space added around legend (unit). linetype=1指的是矩形边框的类型.
    legend.key           = element_rect(colour="transparent", size=2, linetype=1, fill="transparent" ), 	      ## background underneath legend keys. 图例符号. size=1指的是矩形边框的大小.
    legend.key.size      = grid::unit(5, "mm", data=NULL) , 	                                              ## size of legend keys (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(7, "mm", data=NULL) , 	                                              ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(5, "mm", data=NULL) ,                                                       ## key background width (unit; inherits from legend.key.size)
    legend.text          = element_text(family="sans", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
    legend.text.align    = 0, 	                ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),   	## title of legend (element_text; inherits from title)
    legend.title.align   = 0, 	                ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right", 	        ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",        	## layout of items in legends  ("horizontal" or "vertical")   图例排列方向
    legend.justification = "center",      	        ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)  图例居中方式
    legend.box           = NULL, 	                ## arrangement of multiple legends ("horizontal" or "vertical")  多图例的排列方式
    legend.box.just      = NULL, 	                ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")  多图例的居中方式
    
    panel.background   = element_rect(colour="transparent", size=0.0, linetype=1, fill="transparent" ),   	## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", size=0.3, linetype=1, fill=NA ), 	                ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)
    panel.margin       = grid::unit(2, "mm", data=NULL) , 	                                                ## margin around facet panels (unit)  分面绘图区之间的边距
    panel.grid         = element_blank(), 	                                                                ## grid lines (element_line; inherits from line)  绘图区网格线
    panel.grid.major   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	## major grid lines (element_line; inherits from panel.grid)  主网格线
    panel.grid.minor   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## minor grid lines (element_line; inherits from panel.grid)  次网格线
    panel.grid.major.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal minor grid lines (element_line; inherits from panel.grid.minor)
    
    plot.background	= element_rect(colour="transparent", size=NULL, linetype=NULL, fill="transparent" ),                                              ## background of the entire plot (element_rect; inherits from rect)  整个图形的背景
    plot.title      = element_text(family="sans", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	  ## plot title (text appearance) (element_text; inherits from title)  图形标题
    plot.margin     = grid::unit(c(8, 8, 8, 8), "mm", data=NULL), 	                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    
    strip.background = element_rect(colour=NULL, size=NULL, linetype=NULL, fill=NULL ), 	                                                        ## background of facet labels (element_rect; inherits from rect)  分面标签背景
    strip.text       = element_text(family="sans", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="sans", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="sans", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 








## df contains two columns, the first column (cond_col=1) is sample type, the second column (val_col=2) is value. (must be).
whisk_1 <- function(df, cond_col=1, val_col=2) {  
  require(reshape2)
  condname <- names(df)[cond_col]  ## save the name of the first column.
  names(df)[cond_col] <- "cond" 
  names(df)[val_col]  <- "value"
  b   <- boxplot(value~cond, data=df, plot=FALSE)   
  df2 <- cbind(as.data.frame(b$stats), c("min","lq","m","uq","max"))
  names(df2) <- c(levels(df$cond), "pos")
  df2 <- melt(df2, id="pos", variable.name="cond")
  df2 <- dcast(df2,cond~pos)   
  names(df2)[1] <- condname 
  print(df2)
  df2
}





## order: P5_GFP, P5_EED, P14_EED, P25_EED, Ctrl, KO.
## No scale, matrix as input:
Eight_NOL <- function(AAVEED_1,  AAVEED_2,  AAVGFP_1,   AAVGFP_2,    ctrl_1,   ctrl_2,   KO_1, KO_2,  A1, A2,A3,   B1, B2,   File_Local,   Title_Local,   Ymin_Local=0,   Ymax_Local=10) {  
  BinNum_Local = 500
  Position_Local      <-  seq(from = -5,  by=0.02,  length.out=BinNum_Local)
  AAVEED_1_Frame <-  ksmooth(x=Position_Local,   y=colMeans(AAVEED_1),     kernel = "normal",   bandwidth = 0.1)
  AAVEED_2_Frame <-  ksmooth(x=Position_Local,   y=colMeans(AAVEED_2),     kernel = "normal",   bandwidth = 0.1)
  AAVGFP_1_Frame <-  ksmooth(x=Position_Local,   y=colMeans(AAVGFP_1),     kernel = "normal",   bandwidth = 0.1)
  AAVGFP_2_Frame <-  ksmooth(x=Position_Local,   y=colMeans(AAVGFP_2),     kernel = "normal",   bandwidth = 0.1)
  ctrl_1_Frame   <-  ksmooth(x=Position_Local,   y=colMeans(ctrl_1),       kernel = "normal",   bandwidth = 0.1)
  ctrl_2_Frame   <-  ksmooth(x=Position_Local,   y=colMeans(ctrl_2),       kernel = "normal",   bandwidth = 0.1)
  KO_1_Frame     <-  ksmooth(x=Position_Local,   y=colMeans(KO_1),         kernel = "normal",   bandwidth = 0.1)
  KO_2_Frame     <-  ksmooth(x=Position_Local,   y=colMeans(KO_2),         kernel = "normal",   bandwidth = 0.1)
  A1_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(A1),           kernel = "normal",   bandwidth = 0.1)
  A2_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(A2),           kernel = "normal",   bandwidth = 0.1)
  A3_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(A3),           kernel = "normal",   bandwidth = 0.1)
  B1_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(B1),           kernel = "normal",   bandwidth = 0.1)
  B2_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(B2),           kernel = "normal",   bandwidth = 0.1)
    
  AAVEED_1_Y <- AAVEED_1_Frame$y  
  AAVEED_2_Y <- AAVEED_2_Frame$y
  AAVGFP_1_Y <- AAVGFP_1_Frame$y
  AAVGFP_2_Y <- AAVGFP_2_Frame$y
  ctrl_1_Y   <- ctrl_1_Frame$y
  ctrl_2_Y   <- ctrl_2_Frame$y
  KO_1_Y     <- KO_1_Frame$y
  KO_2_Y     <- KO_2_Frame$y
  A1_Y       <- A1_Frame$y
  A2_Y       <- A2_Frame$y
  A3_Y       <- A3_Frame$y
  B1_Y       <- B1_Frame$y
  B2_Y       <- B2_Frame$y
  
  ### order: P5_GFP, P5_EED, P14_EED, P25_EED, Ctrl, KO.
  x_Axis_Local     <- c( Position_Local,  Position_Local,     Position_Local,     Position_Local,  Position_Local, Position_Local,  Position_Local,     Position_Local,     Position_Local,  Position_Local,  Position_Local,     Position_Local,     Position_Local)                       
  y_Axis_Local     <- c( AAVEED_1_Y, AAVEED_2_Y,   AAVGFP_1_Y, AAVGFP_2_Y,     ctrl_1_Y, ctrl_2_Y,    KO_1_Y,  KO_2_Y, A1_Y, A2_Y,  A3_Y,   B1_Y, B2_Y )
  SampleType_Local <- c(  rep("P5_GFP_Rep1", BinNum_Local), rep("P5_GFP_Rep2", BinNum_Local),  rep("P5_EED_Rep1", BinNum_Local), rep("P5_EED_Rep2", BinNum_Local),  rep("P14_EED_Rep1", BinNum_Local),  rep("P14_EED_Rep2", BinNum_Local),   rep("P25_EED_Rep1", BinNum_Local),  rep("P25_EED_Rep2", BinNum_Local),  rep("EED_Ctrl_Rep1", BinNum_Local),  rep("EED_Ctrl_Rep2", BinNum_Local),  rep("EED_Ctrl_Rep3", BinNum_Local),  rep("EED_KO_Rep1", BinNum_Local),  rep("EED_KO_Rep2", BinNum_Local) )                                                                                       
  DataFrame_Local  <- data.frame(xAxis = x_Axis_Local,  yAxis = y_Axis_Local,  SampleType = SampleType_Local) 
  col_values  <- c("P5_GFP_Rep1"="red",  "P5_GFP_Rep2"="red4",   "P5_EED_Rep1"="gold",   "P5_EED_Rep2"="gold4",   "P14_EED_Rep1"="pink",  "P14_EED_Rep2"="pink4",   "P25_EED_Rep1"="blue",  "P25_EED_Rep2"="blue4",   "EED_Ctrl_Rep1"="yellowgreen",  "EED_Ctrl_Rep2"="green",  "EED_Ctrl_Rep3"="green4",  "EED_KO_Rep1"="purple",    "EED_KO_Rep2"="purple4" )
  sample_rank <- c( "EED_Ctrl_Rep1",  "EED_Ctrl_Rep2",  "EED_Ctrl_Rep3",  "EED_KO_Rep1",    "EED_KO_Rep2",  "P5_GFP_Rep1", "P5_GFP_Rep2",   "P5_EED_Rep1",   "P5_EED_Rep2",   "P14_EED_Rep1",  "P14_EED_Rep2",    "P25_EED_Rep1",  "P25_EED_Rep2" )
  
  CairoSVG(file = File_Local,   width = 7,  height = 4, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=col_values, breaks=sample_rank  ) +    
    geom_line(size=0.5) + 
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +   
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
  
  File2_Local = paste(File_Local, ".scaled.svg", seq="")
  CairoSVG(file = File2_Local,   width = 7,  height = 4, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=col_values, breaks=sample_rank  ) + 
    geom_line(size=0.5) + ylim(Ymin_Local, Ymax_Local) +
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +  
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
}  






## order: P5_GFP, P5_EED, P14_EED, P25_EED
Four_NOL <- function(AAVEED,   AAVGFP,    ctrl,    KO,   A1, B1,  File_Local, Title_Local, Ymin_Local=0, Ymax_Local=12) {  
  BinNum_Local = 500
  Position_Local      <-  seq(from = -5,  by=0.02,  length.out=BinNum_Local)

  AAVEED_2_Frame <-  ksmooth(x=Position_Local,   y=colMeans(AAVEED),     kernel = "normal",   bandwidth = 0.1)
  AAVGFP_1_Frame <-  ksmooth(x=Position_Local,   y=colMeans(AAVGFP),     kernel = "normal",   bandwidth = 0.1)
  ctrl_1_Frame   <-  ksmooth(x=Position_Local,   y=colMeans(ctrl),       kernel = "normal",   bandwidth = 0.1)
  KO_1_Frame     <-  ksmooth(x=Position_Local,   y=colMeans(KO),         kernel = "normal",   bandwidth = 0.1)
  A1_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(A1),         kernel = "normal",   bandwidth = 0.1)
  B1_Frame       <-  ksmooth(x=Position_Local,   y=colMeans(B1),         kernel = "normal",   bandwidth = 0.1)
  
  AAVEED_2_Y <- AAVEED_2_Frame$y
  AAVGFP_1_Y <- AAVGFP_1_Frame$y
  ctrl_1_Y   <- ctrl_1_Frame$y
  KO_1_Y     <- KO_1_Frame$y
  A1_Y     <- A1_Frame$y
  B1_Y     <- B1_Frame$y
  
  x_Axis_Local     <- c( Position_Local,  Position_Local,     Position_Local,     Position_Local,     Position_Local,     Position_Local)                       
  y_Axis_Local     <- c( AAVEED_2_Y,   AAVGFP_1_Y,     ctrl_1_Y,    KO_1_Y,   A1_Y,   B1_Y )
  SampleType_Local <- c(  rep("P5_GFP", BinNum_Local), rep("P5_EED", BinNum_Local),   rep("P14_EED", BinNum_Local),   rep("P25_EED", BinNum_Local),   rep("EED_Ctrl", BinNum_Local),   rep("EED_KO", BinNum_Local) )  
  DataFrame_Local  <- data.frame(xAxis = x_Axis_Local,  yAxis = y_Axis_Local,  SampleType = SampleType_Local) 
  
  CairoSVG(file = File_Local,   width = 6,  height = 3.5, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=c("P5_GFP"="red",  "P5_EED"="gold",    "P14_EED"="pink4",   "P25_EED"="blue", "EED_Ctrl"="green", "EED_KO"="purple"), breaks=c("EED_Ctrl", "EED_KO", "P5_GFP", "P5_EED", "P14_EED", "P25_EED")  ) +    
    geom_line(size=0.5) + 
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +   
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
  
  File2_Local = paste(File_Local, ".scaled.svg", seq="")
  CairoSVG(file = File2_Local,   width = 6,  height = 3.5, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=c("P5_GFP"="red",  "P5_EED"="gold",    "P14_EED"="pink4",   "P25_EED"="blue", "EED_Ctrl"="green", "EED_KO"="purple"), breaks=c("EED_Ctrl", "EED_KO", "P5_GFP", "P5_EED", "P14_EED", "P25_EED")  ) +    
    geom_line(size=0.5) + ylim(Ymin_Local, Ymax_Local) +
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +  
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
}  












## order: P5_GFP, P5_EED, P14_EED, P25_EED
## No scale, matrix as input:
Eight_NOL_Scale <- function(AAVEED_1,  AAVEED_2,  AAVGFP_1,   AAVGFP_2,    ctrl_1,   ctrl_2,   KO_1, KO_2,  A1, A2, A3, B1, B2, File_Local,   Title_Local,   Ymin_Local=0,   Ymax_Local=10) {  
  BinNum_Local = 500
  Position_Local      <-  seq(from = -5,  by=0.02,  length.out=BinNum_Local)
  AAVEED_1_Frame <-  ksmooth(x=Position_Local,   y=AAVEED_1,     kernel = "normal",   bandwidth = 0.1)
  AAVEED_2_Frame <-  ksmooth(x=Position_Local,   y=AAVEED_2,     kernel = "normal",   bandwidth = 0.1)
  AAVGFP_1_Frame <-  ksmooth(x=Position_Local,   y=AAVGFP_1,     kernel = "normal",   bandwidth = 0.1)
  AAVGFP_2_Frame <-  ksmooth(x=Position_Local,   y=AAVGFP_2,     kernel = "normal",   bandwidth = 0.1)
  ctrl_1_Frame   <-  ksmooth(x=Position_Local,   y=ctrl_1,       kernel = "normal",   bandwidth = 0.1)
  ctrl_2_Frame   <-  ksmooth(x=Position_Local,   y=ctrl_2,       kernel = "normal",   bandwidth = 0.1)
  KO_1_Frame     <-  ksmooth(x=Position_Local,   y=KO_1,         kernel = "normal",   bandwidth = 0.1)
  KO_2_Frame     <-  ksmooth(x=Position_Local,   y=KO_2,         kernel = "normal",   bandwidth = 0.1)
  A1_Frame       <-  ksmooth(x=Position_Local,   y=A1,           kernel = "normal",   bandwidth = 0.1)
  A2_Frame       <-  ksmooth(x=Position_Local,   y=A2,           kernel = "normal",   bandwidth = 0.1)
  A3_Frame       <-  ksmooth(x=Position_Local,   y=A3,           kernel = "normal",   bandwidth = 0.1)
  B1_Frame       <-  ksmooth(x=Position_Local,   y=B1,           kernel = "normal",   bandwidth = 0.1)
  B2_Frame       <-  ksmooth(x=Position_Local,   y=B2,           kernel = "normal",   bandwidth = 0.1)
  
  AAVEED_1_Y <- AAVEED_1_Frame$y  
  AAVEED_2_Y <- AAVEED_2_Frame$y
  AAVGFP_1_Y <- AAVGFP_1_Frame$y
  AAVGFP_2_Y <- AAVGFP_2_Frame$y
  ctrl_1_Y   <- ctrl_1_Frame$y
  ctrl_2_Y   <- ctrl_2_Frame$y
  KO_1_Y     <- KO_1_Frame$y
  KO_2_Y     <- KO_2_Frame$y
  A1_Y       <- A1_Frame$y
  A2_Y       <- A2_Frame$y
  A3_Y       <- A3_Frame$y
  B1_Y       <- B1_Frame$y
  B2_Y       <- B2_Frame$y
  
  x_Axis_Local     <- c( Position_Local,  Position_Local,     Position_Local,   Position_Local,    Position_Local,  Position_Local,  Position_Local,     Position_Local,     Position_Local,  Position_Local,  Position_Local,     Position_Local,     Position_Local)                       
  y_Axis_Local     <- c( AAVEED_1_Y, AAVEED_2_Y,   AAVGFP_1_Y, AAVGFP_2_Y,     ctrl_1_Y, ctrl_2_Y,   KO_1_Y,  KO_2_Y, A1_Y, A2_Y,  A3_Y, B1_Y, B2_Y )
  SampleType_Local <- c(  rep("P5_GFP_Rep1", BinNum_Local), rep("P5_GFP_Rep2", BinNum_Local),  rep("P5_EED_Rep1", BinNum_Local), rep("P5_EED_Rep2", BinNum_Local),  rep("P14_EED_Rep1", BinNum_Local),  rep("P14_EED_Rep2", BinNum_Local),   rep("P25_EED_Rep1", BinNum_Local),  rep("P25_EED_Rep2", BinNum_Local),  rep("EED_Ctrl_Rep1", BinNum_Local),  rep("EED_Ctrl_Rep2", BinNum_Local),   rep("EED_Ctrl_Rep3", BinNum_Local),  rep("EED_KO_Rep1", BinNum_Local),  rep("EED_KO_Rep2", BinNum_Local) )                                                                                       
  DataFrame_Local  <- data.frame(xAxis = x_Axis_Local,  yAxis = y_Axis_Local,  SampleType = SampleType_Local) 
  col_values  <- c("P5_GFP_Rep1"="red",  "P5_GFP_Rep2"="red4",   "P5_EED_Rep1"="gold",   "P5_EED_Rep2"="gold4",   "P14_EED_Rep1"="pink",  "P14_EED_Rep2"="pink4",   "P25_EED_Rep1"="blue",  "P25_EED_Rep2"="blue4",   "EED_Ctrl_Rep1"="yellowgreen",  "EED_Ctrl_Rep2"="green",  "EED_Ctrl_Rep3"="green4",  "EED_KO_Rep1"="purple",    "EED_KO_Rep2"="purple4" )
  sample_rank <- c( "EED_Ctrl_Rep1",  "EED_Ctrl_Rep2",  "EED_Ctrl_Rep3",  "EED_KO_Rep1",    "EED_KO_Rep2",  "P5_GFP_Rep1", "P5_GFP_Rep2",   "P5_EED_Rep1",   "P5_EED_Rep2",   "P14_EED_Rep1",  "P14_EED_Rep2",    "P25_EED_Rep1",  "P25_EED_Rep2" )
  
  CairoSVG(file = File_Local,   width = 7,  height = 4, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("Scaled H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=col_values, breaks=sample_rank  ) +    
    geom_line(size=0.5) + 
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +   
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
  
  File2_Local = paste(File_Local, ".scaled.svg", seq="")
  CairoSVG(file = File2_Local,   width = 7,  height = 4, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("Scaled H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=col_values, breaks=sample_rank  ) + 
    geom_line(size=0.5) + ylim(Ymin_Local, Ymax_Local) +
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +  
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
}  






## order: P5_GFP, P5_EED, P14_EED, P25_EED
Four_NOL_Scale <- function(AAVEED,   AAVGFP,    ctrl,    KO,   A1, B1,  File_Local, Title_Local, Ymin_Local=0, Ymax_Local=12) {  
  BinNum_Local = 500
  Position_Local      <-  seq(from = -5,  by=0.02,  length.out=BinNum_Local)
  
  AAVEED_2_Frame <-  ksmooth(x=Position_Local,   y=AAVEED,     kernel = "normal",   bandwidth = 0.1)
  AAVGFP_1_Frame <-  ksmooth(x=Position_Local,   y=AAVGFP,     kernel = "normal",   bandwidth = 0.1)
  ctrl_1_Frame   <-  ksmooth(x=Position_Local,   y=ctrl,       kernel = "normal",   bandwidth = 0.1)
  KO_1_Frame     <-  ksmooth(x=Position_Local,   y=KO,         kernel = "normal",   bandwidth = 0.1)
  A1_Frame       <-  ksmooth(x=Position_Local,   y=A1,         kernel = "normal",   bandwidth = 0.1)
  B1_Frame       <-  ksmooth(x=Position_Local,   y=B1,         kernel = "normal",   bandwidth = 0.1)
  
  AAVEED_2_Y <- AAVEED_2_Frame$y
  AAVGFP_1_Y <- AAVGFP_1_Frame$y
  ctrl_1_Y   <- ctrl_1_Frame$y
  KO_1_Y     <- KO_1_Frame$y
  A1_Y     <- A1_Frame$y
  B1_Y     <- B1_Frame$y
  
  x_Axis_Local     <- c( Position_Local,  Position_Local,     Position_Local,     Position_Local,     Position_Local,     Position_Local)                       
  y_Axis_Local     <- c( AAVEED_2_Y,   AAVGFP_1_Y,     ctrl_1_Y,    KO_1_Y,   A1_Y,   B1_Y )
  SampleType_Local <- c(  rep("P5_GFP", BinNum_Local), rep("P5_EED", BinNum_Local),   rep("P14_EED", BinNum_Local),   rep("P25_EED", BinNum_Local),   rep("EED_Ctrl", BinNum_Local),   rep("EED_KO", BinNum_Local) )  
  DataFrame_Local  <- data.frame(xAxis = x_Axis_Local,  yAxis = y_Axis_Local,  SampleType = SampleType_Local) 
  
  CairoSVG(file = File_Local,   width = 6,  height = 3.5, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("Scaled H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=c("P5_GFP"="red",  "P5_EED"="gold",    "P14_EED"="pink4",   "P25_EED"="blue", "EED_Ctrl"="green", "EED_KO"="purple"), breaks=c("EED_Ctrl", "EED_KO", "P5_GFP", "P5_EED", "P14_EED", "P25_EED")  ) +    
    geom_line(size=0.5) + 
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +   
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
  
  File2_Local = paste(File_Local, ".scaled.svg", seq="")
  CairoSVG(file = File2_Local,   width = 6,  height = 3.5, onefile = TRUE,  bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab("Relative Distance (kb)") +  ylab("Scaled H3K27me3 Signal") +  ggtitle(Title_Local) + 
    scale_colour_manual( values=c("P5_GFP"="red",  "P5_EED"="gold",    "P14_EED"="pink4",   "P25_EED"="blue", "EED_Ctrl"="green", "EED_KO"="purple"), breaks=c("EED_Ctrl", "EED_KO", "P5_GFP", "P5_EED", "P14_EED", "P25_EED")  ) +    
    geom_line(size=0.5) + ylim(Ymin_Local, Ymax_Local) +
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +  
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "peak center",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp ) 
  dev.off() 
}  














BoxPlot_ViolinPlot_8 <- function(P5_GFP_Rep1_Local, P5_GFP_Rep2_Local,   P5_EED_Rep1_Local,   P5_EED_Rep2_Local,   P14_EED_Rep1_Local,  P14_EED_Rep2_Local,   P25_EED_Rep1_Local,  P25_EED_Rep2_Local,  A1, A2, A3, B1, B2,   File_Local, Title_Local, OccuT_Local=5 )  {
  a <- OccuT_Local
  y_Axis_local_AllC  <- c( P5_GFP_Rep1_Local, P5_GFP_Rep2_Local,   P5_EED_Rep1_Local,   P5_EED_Rep2_Local,   P14_EED_Rep1_Local,  P14_EED_Rep2_Local,   P25_EED_Rep1_Local,  P25_EED_Rep2_Local, A1, A2, A3, B1, B2 ) 
  y_Axis_local_AllC[y_Axis_local_AllC>a]  <- a
  leng_Local <- length(P5_GFP_Rep1_Local)
  sampleType_local_AllC <-  c( rep("P5_GFP_Rep1", leng_Local),  rep("P5_GFP_Rep2", leng_Local), rep("P5_EED_Rep1", leng_Local), rep("P5_EED_Rep2", leng_Local), rep("P14_EED_Rep1", leng_Local), rep("P14_EED_Rep2", leng_Local), rep("P25_EED_Rep1", leng_Local), rep("P25_EED_Rep2", leng_Local) ,  rep("EED_Ctrl_Rep1", leng_Local),  rep("EED_Ctrl_Rep2", leng_Local),   rep("EED_Ctrl_Rep3", leng_Local),  rep("EED_KO_Rep1", leng_Local),  rep("EED_KO_Rep2", leng_Local)            )                         
  dataframe_local_AllC <- data.frame(sampleType = sampleType_local_AllC, yAxis = y_Axis_local_AllC  )  ## the order is very important.
  dataframePath1 <- data.frame( x=c(1.05, 1.05, 1.95, 1.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath2 <- data.frame( x=c(2.05, 2.05, 2.95, 2.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath3 <- data.frame( x=c(3.05, 3.05, 3.95, 3.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath4 <- data.frame( x=c(4.05, 4.05, 4.95, 4.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath5 <- data.frame( x=c(5.05, 5.05, 5.95, 5.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath6 <- data.frame( x=c(6.05, 6.05, 6.95, 6.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath7 <- data.frame( x=c(7.05, 7.05, 7.95, 7.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath8 <- data.frame( x=c(8.05, 8.05, 8.95, 8.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath9 <- data.frame( x=c(9.05, 9.05, 9.95, 9.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath10 <- data.frame( x=c(10.05, 10.05, 10.95, 10.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath11 <- data.frame( x=c(11.05, 11.05, 11.95, 11.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  dataframePath12 <- data.frame( x=c(12.05, 12.05, 12.95, 12.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) ) 
  
  col_values  <- c("P5_GFP_Rep1"="red",  "P5_GFP_Rep2"="red4",   "P5_EED_Rep1"="gold",   "P5_EED_Rep2"="gold4",   "P14_EED_Rep1"="pink",  "P14_EED_Rep2"="pink4",   "P25_EED_Rep1"="blue",  "P25_EED_Rep2"="blue4",   "EED_Ctrl_Rep1"="yellowgreen",  "EED_Ctrl_Rep2"="green",  "EED_Ctrl_Rep3"="green4",  "EED_KO_Rep1"="purple",    "EED_KO_Rep2"="purple4" )
  sample_rank <- c( "EED_Ctrl_Rep1",  "EED_Ctrl_Rep2",  "EED_Ctrl_Rep3",  "EED_KO_Rep1",    "EED_KO_Rep2",  "P5_GFP_Rep1", "P5_GFP_Rep2",   "P5_EED_Rep1",   "P5_EED_Rep2",   "P14_EED_Rep1",  "P14_EED_Rep2",    "P25_EED_Rep1",  "P25_EED_Rep2" )
  col_values2 <- c( "yellowgreen", "green",  "green4",   "red",  "red4",  "gold",  "gold4",   "pink",  "pink4",  "blue",  "blue4",   "purple",   "purple4" )
  
  sink( paste(File_Local, "-D-Box-plot-violinPlot.txt", sep="") )
  
  CairoSVG( file = paste(File_Local, "-D-boxplot.svg", sep=""),   width = 9.1, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot( dataframe_local_AllC, aes(x=sampleType) ) + geom_errorbar( aes(ymin=min, ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.3, size=0.2 ) +
    geom_boxplot( width=0.7,   aes(y=yAxis), outlier.colour="gray",  outlier.shape=1, outlier.size=0.001, size=0.2, fill=col_values2 ) + 
    xlab( "Samples" ) + ylab( "H3K27me3 Signal" ) + ggtitle(Title_Local )  + pTheme_2(textSize=12) 
  print(FigureTemp) 
  dev.off() 
  
  CairoSVG( file = paste(File_Local, "-D-boxplot-annotate.svg", sep=""),   width = 9.1, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot( dataframe_local_AllC, aes(x=sampleType) ) + geom_errorbar( aes(ymin=min, ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.3, size=0.2 ) +
    geom_boxplot( width=0.7,   aes(y=yAxis), outlier.colour="gray",  outlier.shape=1, outlier.size=0.001, size=0.2, fill=col_values2 ) + 
    xlab( "Samples") + ylab( "H3K27me3 Signal" ) + ggtitle( Title_Local )  + pTheme_2(textSize=12) + 
    geom_path( data = dataframePath1, aes(x = x, y = y), size=0.3 ) + annotate("text", x=1.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath2, aes(x = x, y = y), size=0.3 ) + annotate("text", x=2.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath3, aes(x = x, y = y), size=0.3 ) + annotate("text", x=3.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath4, aes(x = x, y = y), size=0.3 ) + annotate("text", x=4.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath5, aes(x = x, y = y), size=0.3 ) + annotate("text", x=5.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath6, aes(x = x, y = y), size=0.3 ) + annotate("text", x=6.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath7, aes(x = x, y = y), size=0.3 ) + annotate("text", x=7.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath8, aes(x = x, y = y), size=0.3 ) + annotate("text", x=8.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath9, aes(x = x, y = y), size=0.3 ) + annotate("text", x=9.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath10, aes(x = x, y = y), size=0.3 ) + annotate("text", x=10.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath11, aes(x = x, y = y), size=0.3 ) + annotate("text", x=11.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath12, aes(x = x, y = y), size=0.3 ) + annotate("text", x=12.5, y=a+0.5, label="p<3e-16", size=2.7) 
  print(FigureTemp) 
  dev.off() 
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot.svg", sep=""),  width = 9.1, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 5) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=col_values2, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  
  print(FigureTemp) 
  dev.off() 
  
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot-annotate.svg", sep=""),  width = 9.1, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 5) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),    width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),   width=0.3, size=0.2,  fill=col_values2, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  + 
    geom_path( data = dataframePath1, aes(x = x, y = y), size=0.3 ) + annotate("text", x=1.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath2, aes(x = x, y = y), size=0.3 ) + annotate("text", x=2.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath3, aes(x = x, y = y), size=0.3 ) + annotate("text", x=3.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath4, aes(x = x, y = y), size=0.3 ) + annotate("text", x=4.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath5, aes(x = x, y = y), size=0.3 ) + annotate("text", x=5.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath6, aes(x = x, y = y), size=0.3 ) + annotate("text", x=6.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath7, aes(x = x, y = y), size=0.3 ) + annotate("text", x=7.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath8, aes(x = x, y = y), size=0.3 ) + annotate("text", x=8.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath9, aes(x = x, y = y), size=0.3 ) + annotate("text", x=9.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath10, aes(x = x, y = y), size=0.3 ) + annotate("text", x=10.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath11, aes(x = x, y = y), size=0.3 ) + annotate("text", x=11.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath12, aes(x = x, y = y), size=0.3 ) + annotate("text", x=12.5, y=a+0.5, label="p<3e-16", size=2.7) 
  print(FigureTemp) 
  dev.off() 
  
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot-noAdjust.svg", sep=""), width = 9.1, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray") +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),    width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),   width=0.3, size=0.2, fill=col_values2, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  
  print(FigureTemp) 
  dev.off() 
  
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot-annotate-noAdjust.svg", sep=""),  width = 9.1, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray") +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),   width=0.3, size=0.2,  fill=col_values2, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  + 
    #geom_path( data = dataframePath0, aes(x = x, y = y), size=0.3 ) + annotate("text", x=0.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath1, aes(x = x, y = y), size=0.3 ) + annotate("text", x=1.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath2, aes(x = x, y = y), size=0.3 ) + annotate("text", x=2.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath3, aes(x = x, y = y), size=0.3 ) + annotate("text", x=3.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath4, aes(x = x, y = y), size=0.3 ) + annotate("text", x=4.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath5, aes(x = x, y = y), size=0.3 ) + annotate("text", x=5.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath6, aes(x = x, y = y), size=0.3 ) + annotate("text", x=6.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath7, aes(x = x, y = y), size=0.3 ) + annotate("text", x=7.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath8, aes(x = x, y = y), size=0.3 ) + annotate("text", x=8.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath9, aes(x = x, y = y), size=0.3 ) + annotate("text", x=9.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath10, aes(x = x, y = y), size=0.3 ) + annotate("text", x=10.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath11, aes(x = x, y = y), size=0.3 ) + annotate("text", x=11.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath12, aes(x = x, y = y), size=0.3 ) + annotate("text", x=12.5, y=a+0.5, label="p<3e-16", size=2.7) 
  print(FigureTemp) 
  dev.off() 
  
  sink()   
  
}





BoxPlot_ViolinPlot_4 <- function(P5_GFP_Rep1_Local,   P5_EED_Rep1_Local,     P14_EED_Rep1_Local,     P25_EED_Rep1_Local,   A1,  B1,   File_Local, Title_Local, OccuT_Local=5 )  {
  a <- OccuT_Local
  y_Axis_local_AllC  <- c( P5_GFP_Rep1_Local,    P5_EED_Rep1_Local,    P14_EED_Rep1_Local,   P25_EED_Rep1_Local,   A1,  B1) 
  y_Axis_local_AllC[y_Axis_local_AllC>a]  <- a
  leng_Local <- length(P5_GFP_Rep1_Local)
  sampleType_local_AllC <-  c( rep("P5_GFP", leng_Local),   rep("P5_EED", leng_Local),  rep("P14_EED", leng_Local),  rep("P25_EED", leng_Local),   rep("EED_Ctrl", leng_Local),   rep("EED_KO", leng_Local)   )                         
  dataframe_local_AllC <- data.frame(sampleType = sampleType_local_AllC, yAxis = y_Axis_local_AllC  )  ## the order is very important.
  dataframePath1 <- data.frame( x=c(1.05, 1.05, 1.95, 1.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath2 <- data.frame( x=c(2.05, 2.05, 2.95, 2.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath3 <- data.frame( x=c(3.05, 3.05, 3.95, 3.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath4 <- data.frame( x=c(4.05, 4.05, 4.95, 4.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  dataframePath5 <- data.frame( x=c(5.05, 5.05, 5.95, 5.95),  y=c(a+0.05, a+0.1, a+0.1, a+0.05) )  
  
  col_values   <- c("P5_GFP"="red",  "P5_EED"="gold",    "P14_EED"="pink4",   "P25_EED"="blue", "EED_Ctrl"="green", "EED_KO"="purple")
  col_values2  <- c("red",  "gold",    "pink4",   "blue", "green", "purple")
  sample_rank <- c("EED_Ctrl", "EED_KO", "P5_GFP",   "P5_EED",    "P14_EED",    "P25_EED")
  
  sink( paste(File_Local, "-D-Box-plot-violinPlot.txt", sep="") )
  
  CairoSVG( file = paste(File_Local, "-D-boxplot.svg", sep=""),   width = 4, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot( dataframe_local_AllC, aes(x=sampleType) ) + geom_errorbar( aes(ymin=min, ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.3, size=0.2 ) +
    geom_boxplot( width=0.7,   aes(y=yAxis), outlier.colour="gray",  outlier.shape=1, outlier.size=0.001, size=0.2, fill=col_values ) + 
    xlab( "Samples" ) + ylab( "H3K27me3 Signal" ) + ggtitle(Title_Local )  + pTheme_2(textSize=12) 
  print(FigureTemp) 
  dev.off() 
  
  CairoSVG( file = paste(File_Local, "-D-boxplot-annotate.svg", sep=""),   width = 4, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot( dataframe_local_AllC, aes(x=sampleType) ) + geom_errorbar( aes(ymin=min, ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.3, size=0.2 ) +
    geom_boxplot( width=0.7,   aes(y=yAxis), outlier.colour="gray",  outlier.shape=1, outlier.size=0.001, size=0.2, fill=col_values ) + 
    xlab( "Samples") + ylab( "H3K27me3 Signal" ) + ggtitle( Title_Local )  + pTheme_2(textSize=12) + 
    geom_path( data = dataframePath1, aes(x = x, y = y), size=0.3 ) + annotate("text", x=1.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath2, aes(x = x, y = y), size=0.3 ) + annotate("text", x=2.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath3, aes(x = x, y = y), size=0.3 ) + annotate("text", x=3.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath4, aes(x = x, y = y), size=0.3 ) + annotate("text", x=4.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath5, aes(x = x, y = y), size=0.3 ) + annotate("text", x=5.5, y=a+0.5, label="p<3e-16", size=2.7) 
  print(FigureTemp) 
  dev.off() 
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot.svg", sep=""),  width = 4, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 5) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=col_values, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  
  print(FigureTemp) 
  dev.off() 
  
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot-annotate.svg", sep=""),  width = 4, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 5) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),    width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=col_values, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) +  
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  + 
    geom_path( data = dataframePath1, aes(x = x, y = y), size=0.3 ) + annotate("text", x=1.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath2, aes(x = x, y = y), size=0.3 ) + annotate("text", x=2.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath3, aes(x = x, y = y), size=0.3 ) + annotate("text", x=3.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath4, aes(x = x, y = y), size=0.3 ) + annotate("text", x=4.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath5, aes(x = x, y = y), size=0.3 ) + annotate("text", x=5.5, y=a+0.5, label="p<3e-16", size=2.7) 
  print(FigureTemp) 
  dev.off() 
  
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot-noAdjust.svg", sep=""), width = 4, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray") +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),    width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=col_values, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) +  
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  
  print(FigureTemp) 
  dev.off() 
  
  
  CairoSVG(file = paste(File_Local, "-D-violinPlot-annotate-noAdjust.svg", sep=""),  width = 4, height =5, onefile = TRUE, bg = "white",  pointsize = 1 )
  FigureTemp <- ggplot(dataframe_local_AllC, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray") +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(dataframe_local_AllC),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=col_values, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) +  
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab("Samples") + ylab("H3K27me3 Signal") + ggtitle(Title_Local)  + pTheme_2(textSize=12)  + 
    #geom_path( data = dataframePath0, aes(x = x, y = y), size=0.3 ) + annotate("text", x=0.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath1, aes(x = x, y = y), size=0.3 ) + annotate("text", x=1.5, y=a+0.5, label="p<3e-16", size=2.7) + 
    geom_path( data = dataframePath2, aes(x = x, y = y), size=0.3 ) + annotate("text", x=2.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath3, aes(x = x, y = y), size=0.3 ) + annotate("text", x=3.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath4, aes(x = x, y = y), size=0.3 ) + annotate("text", x=4.5, y=a+0.5, label="p<3e-16", size=2.7) +
    geom_path( data = dataframePath5, aes(x = x, y = y), size=0.3 ) + annotate("text", x=5.5, y=a+0.5, label="p<3e-16", size=2.7) 
  print(FigureTemp) 
  dev.off() 
  
  sink()   
  
}




#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
























#################################################################### Start ##########################################################################################################################################
#################################################################### Start ##########################################################################################################################################
P5_EED_Rep1    <- read.table("H3K27me3_P5EED/H3K27me3_CM_adult_P5rescueAAVEED_Rep1_center_heatmap/a.xls",               header=TRUE,   sep="",   quote = "",   comment.char = "")  
P5_EED_Rep2    <- read.table("H3K27me3_P5EED/H3K27me3_CM_adult_P5rescueAAVEED_Rep2_center_heatmap/a.xls",               header=TRUE,   sep="",   quote = "",   comment.char = "")  
P5_GFP_Rep1    <- read.table("H3K27me3_P5GFP/H3K27me3_CM_adult_P5rescueAAVGFP_Rep1_center_heatmap/a.xls",               header=TRUE,   sep="",   quote = "",   comment.char = "")  
P5_GFP_Rep2    <- read.table("H3K27me3_P5GFP/H3K27me3_CM_adult_P5rescueAAVGFP_Rep1_center_heatmap/a.xls",               header=TRUE,   sep="",   quote = "",   comment.char = "")  
P14_EED_Rep1   <- read.table("H3K27me3_P14_AAVEED/H3K27me3_CM_adult_P14rescueAAVEED_Rep1_center_heatmap/a.xls",                    header=TRUE,   sep="",   quote = "",   comment.char = "")  
P14_EED_Rep2   <- read.table("H3K27me3_P14_AAVEED/H3K27me3_CM_adult_P14rescueAAVEED_Rep2_center_heatmap/a.xls",                    header=TRUE,   sep="",   quote = "",   comment.char = "")  
P25_EED_Rep1   <- read.table("H3K27me3_P25_AAVEED/H3K27me3_CM_adult_P25rescueAAVEED_Rep1_center_heatmap/a.xls",                     header=TRUE,   sep="",   quote = "",   comment.char = "")  
P25_EED_Rep2   <- read.table("H3K27me3_P25_AAVEED/H3K27me3_CM_adult_P25rescueAAVEED_Rep2_center_heatmap/a.xls",                    header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_ctrl_Rep1    <- read.table("H3K27me3_ctrl/H3K27me3_CM_adult_Ctrl_Rep1_center_heatmap/a.xls",                   header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_ctrl_Rep2    <- read.table("H3K27me3_ctrl/H3K27me3_CM_adult_Ctrl_Rep2_center_heatmap/a.xls",                   header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_ctrl_Rep3    <- read.table("H3K27me3_ctrl/H3K27me3_CM_adult_Ctrl_Rep3_center_heatmap/a.xls",                   header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_KO_Rep1      <- read.table("H3K27me3_KO/H3K27me3_CM_adult_EEDKO_Rep1_center_heatmap/a.xls",                    header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_KO_Rep2      <- read.table("H3K27me3_KO/H3K27me3_CM_adult_EEDKO_Rep2_center_heatmap/a.xls",                    header=TRUE,   sep="",   quote = "",   comment.char = "") 
    
dim(P5_EED_Rep1)    
dim(P5_EED_Rep2)    
dim(P5_GFP_Rep1)    
dim(P5_GFP_Rep2)    
dim(P14_EED_Rep1)    
dim(P14_EED_Rep2) 
dim(P25_EED_Rep1)    
dim(P25_EED_Rep2) 
dim(H3K27me3_ctrl_Rep1)    
dim(H3K27me3_ctrl_Rep2)   
dim(H3K27me3_ctrl_Rep3)  
dim(H3K27me3_KO_Rep1)    
dim(H3K27me3_KO_Rep2) 

P5_EED_Rep1  <- as.matrix(P5_EED_Rep1[,-(1:4)])    
P5_EED_Rep2  <- as.matrix(P5_EED_Rep2[,-(1:4)])    
P5_GFP_Rep1  <- as.matrix(P5_GFP_Rep1[,-(1:4)])    
P5_GFP_Rep2  <- as.matrix(P5_GFP_Rep2[,-(1:4)])    
P14_EED_Rep1 <- as.matrix(P14_EED_Rep1[,-(1:4)])    
P14_EED_Rep2 <- as.matrix(P14_EED_Rep2[,-(1:4)]) 
P25_EED_Rep1 <- as.matrix(P25_EED_Rep1[,-(1:4)])    
P25_EED_Rep2 <- as.matrix(P25_EED_Rep2[,-(1:4)]) 
H3K27me3_ctrl_Rep1  <- as.matrix(H3K27me3_ctrl_Rep1[,-(1:4)])    
H3K27me3_ctrl_Rep2  <- as.matrix(H3K27me3_ctrl_Rep2[,-(1:4)])    
H3K27me3_ctrl_Rep3  <- as.matrix(H3K27me3_ctrl_Rep3[,-(1:4)])     
H3K27me3_KO_Rep1T    <- as.matrix(H3K27me3_KO_Rep1[,-(1:4)])    
H3K27me3_KO_Rep2T    <- as.matrix(H3K27me3_KO_Rep2[,-(1:4)])


dim(P5_GFP_Rep1)      ## order: P5_GFP, P5_EED, P14_EED, P25_EED 
dim(P5_GFP_Rep2)   
dim(P5_EED_Rep1)    
dim(P5_EED_Rep2)    
dim(P14_EED_Rep1)    
dim(P14_EED_Rep2) 
dim(P25_EED_Rep1)    
dim(P25_EED_Rep2) 
dim(H3K27me3_ctrl_Rep1)    
dim(H3K27me3_ctrl_Rep2)    
dim(H3K27me3_ctrl_Rep3)    
dim(H3K27me3_KO_Rep1T)    
dim(H3K27me3_KO_Rep2T) 

numOfColumns <- ncol(P25_EED_Rep2)
numOfRows    <- nrow(P25_EED_Rep2)  
numOfColumns
numOfRows

Average_P5_EED  <- (P5_EED_Rep1 + P5_EED_Rep2)/2
Average_P5_GFP  <- (P5_GFP_Rep1 + P5_GFP_Rep2)/2 
Average_P14_EED <- (P14_EED_Rep1 + P14_EED_Rep2)/2
Average_P25_EED <- (P25_EED_Rep1 + P25_EED_Rep2)/2
Average_H3K27me3_ctrl  <- (H3K27me3_ctrl_Rep1 + H3K27me3_ctrl_Rep2 + H3K27me3_ctrl_Rep3)/3
Average_H3K27me3_KO  <- (H3K27me3_KO_Rep1T + H3K27me3_KO_Rep2T)/2 
















############################################################1. NO  Scaled
Eight_NOL(P5_GFP_Rep1, P5_GFP_Rep2,   P5_EED_Rep1,   P5_EED_Rep2,   P14_EED_Rep1,  P14_EED_Rep2,   P25_EED_Rep1,  P25_EED_Rep2,  H3K27me3_ctrl_Rep1, H3K27me3_ctrl_Rep2, H3K27me3_ctrl_Rep3,  H3K27me3_KO_Rep1T,  H3K27me3_KO_Rep2T,  paste(AllResults_g, "/1-A-H3K27me3-noscaled.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=0,  Ymax_Local=15)                                                         
Four_NOL( Average_P5_GFP,   Average_P5_EED,   Average_P14_EED,    Average_P25_EED, Average_H3K27me3_ctrl ,  Average_H3K27me3_KO,   paste(AllResults_g, "/1-B-H3K27me3-noscaled-Average.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=0,  Ymax_Local=15)                     

###############
P5_EED_Rep1_A  <- rowMeans(P5_EED_Rep1[,230:270])    
P5_EED_Rep2_A  <- rowMeans(P5_EED_Rep2[,230:270])    
P5_GFP_Rep1_A  <- rowMeans(P5_GFP_Rep1[,230:270])    
P5_GFP_Rep2_A  <- rowMeans(P5_GFP_Rep2[,230:270])    
P14_EED_Rep1_A <- rowMeans(P14_EED_Rep1[,230:270])    
P14_EED_Rep2_A <- rowMeans(P14_EED_Rep2[,230:270]) 
P25_EED_Rep1_A <- rowMeans(P25_EED_Rep1[,230:270])    
P25_EED_Rep2_A <- rowMeans(P25_EED_Rep2[,230:270]) 
H3K27me3_ctrl_Rep1_A   <- rowMeans(H3K27me3_ctrl_Rep1[,230:270])  
H3K27me3_ctrl_Rep2_A   <- rowMeans(H3K27me3_ctrl_Rep2[,230:270])    
H3K27me3_ctrl_Rep3_A   <- rowMeans(H3K27me3_ctrl_Rep3[,230:270])    
H3K27me3_KO_Rep1_A     <- rowMeans(H3K27me3_KO_Rep1T[,230:270])    
H3K27me3_KO_Rep2_A     <- rowMeans(H3K27me3_KO_Rep2T[,230:270])   

Average_P5_EED_A  <- (P5_EED_Rep1_A + P5_EED_Rep2_A)/2
Average_P5_GFP_A  <- (P5_GFP_Rep1_A + P5_GFP_Rep2_A)/2 
Average_P14_EED_A <- (P14_EED_Rep1_A + P14_EED_Rep2_A)/2
Average_P25_EED_A <- (P25_EED_Rep1_A + P25_EED_Rep2_A)/2
Average_H3K27me3_ctrl_A  <- (H3K27me3_ctrl_Rep1_A + H3K27me3_ctrl_Rep2_A + H3K27me3_ctrl_Rep3_A)/3
Average_H3K27me3_KO_A    <- (H3K27me3_KO_Rep1_A + H3K27me3_KO_Rep2_A)/2 

BoxPlot_ViolinPlot_8(P5_GFP_Rep1_A, P5_GFP_Rep2_A,   P5_EED_Rep1_A,   P5_EED_Rep2_A,   P14_EED_Rep1_A,  P14_EED_Rep2_A,   P25_EED_Rep1_A,  P25_EED_Rep2_A,  H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A, H3K27me3_ctrl_Rep3_A,   H3K27me3_KO_Rep1_A,  H3K27me3_KO_Rep2_A,  paste(AllResults_g, "/1-C-noscaled-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=10)     
BoxPlot_ViolinPlot_4(Average_P5_GFP_A,   Average_P5_EED_A,   Average_P14_EED_A,    Average_P25_EED_A,   Average_H3K27me3_ctrl_A ,  Average_H3K27me3_KO_A,    paste(AllResults_g, "/1-D-noscaled-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=10)     


########## PCA
library(matrixStats)

matrix1 <-rbind( Average_P5_GFP_A,   Average_P5_EED_A,   Average_P14_EED_A,    Average_P25_EED_A,   Average_H3K27me3_ctrl_A ,  Average_H3K27me3_KO_A   )                                            
dim(matrix1)
rownames(matrix1)
rownames(matrix1) <-  c("P5_GFP",  "P5_EED",  "P14_EED",  "P25_EED",  "EED_ctrl", "EED_KO")
rownames(matrix1)

var_1 <- colVars(matrix1)
length(var_1)
length(var_1[var_1>0.001])

index1 <- var_1>0.001
length(index1)
matrix2 <- matrix1[, index1]
dim(matrix2)
min(matrix2)
max(matrix2)
# PCA with function prcomp
pca1 = prcomp(matrix2, scale. = TRUE)
# sqrt of eigenvalues
pca1$sdev
summary(pca1)
# loadings
head(pca1$rotation)
# PCs (aka scores)
head(pca1$x)

sink(file = paste(AllResults_g, "/1-E-noscaled-H3K27me3-PCA.txt", sep=""))
print(pca1)
summary(pca1)
sink()

# load ggplot2
library(ggplot2)
# create data frame with scores
scores = as.data.frame(pca1$x)

# plot of observations
My_col_values2  <- c("red",    "pink4",   "blue",  "gold3",  "green4", "purple")
my_T <- 60
xPos <- c(my_T, my_T,  my_T,  my_T, -my_T,  my_T)
my_T2 <- 0
yPos <- c(my_T2, my_T2,  my_T2,  -my_T2, -my_T2,  -my_T2)

CairoSVG( file = paste(AllResults_g, "/1-F-noscaled-H3K27me3-PCAplot.svg", sep=""),   width = 4, height =3.5,  onefile = TRUE, bg = "white",  pointsize = 1 )
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) + geom_point(size=2, colour=My_col_values2) +
  geom_hline(yintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_vline(xintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_text(colour =My_col_values2, alpha = 1, size = 4, x= scores$PC1+xPos, y=scores$PC2+yPos) +
  ggtitle("PCA plot of H3K27me3 profile") + pTheme_1(textSize=12) 
dev.off()

#### 3D
library(rgl)
library(scatterplot3d)

CairoSVG( file = paste(AllResults_g, "/1-G-noscaled-H3K27me3-PCA-3Dplot.svg", sep=""),   width = 6, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )

with(scores, {
s3d <- scatterplot3d(x=PC1,  y=PC2,  z=PC3,  
              color=My_col_values2,  pch=20,
              main="PCA plot of H3K27me3 profile",   sub=NULL,   
              xlim=NULL,   ylim=NULL,   zlim=NULL,
              xlab="PC1",  ylab="PC2", zlab="PC3",
              scale.y=1,   angle=40,
              axis=TRUE,   tick.marks=TRUE,   label.tick.marks=TRUE,
              
              x.ticklabs=NULL,   y.ticklabs=NULL,   z.ticklabs=NULL,
              y.margin.add=0.0,   grid=TRUE,   box=TRUE,   lab=par("lab"),
              lab.z=5,   type="p",   highlight.3d=FALSE,
              mar=c(30,30,30,30),   bg=par("bg"),  
              col.axis=par("col.axis"),   col.grid="grey",   col.lab=par("col.lab"), 
              cex.symbols=10,   cex.axis=10 ,   cex.lab=10,  
              font.axis=par("font.axis"),  font.lab=par("font.lab"),   
              lty.axis=par("lty"),  lty.grid=par("lty"),  
              lty.hide=NULL,   lty.hplot=par("lty"),  log=""
              
              )
  s3d.coords <- s3d$xyz.convert(PC1, PC2, PC3) # convert 3D coords to 2D projection
  
  text(x=s3d.coords$x+0.1, y=s3d.coords$y,             # x and y coordinates
       labels=row.names(scores),               # text to plot
       cex=10, pos=4, col=My_col_values2)       # shrink text 50% and place to right of points)

})

dev.off()







































####################################### 2. scaled individually
vector1_Down <- scale( c(  colMeans(P5_GFP_Rep1), colMeans(P5_GFP_Rep2),    colMeans(P5_EED_Rep1),   colMeans(P5_EED_Rep2)    ), center=TRUE, scale=TRUE)
vector2_Down <- scale( c(  colMeans(P5_GFP_Rep1), colMeans(P5_GFP_Rep2),    colMeans(P14_EED_Rep1),  colMeans(P14_EED_Rep2)   ), center=TRUE, scale=TRUE)
vector3_Down <- scale( c(  colMeans(P5_GFP_Rep1), colMeans(P5_GFP_Rep2),    colMeans(P25_EED_Rep1),  colMeans(P25_EED_Rep2)   ), center=TRUE, scale=TRUE)
vector4_Down <- scale( c(  colMeans(H3K27me3_ctrl_Rep1), colMeans(H3K27me3_ctrl_Rep2),  colMeans(H3K27me3_ctrl_Rep3),  colMeans(H3K27me3_KO_Rep1T),  colMeans(H3K27me3_KO_Rep2T) ), center=TRUE, scale=TRUE)
length(vector1_Down)
length(vector2_Down)

P5_GFP_Rep1_S <- vector1_Down[(numOfColumns*0+1):(numOfColumns*1)]
P5_GFP_Rep2_S <- vector1_Down[(numOfColumns*1+1):(numOfColumns*2)]   
P5_EED_Rep1_S <- vector1_Down[(numOfColumns*2+1):(numOfColumns*3)]    
P5_EED_Rep2_S <- vector1_Down[(numOfColumns*3+1):(numOfColumns*4)]    

P14_EED_Rep1_S <- vector2_Down[(numOfColumns*2+1):(numOfColumns*3)]    
P14_EED_Rep2_S <- vector2_Down[(numOfColumns*3+1):(numOfColumns*4)] 
P25_EED_Rep1_S <- vector3_Down[(numOfColumns*2+1):(numOfColumns*3)]    
P25_EED_Rep2_S <- vector3_Down[(numOfColumns*3+1):(numOfColumns*4)]

H3K27me3_ctrl_Rep1_S   <- vector4_Down[(numOfColumns*0+1):(numOfColumns*1)]   
H3K27me3_ctrl_Rep2_S   <- vector4_Down[(numOfColumns*1+1):(numOfColumns*2)]    
H3K27me3_ctrl_Rep3_S   <- vector4_Down[(numOfColumns*2+1):(numOfColumns*3)]    
H3K27me3_KO_Rep1_S     <- vector4_Down[(numOfColumns*3+1):(numOfColumns*4)]    
H3K27me3_KO_Rep2_S     <- vector4_Down[(numOfColumns*4+1):(numOfColumns*5)] 

Average_P5_EED_S  <- (P5_EED_Rep1_S + P5_EED_Rep2_S)/2
Average_P5_GFP_S  <- (P5_GFP_Rep1_S + P5_GFP_Rep2_S)/2 
Average_P14_EED_S <- (P14_EED_Rep1_S + P14_EED_Rep2_S)/2
Average_P25_EED_S <- (P25_EED_Rep1_S + P25_EED_Rep2_S)/2
Average_H3K27me3_ctrl_S  <- (H3K27me3_ctrl_Rep1_S + H3K27me3_ctrl_Rep2_S + H3K27me3_ctrl_Rep3_S)/3
Average_H3K27me3_KO_S  <- (H3K27me3_KO_Rep1_S + H3K27me3_KO_Rep2_S)/2 

Eight_NOL_Scale(P5_GFP_Rep1_S, P5_GFP_Rep2_S,   P5_EED_Rep1_S,   P5_EED_Rep2_S,   P14_EED_Rep1_S,  P14_EED_Rep2_S,   P25_EED_Rep1_S,  P25_EED_Rep2_S,  H3K27me3_ctrl_Rep1_S, H3K27me3_ctrl_Rep2_S, H3K27me3_ctrl_Rep3_S,   H3K27me3_KO_Rep1_S,  H3K27me3_KO_Rep2_S,  paste(AllResults_g, "/2-A-scaled-individually-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=-2,  Ymax_Local=12)                                                         
Four_NOL_Scale( Average_P5_GFP_S,   Average_P5_EED_S,   Average_P14_EED_S,    Average_P25_EED_S, Average_H3K27me3_ctrl_S ,  Average_H3K27me3_KO_S,   paste(AllResults_g, "/2-B-scaled-individually-H3K27me3-Average.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=-2,  Ymax_Local=12)                     



#####################
vector1_Down <- scale( c(  P5_GFP_Rep1_A, P5_GFP_Rep2_A,    P5_EED_Rep1_A,   P5_EED_Rep2_A    ), center=TRUE, scale=TRUE)
vector2_Down <- scale( c(  P5_GFP_Rep1_A, P5_GFP_Rep2_A,    P14_EED_Rep1_A,  P14_EED_Rep2_A   ), center=TRUE, scale=TRUE)
vector3_Down <- scale( c(  P5_GFP_Rep1_A, P5_GFP_Rep2_A,    P25_EED_Rep1_A,  P25_EED_Rep2_A   ), center=TRUE, scale=TRUE)
vector4_Down <- scale( c(  H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A,  H3K27me3_ctrl_Rep3_A,  H3K27me3_KO_Rep1_A,  H3K27me3_KO_Rep2_A ), center=TRUE, scale=TRUE)
length(vector1_Down)
length(vector2_Down)

P5_GFP_Rep1_A_S <- vector1_Down[(numOfRows*0+1):(numOfRows*1)]
P5_GFP_Rep2_A_S <- vector1_Down[(numOfRows*1+1):(numOfRows*2)]   
P5_EED_Rep1_A_S <- vector1_Down[(numOfRows*2+1):(numOfRows*3)]    
P5_EED_Rep2_A_S <- vector1_Down[(numOfRows*3+1):(numOfRows*4)]    

P14_EED_Rep1_A_S <- vector2_Down[(numOfRows*2+1):(numOfRows*3)]    
P14_EED_Rep2_A_S <- vector2_Down[(numOfRows*3+1):(numOfRows*4)] 
P25_EED_Rep1_A_S <- vector3_Down[(numOfRows*2+1):(numOfRows*3)]    
P25_EED_Rep2_A_S <- vector3_Down[(numOfRows*3+1):(numOfRows*4)]

H3K27me3_ctrl_Rep1_A_S   <- vector4_Down[(numOfRows*0+1):(numOfRows*1)]   
H3K27me3_ctrl_Rep2_A_S   <- vector4_Down[(numOfRows*1+1):(numOfRows*2)]    
H3K27me3_ctrl_Rep3_A_S   <- vector4_Down[(numOfRows*2+1):(numOfRows*3)]    
H3K27me3_KO_Rep1_A_S     <- vector4_Down[(numOfRows*3+1):(numOfRows*4)]    
H3K27me3_KO_Rep2_A_S     <- vector4_Down[(numOfRows*4+1):(numOfRows*5)] 

Average_P5_EED_A_S  <- (P5_EED_Rep1_A_S + P5_EED_Rep2_A_S)/2
Average_P5_GFP_A_S  <- (P5_GFP_Rep1_A_S + P5_GFP_Rep2_A_S)/2 
Average_P14_EED_A_S <- (P14_EED_Rep1_A_S + P14_EED_Rep2_A_S)/2
Average_P25_EED_A_S <- (P25_EED_Rep1_A_S + P25_EED_Rep2_A_S)/2
Average_H3K27me3_ctrl_A_S  <- (H3K27me3_ctrl_Rep1_A_S + H3K27me3_ctrl_Rep2_A_S + H3K27me3_ctrl_Rep3_A_S)/3
Average_H3K27me3_KO_A_S    <- (H3K27me3_KO_Rep1_A_S + H3K27me3_KO_Rep2_A_S)/2 


BoxPlot_ViolinPlot_8(P5_GFP_Rep1_A_S, P5_GFP_Rep2_A_S,   P5_EED_Rep1_A_S,   P5_EED_Rep2_A_S,   P14_EED_Rep1_A_S,  P14_EED_Rep2_A_S,   P25_EED_Rep1_A_S,  P25_EED_Rep2_A_S,  H3K27me3_ctrl_Rep1_A_S, H3K27me3_ctrl_Rep2_A_S, H3K27me3_ctrl_Rep3_A_S,   H3K27me3_KO_Rep1_A_S,  H3K27me3_KO_Rep2_A_S,  paste(AllResults_g, "/2-C-scaled-individually-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=1.5)     
BoxPlot_ViolinPlot_4(Average_P5_GFP_A_S,   Average_P5_EED_A_S,   Average_P14_EED_A_S,    Average_P25_EED_A_S,   Average_H3K27me3_ctrl_A_S ,  Average_H3K27me3_KO_A_S,    paste(AllResults_g, "/2-D-scaled-individually-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=1.5)     




########## PCA
library(matrixStats)

matrix1 <-rbind( Average_P5_GFP_A_S,   Average_P5_EED_A_S,   Average_P14_EED_A_S,    Average_P25_EED_A_S,   Average_H3K27me3_ctrl_A_S ,  Average_H3K27me3_KO_A_S   )                                            
dim(matrix1)
rownames(matrix1)
rownames(matrix1) <-  c("P5_GFP",  "P5_EED",  "P14_EED",  "P25_EED",  "EED_ctrl", "EED_KO")
rownames(matrix1)

var_1 <- colVars(matrix1)
length(var_1)
length(var_1[var_1>0.001])

index1 <- var_1>0.001
length(index1)
matrix2 <- matrix1[, index1]
dim(matrix2)
min(matrix2)
max(matrix2)
# PCA with function prcomp
pca1 = prcomp(matrix2, scale. = TRUE)
# sqrt of eigenvalues
pca1$sdev
summary(pca1)
# loadings
head(pca1$rotation)
# PCs (aka scores)
head(pca1$x)

sink(file = paste(AllResults_g, "/2-E-scaled-individually-H3K27me3-PCA.txt", sep=""))
print(pca1)
summary(pca1)
sink()

# load ggplot2
library(ggplot2)
# create data frame with scores
scores = as.data.frame(pca1$x)

# plot of observations
My_col_values2  <- c("red",    "pink4",   "blue",  "gold3",  "green4", "purple")
my_T <- 60
xPos <- c(my_T, my_T,  my_T,  my_T, -my_T,  my_T)
my_T2 <- 0
yPos <- c(my_T2, my_T2,  my_T2,  -my_T2, -my_T2,  -my_T2)

CairoSVG( file = paste(AllResults_g, "/2-F-scaled-individually-H3K27me3-PCAplot.svg", sep=""),   width = 4, height =3.5,  onefile = TRUE, bg = "white",  pointsize = 1 )
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) + geom_point(size=2, colour=My_col_values2) +
  geom_hline(yintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_vline(xintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_text(colour =My_col_values2, alpha = 1, size = 4, x= scores$PC1+xPos, y=scores$PC2+yPos) +
  ggtitle("PCA plot of H3K27me3 profile") + pTheme_1(textSize=12) 
dev.off()

#### 3D
library(rgl)
library(scatterplot3d)

CairoSVG( file = paste(AllResults_g, "/2-G-scaled-individually-H3K27me3-PCA-3Dplot.svg", sep=""),   width = 6, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )

with(scores, {
s3d <- scatterplot3d(x=PC1,  y=PC2,  z=PC3,  
              color=My_col_values2,  pch=20,
              main="PCA plot of H3K27me3 profile",   sub=NULL,   
              xlim=NULL,   ylim=NULL,   zlim=NULL,
              xlab="PC1",  ylab="PC2", zlab="PC3",
              scale.y=1,   angle=40,
              axis=TRUE,   tick.marks=TRUE,   label.tick.marks=TRUE,
              
              x.ticklabs=NULL,   y.ticklabs=NULL,   z.ticklabs=NULL,
              y.margin.add=0.0,   grid=TRUE,   box=TRUE,   lab=par("lab"),
              lab.z=5,   type="p",   highlight.3d=FALSE,
              mar=c(30,30,30,30),   bg=par("bg"),  
              col.axis=par("col.axis"),   col.grid="grey",   col.lab=par("col.lab"), 
              cex.symbols=10,   cex.axis=10 ,   cex.lab=10,  
              font.axis=par("font.axis"),  font.lab=par("font.lab"),   
              lty.axis=par("lty"),  lty.grid=par("lty"),  
              lty.hide=NULL,   lty.hplot=par("lty"),  log=""
              
              )
  s3d.coords <- s3d$xyz.convert(PC1, PC2, PC3) # convert 3D coords to 2D projection
  
  text(x=s3d.coords$x+0.1, y=s3d.coords$y,             # x and y coordinates
       labels=row.names(scores),               # text to plot
       cex=10, pos=4, col=My_col_values2)       # shrink text 50% and place to right of points)

})

dev.off()







































################################## 3. scale by z-score simultaneously:
vector1_Down <- scale( c(  colMeans(P5_GFP_Rep1), colMeans(P5_GFP_Rep2),    colMeans(P5_EED_Rep1),   colMeans(P5_EED_Rep2),   colMeans(P14_EED_Rep1),  colMeans(P14_EED_Rep2),  colMeans(P25_EED_Rep1),  colMeans(P25_EED_Rep2),  colMeans(H3K27me3_ctrl_Rep1), colMeans(H3K27me3_ctrl_Rep2),  colMeans(H3K27me3_ctrl_Rep3),  colMeans(H3K27me3_KO_Rep1T),  colMeans(H3K27me3_KO_Rep2T)  ), center=TRUE, scale=TRUE)
P5_GFP_Rep1_S  <- vector1_Down[(numOfColumns*0+1):(numOfColumns*1)]
P5_GFP_Rep2_S  <- vector1_Down[(numOfColumns*1+1):(numOfColumns*2)]   
P5_EED_Rep1_S  <- vector1_Down[(numOfColumns*2+1):(numOfColumns*3)]    
P5_EED_Rep2_S  <- vector1_Down[(numOfColumns*3+1):(numOfColumns*4)]    
P14_EED_Rep1_S <- vector1_Down[(numOfColumns*4+1):(numOfColumns*5)]    
P14_EED_Rep2_S <- vector1_Down[(numOfColumns*5+1):(numOfColumns*6)] 
P25_EED_Rep1_S <- vector1_Down[(numOfColumns*6+1):(numOfColumns*7)]    
P25_EED_Rep2_S <- vector1_Down[(numOfColumns*7+1):(numOfColumns*8)]
H3K27me3_ctrl_Rep1_S   <- vector1_Down[(numOfColumns*8+1):(numOfColumns*9)]   
H3K27me3_ctrl_Rep2_S   <- vector1_Down[(numOfColumns*9+1):(numOfColumns*10)]    
H3K27me3_ctrl_Rep3_S   <- vector1_Down[(numOfColumns*10+1):(numOfColumns*11)]    
H3K27me3_KO_Rep1_S     <- vector1_Down[(numOfColumns*11+1):(numOfColumns*12)]    
H3K27me3_KO_Rep2_S     <- vector1_Down[(numOfColumns*12+1):(numOfColumns*13)] 

Average_P5_EED_S  <- (P5_EED_Rep1_S + P5_EED_Rep2_S)/2
Average_P5_GFP_S  <- (P5_GFP_Rep1_S + P5_GFP_Rep2_S)/2 
Average_P14_EED_S <- (P14_EED_Rep1_S + P14_EED_Rep2_S)/2
Average_P25_EED_S <- (P25_EED_Rep1_S + P25_EED_Rep2_S)/2
Average_H3K27me3_ctrl_S  <- (H3K27me3_ctrl_Rep1_S + H3K27me3_ctrl_Rep2_S + H3K27me3_ctrl_Rep3_S)/3
Average_H3K27me3_KO_S  <- (H3K27me3_KO_Rep1_S + H3K27me3_KO_Rep2_S)/2 

Eight_NOL_Scale(P5_GFP_Rep1_S, P5_GFP_Rep2_S,   P5_EED_Rep1_S,   P5_EED_Rep2_S,   P14_EED_Rep1_S,  P14_EED_Rep2_S,   P25_EED_Rep1_S,  P25_EED_Rep2_S,  H3K27me3_ctrl_Rep1_S, H3K27me3_ctrl_Rep2_S, H3K27me3_ctrl_Rep3_S,   H3K27me3_KO_Rep1_S,  H3K27me3_KO_Rep2_S,  paste(AllResults_g, "/3-A-scaled-simultaneously-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=-2,  Ymax_Local=12)                                                         
Four_NOL_Scale( Average_P5_GFP_S,   Average_P5_EED_S,   Average_P14_EED_S,    Average_P25_EED_S, Average_H3K27me3_ctrl_S ,  Average_H3K27me3_KO_S,   paste(AllResults_g, "/3-B-scaled-simultaneously-H3K27me3-Average.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=-2,  Ymax_Local=12)                     


#####################
vector1_Down <- scale( c(  P5_GFP_Rep1_A, P5_GFP_Rep2_A,    P5_EED_Rep1_A,   P5_EED_Rep2_A,   P14_EED_Rep1_A,  P14_EED_Rep2_A,  P25_EED_Rep1_A,  P25_EED_Rep2_A,  H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A,  H3K27me3_ctrl_Rep3_A,  H3K27me3_KO_Rep1_A,  H3K27me3_KO_Rep2_A)  , center=TRUE, scale=TRUE)
P5_GFP_Rep1_A_S  <- vector1_Down[(numOfRows*0+1):(numOfRows*1)]
P5_GFP_Rep2_A_S  <- vector1_Down[(numOfRows*1+1):(numOfRows*2)]   
P5_EED_Rep1_A_S  <- vector1_Down[(numOfRows*2+1):(numOfRows*3)]    
P5_EED_Rep2_A_S  <- vector1_Down[(numOfRows*3+1):(numOfRows*4)]    
P14_EED_Rep1_A_S <- vector1_Down[(numOfRows*4+1):(numOfRows*5)]    
P14_EED_Rep2_A_S <- vector1_Down[(numOfRows*5+1):(numOfRows*6)] 
P25_EED_Rep1_A_S <- vector1_Down[(numOfRows*6+1):(numOfRows*7)]    
P25_EED_Rep2_A_S <- vector1_Down[(numOfRows*7+1):(numOfRows*8)]
H3K27me3_ctrl_Rep1_A_S   <- vector1_Down[(numOfRows*8+1):(numOfRows*9)]   
H3K27me3_ctrl_Rep2_A_S   <- vector1_Down[(numOfRows*9+1):(numOfRows*10)]    
H3K27me3_ctrl_Rep3_A_S   <- vector1_Down[(numOfRows*10+1):(numOfRows*11)]    
H3K27me3_KO_Rep1_A_S     <- vector1_Down[(numOfRows*11+1):(numOfRows*12)]    
H3K27me3_KO_Rep2_A_S     <- vector1_Down[(numOfRows*12+1):(numOfRows*13)] 

Average_P5_EED_A_S  <- (P5_EED_Rep1_A_S + P5_EED_Rep2_A_S)/2
Average_P5_GFP_A_S  <- (P5_GFP_Rep1_A_S + P5_GFP_Rep2_A_S)/2 
Average_P14_EED_A_S <- (P14_EED_Rep1_A_S + P14_EED_Rep2_A_S)/2
Average_P25_EED_A_S <- (P25_EED_Rep1_A_S + P25_EED_Rep2_A_S)/2
Average_H3K27me3_ctrl_A_S  <- (H3K27me3_ctrl_Rep1_A_S + H3K27me3_ctrl_Rep2_A_S + H3K27me3_ctrl_Rep3_A_S)/3
Average_H3K27me3_KO_A_S    <- (H3K27me3_KO_Rep1_A_S + H3K27me3_KO_Rep2_A_S)/2 

BoxPlot_ViolinPlot_8(P5_GFP_Rep1_A_S, P5_GFP_Rep2_A_S,   P5_EED_Rep1_A_S,   P5_EED_Rep2_A_S,   P14_EED_Rep1_A_S,  P14_EED_Rep2_A_S,   P25_EED_Rep1_A_S,  P25_EED_Rep2_A_S,  H3K27me3_ctrl_Rep1_A_S, H3K27me3_ctrl_Rep2_A_S, H3K27me3_ctrl_Rep3_A_S,   H3K27me3_KO_Rep1_A_S,  H3K27me3_KO_Rep2_A_S,  paste(AllResults_g, "/3-C-scaled-simultaneously-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=1.5)     

BoxPlot_ViolinPlot_4(Average_P5_GFP_A_S,   Average_P5_EED_A_S,   Average_P14_EED_A_S,    Average_P25_EED_A_S,   Average_H3K27me3_ctrl_A_S ,  Average_H3K27me3_KO_A_S,    paste(AllResults_g, "/3-D-scaled-simultaneously-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=1.5)     



########## PCA
library(matrixStats)

matrix1 <-rbind( Average_P5_GFP_A_S,   Average_P5_EED_A_S,   Average_P14_EED_A_S,    Average_P25_EED_A_S,   Average_H3K27me3_ctrl_A_S ,  Average_H3K27me3_KO_A_S   )                                            
dim(matrix1)
rownames(matrix1)
rownames(matrix1) <-  c("P5_GFP",  "P5_EED",  "P14_EED",  "P25_EED",  "EED_ctrl", "EED_KO")
rownames(matrix1)

var_1 <- colVars(matrix1)
length(var_1)
length(var_1[var_1>0.001])

index1 <- var_1>0.001
length(index1)
matrix2 <- matrix1[, index1]
dim(matrix2)
min(matrix2)
max(matrix2)
# PCA with function prcomp
pca1 = prcomp(matrix2, scale. = TRUE)
# sqrt of eigenvalues
pca1$sdev
summary(pca1)
# loadings
head(pca1$rotation)
# PCs (aka scores)
head(pca1$x)

sink(file = paste(AllResults_g, "/3-E-scaled-simultaneously-H3K27me3-PCA.txt", sep=""))
print(pca1)
summary(pca1)
sink()

# load ggplot2
library(ggplot2)
# create data frame with scores
scores = as.data.frame(pca1$x)

# plot of observations
My_col_values2  <- c("red",    "pink4",   "blue",  "gold3",  "green4", "purple")
my_T <- 60
xPos <- c(my_T, my_T,  my_T,  my_T, -my_T,  my_T)
my_T2 <- 0
yPos <- c(my_T2, my_T2,  my_T2,  -my_T2, -my_T2,  -my_T2)

CairoSVG( file = paste(AllResults_g, "/3-F-scaled-simultaneously-H3K27me3-PCAplot.svg", sep=""),   width = 4, height =3.5,  onefile = TRUE, bg = "white",  pointsize = 1 )
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) + geom_point(size=2, colour=My_col_values2) +
  geom_hline(yintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_vline(xintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_text(colour =My_col_values2, alpha = 1, size = 4, x= scores$PC1+xPos, y=scores$PC2+yPos) +
  ggtitle("PCA plot of H3K27me3 profile") + pTheme_1(textSize=12) 
dev.off()

#### 3D
library(rgl)
library(scatterplot3d)

CairoSVG( file = paste(AllResults_g, "/3-G-scaled-simultaneously-H3K27me3-PCA-3Dplot.svg", sep=""),   width = 6, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )

with(scores, {
s3d <- scatterplot3d(x=PC1,  y=PC2,  z=PC3,  
              color=My_col_values2,  pch=20,
              main="PCA plot of H3K27me3 profile",   sub=NULL,   
              xlim=NULL,   ylim=NULL,   zlim=NULL,
              xlab="PC1",  ylab="PC2", zlab="PC3",
              scale.y=1,   angle=40,
              axis=TRUE,   tick.marks=TRUE,   label.tick.marks=TRUE,
              
              x.ticklabs=NULL,   y.ticklabs=NULL,   z.ticklabs=NULL,
              y.margin.add=0.0,   grid=TRUE,   box=TRUE,   lab=par("lab"),
              lab.z=5,   type="p",   highlight.3d=FALSE,
              mar=c(30,30,30,30),   bg=par("bg"),  
              col.axis=par("col.axis"),   col.grid="grey",   col.lab=par("col.lab"), 
              cex.symbols=10,   cex.axis=10 ,   cex.lab=10,  
              font.axis=par("font.axis"),  font.lab=par("font.lab"),   
              lty.axis=par("lty"),  lty.grid=par("lty"),  
              lty.hide=NULL,   lty.hplot=par("lty"),  log=""
              
              )
  s3d.coords <- s3d$xyz.convert(PC1, PC2, PC3) # convert 3D coords to 2D projection
  
  text(x=s3d.coords$x+0.1, y=s3d.coords$y,             # x and y coordinates
       labels=row.names(scores),               # text to plot
       cex=10, pos=4, col=My_col_values2)       # shrink text 50% and place to right of points)

})

dev.off()






































####################################### 4. scaled individually 2
vector1_Down <- scale( c(  colMeans(P5_GFP_Rep1), colMeans(P5_GFP_Rep2),    colMeans(P5_EED_Rep1),   colMeans(P5_EED_Rep2)     ), center=TRUE, scale=TRUE)
vector2_Down <- scale( c(     colMeans(P14_EED_Rep1),  colMeans(P14_EED_Rep2),  colMeans(H3K27me3_ctrl_Rep1), colMeans(H3K27me3_ctrl_Rep2),  colMeans(H3K27me3_ctrl_Rep3)   ), center=TRUE, scale=TRUE)
vector3_Down <- scale( c(     colMeans(P25_EED_Rep1),  colMeans(P25_EED_Rep2),  colMeans(H3K27me3_ctrl_Rep1), colMeans(H3K27me3_ctrl_Rep2),  colMeans(H3K27me3_ctrl_Rep3)    ), center=TRUE, scale=TRUE)
vector4_Down <- scale( c(  colMeans(H3K27me3_ctrl_Rep1), colMeans(H3K27me3_ctrl_Rep2),  colMeans(H3K27me3_ctrl_Rep3),  colMeans(H3K27me3_KO_Rep1T),  colMeans(H3K27me3_KO_Rep2T) ), center=TRUE, scale=TRUE)
length(vector1_Down)
length(vector2_Down)

P5_GFP_Rep1_S <- vector1_Down[(numOfColumns*0+1):(numOfColumns*1)]
P5_GFP_Rep2_S <- vector1_Down[(numOfColumns*1+1):(numOfColumns*2)]   
P5_EED_Rep1_S <- vector1_Down[(numOfColumns*2+1):(numOfColumns*3)]    
P5_EED_Rep2_S <- vector1_Down[(numOfColumns*3+1):(numOfColumns*4)]    

P14_EED_Rep1_S <- vector2_Down[(numOfColumns*0+1):(numOfColumns*1)]    
P14_EED_Rep2_S <- vector2_Down[(numOfColumns*1+1):(numOfColumns*2)] 
P25_EED_Rep1_S <- vector3_Down[(numOfColumns*0+1):(numOfColumns*1)]    
P25_EED_Rep2_S <- vector3_Down[(numOfColumns*1+1):(numOfColumns*2)]

H3K27me3_ctrl_Rep1_S   <- vector4_Down[(numOfColumns*0+1):(numOfColumns*1)]   
H3K27me3_ctrl_Rep2_S   <- vector4_Down[(numOfColumns*1+1):(numOfColumns*2)]    
H3K27me3_ctrl_Rep3_S   <- vector4_Down[(numOfColumns*2+1):(numOfColumns*3)]    
H3K27me3_KO_Rep1_S     <- vector4_Down[(numOfColumns*3+1):(numOfColumns*4)]    
H3K27me3_KO_Rep2_S     <- vector4_Down[(numOfColumns*4+1):(numOfColumns*5)] 

Average_P5_EED_S  <- (P5_EED_Rep1_S + P5_EED_Rep2_S)/2
Average_P5_GFP_S  <- (P5_GFP_Rep1_S + P5_GFP_Rep2_S)/2 
Average_P14_EED_S <- (P14_EED_Rep1_S + P14_EED_Rep2_S)/2
Average_P25_EED_S <- (P25_EED_Rep1_S + P25_EED_Rep2_S)/2
Average_H3K27me3_ctrl_S  <- (H3K27me3_ctrl_Rep1_S + H3K27me3_ctrl_Rep2_S + H3K27me3_ctrl_Rep3_S)/3
Average_H3K27me3_KO_S  <- (H3K27me3_KO_Rep1_S + H3K27me3_KO_Rep2_S)/2 

Eight_NOL_Scale(P5_GFP_Rep1_S, P5_GFP_Rep2_S,   P5_EED_Rep1_S,   P5_EED_Rep2_S,   P14_EED_Rep1_S,  P14_EED_Rep2_S,   P25_EED_Rep1_S,  P25_EED_Rep2_S,  H3K27me3_ctrl_Rep1_S, H3K27me3_ctrl_Rep2_S, H3K27me3_ctrl_Rep3_S,   H3K27me3_KO_Rep1_S,  H3K27me3_KO_Rep2_S,  paste(AllResults_g, "/4-A-scaled-individually-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=-2,  Ymax_Local=12)                                                         
Four_NOL_Scale( Average_P5_GFP_S,   Average_P5_EED_S,   Average_P14_EED_S,    Average_P25_EED_S, Average_H3K27me3_ctrl_S ,  Average_H3K27me3_KO_S,   paste(AllResults_g, "/4-B-scaled-individually-H3K27me3-Average.svg", sep=""),  "All H3K27me3 Peaks",   Ymin_Local=-2,  Ymax_Local=12)                     



#####################
vector1_Down <- scale( c(  P5_GFP_Rep1_A, P5_GFP_Rep2_A,    P5_EED_Rep1_A,   P5_EED_Rep2_A    ), center=TRUE, scale=TRUE)
vector2_Down <- scale( c(      P14_EED_Rep1_A,  P14_EED_Rep2_A ,  H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A,  H3K27me3_ctrl_Rep3_A  ), center=TRUE, scale=TRUE)
vector3_Down <- scale( c(      P25_EED_Rep1_A,  P25_EED_Rep2_A ,  H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A,  H3K27me3_ctrl_Rep3_A  ), center=TRUE, scale=TRUE)
vector4_Down <- scale( c(  H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A,  H3K27me3_ctrl_Rep3_A,  H3K27me3_KO_Rep1_A,  H3K27me3_KO_Rep2_A ), center=TRUE, scale=TRUE)
length(vector1_Down)
length(vector2_Down)

P5_GFP_Rep1_A_S <- vector1_Down[(numOfRows*0+1):(numOfRows*1)]
P5_GFP_Rep2_A_S <- vector1_Down[(numOfRows*1+1):(numOfRows*2)]   
P5_EED_Rep1_A_S <- vector1_Down[(numOfRows*2+1):(numOfRows*3)]    
P5_EED_Rep2_A_S <- vector1_Down[(numOfRows*3+1):(numOfRows*4)]    

P14_EED_Rep1_A_S <- vector2_Down[(numOfRows*0+1):(numOfRows*1)]    
P14_EED_Rep2_A_S <- vector2_Down[(numOfRows*1+1):(numOfRows*2)] 
P25_EED_Rep1_A_S <- vector3_Down[(numOfRows*0+1):(numOfRows*1)]    
P25_EED_Rep2_A_S <- vector3_Down[(numOfRows*1+1):(numOfRows*2)]

H3K27me3_ctrl_Rep1_A_S   <- vector4_Down[(numOfRows*0+1):(numOfRows*1)]   
H3K27me3_ctrl_Rep2_A_S   <- vector4_Down[(numOfRows*1+1):(numOfRows*2)]    
H3K27me3_ctrl_Rep3_A_S   <- vector4_Down[(numOfRows*2+1):(numOfRows*3)]    
H3K27me3_KO_Rep1_A_S     <- vector4_Down[(numOfRows*3+1):(numOfRows*4)]    
H3K27me3_KO_Rep2_A_S     <- vector4_Down[(numOfRows*4+1):(numOfRows*5)] 

Average_P5_EED_A_S  <- (P5_EED_Rep1_A_S + P5_EED_Rep2_A_S)/2
Average_P5_GFP_A_S  <- (P5_GFP_Rep1_A_S + P5_GFP_Rep2_A_S)/2 
Average_P14_EED_A_S <- (P14_EED_Rep1_A_S + P14_EED_Rep2_A_S)/2
Average_P25_EED_A_S <- (P25_EED_Rep1_A_S + P25_EED_Rep2_A_S)/2
Average_H3K27me3_ctrl_A_S  <- (H3K27me3_ctrl_Rep1_A_S + H3K27me3_ctrl_Rep2_A_S + H3K27me3_ctrl_Rep3_A_S)/3
Average_H3K27me3_KO_A_S    <- (H3K27me3_KO_Rep1_A_S + H3K27me3_KO_Rep2_A_S)/2 


BoxPlot_ViolinPlot_8(P5_GFP_Rep1_A_S, P5_GFP_Rep2_A_S,   P5_EED_Rep1_A_S,   P5_EED_Rep2_A_S,   P14_EED_Rep1_A_S,  P14_EED_Rep2_A_S,   P25_EED_Rep1_A_S,  P25_EED_Rep2_A_S,  H3K27me3_ctrl_Rep1_A_S, H3K27me3_ctrl_Rep2_A_S, H3K27me3_ctrl_Rep3_A_S,   H3K27me3_KO_Rep1_A_S,  H3K27me3_KO_Rep2_A_S,  paste(AllResults_g, "/4-C-scaled-individually-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=1.5)     
BoxPlot_ViolinPlot_4(Average_P5_GFP_A_S,   Average_P5_EED_A_S,   Average_P14_EED_A_S,    Average_P25_EED_A_S,   Average_H3K27me3_ctrl_A_S ,  Average_H3K27me3_KO_A_S,    paste(AllResults_g, "/4-D-scaled-individually-H3K27me3.svg", sep=""),  "All H3K27me3 Peaks",  OccuT_Local=1.5)     




########## PCA
library(matrixStats)

matrix1 <-rbind( Average_P5_GFP_A_S,   Average_P5_EED_A_S,   Average_P14_EED_A_S,    Average_P25_EED_A_S,   Average_H3K27me3_ctrl_A_S ,  Average_H3K27me3_KO_A_S   )                                            
dim(matrix1)
rownames(matrix1)
rownames(matrix1) <-  c("P5_GFP",  "P5_EED",  "P14_EED",  "P25_EED",  "EED_ctrl", "EED_KO")
rownames(matrix1)

var_1 <- colVars(matrix1)
length(var_1)
length(var_1[var_1>0.001])

index1 <- var_1>0.001
length(index1)
matrix2 <- matrix1[, index1]
dim(matrix2)
min(matrix2)
max(matrix2)
# PCA with function prcomp
pca1 = prcomp(matrix2, scale. = TRUE)
# sqrt of eigenvalues
pca1$sdev
summary(pca1)
# loadings
head(pca1$rotation)
# PCs (aka scores)
head(pca1$x)

sink(file = paste(AllResults_g, "/4-E-scaled-individually-H3K27me3-PCA.txt", sep=""))
print(pca1)
summary(pca1)
sink()

# load ggplot2
library(ggplot2)
# create data frame with scores
scores = as.data.frame(pca1$x)

# plot of observations
My_col_values2  <- c("red",    "pink4",   "blue",  "gold3",  "green4", "purple")
my_T <- 60
xPos <- c(my_T, my_T,  my_T,  my_T, -my_T,  my_T)
my_T2 <- 0
yPos <- c(my_T2, my_T2,  my_T2,  -my_T2, -my_T2,  -my_T2)

CairoSVG( file = paste(AllResults_g, "/4-F-scaled-individually-H3K27me3-PCAplot.svg", sep=""),   width = 4, height =3.5,  onefile = TRUE, bg = "white",  pointsize = 1 )
ggplot(data = scores, aes(x = PC1, y = PC2, label = rownames(scores))) + geom_point(size=2, colour=My_col_values2) +
  geom_hline(yintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_vline(xintercept = 0,   lty=2,  col="gray", size=0.5) +
  geom_text(colour =My_col_values2, alpha = 1, size = 4, x= scores$PC1+xPos, y=scores$PC2+yPos) +
  ggtitle("PCA plot of H3K27me3 profile") + pTheme_1(textSize=12) 
dev.off()

#### 3D
library(rgl)
library(scatterplot3d)

CairoSVG( file = paste(AllResults_g, "/4-G-scaled-individually-H3K27me3-PCA-3Dplot.svg", sep=""),   width = 6, height =5,  onefile = TRUE, bg = "white",  pointsize = 1 )

with(scores, {
s3d <- scatterplot3d(x=PC1,  y=PC2,  z=PC3,  
              color=My_col_values2,  pch=20,
              main="PCA plot of H3K27me3 profile",   sub=NULL,   
              xlim=NULL,   ylim=NULL,   zlim=NULL,
              xlab="PC1",  ylab="PC2", zlab="PC3",
              scale.y=1,   angle=40,
              axis=TRUE,   tick.marks=TRUE,   label.tick.marks=TRUE,
              
              x.ticklabs=NULL,   y.ticklabs=NULL,   z.ticklabs=NULL,
              y.margin.add=0.0,   grid=TRUE,   box=TRUE,   lab=par("lab"),
              lab.z=5,   type="p",   highlight.3d=FALSE,
              mar=c(30,30,30,30),   bg=par("bg"),  
              col.axis=par("col.axis"),   col.grid="grey",   col.lab=par("col.lab"), 
              cex.symbols=10,   cex.axis=10 ,   cex.lab=10,  
              font.axis=par("font.axis"),  font.lab=par("font.lab"),   
              lty.axis=par("lty"),  lty.grid=par("lty"),  
              lty.hide=NULL,   lty.hplot=par("lty"),  log=""
              
              )
  s3d.coords <- s3d$xyz.convert(PC1, PC2, PC3) # convert 3D coords to 2D projection
  
  text(x=s3d.coords$x+0.1, y=s3d.coords$y,             # x and y coordinates
       labels=row.names(scores),               # text to plot
       cex=10, pos=4, col=My_col_values2)       # shrink text 50% and place to right of points)

})

dev.off()










####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################





























