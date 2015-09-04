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
library("matrixStats")
library("extrafont")
#font_import()
#fonts()
#fonttable()
#loadfonts()
#loadfonts(device="postscript")
names(postscriptFonts())













pTheme_1 <- function( textSize=12 ) {  
  theme(  
    line  = element_line(colour="black", size=1.0,   linetype=1,      lineend=NULL),                                                          ## all line elements.  局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
    rect  = element_rect(colour="black", size=1.0,   linetype=1,      fill="transparent" ),                                                   ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
    text  = element_text(family="Arial",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),   ## all text elements.  "Arial" for a Arial-Arial font. 所有文本相关属性.
    title = element_text(family="Arial",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=2,   angle=NULL, lineheight=NULL),    ## all title elements: plot, axes, legends. hjust:水平对齐的方向.  所有标题属性.
    
    axis.title        = element_text(family="Arial", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
    axis.title.x      = element_text(family="Arial", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=-0.5,  angle=NULL, lineheight=NULL),      ## x axis label (element_text; inherits from axis.title)
    axis.title.y      = element_text(family="Arial", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=1.5,   angle=NULL, lineheight=NULL),      ## y axis label (element_text; inherits from axis.title)
    axis.text         = element_text(family="Arial", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
    axis.text.x       = element_text(family="Arial", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## x axis tick labels (element_text; inherits from axis.text)
    axis.text.y       = element_text(family="Arial", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## y axis tick labels (element_text; inherits from axis.text)
    axis.ticks        = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                     ## tick marks along axes (element_line; inherits from line). 坐标轴刻度线.
    axis.ticks.x      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                     ## x axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.y      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                     ## y axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.length = grid::unit(1.5, "mm", data=NULL),                                                                                                     ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm.  刻度线长度
    axis.ticks.margin = grid::unit(1.0, "mm", data=NULL),  	                                                                                                  ## space between tick mark and tick label (unit),  ‘"mm"’ Millimetres.  10 mm = 1 cm. 刻度线和刻度标签之间的间距.                                                                           
    axis.line         = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                            ## lines along axes (element_line; inherits from line). 坐标轴线
    axis.line.x       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                            ## line along x axis (element_line; inherits from axis.line)
    axis.line.y       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),	                                                              ## line along y axis (element_line; inherits from axis.line)
    
    legend.background    = element_rect(colour="transparent", size=1, linetype=1, fill="transparent" ), 	      ## background of legend (element_rect; inherits from rect)
    legend.margin        = grid::unit(0.3, "mm", data=NULL), 	                                                  ## extra space added around legend (unit). linetype=1指的是矩形边框的类型.
    legend.key           = element_rect(colour="transparent", size=2, linetype=1, fill="transparent" ), 	      ## background underneath legend keys. 图例符号. size=1指的是矩形边框的大小.
    legend.key.size      = grid::unit(5, "mm", data=NULL) , 	                                                  ## size of legend keys (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(5.7, "mm", data=NULL) , 	                                                ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(5, "mm", data=NULL) ,                                                     ## key background width (unit; inherits from legend.key.size)
    legend.text          = element_text(family="Arial", face=NULL, colour="black", size=textSize-2, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
    legend.text.align    = 0, 	                    ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),   	    ## title of legend (element_text; inherits from title)
    legend.title.align   = 0, 	                    ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right", 	              ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",        	    ## layout of items in legends  ("horizontal" or "vertical")   图例排列方向
    legend.justification = "center",      	        ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)  图例居中方式
    legend.box           = NULL, 	                  ## arrangement of multiple legends ("horizontal" or "vertical")  多图例的排列方式
    legend.box.just      = NULL, 	                  ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")  多图例的居中方式
    
    panel.background   = element_rect(colour="transparent", size=0.0, linetype=1, fill="transparent" ),   	## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", size=0.3, linetype=1, fill=NA ), 	                    ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)
    panel.margin       = grid::unit(2, "mm", data=NULL) , 	                                                ## margin around facet panels (unit)  分面绘图区之间的边距
    panel.grid         = element_blank(), 	                                                                ## grid lines (element_line; inherits from line)  绘图区网格线
    panel.grid.major   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## major grid lines (element_line; inherits from panel.grid)  主网格线
    panel.grid.minor   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## minor grid lines (element_line; inherits from panel.grid)  次网格线
    panel.grid.major.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal minor grid lines (element_line; inherits from panel.grid.minor)
    
    plot.background	= element_rect(colour="transparent", size=NULL, linetype=NULL, fill="transparent" ),                                              ## background of the entire plot (element_rect; inherits from rect)  整个图形的背景
    plot.title      = element_text(family="Arial", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	  ## plot title (text appearance) (element_text; inherits from title)  图形标题
    plot.margin     = grid::unit(c(8, 8, 8, 8), "mm", data=NULL), 	                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    
    strip.background = element_rect(colour=NULL, size=NULL, linetype=NULL, fill=NULL ), 	                                                        ## background of facet labels (element_rect; inherits from rect)  分面标签背景
    strip.text       = element_text(family="Arial", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="Arial", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="Arial", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	  ## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 










pTheme_2 <- function( textSize=12 ) {  
  theme(  
    line  = element_line(colour="black", size=1.0,   linetype=1,      lineend=NULL),                                                           ## all line elements.  局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
    rect  = element_rect(colour="black", size=1.0,   linetype=1,      fill="transparent" ),                                                    ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
    text  = element_text(family="Arial",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),    ## all text elements.  "Arial" for a Arial-serif font. 所有文本相关属性.
    title = element_text(family="Arial",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=2,   angle=NULL, lineheight=NULL),     ## all title elements: plot, axes, legends. hjust:水平对齐的方向.  所有标题属性.
    
    axis.title        = element_text(family="Arial", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
    axis.title.x      = element_text(family="Arial", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=-0.5,  angle=NULL, lineheight=NULL),      ## x axis label (element_text; inherits from axis.title)
    axis.title.y      = element_text(family="Arial", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=1.5,   angle=NULL, lineheight=NULL),      ## y axis label (element_text; inherits from axis.title)
    axis.text         = element_text(family="Arial", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
    axis.text.x       = element_text(family="Arial", face=NULL, colour="black", size=textSize-2,  hjust=1, vjust=1, angle=45, lineheight=NULL),               ## x axis tick labels (element_text; inherits from axis.text)
    axis.text.y       = element_text(family="Arial", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## y axis tick labels (element_text; inherits from axis.text)
    axis.ticks        = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                     ## tick marks along axes (element_line; inherits from line). 坐标轴刻度线.
    axis.ticks.x      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                     ## x axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.y      = element_line(colour="black", size=0.3, linetype=1, lineend=NULL),                                                                     ## y axis tick marks (element_line; inherits from axis.ticks)
    axis.ticks.length = grid::unit(1.5, "mm", data=NULL),                                                                                                     ## length of tick marks (unit), ‘"mm"’ Millimetres.  10 mm = 1 cm.  刻度线长度
    axis.ticks.margin = grid::unit(1.0, "mm", data=NULL),                                                                                                     ## space between tick mark and tick label (unit),  ‘"mm"’ Millimetres.  10 mm = 1 cm. 刻度线和刻度标签之间的间距.                                                                           
    axis.line         = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                            ## lines along axes (element_line; inherits from line). 坐标轴线
    axis.line.x       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL), 	                                                            ## line along x axis (element_line; inherits from axis.line)
    axis.line.y       = element_line(colour="transparent", size=0.3, linetype=1, lineend=NULL),	
    
    legend.background    = element_rect(colour="transparent", size=1, linetype=1, fill="transparent" ), 	      ## background of legend (element_rect; inherits from rect)
    legend.margin        = grid::unit(0.5, "mm", data=NULL), 	                                                  ## extra space added around legend (unit). linetype=1指的是矩形边框的类型.
    legend.key           = element_rect(colour="transparent", size=2, linetype=1, fill="transparent" ), 	      ## background underneath legend keys. 图例符号. size=1指的是矩形边框的大小.
    legend.key.size      = grid::unit(5, "mm", data=NULL) , 	                                                  ## size of legend keys (unit; inherits from legend.key.size)
    legend.key.height    = grid::unit(5.7, "mm", data=NULL) , 	                                                ## key background height (unit; inherits from legend.key.size)
    legend.key.width     = grid::unit(5, "mm", data=NULL) ,                                                     ## key background width (unit; inherits from legend.key.size)
    legend.text          = element_text(family="Arial", face=NULL, colour="black", size=textSize-2, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
    legend.text.align    = 0, 	                ## alignment of legend labels (number from 0 (left) to 1 (right))
    legend.title         = element_blank(),   	## title of legend (element_text; inherits from title)
    legend.title.align   = 0, 	                ## alignment of legend title (number from 0 (left) to 1 (right))
    legend.position      = "right", 	          ## the position of legends. ("left", "right", "bottom", "top", or two-element numeric vector)
    legend.direction     = "vertical",        	## layout of items in legends  ("horizontal" or "vertical")   图例排列方向
    legend.justification = "center",      	    ## anchor point for positioning legend inside plot ("center" or two-element numeric vector)  图例居中方式
    legend.box           = NULL, 	              ## arrangement of multiple legends ("horizontal" or "vertical")  多图例的排列方式
    legend.box.just      = NULL, 	              ## justification of each legend within the overall bounding box, when there are multiple legends ("top", "bottom", "left", or "right")  多图例的居中方式
    
    panel.background   = element_rect(colour="transparent", size=0.0, linetype=1, fill="transparent" ),   	## background of plotting area, drawn underneath plot (element_rect; inherits from rect)
    panel.border       = element_rect(colour="black", size=0.3, linetype=1, fill=NA ), 	                    ## border around plotting area, drawn on top of plot so that it covers tick marks and grid lines. This should be used with fill=NA (element_rect; inherits from rect)
    panel.margin       = grid::unit(2, "mm", data=NULL) , 	                                                ## margin around facet panels (unit)  分面绘图区之间的边距
    panel.grid         = element_blank(), 	                                                                ## grid lines (element_line; inherits from line)  绘图区网格线
    panel.grid.major   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## major grid lines (element_line; inherits from panel.grid)  主网格线
    panel.grid.minor   = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## minor grid lines (element_line; inherits from panel.grid)  次网格线
    panel.grid.major.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) , 	    ## vertical major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.major.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal major grid lines (element_line; inherits from panel.grid.major)
    panel.grid.minor.x = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## vertical minor grid lines (element_line; inherits from panel.grid.minor)
    panel.grid.minor.y = element_line(colour="transparent", size=NULL, linetype=NULL, lineend=NULL) ,     	## horizontal minor grid lines (element_line; inherits from panel.grid.minor)
    
    plot.background	= element_rect(colour="transparent", size=NULL, linetype=NULL, fill="transparent" ),                                              ## background of the entire plot (element_rect; inherits from rect)  整个图形的背景
    plot.title      = element_text(family="Arial", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	  ## plot title (text appearance) (element_text; inherits from title)  图形标题
    plot.margin     = grid::unit(c(8, 8, 8, 8), "mm", data=NULL), 	                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    
    strip.background = element_rect(colour=NULL, size=NULL, linetype=NULL, fill=NULL ), 	                                                        ## background of facet labels (element_rect; inherits from rect)  分面标签背景
    strip.text       = element_text(family="Arial", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="Arial", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="Arial", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	  ## facet labels along vertical direction (element_text; inherits from strip.text) 
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











averageLines <- function(vector1,  numSample1,  sampleType1,   colours1,  sampleRank1,   file1,   title1,  xLab1,  yLab1,   Ymin1=0,   Ymax1=10, width1=4) {  
  binLen <- 20   ## 20 bp
  binNum <- 500  ## 500 * 20 = 10000 bp
  Position_Local   <-  seq(from = -5,  by=0.02,  length.out=binNum)   ## unit is kb
  DataFrame_Local  <- data.frame( xAxis = c( rep(Position_Local, numSample1) ),      yAxis = vector1,    SampleType = sampleType1 ) 
  
  postscript( file=paste(file1, ".eps", sep="", collapse=NULL),   height = 3,   width = width1,    family = "Arial",  paper = "special",  onefile = FALSE,  horizontal = FALSE)
  FigureTemp1 <- ggplot(DataFrame_Local,   aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab(xLab1) +  ylab(yLab1) +  ggtitle(title1) + 
    scale_colour_manual( values=colours1, breaks=sampleRank1  ) +    
    geom_line(size=0.5) +  geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +   
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "TSS",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp1 ) 
  dev.off()
  ggsave( filename = paste(file1, ".svg", sep="", collapse=NULL),    height=3,  width=width1, dpi=1000 )
  
  postscript(file=paste( file1, "-limitYaxis.eps", sep="", collapse=NULL),    height = 3, width = width1,  family = "Arial",  paper = "special",  onefile = FALSE,  horizontal = FALSE)
  FigureTemp2 <- ggplot(DataFrame_Local, aes(x=xAxis, y=yAxis,  color=SampleType) )  +   
    xlab(xLab1) +  ylab(yLab1) +  ggtitle(title1) + 
    scale_colour_manual( values=colours1, breaks=sampleRank1  ) +    
    geom_line(size=0.5) + ylim(Ymin1, Ymax1) +
    geom_vline(xintercept=0, lty=2, col="gray", size=0.5) +  
    scale_x_continuous( breaks=c(-5,  -2.5,  0,  2.5,   5  ), labels=c("-5",  "-2.5",  "TSS",  "2.5",  "5") ) +  
    pTheme_1(textSize=12) +  guides( colour = guide_legend(override.aes = list(size=2)) )  
  print( FigureTemp2 ) 
  dev.off()  
  ggsave( filename = paste(file1, "-limitYaxis.svg", sep="", collapse=NULL),   height=3,  width=width1, dpi=1000 )
  
}  











BoxPlot_ViolinPlot <- function(vector1,  numSample1,  sampleType1,  colours1,   file1,  title1,  xLab1,  yLab1,   occuT1=10) { 
  vector1[vector1>occuT1] <- occuT1
  DataFrame_Local  <- data.frame(   sampleType=sampleType1,   yAxis=vector1    ) 
  width1 <- numSample1*0.5
  
  postscript( file=paste(file1, "-boxPlot.eps", sep="", collapse=NULL),   height = 4, width = width1,  family = "Arial",  paper = "special",  onefile = FALSE,  horizontal = FALSE)
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType) ) + 
    geom_errorbar( aes(ymin=min, ymax=max),  data=whisk_1(DataFrame_Local),   width=0.3, size=0.2 ) +
    geom_boxplot( width=0.7,   aes(y=yAxis), outlier.colour="gray",  outlier.shape=1, outlier.size=0.1, size=0.2, fill=colours1 ) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12) 
  print(FigureTemp1) 
  dev.off()
  ggsave(filename = paste(file1, "-boxPlot.svg", sep="", collapse=NULL),   height=4,  width=width1,  dpi = 1000 )
  
  postscript(file=paste(file1, "-ViolinPlot.eps", sep="", collapse=NULL), height = 4, width = width1,  family = "Arial",  paper = "special",  onefile = FALSE,  horizontal = FALSE)
  FigureTemp2 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 5) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(DataFrame_Local),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=colours1, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   
  print(FigureTemp2) 
  dev.off()
  ggsave(filename = paste(file1, "-ViolinPlot.svg", sep="", collapse=NULL),   height=4,  width=width1, dpi = 1000 )
  
  postscript(file=paste(file1, "-ViolinPlot-noAdjust.eps", sep="", collapse=NULL), height = 4, width = width1,  family = "Arial",  paper = "special",  onefile = FALSE,  horizontal = FALSE)
  FigureTemp <- ggplot(DataFrame_Local, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray") +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(DataFrame_Local),    width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),   width=0.3, size=0.2, fill=colours1, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   
  print(FigureTemp) 
  dev.off()
  ggsave(filename = paste(file1, "-ViolinPlot-noAdjust.svg", sep="", collapse=NULL),   height=4,  width=width1, dpi = 1000 )
  
}  











#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################
#################################################################### End ##########################################################################################################################################















dir1_g  <-  "Figures_H3K27acMe3_ctrlKO_upGenes"
system( paste("mkdir  ",  dir1_g,  sep = "",  collapse = NULL) )

dir2A_g  <-  "1-NoScale"
system( paste("mkdir  ",  dir1_g,  "/",  dir2A_g,  sep = "",  collapse = NULL) )



#################################################################### Start ##########################################################################################################################################
#################################################################### Start ##########################################################################################################################################
H3K27ac_ctrl_Rep1    <- read.table("H3K27ac_ctrl/H3K27ac_ctrl1_TSS_heatmap/a.xls",              header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27ac_ctrl_Rep2    <- read.table("H3K27ac_ctrl/H3K27ac_ctrl2_TSS_heatmap/a.xls",              header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27ac_KO_Rep1      <- read.table("H3K27ac_KO/H3K27ac_KO1_TSS_heatmap/a.xls",                  header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27ac_KO_Rep2      <- read.table("H3K27ac_KO/H3K27ac_KO2_TSS_heatmap/a.xls",                  header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_ctrl_Rep1   <- read.table("H3K27me3_ctrl/H3K27me3_ctrl1_TSS_heatmap/a.xls",            header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_ctrl_Rep2   <- read.table("H3K27me3_ctrl/H3K27me3_ctrl2_TSS_heatmap/a.xls",            header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_ctrl_Rep3   <- read.table("H3K27me3_ctrl/H3K27me3_ctrl3_TSS_heatmap/a.xls",            header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_KO_Rep1     <- read.table("H3K27me3_KO/H3K27me3_KO1_TSS_heatmap/a.xls",                header=TRUE,   sep="",   quote = "",   comment.char = "")  
H3K27me3_KO_Rep2     <- read.table("H3K27me3_KO/H3K27me3_KO2_TSS_heatmap/a.xls",                header=TRUE,   sep="",   quote = "",   comment.char = "")  

    
dim(H3K27ac_ctrl_Rep1)    
dim(H3K27ac_ctrl_Rep2)    
dim(H3K27ac_KO_Rep1)    
dim(H3K27ac_KO_Rep2)    
dim(H3K27me3_ctrl_Rep1)    
dim(H3K27me3_ctrl_Rep2) 
dim(H3K27me3_ctrl_Rep3) 
dim(H3K27me3_KO_Rep1)    
dim(H3K27me3_KO_Rep2) 


H3K27ac_ctrl_Rep1  <- as.matrix(H3K27ac_ctrl_Rep1[,-(1:4)])    
H3K27ac_ctrl_Rep2  <- as.matrix(H3K27ac_ctrl_Rep2[,-(1:4)])    
H3K27ac_KO_Rep1  <- as.matrix(H3K27ac_KO_Rep1[,-(1:4)])    
H3K27ac_KO_Rep2  <- as.matrix(H3K27ac_KO_Rep2[,-(1:4)])    
H3K27me3_ctrl_Rep1 <- as.matrix(H3K27me3_ctrl_Rep1[,-(1:4)])    
H3K27me3_ctrl_Rep2 <- as.matrix(H3K27me3_ctrl_Rep2[,-(1:4)]) 
H3K27me3_ctrl_Rep3 <- as.matrix(H3K27me3_ctrl_Rep3[,-(1:4)]) 
H3K27me3_KO_Rep1 <- as.matrix(H3K27me3_KO_Rep1[,-(1:4)])    
H3K27me3_KO_Rep2 <- as.matrix(H3K27me3_KO_Rep2[,-(1:4)]) 


dim(H3K27ac_KO_Rep1)      
dim(H3K27ac_KO_Rep2)   
dim(H3K27ac_ctrl_Rep1)    
dim(H3K27ac_ctrl_Rep2)    
dim(H3K27me3_ctrl_Rep1)    
dim(H3K27me3_ctrl_Rep2) 
dim(H3K27me3_ctrl_Rep3) 
dim(H3K27me3_KO_Rep1)    
dim(H3K27me3_KO_Rep2) 


numOfColumns <- ncol(H3K27me3_KO_Rep2)
numOfRows    <- nrow(H3K27me3_KO_Rep2)  
numOfColumns
numOfRows


Average_H3K27ac_ctrl  <- (H3K27ac_ctrl_Rep1 + H3K27ac_ctrl_Rep2)/2
Average_H3K27ac_KO  <- (H3K27ac_KO_Rep1 + H3K27ac_KO_Rep2)/2 
Average_H3K27me3_ctrl <- (H3K27me3_ctrl_Rep1 + H3K27me3_ctrl_Rep2+H3K27me3_ctrl_Rep3)/3
Average_H3K27me3_KO <- (H3K27me3_KO_Rep1 + H3K27me3_KO_Rep2)/2

dim(Average_H3K27ac_ctrl)
dim(Average_H3K27ac_KO) 
dim(Average_H3K27me3_ctrl)
dim(Average_H3K27me3_KO)

vec_H3K27ac_KO_Rep1 <- colMeans(H3K27ac_KO_Rep1)      
vec_H3K27ac_KO_Rep2 <- colMeans(H3K27ac_KO_Rep2)   
vec_H3K27ac_ctrl_Rep1 <- colMeans(H3K27ac_ctrl_Rep1)    
vec_H3K27ac_ctrl_Rep2 <- colMeans(H3K27ac_ctrl_Rep2)    
vec_H3K27me3_ctrl_Rep1 <- colMeans(H3K27me3_ctrl_Rep1)    
vec_H3K27me3_ctrl_Rep2 <- colMeans(H3K27me3_ctrl_Rep2) 
vec_H3K27me3_ctrl_Rep3 <- colMeans(H3K27me3_ctrl_Rep3) 
vec_H3K27me3_KO_Rep1 <- colMeans(H3K27me3_KO_Rep1)    
vec_H3K27me3_KO_Rep2 <- colMeans(H3K27me3_KO_Rep2) 

vec_Average_H3K27ac_ctrl <- colMeans(Average_H3K27ac_ctrl)
vec_Average_H3K27ac_KO <- colMeans(Average_H3K27ac_KO) 
vec_Average_H3K27me3_ctrl <- colMeans(Average_H3K27me3_ctrl)
vec_Average_H3K27me3_KO <- colMeans(Average_H3K27me3_KO)


length(vec_H3K27ac_KO_Rep1)    
length(vec_H3K27ac_KO_Rep2) 
length(vec_H3K27ac_ctrl_Rep1)    
length(vec_H3K27ac_ctrl_Rep2)   
length(vec_H3K27me3_ctrl_Rep1)  
length(vec_H3K27me3_ctrl_Rep2)
length(vec_H3K27me3_ctrl_Rep3)
length(vec_H3K27me3_KO_Rep1) 
length(vec_H3K27me3_KO_Rep2) 

length(vec_Average_H3K27ac_ctrl)
length(vec_Average_H3K27ac_KO)
length(vec_Average_H3K27me3_ctrl)
length(vec_Average_H3K27me3_KO)





############################################################################################## 1. NO  Scaled


averageLines(vector1= c(vec_H3K27ac_KO_Rep1, vec_H3K27ac_KO_Rep2, vec_H3K27ac_ctrl_Rep1, vec_H3K27ac_ctrl_Rep2, vec_H3K27me3_ctrl_Rep1, vec_H3K27me3_ctrl_Rep2, vec_H3K27me3_ctrl_Rep3,   vec_H3K27me3_KO_Rep1,  vec_H3K27me3_KO_Rep2),                                                                  
             numSample1=9, 
             sampleType1=c(  rep("H3K27ac_KO_Rep1", numOfColumns), rep("H3K27ac_KO_Rep2", numOfColumns),  rep("H3K27ac_ctrl_Rep1", numOfColumns), rep("H3K27ac_ctrl_Rep2", numOfColumns),  rep("H3K27me3_ctrl_Rep1", numOfColumns),  rep("H3K27me3_ctrl_Rep2", numOfColumns),   rep("H3K27me3_ctrl_Rep3", numOfColumns),  rep("H3K27me3_KO_Rep1", numOfColumns),  rep("H3K27me3_KO_Rep2", numOfColumns) ),                                   
             colours1=c("H3K27ac_KO_Rep1"="red", "H3K27ac_KO_Rep2"="red4", "H3K27ac_ctrl_Rep1"="orange", "H3K27ac_ctrl_Rep2"="orange4", "H3K27me3_ctrl_Rep1"="yellowgreen", "H3K27me3_ctrl_Rep2"="green", "H3K27me3_ctrl_Rep3"="green4",  "H3K27me3_KO_Rep1"="blue",  "H3K27me3_KO_Rep2"="blue4" ), 
             sampleRank1=c(  "H3K27ac_ctrl_Rep1", "H3K27ac_ctrl_Rep2",  "H3K27ac_KO_Rep1", "H3K27ac_KO_Rep2",  "H3K27me3_ctrl_Rep1", "H3K27me3_ctrl_Rep2", "H3K27me3_ctrl_Rep3",  "H3K27me3_KO_Rep1",  "H3K27me3_KO_Rep2" ),  
             file1=paste(dir1_g,  "/",  dir2A_g,  "/1-A-NoScale-ctrlKO-9samples", sep=""),  
             title1="KO Down (96 Genes)", 
             xLab1="Relative Distance (kb)", 
             yLab1="Normalized Reads Desity",  
             Ymin1=0,   
             Ymax1=8, 
             width1=4.9
) 




averageLines(vector1= c( vec_Average_H3K27ac_ctrl, vec_Average_H3K27ac_KO, vec_Average_H3K27me3_ctrl, vec_Average_H3K27me3_KO),                                                                  
             numSample1=4, 
             sampleType1=c(  rep("H3K27ac_ctrl", numOfColumns),  rep("H3K27ac_KO", numOfColumns),   rep("H3K27me3_ctrl", numOfColumns),     rep("H3K27me3_KO", numOfColumns) ),                                   
             colours1=c("H3K27ac_KO"="red4",  "H3K27ac_ctrl"="red",     "H3K27me3_KO"="green4",  "H3K27me3_ctrl"="green" ), 
             sampleRank1=c( "H3K27ac_ctrl",  "H3K27ac_KO",  "H3K27me3_ctrl",    "H3K27me3_KO" ),  
             file1=paste(dir1_g,   "/",  dir2A_g, "/1-B-NoScale-ctrlKO-4samples-average", sep=""),  
             title1="KO Down (96 Genes)", 
             xLab1="Relative Distance (kb)", 
             yLab1="Normalized Reads Desity",  
             Ymin1=0,   
             Ymax1=7,
             width1=4.4
) 







#####################
H3K27ac_KO_Rep1_A  <- rowMeans(H3K27ac_KO_Rep1)      
H3K27ac_KO_Rep2_A  <- rowMeans(H3K27ac_KO_Rep2)   
H3K27ac_ctrl_Rep1_A  <- rowMeans(H3K27ac_ctrl_Rep1)    
H3K27ac_ctrl_Rep2_A  <- rowMeans(H3K27ac_ctrl_Rep2)    
H3K27me3_ctrl_Rep1_A  <- rowMeans(H3K27me3_ctrl_Rep1)    
H3K27me3_ctrl_Rep2_A  <- rowMeans(H3K27me3_ctrl_Rep2) 
H3K27me3_ctrl_Rep3_A  <- rowMeans(H3K27me3_ctrl_Rep3) 
H3K27me3_KO_Rep1_A  <- rowMeans(H3K27me3_KO_Rep1)    
H3K27me3_KO_Rep2_A  <- rowMeans(H3K27me3_KO_Rep2) 

Average_H3K27ac_ctrl_A  <- (H3K27ac_ctrl_Rep1_A + H3K27ac_ctrl_Rep2_A)/2
Average_H3K27ac_KO_A  <- (H3K27ac_KO_Rep1_A + H3K27ac_KO_Rep2_A)/2 
Average_H3K27me3_ctrl_A <- (H3K27me3_ctrl_Rep1_A + H3K27me3_ctrl_Rep2_A + H3K27me3_ctrl_Rep3_A)/3
Average_H3K27me3_KO_A <- (H3K27me3_KO_Rep1_A + H3K27me3_KO_Rep2_A)/2

length(Average_H3K27ac_KO_A)


BoxPlot_ViolinPlot(vector1= c(H3K27ac_KO_Rep1_A, H3K27ac_KO_Rep2_A, H3K27ac_ctrl_Rep1_A, H3K27ac_ctrl_Rep2_A, H3K27me3_ctrl_Rep1_A, H3K27me3_ctrl_Rep2_A, H3K27me3_ctrl_Rep3_A,   H3K27me3_KO_Rep1_A,  H3K27me3_KO_Rep1_A),                                                                  
                   numSample1=10,
                   sampleType1=c(  rep("H3K27ac_KO_Rep1", numOfRows), rep("H3K27ac_KO_Rep2", numOfRows),  rep("H3K27ac_ctrl_Rep1", numOfRows), rep("H3K27ac_ctrl_Rep2", numOfRows),  rep("H3K27me3_ctrl_Rep1", numOfRows),  rep("H3K27me3_ctrl_Rep2", numOfRows),   rep("H3K27me3_ctrl_Rep3", numOfRows),  rep("H3K27me3_KO_Rep1", numOfRows),  rep("H3K27me3_KO_Rep2", numOfRows) ),                                   
                   colours1=c("red", "red4",    "purple",   "purple4" ,   "yellowgreen",  "green",  "green4",   "blue",  "blue4"  ), 
                   file1=paste(dir1_g,   "/",  dir2A_g,  "/2-A-NoScale-ctrlKO-9samples", sep=""),  
                   title1="KO Down (96 Genes)", 
                   xLab1="Samples", 
                   yLab1="Normalized Reads Desity",  
                   occuT1=5
) 



BoxPlot_ViolinPlot(vector1= c( Average_H3K27ac_ctrl_A, Average_H3K27ac_KO_A, Average_H3K27me3_ctrl_A, Average_H3K27me3_KO_A ),                                                                  
                   numSample1=5,
                   sampleType1=c(  rep("H3K27ac_ctrl", numOfRows),  rep("H3K27ac_KO", numOfRows),   rep("H3K27me3_ctrl", numOfRows),     rep("H3K27me3_KO", numOfRows) ),                                   
                   colours1=c("red",     "red4",   "green",    "green4" ), 
                   file1=paste(dir1_g,   "/",  dir2A_g,  "/2-B-NoScale-ctrlKO-4samples", sep=""),  
                   title1="KO Down (96 Genes)", 
                   xLab1="Samples", 
                   yLab1="Normalized Reads Desity",  
                   occuT1=5
) 






####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################





























