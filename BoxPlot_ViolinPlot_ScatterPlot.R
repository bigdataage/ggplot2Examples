



####################################################################  Start  ##########################################################################################################################################
####################################################################  Start  ##########################################################################################################################################
####################################################################  Start  ##########################################################################################################################################
####################################################################  load required libraries and define functions
library("reshape2")
library("ggplot2") 
library("grid")
library("Cairo")
library("RColorBrewer")
library("gplots")  
library("stats")
library("KernSmooth")
library("matrixStats")
library("extrafont")
font_import()
fonttable()
loadfonts()
loadfonts(device="postscript")
names(postscriptFonts())







pTheme_1 <- function( textSize=12 ) {  
  theme(  
    line  = element_line(colour="black", size=1.0,   linetype=1,      lineend=NULL),                                                          ## all line elements.  局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
    rect  = element_rect(colour="black", size=1.0,   linetype=1,      fill="transparent" ),                                                   ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
    text  = element_text(family="serif",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),   ## all text elements.  "serif" for a serif-serif font. 所有文本相关属性.
    title = element_text(family="serif",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=2,   angle=NULL, lineheight=NULL),    ## all title elements: plot, axes, legends. hjust:水平对齐的方向.  所有标题属性.
    
    axis.title        = element_text(family="serif", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
    axis.title.x      = element_text(family="serif", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=-0.5,  angle=NULL, lineheight=NULL),      ## x axis label (element_text; inherits from axis.title)
    axis.title.y      = element_text(family="serif", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=1.5,   angle=NULL, lineheight=NULL),      ## y axis label (element_text; inherits from axis.title)
    axis.text         = element_text(family="serif", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
    axis.text.x       = element_text(family="serif", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## x axis tick labels (element_text; inherits from axis.text)
    axis.text.y       = element_text(family="serif", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## y axis tick labels (element_text; inherits from axis.text)
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
    legend.text          = element_text(family="serif", face=NULL, colour="black", size=textSize-2, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
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
    plot.title      = element_text(family="serif", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	  ## plot title (text appearance) (element_text; inherits from title)  图形标题
    plot.margin     = grid::unit(c(8, 8, 8, 8), "mm", data=NULL), 	                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    
    strip.background = element_rect(colour=NULL, size=NULL, linetype=NULL, fill=NULL ), 	                                                        ## background of facet labels (element_rect; inherits from rect)  分面标签背景
    strip.text       = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	  ## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 










pTheme_2 <- function( textSize=12 ) {  
  theme(  
    line  = element_line(colour="black", size=1.0,   linetype=1,      lineend=NULL),                                                           ## all line elements.  局部优先总体,下面3个也是,只对非局部设置有效.   所有线属性.
    rect  = element_rect(colour="black", size=1.0,   linetype=1,      fill="transparent" ),                                                    ## all rectangluar elements.    hjust=1: 靠右对齐.   所有矩形区域属性.
    text  = element_text(family="serif",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),    ## all text elements.  "serif" for a serif-serif font. 所有文本相关属性.
    title = element_text(family="serif",  face=NULL,  colour="black",  size=textSize, hjust=NULL, vjust=2,   angle=NULL, lineheight=NULL),     ## all title elements: plot, axes, legends. hjust:水平对齐的方向.  所有标题属性.
    
    axis.title        = element_text(family="serif", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## label of axes (element_text; inherits from text).  horizontal: 水平的, 水平线 
    axis.title.x      = element_text(family="serif", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=-0.5,  angle=NULL, lineheight=NULL),      ## x axis label (element_text; inherits from axis.title)
    axis.title.y      = element_text(family="serif", face=NULL, colour="black", size=textSize,    hjust=NULL, vjust=1.5,   angle=NULL, lineheight=NULL),      ## y axis label (element_text; inherits from axis.title)
    axis.text         = element_text(family="serif", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## tick labels along axes (element_text; inherits from text). 坐标轴刻度的标签的属性.                                                         
    axis.text.x       = element_text(family="serif", face=NULL, colour="black", size=textSize-2,  hjust=1, vjust=1, angle=45, lineheight=NULL),               ## x axis tick labels (element_text; inherits from axis.text)
    axis.text.y       = element_text(family="serif", face=NULL, colour="black", size=textSize-2,  hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL),       ## y axis tick labels (element_text; inherits from axis.text)
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
    legend.text          = element_text(family="serif", face=NULL, colour="black", size=textSize-2, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	##legend item labels. 图例文字标签.
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
    plot.title      = element_text(family="serif", face=NULL, colour="black", size=textSize, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	  ## plot title (text appearance) (element_text; inherits from title)  图形标题
    plot.margin     = grid::unit(c(8, 8, 8, 8), "mm", data=NULL), 	                                                                                  ## margin around entire plot (unit with the sizes of the top, right, bottom, and left margins)
    
    strip.background = element_rect(colour=NULL, size=NULL, linetype=NULL, fill=NULL ), 	                                                        ## background of facet labels (element_rect; inherits from rect)  分面标签背景
    strip.text       = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels (element_text; inherits from text)
    strip.text.x     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL), 	    ## facet labels along horizontal direction (element_text; inherits from strip.text)
    strip.text.y     = element_text(family="serif", face=NULL, colour=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, lineheight=NULL)   	  ## facet labels along vertical direction (element_text; inherits from strip.text) 
  ) 
} 








normalize_1 <- function(vector1) {
  max1 = max(vector1)  
  min1 = min(vector1)  
  lower1 = -1    
  upper1 = 1
  vector2 = lower1 + (upper1-lower1)*(vector1-min1)/(max1-min1)
  return(vector2)
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








BoxPlot_ViolinPlot <- function(vector1,  numSample1,  sampleType1,  colours1,   path1,   fileName1,    title1,  xLab1,  yLab1,   yMin,  yMax) { 
  DataFrame_Local  <- data.frame(   sampleType=sampleType1,   yAxis=vector1    ) 
  width1 <- numSample1*0.5
  
  SVG1 <- paste(path1,  "/",  "SVG",  sep = "",  collapse = NULL)
  PNG1 <- paste(path1,  "/",  "PNG",  sep = "",  collapse = NULL)
  PDF1 <- paste(path1,  "/",  "PDF",  sep = "",  collapse = NULL)
  EPS1 <- paste(path1,  "/",  "EPS",  sep = "",  collapse = NULL)
  system( paste("mkdir  ",  SVG1,  sep = "",  collapse = NULL) )
  system( paste("mkdir  ",  PNG1,  sep = "",  collapse = NULL) )
  system( paste("mkdir  ",  PDF1,  sep = "",  collapse = NULL) )
  system( paste("mkdir  ",  EPS1,  sep = "",  collapse = NULL) )
  
  
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType) ) + 
    geom_errorbar( aes(ymin=min, ymax=max),  data=whisk_1(DataFrame_Local),   width=0.3, size=0.2 ) +
    geom_boxplot( width=0.7,   aes(y=yAxis), outlier.colour="gray",  outlier.shape=1, outlier.size=0.1, size=0.2, fill=colours1 ) +    
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12) + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-boxPlot.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp1 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-boxPlot.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-boxPlot.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-boxPlot.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  FigureTemp2 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 3) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(DataFrame_Local),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=colours1, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-3adjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp2 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-3adjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-3adjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-3adjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  FigureTemp3 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 6) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(DataFrame_Local),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=colours1, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-6adjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp3 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-6adjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-6adjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-6adjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  
  
  FigureTemp4 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray", adjust = 12) +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(DataFrame_Local),   width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),  width=0.3, size=0.2, fill=colours1, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-12adjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp4 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-12adjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-12adjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-12adjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  
  
  FigureTemp5 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + geom_violin(aes(y=yAxis), fill = "gray", colour = "gray") +  
    geom_errorbar(aes(ymin=min,ymax=max),  data=whisk_1(DataFrame_Local),    width = 0.1, size=0.1, colour = "black") +
    geom_boxplot( aes(y=yAxis),   width=0.3, size=0.2, fill=colours1, outlier.size=0,  colour = "black", notch=FALSE,  notchwidth = 0.15) + 
    stat_summary( aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-noAdjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp5 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-noAdjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-noAdjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-noAdjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
}  











BoxPlot_ViolinPlot_ScatterPlot <- function(vector1,  numSample1,  sampleType1,  colours1,   pointSize1,   path1,   fileName1,    title1,  xLab1,  yLab1,   yMin,  yMax) { 
  DataFrame_Local  <- data.frame(   sampleType=sampleType1,   yAxis=vector1    ) 
  width1 <- numSample1*0.5
  
  SVG1 <- paste(path1,  "/",  "SVG",  sep = "",  collapse = NULL)
  PNG1 <- paste(path1,  "/",  "PNG",  sep = "",  collapse = NULL)
  PDF1 <- paste(path1,  "/",  "PDF",  sep = "",  collapse = NULL)
  EPS1 <- paste(path1,  "/",  "EPS",  sep = "",  collapse = NULL)
  system( paste("mkdir  ",  SVG1,  sep = "",  collapse = NULL) )
  system( paste("mkdir  ",  PNG1,  sep = "",  collapse = NULL) )
  system( paste("mkdir  ",  PDF1,  sep = "",  collapse = NULL) )
  system( paste("mkdir  ",  EPS1,  sep = "",  collapse = NULL) )
  
  
  
  FigureTemp1 <- ggplot( DataFrame_Local, aes(x=sampleType) ) + 
    geom_jitter(aes(y=yAxis), size=pointSize1, colour=colours1) +
    geom_boxplot( alpha=0, width=0.7,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA ) +    
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12) + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-boxPlot.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp1 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-boxPlot.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-boxPlot.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-boxPlot.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  FigureTemp2 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + 
    geom_jitter(aes(y=yAxis), size=pointSize1, colour=colours1) + 
    geom_violin(aes(y=yAxis), fill = NA, colour = "grey40", adjust = 3,  alpha=0) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA ) +  
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-3adjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp2 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-3adjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-3adjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-3adjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  FigureTemp3 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + 
    geom_jitter(aes(y=yAxis), size=pointSize1, colour=colours1)  +
    geom_violin(aes(y=yAxis), fill = NA, colour = "grey40", adjust = 6,  alpha=0) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA ) + 
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-6adjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp3 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-6adjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-6adjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-6adjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  
  
  FigureTemp4 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + 
    geom_jitter(aes(y=yAxis), size=pointSize1, colour=colours1)  +
    geom_violin(aes(y=yAxis), fill = NA, colour = "grey40", adjust = 12,  alpha=0) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA ) + 
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-12adjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp4 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-12adjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-12adjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-12adjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
  
  
  FigureTemp5 <- ggplot(DataFrame_Local, aes(x=sampleType) ) + 
    geom_jitter(aes(y=yAxis), size=pointSize1, colour=colours1) +
    geom_violin(aes(y=yAxis), fill = NA, colour = "grey40",  alpha=0) +  
    geom_boxplot( alpha=0, width=0.3,   aes(y=yAxis),  outlier.size=0,  size=0.7,  fill=NA ) +
    stat_summary(   aes(y=yAxis),   fun.y=mean, colour="white", geom="point", shape=19, size=0.0, show_guide = FALSE) + 
    xlab(xLab1 ) + ylab( yLab1 ) + ggtitle( title1 )  + pTheme_2(textSize=12)   + ylim(yMin,  yMax)
  
  postscript( file=paste(EPS1,  "/",  fileName1,  "-ViolinPlot-noAdjust.eps",  sep="",  collapse=NULL),   height = 4, width = width1,  family = "serif",  paper = "special",  onefile = FALSE,  horizontal = FALSE)      
  print( FigureTemp5 )
  dev.off()  
  ggsave( filename = paste(SVG1,  "/",  fileName1,  "-ViolinPlot-noAdjust.svg",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PNG1,  "/",  fileName1,  "-ViolinPlot-noAdjust.png",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  ggsave( filename = paste(PDF1,  "/",  fileName1,  "-ViolinPlot-noAdjust.pdf",  sep="",  collapse=NULL),     height=4,    width=width1,      dpi = 1000 )
  
  
}  



####################################################################  Start  ##########################################################################################################################################
####################################################################  Start  ##########################################################################################################################################
####################################################################  Start  ##########################################################################################################################################
####################################################################  read the input files and get the n*m matrix (n genes and m bins)
matrix1 <- read.table("UpGenes-diffPeaks-distance.sorted",         header=TRUE,   sep="\t",    quote = "",   comment.char = "")
dim(matrix1)
numOfRows <- nrow(matrix1)

BoxPlot_ViolinPlot(vector1 =  log10( c( matrix1[,5],  matrix1[,6], matrix1[,7] ) ),                                 
                   numSample1 = 6,  
                   sampleType1=c(  rep("Commom", numOfRows),    rep("Ctrl_only", numOfRows),  rep("KO_only", numOfRows) ),               
                   colours1=c(  "cyan3",   "red",   "blue" ),                  
                   path1 = "4-figures",      
                   fileName1="1-up-genes-H3K27acPeaks",    
                   title1="Distance between TSSs and H3K27ac Peaks",  
                   xLab1="Three kinds of H3K27ac peaks", 
                   yLab1="Distance (log10)", yMin=1, yMax=6  
)





BoxPlot_ViolinPlot_ScatterPlot(vector1 =  log10( c( matrix1[,5],  matrix1[,6], matrix1[,7] ) ),                                 
                               numSample1 = 6,  
                               sampleType1=c(  rep("Commom", numOfRows),    rep("Ctrl_only", numOfRows),  rep("KO_only", numOfRows) ),               
                               colours1= c( rep("cyan3", numOfRows),    rep("red", numOfRows),  rep("blue", numOfRows) ),     
                               pointSize1= c( rep(0.7, numOfRows),    rep(0.7, numOfRows),  rep(0.7, numOfRows) ), 
                               path1 = "4-figures",      
                               fileName1="2-up-genes-H3K27acPeaks-scatterplot",    
                               title1="Distance between TSSs and H3K27ac Peaks",  
                               xLab1="Three kinds of H3K27ac peaks", 
                               yLab1="Distance (log10)", yMin=1, yMax=6  
)




####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
####################################################################  End    ##########################################################################################################################################
























