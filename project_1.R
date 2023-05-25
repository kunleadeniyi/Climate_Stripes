# graphics device
#dev.new()
#options(device="quartz")
#quartz( width=5.5, height=7.4)

# import library
library(RColorBrewer)
library(shape)

# set font
par(family='Avenir Black')

# read data
data <- read.csv('/Users/ayokunle/Documents/Advanced Visualisation/HadCRUT.5.0.1.0.analysis.summary_series.global.annual.csv')


# scaler function 
  # used in my graphical report but refactored here to a create scale of -1 to 1
  scaler <- function(val) {
    return ((val - min(val))/(max(val) - min(val)) * (1 - -1) + -1)
  }

# data preparation
  years <- data$Time
  temp_data <- data$Anomaly..deg.C.
  
  df <- data.frame(years, temp_data)
  df$scaled_temp_data = scaler(temp_data)
  df$strip_bin <- cut(df$scaled_temp_data, breaks=21)
  
  # map bin to color pallette
  # sorting the bins in reverse order because the colours are in reverse order
  # colors go from red to blue with blue being the highest
  df_pallette_bin <- data.frame(sort(unique(df$strip_bin), decreasing=T ), col_strip)
  names(df_pallette_bin) <- c('bins', 'colour')
  
  # join color to the entire dataframe
  df <- merge(df, df_pallette_bin, by.x = "strip_bin", by.y = 'bins')
  out_df <- df[order(df$years),]



#dev.new(width = 400, height = 600, unit = "px")
#plot(1:20)
#dev.new(width = 550, height = 330, unit = "px")
#plot(1:20)
  #par(mfrow=c(1,1))

# boundary box
  par(fig=c(0,1,0,1), bg="white")
  par(mar=c(1,1,1,1))
  par(oma=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
  par(xpd=T)
  plot(-99, -99, axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
  title("Visual Decomposition of the Global Climate Stripes")
  #box()

# layout

#dev.new(width=4, height=6, unit="in")
# start parameters, blank plot with max x=1 and max y=1
par(fig=c(0,1,0,1), new=TRUE)
#par(mar=c(0,0,0,0))
#par(oma=c(1,1,1,1)) #bottom, left, top, right

# inner layout
#x1, y1, x2, y2
  #rect(0.0, 0.35, 1.0, 1.0, border=F, col='darkgrey') # top two-third
  #  rect(0.0, 0.75, 1.0, 1.0, border=F, col='lightgrey')
  #  rect(0.0, 0.35, 1.0, 0.75, border=F, col='slateblue')
  #rect(0.0, 0.0, 1.0, 0.35, border=F, col='white') # bottom left plot

#rect(0.00, 0.6, 0.25, 0.95, border=F, col='lightgrey') # top plot 1
#rect(0.25, 0.6, 0.50, 0.95, border=F, col='lightgrey') # top plot 2
#rect(0.50, 0.6, 0.75, 0.95, border=F, col='lightgrey') # top plot 3
#rect(0.75, 0.6, 1.00, 0.95, border=F, col='lightgrey') # top plot 4

#rect(0, 0.0, 0.5, 0.10, border=F, col='lightgrey') # bottom left corner

# color pallette
col_strip <- brewer.pal(11, "RdBu")
col_strip <- colorRampPalette(col_strip)(21) # extend color pallette 

#"#67001F" "#8C0C25" "#B2182B" "#C43C3C" "#D6604D" "#E58267" "#F4A582" "#F8C0A4" "#FDDBC7"
#"#FAE9DF" "#F7F7F7" "#E3EEF3" "#D1E5F0" "#B1D5E7" "#92C5DE" "#6AACD0" "#4393C3" "#317CB7"
#"#2166AC" "#124A86" "#053061"

warm_mid_color <- "#B2182B"
cool_mid_color <- "#2166AC"
mid_color <- "#F7F7F7"
  
warmest_color <- "#67001F"
coolest_color <- "#053061"
  

# top section 
  #par(fig=c(0,1,0,1), new=TRUE)
  par(fig=c(0.00, 0.6, 0.75, 0.96), new=TRUE) # x1, x2, y1, y2
  #par(mfrow=c(2,1))
  
    # move the scaler function and othe data preparation steps above this 
    # climate strip
    barplot(rep(4, length(out_df$years)), # names.arg=out_df$years, 
            col=out_df$colour, axes=F, space=0, border=NA)  
  
  par(fig=c(0.60, 0.9, 0.75, 0.96), new=TRUE) # x1, x2, y1, y2
  
    colors_scheme <- c(cool_mid_color, mid_color, warm_mid_color)
    #barplot(rep(4, length(colors_scheme)), col=colors_scheme, border=F)
    plot(-99, -99, type='n', axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
    
    polygon(x = c(0.7, 0.8, 1.0, 0.9),                          
            y = c(0.1, 0.9, 0.88, 0.08),                             
            col = warm_mid_color, border = F)    
    
    polygon(x = c(0.4, 0.4, 0.6, 0.6),                         
            y = c(0.05, 0.9, 0.9, 0.05),                             
            col = 'lightgrey', border = F)  
    
    polygon(x = c(0.1, 0.0, 0.2, 0.3),                           
            y = c(0.08, 0.88, 0.9, 0.1),                             
            col = cool_mid_color, border = F)  # Color of polygon

    
      
# middle section 
  #par(fig=c(0,1,0,1), new=TRUE)
  par(fig=c(0.00, 0.6, 0.35, 0.75), new=TRUE) # x1, x2, y1, y2
  
  plot(-99, -99, type='n', axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='')
  # each bar represents a year
  # coldest year
  polygon(x = c(0.0, 0.0, 0.15, 0.15),                         
          y = c(0.05, 0.9, 0.9, 0.05),                             
          #col = cool_mid_color,
          col = coolest_color, #out_df$colour[1]
          border = F)
  
  #warmest year
  polygon(x = c(0.7, 0.7, 0.85, 0.85),                         
          y = c(0.05, 0.9, 0.9, 0.05),                             
          #col = cool_mid_color,
          col = warmest_color, #out_df$colour[1]
          border = F)

  par(fig=c(0.6, 0.9, 0.35, 0.75), new=TRUE) # x1, x2, y1, y2
  par(xpd=T)
  # complete pallette
  barplot(rep(4, length(col_strip)), col=col_strip, 
          #border=F, 
          border=col_strip,
          horiz=T, axes=F, 
          space=0)
  
  # arrows
  arrows(x0 = 4.2, y0 = 11, x1 = 4.2, y1 = 21, length=0.07) # mid to coolest
  arrows(x0 = 4.2, y0 = 10, x1 = 4.2, y1 = 0, length=0.07) # mid to warmer
  
  # text beside arrows
  text(4.4, 16, #x, y
       "Cool", adj=0.5, family="Avenir", cex=0.65,
       srt=90) # rotate text by 90 degrees
  
  text(4.4, 6, #x, y
       "Warm", adj=0.5, family="Avenir", cex=0.65,
       srt=90)
  
  par(xpd=F)
  
  
# bottom section  
  par(fig=c(0.30, 0.95, 0.175, 0.35), new=TRUE) # x1, x2, y1, y2
  par(mar=c(0.5,0,0.5,0))
  par(oma=c(0,0,0,0))
  
  # plot of line chart and climate stripes
    par(xpd=T)
    #par(mfrow=c(2,1))
    #barplot(out_df$temp_data, col=out_df$colour, border=F)
    bar <- barplot(out_df$scaled_temp_data, col=out_df$colour, border=out_df$colour, axes=F)
    
      points(bar[which(out_df$scaled_temp_data==1)],  out_df[out_df$scaled_temp_data==1,4],
        cex=.6, pch=19,col=out_df[out_df$scaled_temp_data==1,5],
        #bg='red',lwd=2
        )
      points(bar[which(out_df$scaled_temp_data==-1)],  out_df[out_df$scaled_temp_data==-1,4],
             cex=.6, pch=19, col=out_df[out_df$scaled_temp_data==-1,5],
             #bg='red',lwd=2
             )
    
    par(fig=c(0.30, 0.95, 0.05, 0.195), new=TRUE) # x1, x2, y1, y2
    # climate strip
    barplot(rep(4, length(out_df$years)), # names.arg=out_df$years, 
            col=out_df$colour, axes=F, space=0, border=NA)  
     
    # climate strip
    #barplot(rep(4, length(out_df$years)), # names.arg=out_df$years, 
    #        col=out_df$colour, axes=F, space=0, border=NA)
    

    
# Miscellaneous
  par(fig=c(0,1,0,1), new=TRUE)
  par(mar=c(1,1,1,1))
  par(oma=c(0.1,0.1,0.1,0.1)) #bottom, left, top, right
  par(xpd=T)  
  
  
  plot(-99, -99, axes=F, xlim=c(0,1), ylim=c(0,1))
  
  #helper grid lines with axis
    #axis(1, las=2, tck=0.02, cex.axis=0.5, hadj=-0.5)
    #axis(2, las=2, tck=0.02, cex.axis=0.5, hadj=-0.5)
    ##lines(c(0.5,0.5), c(0,1))
    #le = seq(0.50,0.65, by=0.01)
    #for (i in le) {
    #  lines(c(i,i), c(0,1), lty=1, lwd=0.5)
    #  lines(c(0,1), c(i,i), lty=1, lwd=0.5)
    #}

  #top lines
  lines(c(0.18,0), c(0.825,0.71), lty=1, lwd=0.5)
  lines(c(0.185,0.07), c(0.825,0.71), lty=1, lwd=0.5)
  
  lines(c(0.525,0.38), c(0.825,0.71), lty=1, lwd=0.5)
  lines(c(0.53,0.47), c(0.825,0.71), lty=1, lwd=0.5)
  
  #bottom lines 
  lines(c(0.505,0), c(0.14,0.39), lty=1, lwd=0.5)
  lines(c(0.505,0.07), c(0.145,0.39), lty=1, lwd=0.5)
  
  lines(c(0.96,0.38), c(0.305,0.39), lty=1, lwd=0.5)
  lines(c(0.965,0.47), c(0.312,0.39), lty=1, lwd=0.5)
 
  # curved lines and arrows
  par(fig=c(0,1,0,1), new=TRUE)
        
   # attempt 2 - hmmm ... something is cooking 
    library(plotrix)
    plot(-99, -99, axes=F, xlim=c(0,1), ylim=c(0,1), xlab='', ylab='', type='n')
    #draw.arc(0.60, 0.65, 0.30, deg1=80,deg2=98,col="green")
    draw.arc(0.30, 0.35, 0.28, deg1=185,deg2=260,)
    Arrowhead(x0 = 0.26, y0 = 0.11, angle = 0, 
              arr.length = 0.25, arr.type = "triangle", lty = 0.5)
    
    draw.arc(0.72, 0.75, 0.3, deg1=30,deg2=-30)
    Arrowhead(x0 = 0.972, y0 = 0.61, angle = 240, 
              arr.length = 0.25, arr.type = "triangle", lty = 0.5)
    
    #Arrows(0.57, 0.91, 0.65, 0.91, arr.type="triangle", arr.length = 0.25, arr.lty = 0.5)
    lines(c(0.57, 0.65), c(0.91, 0.91))
    Arrowhead(x0 = 0.65, y0 = 0.91, angle = 0, 
              arr.length = 0.25, arr.type = "triangle", lty = 0.5)
    
    lines(c(0.63, 0.49), c(0.55, 0.55))
    Arrowhead(x0 = 0.49, y0 = 0.55, angle = 180, 
              arr.length = 0.25, arr.type = "triangle", lty = 0.5)
    
    #Arrows(0.26,0.11,0.26,0.11, arr.type="triangle", arr.width=0.2, arr.length = 0.2)

  # Annotation Text 
    #par(fig=c(0,1,0,1), new=TRUE)
    text(
      0.1, 0.68, 
      paste("Coldest Temperature", 
            "\nYear: ", out_df[out_df$scaled_temp_data==-1,]$years, 
            "\nAnomaly: ", out_df[out_df$scaled_temp_data==-1,]$temp_data),
      family="Avenir", cex=0.7, adj=c(0,1), col=coolest_color
    )
    
    text(
      0.35, 0.5, 
      paste("Warmest Temperature", 
            "\nYear: ", out_df[out_df$scaled_temp_data==1,]$years, 
            "\nAnomaly: ", out_df[out_df$scaled_temp_data==1,]$temp_data),
      family="Avenir", cex=0.7, adj=c(1,1), col=warmest_color
    )
    
  