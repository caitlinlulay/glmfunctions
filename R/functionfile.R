# -------------------------------------------------------------------------
# General Lake Model Additional Functions
# Programmer: Caitlin Lulay
# Date: October 31, 2022
# -------------------------------------------------------------------------

# Clear the console
cat("\f")
rm(list = ls())

# Setup -------------------------------------------------------------------
# Load the necessary libraries
library(lubridate)
library(ggplot2)
library(patchwork)


# Ensure datetime column in input files to correct format
format.date <- function(csvfile) {
  file <- read.csv(csvfile)
  x <- tryCatch(!is.na(as.Date(file$Date[1], "%Y-%m-%d %H:%M:%S")))
  if (x == FALSE) {
    parsedate <- mdy_hm(file$Date)
    newdate <- format(parsedate, format = "%Y-%m-%d %H:%M:%S")
    file$Date <- newdate
    write.csv(file,csvfile,quote = FALSE,row.names = FALSE)
  }
} 


# Plot temperature heat map comparison
compare.model <- function(field_data, lims, field_sites, title) {
  # Put modeled data from netcdf for specified variable
  modeled <- get_var(file=out_file, var_name='temp', reference='bottom')
  
  # Determine the overall min and max values to set the colormap
  model_min = min(modeled, na.rm=TRUE)
  model_max = max(modeled, na.rm=TRUE)
  data_min = min(field_data$temp, na.rm=TRUE)
  data_max = max(field_data$temp, na.rm=TRUE)
  zmin = min(model_min, data_min)
  zmax = max(model_max, data_max)
  
  
  modeled <- subset(modeled, select=-c(DateTime))
  
  
  p1 <- plot_var(nc_file = out_file, var_name = 'temp', reference='bottom', zlim=c(zmin,zmax)) +
    scale_x_datetime(limit=lims,date_breaks = "1 month", date_labels = "%b-%d") +
    labs(x='') + theme(plot.margin=margin(0,0,0,0,'pt'))
  
  p2 <- ggplot(field_data, aes(x=Datetime, y=depth, color=temp)) + 
    geom_point(size=5)
  
  p2 <- p2 + scale_color_distiller(palette='RdYlBu',limits=c(zmin, zmax)) +
    theme_minimal() + 
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.position='none',
          panel.grid.major.y=element_blank(),
          plot.margin=margin(0,0,0,0,'pt')) + 
    labs(title=title, x='', y='') + scale_y_reverse(lim=c()) +
    scale_x_datetime(lim=lims, date_breaks = "1 month") 
  
  if (field_sites >= 2){
    p2 + plot_spacer() + p1 +
      plot_layout(ncol=1, heights=c(0.1,-0.06,1), guides='collect')
  } else{
    p2 + plot_spacer() + p1 +
      plot_layout(ncol=1, heights=c(0.05,-0.06,1), guides='collect')
  }
  
}
