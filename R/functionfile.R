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

# Diagnostic Plots
plot.phyto.limitation <- function(out_file, elev, phyto = "green", date_break = "6 month") {
  chla <- get_var(out_file, var_name='PHY_tchla', z=elev)
  p1 <- ggplot(chla, aes(DateTime, PHY_tchla.elv_2.3)) + geom_line() +
    theme_minimal() + labs(y='Chl-a (ug/L)', x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  p2 <- plot_var(nc_file = out_file, var_name = paste0('PHY_',phyto,'_fPho'), reference='bottom') +
    labs(x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  p3 <- plot_var(nc_file = out_file, var_name = paste0('PHY_',phyto,'_fNit'), reference='bottom') +
    labs(x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  p4<- plot_var(nc_file = out_file, var_name = paste0('PHY_',phyto,'_fT'), reference='bottom') +
    labs(x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  p5 <- plot_var(nc_file = out_file, var_name = paste0('PHY_',phyto,'_fI'), reference='bottom') +
    labs(x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  p6 <- p2 + p3 + p4 + p5 + p1 +  plot_layout(ncol=1)

  return(p6)
}


phyto.diagnostics <- function(out_file, elev, date_break = "6 month") {
  chla <- get_var(out_file, var_name='PHY_tchla', z=elev)
  p1 <- ggplot(chla, aes(DateTime, PHY_tchla.elv_2.3)) + geom_line() +
    theme_minimal() + labs(y='Chl-a (ug/L)', x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  frp <- get_var(out_file, var_name='PHS_frp', z=elev, reference='bottom')
  p2 <- ggplot(frp, aes(DateTime, PHS_frp.elv_2.3)) + geom_line() +
    theme_minimal() + labs(y='SRP (mmol/m^3)', x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  amm <- get_var(out_file, var_name='NIT_amm', z=elev, reference='bottom')
  p3 <- ggplot(amm, aes(DateTime, NIT_amm.elv_2.3)) + geom_line() +
    theme_minimal() + labs(y='Amm (mmol/m^3)', x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  nit <- get_var(out_file, var_name='NIT_nit', z=elev, reference='bottom')
  p4 <- ggplot(nit, aes(DateTime, NIT_nit.elv_2.3)) + geom_line() +
    theme_minimal() + labs(y='Nitrate (mmol/m^3)', x="") +
    scale_x_datetime(date_breaks = date_break, date_labels = "%b-%y", expand=c(0,0))

  p5 <- p2 + p3 + p4 + p1 + plot_layout(ncol=1)

  return(p5)
}


# Convert output units to mg/L
convert.to.mgL <- function(nc_file) {
  conversion_factor_df <- data.frame('Parameter' = c('NIT_amm',
                                                     'NIT_nit',
                                                     'PHS_frp',
                                                     'PHS_frp_ads',
                                                     'OXY_oxy'),
                                       'CF' = c(0.1703,
                                                0.062,
                                                0.09498,
                                                0.09498,
                                                0.032))

  nc_data <- nc_open(nc_file, write=TRUE)

  for (i in 1:5){
    var_name <- conversion_factor_df[i,1]
    cf <- conversion_factor_df[i,2]

    var <- ncvar_get(nc_data, var_name)
    var_units <- ncatt_get(nc_data, var_name, 'units')$value


    if (var_units != 'mg/L'){
      var_mgL <- var*cf
      ncatt_put(nc_data, var_name,'units','mg/L')
      ncvar_put(nc_data, var_name, var_mgL)
    }

    nc_close(nc_data)

    }

}
