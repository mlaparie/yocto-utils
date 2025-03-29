#!/usr/bin/env Rscript

# This script prepares the data collected from modular I2C
# sensor stations with a Yocto-I2C reporting the data in long
# format (variable and value columns). Several
# dynamic plots are generated to explore teletransmission
# history and quality, as well as data timeseries.

# Environment with minimal dependencies
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(lubridate))

# Supress all warnings
options(warn=-1)

# Get the arguments passed to the script
args <- commandArgs(trailingOnly = TRUE)

# Access the first argument
file_arg <- args[2]
folder <- args[1]

# Load data
df <- fread(file_arg, sep = ",")
colnames(df) <- c("datetime", "node", "second", "variable", "value")

# Variable mutations
df <- df %>% 
  mutate(datetime = as.POSIXct(datetime)) %>%
  mutate(yday = yday(datetime)) %>%
  mutate(variable.orig = variable) %>%
  mutate(node = as.factor(node)) %>%
  mutate(variable = as.factor(recode(as.character(variable),
                         "1" = "Voltage",
                         "2" = "T1",
                         "3" = "RH1",
                         "4" = "L1",
                         "5" = "NTC1",
                         "6" = "NTC2",
                         "7" = "NTC3",
                         "8" = "NTC4",
                         "9" = "NTC5",
                         "10" = "NTC6",
                         "11" = "NTC7",
                         "12" = "NTC8",
                         "13" = "NTC9",
                         "14" = "NTC10",
                         "15" = "T2",
                         "16" = "RH2",
                         "17" = "L2",
                         "18" = "T3",
                         "19" = "RH3",
                         "20" = "L3",
                         "96" = "Tbatt",
                         "97" = "Vc1",
                         "98" = "Vc2",
                         "99" = "Vbatt"))) %>%
  mutate(type = case_when(
             variable %in% c("Voltage", "Vc1", "Vc2", "Vbatt") ~ "Voltage",
             variable %in% c("T1", "T2", "T3", "Tbatt") ~ "Temperature",
             variable %in% c("RH1", "RH2", "RH3") ~ "Humidity",
             variable %in% c("L1", "L2", "L3") ~ "Light",
             TRUE ~ "NTC"
         )) %>%
  mutate(type = factor(type, levels = c("Temperature", "NTC", "Voltage", "Light", "Humidity")))

# Calculate default date range (last week)
default_start <- max(df$datetime, na.rm = TRUE) - days(7)
default_end <- max(df$datetime, na.rm = TRUE)

# Create individual plots with consistent colors and showlegend=FALSE
plot_list <- list()

# Function to create plots with consistent settings
create_plot <- function(data, ytitle, showlegend) {
  plot_ly(data, x = ~datetime, y = ~value, 
          color = ~node,
          opacity = 0.7,
          type = 'scatter', mode = 'markers',
          text = ~paste0("N", node, "<br>", variable, " = ", value, 
                       "<br>", datetime),
          hoverinfo = 'text',
          legendgroup = ~node,
          showlegend = showlegend) %>%
    layout(plot_bgcolor='#e5ecf6',
           margin = list(t = 10),
           legend = list(title = list(text = "<b>Node</b>"), y = 0.5),
           yaxis = list(title = ytitle),
           xaxis = list(title = "",
                        range = c(default_start, default_end))) %>%
    config(displayModeBar = TRUE, scrollZoom = FALSE,
           modeBarButtonsToAdd = list("toggleSpikelines", "toImage"))
}

# Create all plots with showlegend=FALSE except the first
plot_list$Temperature <- create_plot(df %>% filter(type == "Temperature"), "Air temperature (°C)", TRUE)
plot_list$NTC <- create_plot(df %>% filter(type == "NTC"), "NTC (°C)", FALSE)
plot_list$Voltage <- create_plot(df %>% filter(type == "Voltage"), "Voltage (V)", FALSE)
plot_list$Light <- create_plot(df %>% filter(type == "Light"), "Light (klux)", FALSE)
plot_list$Humidity <- create_plot(df %>% filter(type == "Humidity"), "Relative humidity (%)", FALSE)

# Combine all plots vertically with the legend at the top
final_plot <- subplot(
  plot_list$Temperature,
  plot_list$Light,
  plot_list$Voltage,
  plot_list$Humidity,
  plot_list$NTC,
  nrows = 2,
 # widths = c(0.7),
  shareX = TRUE,
  titleY = TRUE
) %>%
layout(
  xaxis = list(
    rangeselector = list(
      buttons = list(
        list(count = 1, label = "1d", step = "day", stepmode = "backward"),
        list(count = 7, label = "1w", step = "day", stepmode = "backward"),
        list(count = 14, label = "2w", step = "day", stepmode = "backward"),
        list(count = 1, label = "1m", step = "month", stepmode = "backward"),
        list(step = "all")
      )
    ),
    hovermode = "x unified"
  )
)

# Set output directory
dir.create(file.path(folder), showWarnings = FALSE)
setwd(folder)

# Save with full height
htmlwidgets::saveWidget(final_plot, "housekeeping.html", selfcontained = TRUE,
                       title = "Sensor Data Dashboard",
                       libdir = "lib",
                       background = "white")
