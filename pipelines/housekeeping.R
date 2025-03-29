# This script prepares the data collected from modular I2C
# sensor stations with a Yocto-I2C reporting the data in long
# format (variable and value columns). Several
# dynamic plots are generated to explore teletransmission
# history and quality, as well as data timeseries.

# Environment with minimal dependencies
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
#suppressMessages(library(ggplot2))
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
  mutate(datetime = as.POSIXct(unixTime)) %>%
  mutate(yday = yday(datetime)) %>%
  mutate(variable.orig = variable) %>%
  mutate(node = as.factor(paste0("Node ", nodeid))) %>%
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

# Create a color palette that works well with node and variable combinations
# Using plotly's default color sequence which works well
get_colors <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Create plots for each type in the desired order
plot_list <- list()

# Temperature plot
temp_data <- df %>% filter(type == "Temperature")
if(nrow(temp_data) > 0) {
  plot_list$Temperature <- plot_ly(temp_data, x = ~datetime, y = ~value, 
                                  color = ~interaction(node, variable, sep = " - "),
                                  colors = get_colors(length(unique(interaction(temp_data$node, temp_data$variable)))),
                                  type = 'scatter', mode = 'lines+markers',
                                  text = ~paste("Node:", node, "<br>Variable:", variable, "<br>Value:", value, 
                                               "<br>Time:", datetime),
                                  hoverinfo = 'text') %>%
    layout(yaxis = list(title = "Temperature (°C)"),
           xaxis = list(title = "", range = c(default_start, default_end)),
           legend = list(title = list(text = "<b>Node - Variable</b>"))) %>%
    config(displayModeBar = TRUE, scrollZoom = TRUE,
           modeBarButtonsToAdd = list("toggleSpikelines", "drawline", "drawopenpath", "drawclosedpath", 
                                     "drawcircle", "drawrect", "eraseshape", "toImage"))
}

# NTC plot
ntc_data <- df %>% filter(type == "NTC")
if(nrow(ntc_data) > 0) {
  plot_list$NTC <- plot_ly(ntc_data, x = ~datetime, y = ~value, 
                          color = ~interaction(node, variable, sep = " - "),
                          colors = get_colors(length(unique(interaction(ntc_data$node, ntc_data$variable)))),
                          type = 'scatter', mode = 'lines+markers',
                          text = ~paste("Node:", node, "<br>Variable:", variable, "<br>Value:", value, 
                                       "<br>Time:", datetime),
                          hoverinfo = 'text') %>%
    layout(yaxis = list(title = "NTC Resistance (Ω)"),
           xaxis = list(title = "", range = c(default_start, default_end)),
           legend = list(title = list(text = "<b>Node - Variable</b>"))) %>%
    config(displayModeBar = TRUE, scrollZoom = TRUE,
           modeBarButtonsToAdd = list("toggleSpikelines", "drawline", "drawopenpath", "drawclosedpath", 
                                     "drawcircle", "drawrect", "eraseshape", "toImage"))
}

# Voltage plot
volt_data <- df %>% filter(type == "Voltage")
if(nrow(volt_data) > 0) {
  plot_list$Voltage <- plot_ly(volt_data, x = ~datetime, y = ~value, 
                              color = ~interaction(node, variable, sep = " - "),
                              colors = get_colors(length(unique(interaction(volt_data$node, volt_data$variable)))),
                              type = 'scatter', mode = 'lines+markers',
                              text = ~paste("Node:", node, "<br>Variable:", variable, "<br>Value:", value, 
                                           "<br>Time:", datetime),
                              hoverinfo = 'text') %>%
    layout(yaxis = list(title = "Voltage (V)"),
           xaxis = list(title = "", range = c(default_start, default_end)),
           legend = list(title = list(text = "<b>Node - Variable</b>"))) %>%
    config(displayModeBar = TRUE, scrollZoom = TRUE,
           modeBarButtonsToAdd = list("toggleSpikelines", "drawline", "drawopenpath", "drawclosedpath", 
                                     "drawcircle", "drawrect", "eraseshape", "toImage"))
}

# Light plot
light_data <- df %>% filter(type == "Light")
if(nrow(light_data) > 0) {
  plot_list$Light <- plot_ly(light_data, x = ~datetime, y = ~value, 
                            color = ~interaction(node, variable, sep = " - "),
                            colors = get_colors(length(unique(interaction(light_data$node, light_data$variable)))),
                            type = 'scatter', mode = 'lines+markers',
                            text = ~paste("Node:", node, "<br>Variable:", variable, "<br>Value:", value, 
                                         "<br>Time:", datetime),
                            hoverinfo = 'text') %>%
    layout(yaxis = list(title = "Light Intensity"),
           xaxis = list(title = "", range = c(default_start, default_end)),
           legend = list(title = list(text = "<b>Node - Variable</b>"))) %>%
    config(displayModeBar = TRUE, scrollZoom = TRUE,
           modeBarButtonsToAdd = list("toggleSpikelines", "drawline", "drawopenpath", "drawclosedpath", 
                                     "drawcircle", "drawrect", "eraseshape", "toImage"))
}

# Humidity plot
humidity_data <- df %>% filter(type == "Humidity")
if(nrow(humidity_data) > 0) {
  plot_list$Humidity <- plot_ly(humidity_data, x = ~datetime, y = ~value, 
                               color = ~interaction(node, variable, sep = " - "),
                               colors = get_colors(length(unique(interaction(humidity_data$node, humidity_data$variable)))),
                               type = 'scatter', mode = 'lines+markers',
                               text = ~paste("Node:", node, "<br>Variable:", variable, "<br>Value:", value, 
                                            "<br>Time:", datetime),
                               hoverinfo = 'text') %>%
    layout(yaxis = list(title = "Humidity (%)"),
           xaxis = list(title = "", range = c(default_start, default_end)),
           legend = list(title = list(text = "<b>Node - Variable</b>"))) %>%
    config(displayModeBar = TRUE, scrollZoom = TRUE,
           modeBarButtonsToAdd = list("toggleSpikelines", "drawline", "drawopenpath", "drawclosedpath", 
                                     "drawcircle", "drawrect", "eraseshape", "toImage"))
}

# Create subplot in desired order: Temperature - NTC - Voltage - Light - Humidity
subplot_list <- list()
subplot_list[[1]] <- plot_list$Temperature
subplot_list[[2]] <- plot_list$NTC
subplot_list[[3]] <- plot_list$Voltage
subplot_list[[4]] <- plot_list$Light
subplot_list[[5]] <- plot_list$Humidity

# Calculate number of rows needed (2 rows)
nrows <- 2

# Create final plot with synchronized x-axes
final_plot <- subplot(subplot_list, nrows = nrows, shareX = TRUE, titleX = FALSE) %>%
  layout(
    plot_bgcolor = 'white',
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 1,
            label = "1d",
            step = "day",
            stepmode = "backward"),
          list(
            count = 7,
            label = "1w",
            step = "day",
            stepmode = "backward"),
          list(
            count = 14,
            label = "2w",
            step = "day",
            stepmode = "backward"),
          list(
            count = 30,
            label = "1m",
            step = "day",
            stepmode = "backward"),
          list(step = "all"))
    ),
    hovermode = "x unified"
  )

# Set output directory
dir.create(file.path(folder), showWarnings = FALSE)
setwd(folder)

# Save the plot
htmlwidgets::saveWidget(final_plot, "housekeeping.html", selfcontained = TRUE)
