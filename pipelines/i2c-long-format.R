# This script prepares the data collected from modular I2C
# sensor stations with a Yocto-I2C reporting the data in long
# format (variable and value columns); see corresponding
# repository at forgemia.inra.fr/mathieu.laparie. Several
# dynamic plots are generated to explore teletransmission
# history and quality, as well as data timeseries.
# Author: Mathieu Laparie <mathieu.laparie [at] inrae.fr>

# Environment
# options(browser = "chromium", warn = -1)
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(viridis))
suppressMessages(library(plotly))
suppressMessages(library(lubridate))
suppressMessages(library(wesanderson))
suppressMessages(library(ggplot2))
suppressMessages(library(magrittr))
suppressMessages(library(htmlwidgets))
suppressMessages(library(plotly))
# suppressMessages(library(tidyverse))
# suppressMessages(library(UpSetR))

# Get the arguments passed to the script
args <- commandArgs(trailingOnly = TRUE)

# Access the first argument
file_arg <- args[2]
folder <- args[1]

# For interactive use only
# Prompt for instance/data folder
# cat("Enter a VH4W instance name: ")
# vh4w <- readLines(con="stdin", 1)
# Else set the variable manually:
# vh4w <- "v02"
# df <- fread("~/Projects/yocto-utils/data/v01/joined/joined_all.csv", sep = ",")

# Load data
df <- fread(file_arg, sep = ",")
colnames(df) <- c("datetime", "node", "second", "variable", "value")

# Variable mutations
df <- df %>% mutate(yday = yday(datetime)) %>%
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
                             "99" = "Vbatt"                             ))) %>%
    mutate(variable = factor(variable, levels = c("Voltage", "T1", "RH1", "L1",
                                                  "NTC1", "NTC2", "NTC3", "NTC4",
                                                  "NTC5", "NTC6", "NTC7", "NTC8",
                                                  "NTC9", "NTC10",
                                                  "T2", "RH2", "L2",
                                                  "T3", "RH3", "L3",
                                                  "Tbatt", "Vc1", "Vc2", "Vbatt"))) %>%
    mutate(type = case_when(
               variable %in% c("Voltage", "Vc1", "Vc2", "Vbatt") ~ "Voltage",
               variable %in% c("T1", "T2", "T3", "Tbatt") ~ "Temperature",
               variable %in% c("RH1", "RH2", "RH3") ~ "Humidity",
               variable %in% c("L1", "L2", "L3") ~ "Light",
               TRUE ~ "NTC" # Assumes all other variables fall under "NTC
           )) %>%
    # Ensure 'type' is a factor with levels in the correct order
    mutate(type = factor(type, levels = c("NTC", "Temperature", "Humidity", "Light", "Voltage")))

# Ensure there's at least one row for each combination of 'type' and 'node', so that empty ggplot facets can be drawn
# This could involve expanding your dataframe to include these combinations with NA values
all_combinations <- expand.grid(variable = levels(df$variable), node = unique(df$node), KEEP.OUT.ATTRS = FALSE) %>%
    mutate(type = case_when(
               variable %in% c("Voltage", "Vc1", "Vc2", "Vbatt") ~ "Voltage",
               variable %in% c("T1", "T2", "T3", "Tbatt") ~ "Temperature",
               variable %in% c("RH1", "RH2", "RH3") ~ "Humidity",
               variable %in% c("L1", "L2", "L3") ~ "Light",
               TRUE ~ "NTC" # Assumes all other variables fall under "NTC
           )) %>%
    # Ensure 'type' is a factor with levels in the correct order
    mutate(type = factor(type, levels = c("NTC", "Temperature", "Humidity", "Light", "Voltage")))

# Left join to ensure all combinations are represented in df_full
df_full <- left_join(all_combinations, df, by = c("node", "variable", "type"))

# Round datetime to 3 minutes (which is longer than a full station wake) and fill in
# NA for 'datetime' and 'value' where 'type' is present but 'datetime' and 'value' are missing
df_rounded <- df_full %>%
    mutate(datetime = floor_date(datetime, unit = "30 minutes")) %>%
    mutate(datetime = if_else(is.na(datetime) & !is.na(type), as.POSIXct(NA), datetime),
           value = if_else(is.na(value) & !is.na(type), NA_real_, value))

# Plots
# Set output directory
setwd(folder)

# ggplot
# Overview in node x type facets
overview <- df_rounded %>%
    ggplot(aes(x = datetime, y = value, color = variable)) +
    geom_point(alpha = 0.7, size = 0.7, aes(text = paste(datetime, "<br>", variable, " = ", value, sep = ""))) +
    geom_line(alpha = 0.7, lwd = 0.2) +
    # scale_color_viridis(n = 7, end = 0.9, option = "C", discrete = TRUE) +
    # Loop over 5 viridis colours instead
    scale_color_manual(values = rep(viridis::viridis(5, option = "C", end = 0.9),
                                    length.out = length(unique(df_rounded$variable)))) +
    facet_grid(type ~ node, scales = "free_y") +
    theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=-0, size = 7), legend.title = element_blank()) +
    xlab("") + ylab("")

# Convert to ggplotly and specify tooltip content
overview <- ggplotly(overview, tooltip = "text")
overview$x$layout$legend$title$text <- ""
overview
saveWidget(overview, "overview.html", selfcontained = TRUE)

# Plotly
# Overview with type facets
summary <- df %>% group_by(type) %>%
    mutate(hour = hour(datetime)) %>%
    group_map(~ plot_ly(.,
    x = ~datetime, 
    y = ~value, 
    color = ~variable, # ~node,
    alpha = 0.5,
#    frame = ~yday,
    text = ~paste0("N", node, ", ", variable),
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers'),
    keep = TRUE) %>%
    subplot(nrows = 2, shareX = TRUE, shareY = FALSE, titleX = FALSE) %>%
    layout(plot_bgcolor='#e5ecf6',
           xaxis = list(
               rangeselector = list(
                   buttons = list(
                       list(
                           count = 1,
                           label = "1 d",
                           step = "day",
                           stepmode = "backward"),
                       list(
                           count = 1,
                           label = "1 w",
                           step = "week",
                           stepmode = "backward"),
                       list(
                           count = 2,
                           label = "2 w",
                           step = "week",
                           stepmode = "backward"),
                       list(
                           count = 3,
                           label = "3 mo",
                           step = "month",
                           stepmode = "backward"),
                       list(step = "all")))))
summary
saveWidget(summary, "summary.html", selfcontained = TRUE)

# Plotly subplots timeseries for type, with node legends
types <- levels(df$type)
plots <- lapply(types, function(t) {
  # Filter the dataframe for the current type
  df_filtered <- df %>% filter(type == t)  
  # Plot for this type
  p <- df_filtered %>%
    group_by(variable, node) %>%
    mutate(hour = hour(datetime)) %>%
    plot_ly(
      x = ~datetime,
      y = ~value,
      color = ~node,
      alpha = 0.7,
      text = ~paste(datetime, "<br>", "N", node, "<br>", variable, " = ", value, sep = ""),
      hoverinfo = 'text',
      type = 'scatter',
      mode = 'markers',
      height = 1500,
      legendgroup = ~node,
      showlegend = ~ifelse(t == "Temperature", TRUE, FALSE),
#      line = list(width = 2, opacity = 0.5),
      marker = list(size = 6, opacity = 0.7),
#      colors = RColorBrewer::brewer.pal(8, "Set3")
      colors = c(wes_palette("GrandBudapest1"), wes_palette("GrandBudapest2"))
    ) %>%
    layout(
        xaxis = list(title = "",
                     showgrid = TRUE,
                     range = ~c(1000 * (as.numeric(ymd_hms(max(datetime))) - 86400 * 2),
                                1000 * (as.numeric(ymd_hms(max(datetime)))))),
        yaxis = list(title = "",
                     fixedrange = FALSE),
        legend = list(title = list(text = "Node")),
        annotations = list(
            list(x = 0.02 , y = 0.95, text = ~toupper(t),
                 showarrow = FALSE, xref = 'paper', yref = 'paper'))
    )
  return(p)
})

timeseries <- subplot(plots, nrows = length(types), shareX = TRUE, shareY = FALSE) %>%
    layout(
        plot_bgcolor='#e5ecf6',
        xaxis = list(
            rangeselector = list(
                buttons = list(
                    list(
                        count = 4,
                        label = "4 h",
                        step = "hour",
                        stepmode = "backward"),
                    list(
                        count = 1,
                        label = "1 d",
                        step = "day",
                        stepmode = "backward"),
                    list(
                        count = 2,
                        label = "2 d",
                        step = "day",
                        stepmode = "backward"),
                    list(
                        count = 1,
                        label = "1 w",
                        step = "week",
                        stepmode = "backward"),
                    list(
                        count = 2,
                        label = "2 w",
                        step = "week",
                        stepmode = "backward"),
                    list(
                        count = 3,
                        label = "3 mo",
                        step = "month",
                        stepmode = "backward"),
                    ## list(
                    ##     count = 1,
                    ##     label = "YTD",
                    ##     step = "year",
                    ##     stepmode = "todate"),
                    list(step = "all"))),
            rangeslider = list(type = "date",
                               bgcolor = "#EBEBEB",
                               thickness = "0.05")),
        updatemenus = list(
            list(
                x = Inf,
                y = Inf,
                buttons = list(
                    list(method = "restyle",
                         args = list("mode", "markers"),
                         label = "Markers"),
                    list(method = "restyle",
                         args = list("mode", "lines+markers"),
                         label = "Lines + markers"),
                    list(method = "restyle",
                         args = list("mode", "lines"),
                         label = "Lines")))))
timeseries
saveWidget(timeseries, "timeseries.html", selfcontained = TRUE)

## Missing data
# geom_raster
present <- df %>%
    select(-yday, -second, -variable.orig, -type) %>%
    filter(datetime >= last(datetime) - dhours(24)) %>%
    mutate(datetime = floor_date(datetime, "30 minutes") - dminutes(0)) %>%
    group_by(datetime, node, variable) %>%
    # Summarize each group: 1 if there's at least one non-NA value, 0 or NA otherwise
    summarise(value = value,
              presence = as.factor(ifelse(any(!is.na(value)), "Received", NA_real_)), .groups = 'drop')

missingplot <- present %>%
    ggplot(aes(x = variable, y = datetime, fill = variable)) +
    geom_raster(aes(text = paste(datetime, "<br>Node ", node, "<br>", variable, " = ",
                                 ifelse(presence == "Received", value, " missing"), sep = ""))) +
    scale_fill_grey(name = "", end = 0.6) +
    labs(y = "Last 24 hours split in 30-min tiles starting at --:00",
         x = "",
         title = "Wake ups every 30 min (--:-0) for 3 min (i.e., within single tiles)") +
    theme(axis.text.x = element_text(size = 6, angle =-55,
                                     hjust = 0, vjust = 1),
          text = element_text(family = "monospace", size = 8),
          panel.spacing = unit(1, "lines"),
          panel.background = element_rect(fill = "white")) +
    coord_fixed(1/1000) +
    scale_y_datetime(expand = c(0, 0)) +
    scale_x_discrete(expand = c(0, 0)) +
    facet_wrap(. ~ node, ncol = 7)
missingplotly <- ggplotly(missingplot)

# Convert to ggplotly and specify tooltip content
missingplotly <- ggplotly(missingplot, tooltip = "text")
missingplotly$x$layout$legend$title$text <- ""
missingplotly %>% layout(autosize = FALSE, width = 1000, height = 800)
missingplotly
saveWidget(missingplotly, "missing.html", selfcontained = TRUE)

## # Using UpSetR
## df_wide_rounded <- df_rounded %>% select(-c(variable.orig, type)) %>%
##     pivot_wider(names_from = variable,
##                 values_from = value) %>%
##     group_by(datetime, node) %>%
## summarise(across(c(second, yday), ~unique(.x)[1]),
##           across(5:ncol(.)-2, mean, na.rm = TRUE), # Apply mean to all numerical columns after 'yday'
##           .groups = 'drop') # This will drop the grouping, so the final tibble is no longer grouped

## binary_df <- df_wide_rounded %>%
##     pivot_longer(cols = -c(datetime, node), names_to = "variable", values_to = "value") %>%
##     mutate(missing = ifelse(is.na(value), 1, 0)) %>%
##     select(datetime, node, variable, missing) %>%
##     unite(col = "date_node", c(datetime, node), remove = FALSE) %>%
##     spread(key = variable, value = missing, fill = 0) %>%
##     data.frame()

## binary_df %>% select(-c("date_node", "datetime", "yday", "second")) %>%
##     upset(., sets = names(.)[-1],
##           query.legend = "bottom",
##           # order.by = "freq",
##           empty.intersections = "on",
##           sets.x.label = "Number of values",
##           mainbar.y.label = "Number of missing combinations")

## # Python, missingno
# df_rounded %>% select(-c("variable.orig")) %>%
#    write.csv("data_for_missingno.csv", row.names = FALSE)
