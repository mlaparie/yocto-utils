# Environment
options(browser = "chromium", warn = -1)
suppressMessages(library(tidyverse))
suppressMessages(library(data.table))
suppressMessages(library(viridis))
suppressMessages(library(plotly))
suppressMessages(library(lubridate))
suppressMessages(library(wesanderson))
suppressMessages(library(htmlwidgets))

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

# Load data
df <- fread(file_arg, sep = ",")
colnames(df) <- c("datetime", "node", "second", "variable", "value")

# Variable mutations
df <- df  %>% mutate(yday = yday(datetime)) %>%
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
                             "20" = "L3"))) %>%
    mutate(type = case_when(
               variable %in% c("Voltage") ~ "Voltage",
               variable %in% c("T1", "T2", "T3") ~ "Temperature",
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
               variable %in% c("Voltage") ~ "Voltage",
               variable %in% c("T1", "T2", "T3") ~ "Temperature",
               variable %in% c("RH1", "RH2", "RH3") ~ "Humidity",
               variable %in% c("L1", "L2", "L3") ~ "Light",
               TRUE ~ "NTC" # Assumes all other variables fall under "NTC
           )) %>%
    # Ensure 'type' is a factor with levels in the correct order
    mutate(type = factor(type, levels = c("NTC", "Temperature", "Humidity", "Light", "Voltage")))

# Left join to ensure all combinations are represented in df_full
df_full <- left_join(all_combinations, df, by = c("node", "variable", "type"))

# Fill in NA for 'datetime' and 'value' where 'type' is present but 'datetime' and 'value' are missing
df_full <- df_full %>%
  mutate(datetime = if_else(is.na(datetime) & !is.na(type), as.POSIXct(NA), datetime),
         value = if_else(is.na(value) & !is.na(type), NA_real_, value))

# Plots
# Set output directory
setwd(folder)

# ggplot
g1 <- df_full %>%
  ggplot(aes(x = datetime, y = value, color = variable)) +
  geom_point(alpha = 0.7, size = 0.7, aes(text = paste(datetime, "<br>", variable, " = ", value, sep = ""))) +
  geom_line(alpha = 0.7, lwd = 0.2) +
  scale_color_viridis(end = 0.9, option = "C", discrete = TRUE) +
  facet_grid(type ~ node, scales = "free_y") +
  theme(axis.text.x = element_text(angle = -45, vjust = 0.5, hjust=-0, size = 7), legend.title = element_blank()) +
  xlab("") + ylab("")

# Convert to ggplotly and specify tooltip content
overview <- ggplotly(g1, tooltip = "text")
overview$x$layout$legend$title$text <- ""
overview

# Save the Plotly widget as an HTML file
saveWidget(overview, "overview.html", selfcontained = TRUE)

# Plotly
# Plotly subplots for type with node legends
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
      text = ~paste(datetime, "<br>", "Node", node, "<br>", variable, "=", value),
      hoverinfo = 'text',
      type = 'scatter',
      mode = 'lines+markers',
      height = 1500,
      legendgroup = ~node,
      showlegend = ~ifelse(t == "Temperature", TRUE, FALSE),
      line = list(width = 2, opacity = 0.7),
      marker = list(size = 5, opacity = 0.7),
#      colors = RColorBrewer::brewer.pal(8, "Set3")
      colors = c(wes_palette("GrandBudapest1"), wes_palette("GrandBudapest2"))
    ) %>%
    layout(
        xaxis = list(title = "",
                     showgrid = TRUE,
                     range = ~c(1000 * (as.numeric(ymd_hms(max(datetime))) - 86400),
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

dashboard <- subplot(plots, nrows = length(types), shareX = TRUE, shareY = FALSE) %>% layout(
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
                     args = list("mode", "lines+markers"),
                     label = "Lines + markers"),
                list(method = "restyle",
                     args = list("mode", "markers"),
                     label = "Markers"),
                list(method = "restyle",
                     args = list("mode", "lines"),
                     label = "Lines")))))
dashboard
saveWidget(dashboard, "dashboard.html", selfcontained = TRUE)

# Overview with type legends
summary <- df %>% group_by(type) %>%
    mutate(hour = hour(datetime)) %>%
    do(p = plot_ly(.,
    x = ~datetime, 
    y = ~value, 
    color = ~variable, 
#    frame = ~node,
    text = ~node,
    hoverinfo = 'text',
    type = 'scatter',
    mode = 'markers')) %>%
    subplot(nrows = 1, shareX = TRUE, shareY = FALSE)
summary
saveWidget(summary, "summary.html", selfcontained = TRUE)
