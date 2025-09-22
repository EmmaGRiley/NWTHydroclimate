#Create GSL daily timeseries
library(tidyhydat)

# Define '%>%' operator in current environment
`%>%` <- magrittr::`%>%`

save_path <- getwd() #change this as desired

df1 <- tidyhydat::hy_daily(station_number = "07SB001")

df2 <- tidyhydat::realtime_ws(station_number = "07SB001",
                                 start_date = max(df1$Date) + 1)

df3 <- df2 %>%
  dplyr::filter(Parameter == 46) %>%
  dplyr::group_by(Date = as.Date(Date)) %>%
  dplyr::summarize(Value = mean(Value))

df <- dplyr::bind_rows(df1, df3) %>%
  dplyr::filter(Date > "1934-01-01") %>%
  dplyr::mutate(prctile = (ecdf(Value)(Value)) * 100,
                Max = max(Value, na.rm = TRUE),
                Min = min(Value, na.rm = TRUE),
                QP90 = quantile(Value, 0.90, na.rm = TRUE),
                QP75 = quantile(Value, 0.75, na.rm = TRUE),
                QP50 = quantile(Value, 0.50, na.rm = TRUE),
                QP25 = quantile(Value, 0.25, na.rm = TRUE),
                QP10 = quantile(Value, 0.10, na.rm = TRUE))

plot <- ggplot2::ggplot(data = df, ggplot2::aes(x = Date, y = Value)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = QP25, ymax = QP75, fill = "IQR"), alpha = 0.5) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = QP10, ymax = QP90, fill = "10-90"), alpha = 0.3) +
  ggplot2::geom_line() +
  ggplot2::scale_fill_manual(name = "",
                             values = c("IQR" = "gray50",
                                        "10-90" = "gray75")) +
  ggplot2::theme_classic() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::labs(x = "Year",
                y = "Water Level (m)")

plot

ggplot2::ggsave(paste0("GSL_Timeseries", Sys.Date(), ".png"), plot = plot, device = "png",
                path = save_path,
                scale = 1, width = 22, height = 10, units = c("cm"), dpi = 900)
