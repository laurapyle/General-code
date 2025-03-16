library(tidyverse)
library(lubridate)
library(hms)
files <- list.files("/Users/pylell/Library/CloudStorage/OneDrive-TheUniversityofColoradoDenver/Vigers/CF/Christine Chan/EnVision CF/Data_Raw/FilesReport_CGM_2025-01-30_1306/FilesReport_CGM_2025-01-30_1306/documents", full.names = T)

lapply(files, function(f) {
  # ID
  id <- basename(f)
  id <- sub("\\.csv|\\.txt", "", id)
  # Clean CGM
  cgm <- read.csv(f, na.strings = "")
  if (ncol(cgm) == 19) {
    cgm <- cgm[-c(1:2), c(3, 5)]
    colnames(cgm) <- c("timestamp", "Glucose")
    cgm$timestamp <- mdy_hm(cgm$timestamp)
  }
  cgm$Date <- date(cgm$timestamp)
  cgm$Time <- as_hms(cgm$timestamp)
  cgm$Glucose <- as.numeric(cgm$Glucose)
  cgm <- cgm %>% select(Date, Time, Glucose)
  # Plot
  p <- ggplot(cgm, aes(x = Time, y = Glucose, group = factor(Date), color = factor(Date))) +
    ylim(40, 400) +
    annotate("rect",
      xmin = -Inf, xmax = Inf, fill = "lightblue",
      ymin = 70, ymax = 180, alpha = .5
    ) +
    geom_line() +
    ggtitle(id) +
    theme_bw() +
    scale_color_discrete("Date")
  # Save
  ggsave(paste0("/Users/pylell/Library/CloudStorage/OneDrive-SharedLibraries-UW/Talks/CFRD 2025/AGPs/", id, ".png"),
    plot = p, width = 9, height = 6, units = "in", device = "png"
  )
})
