suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

data <- tribble(
  ~month, ~linear, ~scaled, ~distribution,
  "January", 0, 0, 0,
  "February", 8.3, 0, 0,
  "March", 16.6, 25, 0,
  "April", 24.9, 25, 0, 
  "May", 33.2, 25, 90,
  "June", 41.5, 25, 90,
  "July", 58.1, 25, 100,
  "August", 66.4, 25, 100,
  "September", 74.7, 25, 100,
  "October", 83, 25, 100,
  "November", 91.3, 100, 100,
  "December", 100, 100, 100
)


# x = month, y = value >>>> type (linear, scaled, distribution)      group = line! colrobrewer = ??? set manually to line type thingy

data %>% 
  tidyr::gather(key = type, value = progress, linear, scaled, distribution) %>% 
  mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", 
                                          "June", "July", "August", "September",
                                          "October", "November", "December"))) %>% 
  group_by(month) %>% 
  ggplot(aes(x = month, y = progress, group = type, color = type)) +
  geom_line(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Months", y = "Progress (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("label", x = "April", y = 75, label = "Distribution of supplies or funds") +
  annotate("label", x = "June", y = 50, label = "Expected") +
  annotate("label", x = "September", y = 20, label = "Scaled training or\n events tied to crop cycle")




# Bullet chart 1 ----

df <- df %>% tidyr::gather(key = time, value = progress, PercWeek, PercYear)


ggplot(df, aes(IndicatorName)) + 
  geom_col(aes(y = Perc, width = 0.1, fill = BehindBy), color = "black") + 
  scale_fill_gradient("Indicator\nBehind By:", limits = c(LowLVL, 0), low = "red", high = "green",
                      labels = c("On Time", "Slightly Behind", "Behind Schedule", ""),
                      guide = FALSE) +
  geom_point(aes(y = time, shape = time), size = 6, stroke = 1) +
  scale_shape_manual(" ", values = c(23, 21)) +
  geom_col(aes(y = 100, width = 0.5), alpha = 0.25) +
  geom_text(y = 1, aes(label = text), vjust = -1, hjust = 0) +
  geom_hline(yintercept = PercentTime, alpha = 0.33) +
  annotate("text", x = 0, y = PercentTime + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
  coord_flip() +  
  labs(y = "Percent of Yearly Target\n&\n Percent of Year",
       x = " ") +
  ggtitle(paste("Ongoing Indicator Accomplishment (", forYear, ")", sep = "")) +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.text.x = element_text(face = "bold", size = 14),
        title = element_text(face = "bold", size = 14),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        legend.position = c(0.8, -0.10)) +
  expand_limits(x = 6.75, y = 102)


