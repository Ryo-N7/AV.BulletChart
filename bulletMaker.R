library(ggplot2)
library(dplyr)
library(lubridate)

## Inputs ----
forYear = 2018 ## Specify Year the analysis represents
FY = TRUE       ## Is this a fiscal year? (as opposed to calendar year)
ProjectStartDate <- "2016/03/01"

df <- structure(list(IndicatorName = c("Ind 04", "Ind 05", "Ind 07", 
                                       "Ind 11", "Ind 17", "Ind 18"), 
                     Actual = c(3, 437, 20, 44, 1,10000), 
                     Actual_lastWeek = c(3, 420, 18, 20, 1, 10000), 
                     Actual_lastYear = c(3, 50, 20, 2000, 1, 10000), 
                     Target = c(14, 81, 21, 10327, 5, 20000)), 
                row.names = c(NA, -6L), class = "data.frame", 
                .Names = c("IndicatorName", "Actual", "Actual_lastWeek", "Actual_lastYear", "Target"))

## If df is empty, break function and just output an empty chart
if (nrow(df) == 0){
  OutputPlot <- "No data yet"
  return()
}

## Protect against the case where there are no actuals:
if (is.null(length(df$Active))) df$Actual <- 0

## Take percents, but protecting against there being no targets
df$Perc <-     df$Actual/(df$Target + 0.0000000000001) * 100
df$PercWeek <- df$Actual_lastWeek/(df$Target + 0.0000000000001) * 100
df$PercYear <- df$Actual_lastYear/(df$Target + 0.0000000000001) * 100

## But truncate results if any are greater than 100
df$Perc[df$Perc > 100] <- 100
df$PercWeek[df$PercWeek > 100] <- 100
df$PercYear[df$PercYear > 100] <- 100

#### Figure out today, within the Fiscal Year 
## If desired, force the project ot respect the startdate
if (FY == TRUE){
  ## Control for the possibility that project starts in a month other than October
  if(month(ProjectStartDate) != 10) {ProjectStartDate <-  as.Date(paste("01 10", year(ProjectStartDate)), format = "%d %m %Y")}
  ## Grab Percent of year
  PercentTime<- 
    as.numeric((Sys.Date() - as.Date(paste(format(as.Date(ProjectStartDate), "%d %b"),
                                           forYear - 1), format = "%d %b %Y"))/365.25*100)
} else {
  ## Grab Percent of year
  PercentTime<- 
    as.numeric(Sys.Date() - as.Date(ProjectStartDate))/365.25*100
}

## Truncate Percent line if greater than 100 (for example, doing the analysis after year end)
if(PercentTime > 100) PercentTime <- 100

## Calculate how far behind TODAY the Indicator Percent is
df$BehindBy <- df$Perc - PercentTime

## Calculate how far behind TODAY (VALUE) for text, but remove NAs
df <- df %>%
  mutate(text = PercentTime/100 * df$Target - df$Actual)
df$text[df$BehindBy > 0] <-"OK!"
df$text[df$BehindBy <= 0 & !is.na(df$BehindBy)] <- 
  paste("Need ", round(as.numeric(df$text[df$BehindBy <= 0  & !is.na(df$BehindBy)])), " more", sep = "")


  
          ## Calculate behind from last week
          df$BehindFromLastWeek <- df$Actual - df$Actual_lastWeek
          
          ## Calculate how far behind LAST for text
          df$text2[df$BehindFromLastWeek > 0 & !is.na(df$BehindFromLastWeek)] <- paste("(+", round(as.numeric(df$BehindFromLastWeek[df$BehindFromLastWeek > 0 & !is.na(df$BehindFromLastWeek)])), " from last week)", sep = "")
          df$text2[df$BehindFromLastWeek < 0 & !is.na(df$BehindFromLastWeek)] <- paste("(",round(as.numeric(df$BehindFromLastWeek[df$BehindFromLastWeek < 0 & !is.na(df$BehindFromLastWeek)])), " from last week)", sep = "")
          
          ## Truncate BEHIND BY at 0, LowLevel, and Percent to 100
          #### Set "low level" here. Low level is the Value that we consider tolerable to be behind the indicators by.r
          LowLVL <- -.20 * PercentTime
          df$BehindBy[df$BehindBy > 0] <- 0
          df$BehindBy[df$BehindBy < LowLVL] <- LowLVL

# 1. Shapes, no bars: ----
          
ggplot(df, aes(IndicatorName)) + 
  geom_col(aes(y = Perc, width = 0.1, fill = BehindBy), color = "black") + 
  scale_fill_gradient("Indicator\nBehind By:", limits = c(LowLVL, 0), low = "red", high = "green",
                      labels = c("On Time", "Slightly Behind", "Behind Schedule"),
                      breaks = c(-1, -3, -6)) +
  geom_point(aes(y = PercWeek, shape = "Last Week"), size = 6, stroke = 2) +
  geom_point(aes(y = PercYear, shape = "Last Year"), size = 6, stroke = 2.5) +         
  scale_shape_manual(" ", values = c(23, 21)) +
  geom_text(y = 1, aes(label = text2), vjust = 2, hjust = 0) +
  geom_col(aes(y = 100, width = 0.5), alpha = 0.25) +
  geom_text(y = 1, aes(label = text), vjust = -1.5, hjust = 0) +
  geom_hline(yintercept = PercentTime, alpha = 0.33) +
  annotate("text", x = 0, y = PercentTime + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
  coord_flip() +  
  labs(y = "Percent of Yearly Target\n&\n Percent of Year",
       x = " ") +
  ggtitle(paste("Ongoing Indicator Accomplishment (", forYear, ")", sep = "")) +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 20),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        legend.text = element_text(size = 16), 
        legend.key.size = unit(2, "lines"),
        legend.key = element_rect(size = 1, color = "white"))
  
  #guide_colorbar(title = "Ind Behind By:")
  #guides(fill = guide_legend(title = "Ind Behind By:"))
## Ind 18 is at 50% of its yearly target at "Today" == ~35-40% of the Year. Same % as last week and same % as @ "Today" in Last Year.





# 2. Triangle + Small Line bar: ----

ggplot(df, aes(IndicatorName)) + 
  geom_col(aes(y = Perc, width = 0.15, fill = BehindBy), color = "black") + 
  scale_fill_gradient("Indicator\nBehind By:", limits = c(LowLVL, 0), low = "red", high = "green",
                      labels = c("On Time", "Slightly Behind", "Behind Schedule", "??"),
                      breaks = c(0, -2, -4, -6)) +
  geom_point(aes(y = PercWeek, shape = "Last Week"), size = 3.5, stroke = 1.5) +
  geom_point(aes(y = PercYear, shape = "Last Year"), size = 7, stroke = 3) +         
  scale_shape_manual(" ", values = c(6, 124)) +
  geom_text(y = 1, aes(label = text2), vjust = 2, hjust = 0) +
  geom_col(aes(y = 100, width = 0.5), alpha = 0.25) +
  geom_text(y = 1, aes(label = text), vjust = -1.5, hjust = 0) +
  geom_hline(yintercept = PercentTime, alpha = 0.33) +
  annotate("text", x = 0, y = PercentTime + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
  coord_flip() +  
  labs(y = "Percent of Yearly Target\n&\n Percent of Year",
       x = " ") +
  ggtitle(paste("Ongoing Indicator Accomplishment (", forYear, ")", sep = "")) +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 20),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        legend.text = element_text(size = 16),
        legend.key.height = unit(2.5, "lines"))

# 2b. categorical color bars: ----

df_2 <- df %>% 
  mutate(schedule = if_else(BehindBy %in% c(0, -2), "On Time", 
                            if_else(BehindBy %in% c(-2.1, -4), "Slightly Behind",
                                    if_else(BehindBy %in% c(-4.1, -6), "Behind Schedule", "Late"))))
df_2 %>% glimpse()  
          
          
## not showing up if not in "schedule" var...          
          
ggplot(df_2, aes(IndicatorName)) +
  geom_col(aes(y = Perc, width = 0.15, fill = schedule), color = "black") +
  scale_fill_manual(
    "Indicator\nBehind By:",
    limits = c(LowLVL, 0),
    values = c(
      "On Time" = "green",
      "Slightly Behind" = "yellow",
      "Behind Schedule" = "orange",
      "Late" = "red"),
    labels = c("On Time", "Slightly Behind", "Behind Schedule", "Late")
    ) +
  geom_point(aes(y = PercWeek, shape = "Last Week"),
             size = 3.5,
             stroke = 1.5) +
  geom_point(aes(y = PercYear, shape = "Last Year"),
             size = 7,
             stroke = 3) +
  scale_shape_manual(" ", values = c(6, 124)) +
  geom_text(y = 1,
            aes(label = text2),
            vjust = 2,
            hjust = 0) +
  geom_col(aes(y = 100, width = 0.5), alpha = 0.25) +
  geom_text(y = 1,
            aes(label = text),
            vjust = -1.5,
            hjust = 0) +
  geom_hline(yintercept = PercentTime, alpha = 0.33) +
  annotate(
    "text",
    x = 0,
    y = PercentTime + 1.5,
    hjust = 0,
    label = "Today",
    angle = 90,
    alpha = 0.5,
    size = 5
  ) +
  coord_flip() +
  labs(y = "Percent of Yearly Target\n&\n Percent of Year",
       x = " ") +
  ggtitle(paste("Ongoing Indicator Accomplishment (", forYear, ")", sep = "")) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "bold", size = 12),
    title = element_text(face = "bold", size = 20),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    legend.text = element_text(size = 16),
    legend.key.height = unit(2.5, "lines")
  )
          




# 3. multiple bars: ----
## instead of messing with the alpha, just set to different grey shades?

ggplot(df, aes(IndicatorName)) + 
  geom_col(aes(y = PercWeek, width = 0.5), alpha = 0.6) +
  geom_col(aes(y = PercYear, width = 0.75), alpha = 0.3) +
  geom_col(aes(y = Perc, width = 0.15, fill = BehindBy), color = "black") + 
  scale_fill_gradient("Indicator\nBehind By:", limits = c(LowLVL, 0), low = "red", high = "green",
                      labels = c("On Time", "Slightly Behind", "Behind Schedule", "??"),
                      breaks = c(0, -2, -4, -6)) +
  geom_label(y = 1, aes(label = text2), vjust = 2, hjust = 0, size = 4) +
  geom_label(y = 1, aes(label = text), vjust = -1.5, hjust = 0, size = 4) +
  geom_hline(yintercept = PercentTime, alpha = 0.33) +
  annotate("text", x = 0, y = PercentTime + 1.5, hjust = 0, label = "Today", angle = 90, alpha = 0.5, size = 5) +
  coord_flip() +
  labs(y = "Percent of Yearly Target\n&\n Percent of Year",
       x = " ") +
  ggtitle(paste("Ongoing Indicator Accomplishment (", forYear, ")", sep = "")) +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.text.x = element_text(face = "bold", size = 12),
        title = element_text(face = "bold", size = 20),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 8),
        legend.text = element_text(size = 16),
        legend.key.height = unit(2.5, "lines")) +
  geom_point(aes(y = PercWeek, shape = "Last Week"), size = 3, stroke = 1) +
  geom_point(aes(y = PercYear, shape = "Last Year"), size = 3, stroke = 1) +
  scale_shape_manual(" ", values = c(6, 21))


# ORIGINAL: ----
          
ggplot(df, aes(IndicatorName) ) + 
       geom_bar(aes(y=Perc,width=0.15,fill=BehindBy),stat = "identity")  + 
       scale_fill_gradient(limits = c(LowLVL,0),low="red", high="green") +
       geom_point(aes(y=PercWeek),shape=6,size=5) +
       geom_text( y=1,aes(label = text2),vjust=2,hjust = 0) +
       geom_bar(aes(y=100,width=0.5),alpha=0.25,stat = "identity") +
       geom_text( y=1,aes(label = text),vjust=-1.5,hjust = 0) +
       geom_hline(yintercept = PercentTime,alpha=0.33) +
       annotate("text", x = 0, y = PercentTime + 1.5,hjust=0, label = "Today",angle=90,alpha=0.33) +
       coord_flip() +  
       ylab("Percent of yearly target & Percent of year") +   xlab(" ") +
       ggtitle(paste("Ongoing Indicator accomplishment (",forYear,")",sep=""))+
       theme_minimal() + 
       theme(axis.text.y = element_text(size=15,face="bold"),
                                    axis.title.x = element_text(face="bold", size=15),
                                    title=element_text(face="bold",size=20))          
    




# example plots: ----

library(dplyr)
library(ggplot2)
library(viridis)


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


seq(0, 100, 8.3)
glimpse(data)

data %>%
  mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", 
                                          "June", "July", "August", "September",
                                          "October", "November", "December"))) %>% 
  group_by(month) %>% 
  ggplot(aes(x = month)) +
  geom_line(aes(y = linear, color = "linear", group = 1), size = 2) +
  geom_line(aes(y = scaled, color = "scaled", group = 1), size = 2) +
  geom_line(aes(y = distribution, color = "distribution", group = 1), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Months", y = "Progress (%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  annotate("label", x = "April", y = 75, label = "Distribution of supplies or funds") +
  annotate("label", x = "June", y = 50, label = "Expected") +
  annotate("label", x = "September", y = 20, label = "Scaled training or\n events tied to crop cycle")
  



