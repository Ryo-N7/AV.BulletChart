library(ggplot2)
library(dplyr)
library(lubridate)

## Inputs
forYear= 2018 ## Specify Year the analysis represents
FY=TRUE       ## Is this a fiscal year? (as opposed to calendar year)
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
if (nrow(df)==0){
  OutputPlot <- "No data yet"
  return()
}

## Protect against the case where there are no actuals:
if (is.null(length(df$Active))) df$Actual <- 0

## Take percents, but protecting against there being no targets
df$Perc <-   df$Actual/(df$Target + 0.0000000000001) * 100
df$PercWeek <- df$Actual_lastWeek/(df$Target + 0.0000000000001) * 100
df$PercYear <- df$Actual_lastYear/(df$Target + 0.0000000000001) * 100

## But truncate results if any are greater than 100
df$Perc[df$Perc>100] <- 100
df$PercWeek[df$PercWeek>100] <- 100
df$PercYear[df$PercYear>100] <- 100

#### Figure out today, within the Fiscal Year 
## If desired, force the project ot respect the startdate
if (FY==TRUE){
  ## Control for the possibility that project starts in a month other than October
  if(month(ProjectStartDate)!=10) {ProjectStartDate <-  as.Date(paste("01 10",year(ProjectStartDate)),format = "%d %m %Y")}
  ## Grab Percent of year
  PercentTime<- 
    as.numeric((Sys.Date() - as.Date(paste(format(as.Date(ProjectStartDate),"%d %b"),
                                           forYear-1),format = "%d %b %Y"))/365.25*100)
} else {
  ## Grab Percent of year
  PercentTime<- 
    as.numeric(Sys.Date() - as.Date(ProjectStartDate))/365.25*100
}

## Truncate Percent line if greater than 100 (for example, doing the analysis after year end)
if(PercentTime>100) PercentTime<- 100

## Calculate how far behind TODAY the Indicator Percent is
df$BehindBy <- df$Perc - PercentTime

## Calculate how far behind TODAY (VALUE) for text, but remove NAs
df <- df %>%
  mutate(text=PercentTime/100 * df$Target - df$Actual)
df$text[df$BehindBy > 0] <-"OK!"
df$text[df$BehindBy <= 0 & !is.na(df$BehindBy)] <- 
  paste("Need ",round(as.numeric(df$text[df$BehindBy <= 0  & !is.na(df$BehindBy)]))," more",sep="")


  
          ## Calculate behind from last week
          df$BehindFromLastWeek <- df$Actual - df$Actual_lastWeek
          
          ## Calculate how far behind LAST for text
          df$text2[df$BehindFromLastWeek > 0 & !is.na(df$BehindFromLastWeek)] <- paste("(+",round(as.numeric(df$BehindFromLastWeek[df$BehindFromLastWeek > 0 & !is.na(df$BehindFromLastWeek)]))," from last)",sep="")
          df$text2[df$BehindFromLastWeek < 0 & !is.na(df$BehindFromLastWeek)] <- paste("(",round(as.numeric(df$BehindFromLastWeek[df$BehindFromLastWeek < 0 & !is.na(df$BehindFromLastWeek)]))," from last)",sep="")
          
          ## Truncate BEHIND BY at 0, LowLevel, and Percent to 100
          #### Set "low level" here. Low level is the Value that we consider tolerable to be behind the indicators by.r
          LowLVL <- -.20 * PercentTime
          df$BehindBy[df$BehindBy>0] <- 0
          df$BehindBy[df$BehindBy<LowLVL] <- LowLVL


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
  theme_minimal() + theme(axis.text.y = element_text(size=15,face="bold"),
                          axis.title.x = element_text(face="bold", size=15),
                          title=element_text(face="bold",size=20))
