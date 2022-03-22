# This script is intended to calculate the number of growing degree days in the Boquet River
# Created by Jonah L. Withers
# Created on 4/23/2021

# Libraries ----
library(dplyr)
library(lubridate)
library(data.table)
library(RODBC)
library(ggplot2)

# 29 growing degree days (GDD) is when salmon are weakly eyed
# 47 GDD is strongly eyed
# 58-61 GDD is 90% hatched
# 100 GDD is first-feeding
# 104 GDD is swim up and first feed

# Identify the year of interest
YOI <- 2021



# Import data ----
# > Connect to access database ####

# Identify channel to connect to database
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=./Databases/HoboLogger_MetaData.accdb")

# Look at tables and queries within database
sqlTables(channel)

Deployments.0 <- as.data.table(sqlFetch(channel, "tbl Hobo Deployment")) # Import list of tags

# Close connection to database
odbcClose(channel)




# > list hobo files ####
df.files <- list.files(path = "./Downloads/BoquetRiver",
                       pattern = "*.csv")

# Create shell and populate with downloads in GMT
df.data <- NULL
for(i in 1:length(df.files)){
  
  x <- fread(paste("./Downloads/BoquetRiver/", df.files[i], sep = ""),
             skip = 1,
             stringsAsFactors = FALSE,
             fill = TRUE,
             col.names = c("Rownumber", "TempTime", "Temp", "Coupler Detached", "Coupler Attached",
                           "Host Connected", "Stopped", "End of File")) 
   
  x1 <- x %>%
    mutate(TempTime = as.POSIXct(TempTime,
                                 origin = "1970-01-01",
                                 format = "%m/%d/%Y %I:%M:%S %p",
                                 tz = "GMT"),
           # Logger = substr(names(x)[4], start = 28, 35),
           # Temp = x[,3],
           FileName = df.files[i]) %>%
    dplyr::select(Rownumber, TempTime, Temp, 
                  # Logger, 
                  FileName) %>% 
    mutate(TempTime = as.POSIXct(paste("20", substr(TempTime, start = 3, stop = 20), sep = ""),
                                 origin = "1970-01-01",
                                 format = "%Y-%m-%d %H:%M:%S",
                                  tz = "UTC"))
  
  df.data <- rbind(df.data, x1)
}



df.1 <- df.data %>%
  mutate(TempYear = lubridate::year(TempTime),
         Test = sub("\\..*", "", FileName)) %>% 
  left_join(Deployments.0, c("FileName" = "HoboID")) %>% 
  filter(TempTime > as.Date(paste(YOI - 1, "-11-15", sep = "")), # Extract data from mid-Nov of previous year to present
         TempTime > (as.Date(`Deployment DateTime`) + 1) & TempTime < (as.Date(`Retrieval DateTime`) - 1),
         TempTime < as.Date(paste(YOI, "-06-30", sep = "")),# Remove data collected when logger was not deployed
         SiteID %in% c("BQ_Temp1", "BQ_Temp2", "BQ_Temp3", "BQ_Temp4", "BQ_Temp5", "BQ_Temp6"))



myplot.0 <- df.1 %>%
  ggplot(aes(x = TempTime, y = Temp, color = SiteID)) +
  geom_line() +
  labs(x = "Datetime",
       y = "Temperate (C)") +
  scale_x_datetime(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()



ggsave(filename = paste("./Figures/BoquetRiver_NorthBranch_Temperatures_",
                        YOI, 
                        ".png", 
                        sep = ""), 
       plot = myplot.0,
       device = "png",
       dpi = 600)




# Calculate growing degree days ----
# > Mean Daily temp per site ####
df.1m <- df.1 %>%
  mutate(DOY = substr(TempTime, start = 1, stop = 10)) %>% 
  group_by(SiteID, DOY) %>% 
  summarize(MeanTempC = mean(Temp)) %>% 
  mutate(DI_Daily = 100 / (exp((6.003) * (exp(-0.0307 * MeanTempC))))) %>% 
  group_by(SiteID) %>% 
  mutate(GrowingDD = cumsum(DI_Daily))



# > Plot GDD ####
myplot.1 <- df.1m %>% 
  ggplot() +
  geom_line(aes(x = as.Date(DOY), y = GrowingDD, color = SiteID)) +
  labs(x = "Date",
       y = "Development Index") +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_hline(yintercept = c(29, 47, 60, 100), color = "red") +
  annotate("text", 
           x = as.Date(paste(YOI - 1, "-11-25", sep = "")), 
           y = c(31, 49, 62, 103), 
           label = c("Weakly Eyed", "Strongly Eyed", "90% Hatched", "SwimUp / First Feeding")) +
  theme_bw()



ggsave(filename = paste("./Figures/BoquetRiver_NorthBranch_GrowingDegreeDays_",
                        YOI, 
                        ".png", 
                        sep = ""), 
       plot = myplot.1,
       device = "png",
       dpi = 600)