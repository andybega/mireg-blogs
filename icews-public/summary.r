#   Download and summrize the public ICEWS data
#   Andreas Beger
#   6 April 2015
#
#   You can get the public data from http://thedata.harvard.edu/dvn/dv/icews
#   This script uses the files in ICEWS Coded Event Data

library(countrycode)
library(cshapes)
library(RColorBrewer)
library(shape)
library(dplyr)
library(ggplot2)
library(magrittr)
library(WDI)
library(zoo)
library(sqldf)

#   Load ICEWS event data to SQLite. See `icews-to-sql.r` for more details.

#   Set up a SQLite connection. The `dbname` argument will create a .sqlite
#   file in the `data` directory, if it doesn't exist. 
con <- dbConnect(SQLite(), dbname = "data/icews.sqlite")

# List of zip files containing the data. Change the directory below if you
# downloaded to another loccation.
icews_dir <- "data/study_28075/Data"
zip_lst <- file.path(icews_dir, list.files(icews_dir))
zip_lst <- zip_lst[grep(".zip", zip_lst)]  # make sure only .zip files

# Loop through the zip files and: unzip, read the .tab file, push it to 
# sqlite, adn delete the .tab file to save space. 
for (filename in zip_lst) {
  # Print progress
  cat(substr(regmatches(filename, regexpr("\\.[0-9]{4}", filename)), 2, 5), "\n")
  
  # Unzip tab file, read and add to db, then delete again to save space
  unzip(filename, exdir = icews_dir, overwrite=T)
  tabfile <- gsub(".zip", "", filename)
  tmp <- read.delim(tabfile, sep="\t", quote="")
  # By default, spaces in column names become periods, this doesn't play well
  # with SQL
  colnames(tmp) <- gsub("\\.", "_", colnames(tmp))
  file.remove(tabfile)
  dbWriteTable(con, name = "public", value = tmp, append = TRUE)
}


# Plot daily event totals -------------------------------------------------


# SQL query to extract count of daily events
sql <- "
SELECT Event_Date AS date, count(*) AS events
FROM   public
GROUP  BY Event_Date;"
daily <- dbGetQuery(con, sql)
daily$date <- as.Date(daily$date)

daily$m90 <- rollmean(daily[, "events"], k=90, fill=NA, align = "center")

p <- ggplot(daily) + 
  geom_point(aes(x = date, y = events), alpha = 0.2) +
  geom_line(aes(x = date, y = m90, colour = "90-day MA"), size = 2) +
  theme_bw() +
  labs(x = "", y = "", title = "Events per day in the public ICEWS event data") +
  theme(legend.title=element_blank())

ggsave("graphics/icews-daily.png", plot=p)



# Map country totals ------------------------------------------------------
#
#   This will create a thematic world map showing per capita event totals by
#   country using 2013 WDI population figures and totals from the public
#   ICEWS event data.
#

# SQL query to extract country totals
sql <- "
SELECT Country AS country, count(*) AS events
FROM   public
GROUP  BY Country;"
cntry <- dbGetQuery(con, sql)

# Fix non-latin character
cntry$country[cntry$country=="Curaçao"] <- "Curacao"

# Add ISO 2 letter country codes
cntry$iso2c <- countrycode(cntry$country, "country.name", "iso2c")
cntry[is.na(cntry$iso2c), "country"]
cntry$iso2c[cntry$country=="Kosovo"] <- "XK"

# Get World Bank WDI basic structural data
ind <- c("NY.GDP.MKTP.PP.KD", "SP.POP.TOTL", "IT.CEL.SETS", "IT.NET.BBND")
wdi <- WDI(indicator = ind, start = 2013, end = 2013)

# Rescale pop to millions
wdi$SP.POP.TOTL <- wdi$SP.POP.TOTL / 1e+6

# The code from here on below is for creating a world map with the contry 
# totals, per capita, using 2013 WDI population data. 

# Country map data
world <- cshp(date=as.Date("2012-06-30"))
world@data$ISO1AL2 <- as.character(world@data$ISO1AL2)

# Check mismatches in ID codes between WDI and cshp:
fix <- wdi$iso2c[!wdi$iso2c %in% world@data$ISO1AL2]
countrycode(fix, "iso2c", "country.name")

# Add WDI and event counts to country datas
world@data <- left_join(world@data, wdi, by = c("ISO1AL2" = "iso2c"))
world@data <- left_join(world@data, cntry, by = c("ISO1AL2" = "iso2c"))

# Add log of per capita event counts
world@data %<>%
  mutate(log10.events.pcap = log10(events / SP.POP.TOTL))

# plot on world map

# Pick colors
colorpal <- brewer.pal(9, 'RdBu')
colorpal <- colorRampPalette(colorpal)

# Rescale y for color selection
y <- world@data[, c("log10.events.pcap")]
miny <- min(y, na.rm=T)
maxy <- max(y, na.rm=T)
y <- y - miny
y <- round(y * (49/(maxy - miny)) + 1)
colors <- vector("character", length=length(y))
colors <- ifelse(is.na(y), '#B0B0B0', rev(colorpal(50))[y])

# Plot map
png("graphics/icews-map.png", height=800, width=1600, pointsize=25)
par(mar=c(1, 4, 3, 2))
plot(world, col='gray30', border='gray30', lwd=1)
plot(world, col=colors, border=F, add=T)
title(main = "ICEWS events from 1995 to 2014 per million")

# Add legend
breaks <- c(miny, (maxy+miny)/2, maxy)
leg.title <- expression(paste(log[10], " events / mil."))
colorlegend(posy=c(0.1, 0.45), posx=c(0.12, 0.14),
            col=rev(colorpal(50)), zlim=c(miny, maxy), zval=breaks,
            main=leg.title, main.cex=1, cex=1)
dev.off()

# Histogram of raw events per capita, not logged
df <- world@data
df$events.pcap <- with(df, events / SP.POP.TOTL)

p <- ggplot(df) + geom_histogram(aes(x = events.pcap)) +
  theme_bw() + 
  labs(x="", y="", title="Events per million by country")

ggsave(plot=p, file="graphics/events-pcap-hist.png")


# Linear model of log10 events --------------------------------------------
#
#   Some quick linear modelling of country event totals, using some variables
#   from WDI.

# Subset data and rescale
df <- world@data
df$area <- df$AREA / 1000  # area in 1k sq km
df$IT.NET.BBND <- df$IT.NET.BBND / 1e+6  # internet sub in mil
df$IT.CEL.SETS <- df$IT.CEL.SETS / 1e+6  # cell sub in mil

# Just population and GDP
m1 <- lm(
  log10(events) ~ log10(SP.POP.TOTL) + log10(NY.GDP.MKTP.PP.KD),
  data=df)

# Adds area
m2 <- lm(
  log10(events) ~ log10(SP.POP.TOTL) + log10(NY.GDP.MKTP.PP.KD) +
    area,
  data=df)

# Adds cell and broadband subscribers
m3 <- lm(
  log10(events) ~ log10(SP.POP.TOTL) + log10(NY.GDP.MKTP.PP.KD) +
    log10(IT.NET.BBND+1) + log10(IT.CEL.SETS),
  data=df)

# Summarize models
summary(m1)
summary(m2)
summary(m3)


# Done;
# Close DB connection
dbDisconnect(con)



