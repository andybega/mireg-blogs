library(plotly)
library(dplyr)
library(magrittr)
library(WDI)
library(countrycode)
library(lubridate)
library(scales)

# Function to format countrycode country names
source("prettyc.r")

# Setup to write your own plots to plotly, otherwise you can still
# get the 2d ggplot2 plots here
py <- plotly(username = "", key = "")
 
WDIsearch("GDP")[98, 2]


# GDP per capita ------------------------------------------------------
#
#   Compare GDP to a smilar plot of event data, using monthly ILC forecast
#   data from our Research and Politics article.
#

df <- WDI(indicator = "NY.GDP.PCAP.PP.KD", start = 1990, end = 2013)

df %<>% 
  mutate(cowcode = countrycode(df$iso2c, "iso2c", "cowc")) %>%
  filter(!is.na(cowcode))

df_no_na <- df %>%
  group_by(cowcode) %>%
  mutate(no_na = sum(is.na(NY.GDP.PCAP.PP.KD))) %>%
  filter(no_na==0)

# Sort countries by GDP pc in 2013
df_no_na$country <- with(df_no_na, factor(country, levels=unique(country)[order(NY.GDP.PCAP.PP.KD[year==2013])]))


# Raw GDP per capita
p <- ggplot(df_no_na, aes(x = year, y = country)) + 
  geom_tile(aes(fill = NY.GDP.PCAP.PP.KD)) +
  scale_fill_gradient2("GDP per capita, PPP",
                       midpoint = mean(df_no_na$NY.GDP.PCAP.PP.KD, na.rm=TRUE))

py_api <- py$ggplotly(
  p, 
  kwargs=list(filename="gdp-per-capita", fileopt="overwrite")
  )
py_api$response$url

# Logged
p <- ggplot(df_no_na, aes(x = year, y = country)) + 
  geom_tile(aes(fill = log(NY.GDP.PCAP.PP.KD))) +
  scale_fill_gradient2("GDP per capita, PPP",
                       midpoint = mean(log(df_no_na$NY.GDP.PCAP.PP.KD), na.rm=TRUE))

py_api <- py$ggplotly(
  p, 
  kwargs=list(filename="gdp-per-capita-logged", fileopt="overwrite")
  )
py_api$response$url

# Event data plot ---------------------------------------------------------
#
#   Compare GDP to a smilar plot of event data, using monthly ILC forecast
#   data from our Research and Politics article.
#

#events <- filter(ilc_data, date >= "1995-01-01" & date <= "2013-12-31")
#events <- select(events, gwcode, country, date, protest.tGOV.l1, )
#save(events, file="~/Desktop/events.rda")

load("events.rda")

gdp <- WDI(indicator = "NY.GDP.PCAP.PP.KD", start = 1995, end = 2013)
gdp %<>% 
  mutate(cowcode = countrycode(gdp$iso2c, "iso2c", "cown")) %>%
  filter(!is.na(cowcode))

# COW -> GW; partial
gdp$gwcode <- gdp$cowcode
gdp$gwcode[gdp$gwcode==255] <- 260  # Germany
gdp$gwcode[gdp$gwcode==679] <- 678  # Yemen

# Otherwise duplicate column in events
gdp %<>% dplyr::select(-country)

events$year <- year(events$date)
events <- left_join(events, gdp, by = c("gwcode", "year"))

source("prettyc.r")  # format countrynames to be shorter
events_no_na <- events %>%
  group_by(gwcode) %>%
  mutate(no_na = sum(is.na(protest.tGOV.l1) | is.na(NY.GDP.PCAP.PP.KD)),
         country = prettyc(country))

# Check which are missing
events_no_na %>% 
  group_by(country) %>% 
  summarise(no_na = max(no_na)) %>% 
  filter(no_na > 0) %>%
  as.data.frame

events_no_na %<>%
  filter(no_na==0)

# Sort countries by GDP pc in 2013
events_no_na$country <- with(
  events_no_na, 
  factor(country, 
         levels=unique(country)[order(NY.GDP.PCAP.PP.KD[date=="2013-12-01"])]))

p <- ggplot(events_no_na, aes(x = date, y = country)) + 
  geom_tile(aes(fill = log(protest.tGOV.l1 + 1))) +
  scale_fill_gradient("ln Protests,\nanti-Gov", 
                      high=muted("blue"), low="white",
                      breaks = log(c(1, 11, 101, max(events_no_na$protest.tGOV.l1))),
                      labels = c(0, 10, 100, max(events_no_na$protest.tGOV.l1)-1)) +
  labs(x = "Date (year-month)", y = "Country")

py_api <- py$ggplotly(
  p, 
  kwargs=list(filename="anti-gov-protests", fileopt="overwrite")
)
py_api$response$url
