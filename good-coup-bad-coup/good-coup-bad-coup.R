# Add Polity regime info to Powell and Thyne coup list to see how regimes
# change after coups.
#
# May 2015
#
# see also 
#   Thyne & Powell, 2014, "Coup d'etat or coup d'autocracy", Foreign Policy 
#     Analysis
#   http://www.uky.edu/~clthyn2/thyne_powell_FPA2013.pdf


library(dplyr)
library(gdata)
library(Gmisc)
library(lubridate)
library(tidyr)
library(magrittr)
library(ggplot2)
library(grid)
library(scales)
library(xtable)

setwd("~/Work/mireg-blogs/good-coup-bad-coup")

# graphics are saved to graphics folder
if (!dir.exists("graphics")) dir.create("graphics")


# Create data -------------------------------------------------------------
#
#   Combine Powell and Thyne coup data with Polity regime scores
#

url <- "http://www.uky.edu/~clthyn2/coup_data/powell_thyne_coups_final.txt"
pt <- read.delim(url, header = TRUE)
# or
#pt <- read.delim("powell_thyne_coups_final.txt", header=TRUE)

pt$date <- with(pt, paste(year, month, day, sep = "-"))
pt$date <- as.Date(pt$date)

p4d <- read.xls("p4v2014d.xls", header = TRUE)

p4d$start <- with(p4d, paste(byear, bmonth, bday, sep = "-"))
p4d$start <- as.Date(p4d$start)

p4d$end <- with(p4d, paste(eyear, emonth, eday, sep = "-"))
p4d$end <- as.Date(p4d$end) 

# Correct missing end dates to 2014
p4d$end[is.na(p4d$end)] <- "2014-12-31"

# Fix some country code mismatches
p4d$ccode[p4d$ccode==364] <- 365  # P4 codes USSR as 364, Russia as 365
p4d$ccode[p4d$ccode==626] <- 625  # post-seccession Sudan
p4d$ccode[p4d$ccode==769] <- 770  # Pakistan pre/post civil war

# Initialize vectors for Polity score 7 days before, and 1 and 2 years after a 
# coup
pt$polity_m7d <- pt$polity_p1y <- pt$polity_p2y <- NA

# Fill in Polity values
for (i in 1:nrow(pt)) {
  # Create vectors of the lag and lead dates after coup
  date_m7d <- pt$date - 7
  date_p1y <- pt$date
  year(date_p1y) <-  year(date_p1y) + 1
  date_p2y <- pt$date
  year(date_p2y) <-  year(date_p2y) + 2
  
  # Fill in values from P4 matches
  pt$polity_m7d[i] <- p4d %>%
    filter(start <= date_m7d[i] & date_m7d[i] <= end & ccode==pt$ccode[i]) %>%
    select(polity) %>% as.integer()
  
  pt$polity_p1y[i] <- p4d %>%
    filter(start <= date_p1y[i] & date_p1y[i] <= end & ccode==pt$ccode[i]) %>%
    select(polity) %>% as.integer()
  
  pt$polity_p2y[i] <- p4d %>%
    filter(start <= date_p2y[i] & date_p2y[i] <= end & ccode==pt$ccode[i]) %>%
    select(polity) %>% as.integer()
}

# What is still missing?
# Coup within 1/2 years of P4d data end date will be missing
# Small countries missing in P4
pt[is.na(pt$polity_m7d), c("country", "ccode", "year", "date", "coup")]
pt[is.na(pt$polity_p1y), c("country", "ccode", "year", "date", "coup")]

# Translate Polity scale to Polity regime categories
reg_lbl <- c("Unstable", "Autocracy", "Closed Anocracy", "Open Anocracy", 
             "Democracy", "Full Democracy")

# m7d = minus 7 days from coup
# p1y = plus 1 year from coup, etc.
pt$regime_m7d <- cut(pt$polity_m7d, breaks=c(-100, -11, -6, 0, 5, 9, 10), labels=reg_lbl)
pt$regime_p1y <- cut(pt$polity_p1y, breaks=c(-100, -11, -6, 0, 5, 9, 10), labels=reg_lbl)
pt$regime_p2y <- cut(pt$polity_p2y, breaks=c(-100, -11, -6, 0, 5, 9, 10), labels=reg_lbl)


# Rough region coding, just need if Sub-Saharan Africa or not
pt$region <- ifelse(pt$ccode %in% c(400:591, 625, 626), "Sub-Saharan Africa", "other")

pt$change_1y <- NA
pt$change_1y <- with(pt, ifelse(as.numeric(regime_m7d) > as.numeric(regime_p1y), "Worse", "Better"))
pt$change_1y <- with(pt, ifelse(regime_m7d==regime_p1y, "Same", change_1y))
pt$change_1y <- factor(pt$change_1y, levels = c("Worse", "Same", "Better"))

# Better/worse/same coding
pt$change_2y <- NA
pt$change_2y <- with(pt, ifelse(as.numeric(regime_m7d) > as.numeric(regime_p2y), "Worse", "Better"))
pt$change_2y <- with(pt, ifelse(regime_m7d==regime_p2y, "Same", change_2y))
pt$change_2y <- factor(pt$change_2y, levels = c("Worse", "Same", "Better"))

# Bin change for plot labels
pt$change_2y_bin <- with(pt, ifelse(change_2y=="Better", 
                                    "More democratic", 
                                    "More authoritarian/\nsame/still in transition"))
pt$change_2y_bin <- with(pt, ifelse(is.na(change_2y), "No data", change_2y_bin))


# Transition matrix and plotting ------------------------------------------


# Tabulate regime 7 days before against 1 and 2 years after
trans_1y <- with(filter(pt, coup==2), table(regime_m7d, regime_p1y))
trans_1y

trans_2y <- with(filter(pt, coup==2), table(regime_m7d, regime_p2y))
trans_2y


# Transition plot from the Gmisc package. 
# http://www.r-bloggers.com/visualizing-transitions-with-the-transitionplot-function/
png("graphics/trans-plot-1-year.png", width = 1024, height = 1024)
transitionPlot(trans_1y,
               type_of_arrow = "simple",
               txt_start_clr = "black", fill_start_box = "white",
               txt_end_clr = "black", fill_end_box = "white",
               new_page = T)
dev.off()

png("graphics/trans-plot-2-year.png", width = 1024, height = 1024)
transitionPlot(trans_2y,
               type_of_arrow = "simple",
               txt_start_clr = "black", fill_start_box = "white",
               txt_end_clr = "black", fill_end_box = "white",
               new_page = T)
dev.off()

#
#   Plot transitions for all countries, from 1950 on
#   _________________________________________________

# Table for all country 2-year Polity score changes
trans_mat <- with(
  filter(pt, coup==2), 
  table(regime_m7d, regime_p2y)
)

trans_mat %<>% as.data.frame()

# Code whether change was to better, worse, or same regime
trans_mat$change <- NA
trans_mat$change <- with(trans_mat, ifelse(as.numeric(regime_m7d) > as.numeric(regime_p2y), "Worse", "Better"))
trans_mat$change <- with(trans_mat, ifelse(regime_m7d==regime_p2y, "Same", change))
trans_mat$change <- factor(trans_mat$change, levels = c("Worse", "Same", "Better"))

# Cut out 0's for text labels
trans_mat$text_lbl <- with(trans_mat, ifelse(Freq > 0, Freq, ""))

# How many are worse, same, better?
trans_mat %>% group_by(change) %>% summarize(sum(Freq))

# Plot transition matrix
p <- ggplot(trans_mat, aes(regime_p2y, regime_m7d, fill = change)) + 
  geom_tile() + 
  geom_text(aes(label = text_lbl), color = "#073642", size = 3) +
  scale_fill_brewer(name="", 
                    type = "diverging",
                    palette = "RdBu") +
  labs(x = "2 years after",
       y = "7 days before") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.6)),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, vjust=2, size = rel(1.1)),
        legend.text = element_text(size = rel(0.8)),
        legend.key.size = unit(0.15, "in")) +
  ggtitle("Regime transition after a successfull coup")

ggsave(filename="graphics/2-year-transitions.png", plot=p, width=5.2, height=3.8, 
       units="in", dpi=300, scale = 0.8)

#
#   Plot transitions for Sub-Saharan Africa, from 1990
#   _________________________________________________

# Transition matrix for SSA post 1990 only
trans_mat <- with(
  filter(pt, coup==2 & region=="Sub-Saharan Africa" & date >= "1990-01-01"), 
  table(regime_m7d, regime_p2y)
  )

trans_mat %<>% as.data.frame()

# Code whether changes are better, worse, or same
trans_mat$change <- NA
trans_mat$change <- with(trans_mat, ifelse(as.numeric(regime_m7d) > as.numeric(regime_p2y), "Worse", "Better"))
trans_mat$change <- with(trans_mat, ifelse(regime_m7d==regime_p2y, "Same", change))
trans_mat$change <- factor(trans_mat$change, levels = c("Worse", "Same", "Better"))

# Blank for 0 counts
trans_mat$text_lbl <- with(trans_mat, ifelse(Freq > 0, Freq, ""))

# How many are worse, same, better?
trans_mat %>% group_by(change) %>% summarize(sum(Freq))

# List of cases:
filter(pt, coup==2 & region=="Sub-Saharan Africa" & date >= "1990-01-01") %>% 
  select(country, coup, date, regime_m7d, regime_p2y, change_2y)

p <- ggplot(trans_mat, aes(regime_p2y, regime_m7d, fill = change)) + 
  geom_tile() + 
  geom_text(aes(label = text_lbl), color = "#073642", size = 3) +
  scale_fill_brewer(name="", 
                    type = "diverging",
                    palette = "RdBu") +
  labs(x = "2 years after",
       y = "7 days before") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(0.6)),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.5, vjust=2, size = rel(1.1)),
        legend.text = element_text(size = rel(0.8)),
        legend.key.size = unit(0.15, "in")) +
  ggtitle("Regime transition after a successfull coup,\nSub-Saharan Africa from 1990 to 2014")

ggsave(filename="graphics/2-year-transitions-ssa-90on.png", plot=p, width=5.2, height=4, 
       units="in", dpi=300, scale = 0.8)


# Are more recent coups more democratizing? -------------------------------
#
#   Maybe more recent coups tend to bring about more democratic regimes.
#   Plot counts of transitions from/to more demo/auth regime by year of coup.
#

# First, for just post-1990 sub-Saharan Africa

df <- pt %>% 
  filter(coup==2 & region=="Sub-Saharan Africa" & year >= 1990) %>%
  select(country, year, change_2y, change_2y_bin)

df %<>% 
  group_by(year) %>%
  arrange(desc(change_2y)) %>%
  dplyr::mutate(y = 1:n())

p <- ggplot(df, aes(x = year, y = y)) +
  geom_tile(aes(fill = change_2y_bin), alpha = 0.9) + 
  geom_text(aes(label = country), size = 2, color = "gray20") +
  scale_fill_manual(name = "Regime category 2 year2 later", 
                    values = c("#ef8a62", "#67a9cf", "gray70")) +
  scale_y_continuous(breaks = c(0.5:4.5), labels = c(0:4), expand = c(0, 0)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(0.8)),
        axis.line.x = element_line("black", size = 1, "solid"),
        axis.text.y = element_text(size = rel(0.8)),
        axis.title  = element_text(size = rel(0.6)),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.4, vjust=2, size = rel(1)),
        panel.border = element_blank(),
        legend.text = element_text(size = rel(0.6)),
        legend.title = element_text(size = rel(0.6)),
        legend.key.size = unit(0.15, "in"),
        legend.position = c(0.75, 0.95),
        legend.margin = unit(0, "cm"),
        legend.direction = "horizontal") +
  geom_hline(yintercept = 0.5) +  # axis.line.x is not working for some reason
  ggtitle("Regime change 2 years after a sucessfull coup, Sub-Saharan Africa 1990 to 2014")


ggsave(file = "graphics/ssa-over-time-2yrs.png", plot = p, width = 10, height = 2.5)


# Second, for all countries, over a longer time period

df <- pt %>% 
  filter(coup==2) %>%
  select(country, year, change_2y, change_2y_bin)

df %<>% 
  group_by(year) %>%
  arrange(desc(change_2y)) %>%
  dplyr::mutate(y = 1:n())

df$year2 <- factor(df$year, levels = c(1950:2015))

p <- ggplot(df, aes(x = year2, fill = change_2y_bin)) + 
  stat_bin(drop = FALSE, alpha = 0.9) +
  scale_x_discrete(drop = FALSE, breaks = seq(1950, 2010, by = 10), 
                   expand = c(0, 0)) +
  scale_fill_manual(name = "", 
                    values = c("#ef8a62", "#67a9cf", "gray70")) +
  scale_y_continuous(breaks = c(0, 3, 6, 9, 12), expand = c(0, 0)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = rel(0.8)),
        axis.line.x = element_line("black", size = 1, "solid"),
        axis.text.y = element_text(size = rel(0.8)),
        axis.title  = element_text(size = rel(0.6)),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title = element_text(hjust=0.4, vjust=2, size = rel(1)),
        panel.border = element_blank(),
        legend.text = element_text(size = rel(0.6)),
        legend.title = element_text(size = rel(0.6)),
        legend.key.size = unit(0.15, "in"),
        legend.position = c(0.75, 0.9),
        legend.margin = unit(0, "cm"),
        legend.direction = "horizontal") +
  geom_hline(yintercept = 0) +  # axis.line.x is not working for some reason
  ggtitle("Regime change 2 years after a sucessfull coup, all countries")


ggsave(file = "graphics/all-over-time-2yrs.png", plot = p, width = 10, height = 2.5)



# Write CSV of core data --------------------------------------------------

pt %>% 
  select(country, ccode, date, coup, regime_m7d, regime_p1y, change_1y, change_2y) %>%
  write.csv("coup-list-with-polity.csv")
