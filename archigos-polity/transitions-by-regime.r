# Archigos transitions and tenure by regime type
#
# Andreas Beger
# 15 December 2014

setwd("~/Desktop/archigos-polity")

library(foreign)
library(gdata)

# Polity IV d (case format)
# download at 
# http://www.systemicpeace.org/inscr/p4v2013d.xls
p4d <- read.xls("p4v2013d.xls", stringsAsFactors=FALSE)

p4d$start <- with(p4d, as.Date(paste(byear, bmonth, bday, sep="-")))
p4d$end   <- with(p4d, as.Date(paste(eyear, emonth, eday, sep="-")))
p4d$end[is.na(p4d$end)] <- as.Date("2013-12-31")

p4d$polity <- with(p4d, democ-autoc)

# Read and fix Archigos
# download at
# http://www.rochester.edu/college/faculty/hgoemans/Archigos_2.9-Public.dta
arch <- read.dta("Archigos_2.9-Public.dta")

arch <- within(arch, {
	startdate <- as.Date(startdate, format="%d/%m/%Y")
	enddate <- as.Date(enddate, format="%d/%m/%Y")
})

# Code entry and exit as factor
table(arch$entry)
arch$entry <- factor(arch$entry, labels=c("unknown", "regular", "irregular", "foreign_imposed"))
table(arch$entry)

table(arch$exit)
arch$exit <- factor(arch$exit, labels=c("in_power", "unknown", "regular", "natural_death", "ill_health", "suicide", "irregular", "foreign_deposed"))
table(arch$exit)

# Code average Polity score for a country under a leader's tenure
# Need to weigh by the amount of time that a polity coding overlaps with a 
# leader's tenure. 
arch$polity <- NA
for (i in 1:nrow(arch)) {
	ldr_start <- arch$startdate[i]
	ldr_end   <- arch$enddate[i]
	ldr_ccode <- arch$ccode[i]

	# Keep only Polity records matchig counry and which overlap ldr tenure
	before <- with(p4d, (start < ldr_start & end < ldr_start))
	after  <- with(p4d, (start > ldr_end & end > ldr_end))
	pol <- subset(p4d, !before & !after & p4d$ccode==ldr_ccode, select=c("start", "end", "polity"), drop=TRUE)

	# Identify between regime lengths and leader tenure, so we can weigh that 
	# regimes polity contribution 
	lead_hang <- difftime(ldr_start, pol$start, unit="days")
	lead_hang[lead_hang < 0] <- 0
	tail_hang <- difftime(pol$end, ldr_end, unit="days")
	tail_hang[tail_hang < 0] <- 0
	pol$overlap <- as.numeric(round(with(pol, difftime(end, start) - lead_hang - tail_hang)))
	pol$w <- pol$overlap / sum(pol$overlap)

	arch$polity[i] <- with(pol, weighted.mean(polity, w))
}

# Entries for autocracies
table(arch$entry[arch$polity < 6])
table(arch$entry[arch$polity < -4])

# Exits for autocracies
table(arch$exit[arch$polity < 6])
table(arch$exit[arch$polity < -4])

# Translate to Polity regime categories
reg_lbl <- c("autocracy", "closed_anocracy", "open_anocracy", "democracy", 
		"full_democracy")
arch$regime <- cut(arch$polity, breaks=c(-11, -6, 0, 5, 9, 10), labels=reg_lbl)
arch$regime <- factor(arch$regime, exclude=FALSE, labels=c(reg_lbl, "failed/occupied"))


# Entry/exit by regime ----------------------------------------------------
#
#   Mosaic plots of entry and exit by regime.
#

library(vcd)

height=400
png("entry-by-regime.png", height=height, width=height*1.3)
mosaic(~ regime + entry, data=arch, main="Leader entry",
       labeling=labeling_border(
         rot_labels=c(45, 0, 0, 0), just_labels=c("left", "right"), 
         varnames=c(FALSE, FALSE)))
dev.off()

png("exit-by-regime.png", height=height, width=height*1.3)
mosaic(~ regime + exit, data=arch, main="Leader exit",
       labeling=labeling_border(
         rot_labels=c(45, 0, 0, 0), just_labels=c("left", "right"),
         varnames=c(FALSE, FALSE)))
dev.off()


# Histograms of tenure by regime ------------------------------------------
# 
#   Distribution of tenure times by regime. Does not account for 
#   leaders still in power at end of data collection (right-censored)
#

arch$months_in_power <- with(arch, difftime(enddate, startdate, units="days"))
arch$months_in_power <- with(arch, as.numeric(months_in_power)/30)

library(ggplot2)

pp <- ggplot(data=arch, aes(x=months_in_power)) +
  geom_histogram(binwidth=0.05) + 
  facet_wrap(~ regime) +
  scale_x_log10() +
  theme_bw() +
  geom_vline(xintercept=c(c(1, 4, 8)*12), col="cyan", alpha=0.3) + 
  ylab("") + xlab("Months in power")

pp

ggsave(plot=pp, file="months-in-power-by-reg.png", width=6, height=4)


# Leader tenure over time -------------------------------------------------
# 
#   Plot of leader tenure (time in office) over time. 
#
#   Some leaders in the data assumed power before the nominal start date of
#   Archigos, 1875. While those entry dates are recorded, there are no records
#   for other leaders who entered around the same but left before 1875, so the
#   records are in- complete. Similarly, some current leader are still in office
#   at the time data collection stopped and thus right-censored.
#
 
arch[arch$months_in_power > 400, c("idacr", "leader", "startdate", "months_in_power")]
arch$censored <- with(arch, startdate < "1875-01-01" | enddate == "2004-12-31")

pp <- ggplot(data=arch, aes(x=startdate, y=months_in_power, col=censored)) +
  geom_point() +
  stat_smooth(data=arch[!arch$censored, ], aes(x=startdate, y=months_in_power)) +
  xlab("Entry to power") + ylab("Months in power")
pp

ggsave(plot=pp, file="in-power-over-time.png", width=6, height=4)
