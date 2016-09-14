

library("rio")
library("ggplot2")
library("knitr")
library("dplyr")
library("countrycode")

setwd("~/Desktop/blogs/data-management")
coups <- import("ex-data.csv")

knitr::kable(head(coups))

# Argh, imputed GDP per capita values!
coups2 <- subset(coups, year %in% c(1998, 1999, 2000), select = c("cowcode", "year", "ln_gdpen"))
coups2$gdpen <- exp(coups2$ln_gdpen)
ggplot(coups2, aes(x = gdpen)) +
  geom_histogram() + 
  facet_wrap(~ year, ncol = 1, scales = "free_y") +
  labs(x = "log GDP per capita") +
  theme_bw() +
  ggtitle("GDP p.c. for select years; imputation problems?")
ggsave(file="gdppc-by-year-problems.png", heigh = 6, width = 5)


# Spaghetti plot of GDP per capita
noodles <- function(x, space, time, data) {
  require(ggplot2)
  p <- ggplot(data = data, aes_string(x = time, y = x, group = space)) +
    geom_line(alpha = 0.3) + theme_bw()
  plot(p)
}

noodles("ln_gdpen", "cowcode", "year", data = coups)
ggsave(file="lngdpen-noodles.png", height=6, width=6)


# Plot missing values

source("plot_missing.R")
source("state-panel.R")

coups$date <- as.Date(paste(coups$year, "06", "30", sep = "-"))



plot_missing(coups, "cowcode", "date", "ln_gdpen", "year") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_discrete("cowcode", breaks = NULL)
ggsave(file="missing-plot-1.png", height = 10, width = 8)

# Which country-year combinations are absent suddenly in 2000?
c1999 <- dplyr::filter(coups, year == 1999) %>% .$cowcode
c2000 <- dplyr::filter(coups, year == 2000) %>% .$cowcode
cbind(
  setdiff(c1999, c2000),
  countrycode(setdiff(c1999, c2000), "cown", "country.name")
  )

# For examples, set some values to missing 
# missing by year
coups2 <- coups
coups2$ln_gdpen[coups$year==max(coups$year)] <- NA
coups2$type <- "Whole year missing"

p1 <- plot_missing(coups2, "cowcode", "date", "ln_gdpen", "year") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_discrete("cowcode", breaks = NULL)
#ggsave(file="missing-plot-2.png", height = 10, width = 8)

# missing for some countries
coups3 <- coups
coups3$ln_gdpen[coups$cowcode %in% sample(coups$cowcode, 10)] <- NA
coups3$type <- "Whole country missing"

p2 <- plot_missing(coups3, "cowcode", "date", "ln_gdpen", "year") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_discrete("cowcode", breaks = NULL)
#ggsave(file="missing-plot-3.png", height = 10, width = 8)

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
ggsave(file = "missing-plot-2.png", arrangeGrob(p1, p2), height = 6, width = 8)

# Check state system membership too
# First, manually recreate the data for the plot so we can pick which countries
# to highlight (label)
data  <- coups
space <- "cowcode"
time  <- "date"
var   <- "ln_gdpen"
time_unit <- "year"
checkSS <- "COW"
space_range <- unique(data[, space])
time_range  <- seq.Date(from = min(data[, time]),
                        to   = max(data[, time]),
                        by   = time_unit
                        )
full_mat <- expand.grid(space_range, time_range)
colnames(full_mat) <- c(space, time)
data[, "z"] <- !complete.cases(data[, var])
data <- data[, c(space, time, "z")]
full_mat <- dplyr::left_join(full_mat, data, by = c(space, time))
full_mat$z[full_mat$z==TRUE]  <- "Mssng. values"
full_mat$z[full_mat$z==FALSE] <- "Complete" 
full_mat$z[is.na(full_mat$z)] <- "No obs."
full_mat$z <- factor(full_mat$z, levels = c("Complete", "Mssng. values", "No obs."))
# set ss type GW or COW
ccname <- ifelse(checkSS=="GW", "gwcode", "cowcode")
target <- state_panel(min(full_mat$date), max(full_mat$date), by = time_unit, useGW = (checkSS=="GW"))
target$id <- NULL
target$.ss <- 1
target[, ccname] <- as.integer(as.character(target[, ccname]))
colnames(target) <- c(space, time, ".ss")
full_mat[, space] <- as.integer(as.character(full_mat[, space]))
full_mat <- left_join(full_mat, target)
full_mat$.ss <- ifelse(is.na(full_mat$.ss), "out ss", "in ss")
full_mat$.ssmssng <- paste(full_mat$z, full_mat$.ss, sep = " ")
lvls <- as.vector(outer(c("Complete", "Mssng. values", "No obs."), c("in ss", "out ss"), paste, sep = " "))
full_mat$.ssmssng <- factor(full_mat$.ssmssng, levels = lvls)
full_mat[, space] <- factor(full_mat[, space], levels = rev(sort(unique(full_mat[, space]))))

anomalies <- full_mat %>%
  dplyr::group_by(cowcode) %>%
  dplyr::summarize(comp_oss = sum(.ssmssng=="Complete out ss"),
            abs_iss  = sum(.ssmssng=="No obs. in ss")) %>%
  arrange(desc(comp_oss)) %>%
  mutate(country = countrycode(cowcode, "cown", "country.name")) 

anomalies %>% dplyr::filter(comp_oss > 1 | abs_iss > 0) %>% print(n = 200)
table(anomalies$comp_oss)

label <- c(364, 365, 511, 818, 345, 347, 529, 530)

# Ok, now the plot
plot_missing(coups, "cowcode", "date", "ln_gdpen", "year", checkSS = "COW") + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_discrete("cowcode", breaks = label)
ggsave(file="missing-plot-w-sscheck.png", height = 10, width = 8)





