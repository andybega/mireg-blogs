# Using dplyr

# http://cran.rstudio.com/web/packages/dplyr/vignettes/databases.html

library(dplyr)
library(magrittr)
library(ggplot2)
library(sqldf)

# Create year-month variable
con <- dbConnect(SQLite(), dbname = "data/icews.sqlite")

dbGetQuery(con, "PRAGMA table_info([public]);")

sql <- "
ALTER TABLE public DROP COLUMN year_month;"
dbSendQuery(con, sql)

sql <- "
INSERT INTO public (year_month)
SELECT substr(Event_Date, 1, 7) AS year_month FROM public;"
dbSendQuery(con, sql)

sql <- "
SELECT Event_Date, year_month FROM public LIMIT 5;"
dbGetQuery(con, sql)

sql <- "
SELECT substr(Event_Date, 1, 7) FROM public LIMIT 5;"
dbGetQuery(con, sql)


db <- src_sqlite("data/icews.sqlite", create = F)
icews <- tbl(db, "public")

icews <- icews %>%
  mutate(year_month = substr(Event_Date, 1, 7))

cm_counts <- icews %>%
  group_by(Country, year_month) %>%
  summarize(
    protest = sum(CAMEO_Code %in% c(1411, 1414, 1431, 1434, 1441, 1444, 
                                          1451, 1454)),
    repression = sum(CAMEO_Code %in% c(175, 172, 1721:1724)),
    fighting = sum(CAMEO_Code %in% c(1823, 183:186, 1831:1833))
    )

cm_counts <- dbGetQuery(con, "
SELECT substr(Event_Date, 1, 7) AS year_month, count(*) AS protest 
FROM   public
WHERE  CAMEO_Code IN (1411, 1414, 1431, 1434, 1441, 1444, 1451, 1454)
GROUP  BY substr(Event_Date, 1, 7);")

cm_counts %<>% arrange(year_month) %>%
  mutate(year_month = as.Date(paste0(year_month, "-01"), format="%Y-%m-%d"))

ggplot(cm_counts, aes(x=year_month, y=protest)) + geom_line() + theme_bw()

cm_counts2 <- icews %>%
  group_by(Country, year_month) %>%
  summarize(
    protest = sum(event[CAMEO.Code==1411 | CAMEO.Code==1414 | CAMEO.Code==1431 | CAMEO.Code==1434 |
                          CAMEO.Code==1441 | CAMEO.Code==1444 | CAMEO.Code==1451 | CAMEO.Code==1454]),
    repression = sum(event[CAMEO.Code==175 | CAMEO.Code ==172 |
                             (CAMEO.Code >= 1721 & CAMEO.Code <= 1724)]),
    fighting = sum(event[CAMEO.Code==1823 | (CAMEO.Code >= 183 & CAMEO.Code <= 186) |
                           (CAMEO.Code >= 1831 & CAMEO.Code <= 1833)]))
  )

  counts.long <- ddply(file, .(Country, year, month), summarise,
                       protest = sum(event[CAMEO.Code==1411 | CAMEO.Code==1414 | CAMEO.Code==1431 | CAMEO.Code==1434 |
                                             CAMEO.Code==1441 | CAMEO.Code==1444 | CAMEO.Code==1451 | CAMEO.Code==1454]),
                       repression = sum(event[CAMEO.Code==175 | CAMEO.Code ==172 |
                                                (CAMEO.Code >= 1721 & CAMEO.Code <= 1724)]),
                       fighting = sum(event[CAMEO.Code==1823 | (CAMEO.Code >= 183 & CAMEO.Code <= 186) |
                                              (CAMEO.Code >= 1831 & CAMEO.Code <= 1833)]))
# Indexing