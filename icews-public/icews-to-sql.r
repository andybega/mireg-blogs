#   Load ICEWS event data to SQLite
#   Andreas Beger
#   6 April 2015
#
#   This script will run through the ICEWS event data zip files and load them
#   to a SQLite database. With all data back to 1995, the final database file
#   is about 4.3 GB in size.
#   
#   To work, you will need to download the zip files from
#
#   http://thedata.harvard.edu/dvn/dv/icews
#
#   and change `icews_dir` below to point to the folder containing the zip
#   data files.
#
#   SQLite is a lightweight database, similar otherwise to MySQL or PostgreSQL,
#   but much easier to install and get going. In fact, the DB part is installed
#   when you install the `sqldf` or `RSQLite` packages. 

library(sqldf)

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

#
# Some example queries
#

# List tables in the DB. Just `public` for now, but there could be more here.
dbListTables(con)

# List column names in the `public` table.
dbListFields(con, "public")

# A bit more table info:
dbGetQuery(con, "PRAGMA table_info('public') ")

# Get unique country names, first 5 only
dbGetQuery(con, "SELECT distinct(Country) FROM public LIMIT 5;")

# Get count of events per country, first 5 only;
# formatting is just to make the query more readable
sql <- "
SELECT Country, count(*) AS events 
FROM   public 
GROUP  BY Country 
LIMIT 5;"
dbGetQuery(con, "")

# Get count of events per country, highest 5 only
sql <- "
SELECT Country, count(*) AS events 
FROM   public 
GROUP  BY Country 
ORDER  BY events DESC 
LIMIT 5;"
dbGetQuery(con, sql)

# When done, you can close the DB connection
dbDisconnect(con)