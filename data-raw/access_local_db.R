
library(dplyr)

usr <- ""
pwd <- ""
db_host <- ''

con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = db_host,
                      user = usr,
                      password = pwd  #rstudioapi::askForPassword("Database password")
)

DBI::dbListTables(con)

test <- tbl(con, "orghierarchy")



# Nega's Code -------------------------------------------------------------

# options(max.print = 99999999)
library(DBI)
library(RMySQL)



# 2. Settings
# 2. Settings
db_user <- ''
db_password <- ''
db_name <- 'fhrusaid'
db_table <- 'hfrweeklydataenhanced1'
db_host <- '' # for local access
db_port <- ''
drv <- dbDriver("MySQL")

# 3. Read data from db
mydb =  dbConnect(drv, user = db_user, password = db_password,
                  dbname = db_name, host = db_host, port = db_port)
s = paste0("select * from ", db_table)
rs = dbSendQuery(mydb, s)
df =  fetch(rs, n = -1)
df
on.exit(dbDisconnect(mydb))


s = paste0("select * from ", "consolidated_view")
rs = dbSendQuery(mydb, s)
df =  fetch(rs, n = -1)
df
