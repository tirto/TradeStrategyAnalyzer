
#
# database utils
#
readFromDB <- function(tablename, where="") {
    m <- dbDriver("SQLite")
    con <- dbConnect(m,dbname="sim.db")
    sql <- paste("select * from ", tablename, where)
    print(sql)
    data <- dbGetQuery(con, sql)
    sqliteCloseConnection(con)
    sqliteCloseDriver(m)
    if ("ACTIVITY_TS" %in% colnames(data)) 
       data$ACTIVITY_TS <- as.Date(data$ACTIVITY_TS,'%Y-%m-%d')
    data
}

writeToDb <- function(tablename, df) {
  # Write dataframe to database table
  m <- dbDriver("SQLite")
	conn <- dbConnect(m, dbname="sim.db")
	dbWriteTable(conn, tablename, df, overwrite=T)

	# cleanup sql connection
	sqliteCloseConnection(conn)
	sqliteCloseDriver(m)

}
