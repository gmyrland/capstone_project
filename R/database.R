##############
## Database IO

## Database path
db_path <- "data/db.s3db"

## Generate database connection
db <- function() {dbConnect(SQLite(), dbname=db_path)}

## Read dataframe from sqlite database
read_db <- function(name) {
    tbl(src_sqlite(db_path), name) %>% as.data.frame(n=-1)
}

## Cache dataframe to sqlite database
write_db <- function(tbl, df) {
    db <- db()
    tryCatch(
        dbWriteTable(db, name=tbl, as.data.frame(df), overwrite=TRUE, append=FALSE),
        error = function(cond) cat(cond$message),
        finally = dbDisconnect(db)
    )
}
