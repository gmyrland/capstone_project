##############
## Database IO

## Generate database connection
db <- function() {dbConnect(SQLite(), dbname="data/db.s3db")}

## Read dataframe from sqlite database
read_db <- function(tbl) {
    db <- db()
    df <- NULL
    tryCatch(
        df <- dbReadTable(db, name=tbl),
        error = function(cond) cat(cond$message),
        finally = dbDisconnect(db)
    )
    return(df)
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
