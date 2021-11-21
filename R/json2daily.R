
## Create daily (confirmed,recovered,deceased,other,tested) as well as
## vaccination files from data in timeseries*.min.json files

## Filename format: ${DAILYCSV_DIR}/<date>/<state-code>_CRD.csv

## Most of this is similar to json2districts(), but only do one state
## at a time. In addition to the "districts" component with
## district-level data, there is a state level component (named by
## state code) which should give state total. This should go in a
## special row with District="_STATE_". We should probably also have a
## column for (a) notes, and (b) tested data source. But are these in
## the json files?

json2dailycsv <- function(src = NULL, dest = NULL, scode = NULL,
                          verbose = TRUE, columns = TRUE)
{
    op <- options(scipen = 5)
    on.exit(options(op))
    if (is.null(src))
        src <- Sys.getenv("JSON_API_DIR",
                          stop("'src' must be provided explicitly or via environment variable JSON_API_DIR"))
    if (is.null(dest))
        dest <- Sys.getenv("DAILYCSV_DIR",
                           stop("'dest' must be provided explicitly or via environment variable DAILYCSV_DIR"))

    ## Loop through states and collect results in a list

    ## FIXME: What to do if no district-level data? Could use state
    ## data instead, with

    ## Use dates starting from "2020-01-30", but many dates will be
    ## missing, so check

    DATES <- as.character(seq(as.Date("2020-01-30"), Sys.Date(), by = 1))
    SCODE <- scode
    if (is.null(SCODE)) stop("state code must be specified as 'scode'")

    if (verbose) message("Reading file for ", SCODE)
    file <- file.path(src, sprintf("timeseries-%s.min.json", SCODE))
    jdata <- read_json(file)
    jdata_districts <- jdata[[SCODE]][["districts"]]
    jdata_state <- jdata[SCODE] # note: keep list structure so that we can use extractStateDataByDate()

    for (date in DATES)
    {
        df <- extractDistrictDataByDate(date, jdata = jdata_districts, STATE = SCODE)
        if (!is.null(df)) # Otherwise no data for this date, skip
        {
            message(date, ", ", nrow(df))
            state_total <- extractStateDataByDate(date, jdata = jdata_state, SCODES = SCODE, add.unassigned = FALSE, verbose = TRUE)
            state_total$District <- "_STATE_"
            df <- rbind(df, state_total[names(df)]) # because names in different order
            if (!dir.exists(file.path(dest, date)))
                dir.create(file.path(dest, date))
            write.csv(df[columns],
                      file = file.path(dest, date, sprintf("%s_CRD.csv", SCODE)),
                      row.names = FALSE, quote = FALSE, na = "")
        }
    }
}
