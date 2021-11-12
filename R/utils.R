
count_districts <- function(src = NULL, scode, verbose = TRUE)
{
    op <- options(scipen = 5)
    on.exit(options(op))
    if (is.null(src))
        src <- Sys.getenv("JSON_API_DIR",
                          stop("'src' must be provided explicitly or via environment variable JSON_API_DIR"))

    ## Loop through states and collect results in a list

    ## FIXME: What to do if no district-level data? Could use state
    ## data instead, with

    ## Use dates starting from "2020-01-30", but many dates will be
    ## missing, so check

    DATES <- as.character(seq(as.Date("2020-01-30"), Sys.Date(), by = 1))
    SCODE <- scode
    if (is.null(SCODE)) stop("state code must be specified as 'scode'")

    file <- file.path(src, sprintf("timeseries-%s.min.json", SCODE))
    jdata <- read_json(file)[[SCODE]][["districts"]]

    for (date in DATES)
    {
        df <- extractDistrictDataByDate(date, jdata = jdata, STATE = SCODE)
        if (!is.null(df)) # Otherwise no data for this date, skip
        {
            cat(sprintf("%s,%s,%d\n", SCODE, date, nrow(df)))
        }
    }
}

