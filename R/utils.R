
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


## check for inconsistencies like (a) total counts decreasing (which
## may happen legitimately due to state corrections), (b) total and
## delta / delta7 counts not matching. This may be complicated by some
## dates being missing for certain dates. We need to figure out a
## convention for such cases (see if covid19india has some precedent).

identify_inconsistencies <-
    function(src = NULL, scode,
             what = c("confirmed", "recovered", "deceased", "vaccinated1", "vaccinated2"),
             verbose = TRUE)
{
    what <- match.arg(what)
    op <- options(scipen = 5)
    on.exit(options(op))
    if (is.null(src))
        src <- Sys.getenv("JSON_API_DIR",
                          stop("'src' must be provided explicitly or via environment variable JSON_API_DIR"))

    ## Use dates starting from "2020-01-30", but many dates will be
    ## missing, so check

    DATES <- as.character(seq(as.Date("2020-01-30"), Sys.Date(), by = 1))
    SCODE <- scode
    if (is.null(SCODE)) stop("state code must be specified as 'scode'")

    file <- file.path(src, sprintf("timeseries-%s.min.json", SCODE))
    jdata <- read_json(file)[[SCODE]][["districts"]]

    ## First extract as a list of dates. Will have to check by district then.
    dflist.total <- 
        lapply(DATES,
               function(date)
               {
                   df <- extractDistrictDataByDate(date, jdata = jdata, STATE = SCODE,
                                                   FIELDS = what, COMPONENT = "total",
                                                   na.zero = FALSE, drop0 = FALSE)
                   ## print(df)
                   df
               })
    dflist.delta <- 
        lapply(DATES,
               function(date)
               {
                   df <- extractDistrictDataByDate(date, jdata = jdata, STATE = SCODE,
                                                   FIELDS = what, COMPONENT = "delta",
                                                   na.zero = FALSE, drop0 = FALSE)
                   ## print(df)
                   df
               })
    dflist.delta7 <- 
        lapply(DATES,
               function(date)
               {
                   df <- extractDistrictDataByDate(date, jdata = jdata, STATE = SCODE,
                                                   FIELDS = what, COMPONENT = "delta7",
                                                   na.zero = FALSE, drop0 = FALSE)
                   ## print(df)
                   df
               })
    dflist.total <- Filter(Negate(is.null), dflist.total)
    dflist.delta <- Filter(Negate(is.null), dflist.delta)
    dflist.delta <- Filter(Negate(is.null), dflist.delta)
    ## all districts represented in data
    districts.total <- sort(unique(unlist(lapply(dflist.total, "[[", "District"))))
    districts.delta <- sort(unique(unlist(lapply(dflist.delta, "[[", "District"))))
    districts.delta7 <- sort(unique(unlist(lapply(dflist.delta7, "[[", "District"))))
    stopifnot(identical(districts.total, districts.delta))
    stopifnot(identical(districts.total, districts.delta7))
    ## For each district, get time series --- but how to ensure same
    ## dates?  Assume that total will always have consecutive dates
    ## (which we should actually check also), but others may not
    ## (delta=0 may be skipped in json file). So calculate delta and
    ## delta7 from total, and match by date.
    DecreasingTotalEntries <- NULL
    DeltaMismatchEntries <- NULL
    Delta7MismatchEntries <- NULL
    for (d in districts.total)
    {
        dtotal <- do.call(rbind, lapply(dflist.total, function(df) df[df$District == d, ]))
        rownames(dtotal) <- dtotal$Date
        ## str(dtotal)
        ddelta <- do.call(rbind, lapply(dflist.delta, function(df) df[df$District == d, ]))
        rownames(ddelta) <- ddelta$Date
        ## str(ddelta)
        ddelta7 <- do.call(rbind, lapply(dflist.delta7, function(df) df[df$District == d, ]))
        rownames(ddelta7) <- ddelta7$Date
        ## str(ddelta7)
        ## ## verify that first three columns are identical
        ## stopifnot(identical(dtotal[1:3], ddelta[1:3]))
        ## stopifnot(identical(dtotal[1:3], ddelta7[1:3]))
        dtotal$total <- dtotal[[what]] # so that we can refer to 'total'
        dtotal <- within(dtotal,
        {
            delta <- diff(c(0, total))
            delta7 <- diff(c(rep(0, 7), total), lag = 7)
        })
        ## checks
        ## (a) total should be non-decreasing
        decreasingTotal <- which(dtotal$delta < 0)
        if (length(decreasingTotal))
            DecreasingTotalEntries <-
                rbind(DecreasingTotalEntries,
                      dtotal[ sort(c(decreasingTotal-1L, decreasingTotal)) , , drop = FALSE])
        ## (b) delta should match
        deltaMismatch <- ddelta[[what]] != dtotal[rownames(ddelta), "delta"]
        delta7Mismatch <- ddelta7[[what]] != dtotal[rownames(ddelta7), "delta7"]
        ## str(deltaMismatch)
        if (any(deltaMismatch, na.rm = TRUE))
            DeltaMismatchEntries <-
                rbind(DeltaMismatchEntries,
                      cbind(ddelta[deltaMismatch, ],
                            dtotal[rownames(ddelta)[deltaMismatch], -(1:3)]))
    }
    message("Decreasing Total")
    print(DecreasingTotalEntries)
    message("Delta Mismatch")
    print(DeltaMismatchEntries)
}


## Helper functions

## Extract relevant numbers from a 'total' component. FIXME: Missing
## numbers usually mean 0, expect Tested is NA. Do that here or later?

extractNumbers <-
    function(x,
             FIELDS = c("Confirmed", "Recovered", "Deceased", "Other", "Tested"),
             COMPONENT = "total",
             na.zero = TRUE)
{
    if (is.null(x)) stop("Data not available, check before calling.")
    if (!length(x[[COMPONENT]])) return(NULL) # FIXME: should this happen?
    ## FIELDS <- c("Confirmed", "Recovered", "Deceased", "Other", "Tested")
    fields <- tolower(FIELDS)
    ## not all names are always there. Replace others by 0
    ans <- unlist(x[[COMPONENT]])[fields]
    if (na.zero) ans[is.na(ans)] <- 0
    if (length(ans) == 0) str(x) # FIXME
    names(ans) <- FIELDS
    ans
}

extractStateDataByDate <-
    function(D, jdata, SCODES = STATE.CODES,
             add.unassigned = TRUE, verbose = TRUE,
             ...)
{
    ldate <- 
        lapply(SCODES,
               function(S)
               {
                   x <- jdata[[S]]$dates[[D]]
                   if (is.null(x)) NULL
                   else extractNumbers(x, ...)
               })
    if (is.null(names(ldate))) names(ldate) <- SCODES # default gives state names as names because STATE.CODES is named
    ddate <- do.call(rbind, ldate)
    if (is.null(ddate))
        return(NULL)
    else {
        ddate <-
            cbind(Date = D, State = rownames(ddate),
                  as.data.frame(ddate))
        if (add.unassigned)
        {
            ## If India number does not equal state total, add a row
            ## with State="State Unassigned".
            w <- which(ddate$State == "India")
            stopifnot(length(w) == 1)
            unassigned.c <- ddate$Confirmed[w] - sum(ddate$Confirmed[-w])
            unassigned.r <- ddate$Recovered[w] - sum(ddate$Recovered[-w])
            unassigned.d <- ddate$Deceased[w] - sum(ddate$Deceased[-w])
            if (unassigned.c | unassigned.r | unassigned.d)
            {
                if (verbose)
                    message(sprintf("(C,R,D) discrepancy on %s = (%d,%d,%d)",
                                    D, unassigned.c, unassigned.r, unassigned.d))
                ## Insert new column right after India
                ddate <- rbind(head(ddate, w),
                               tail(ddate, -(w-1))) # second one is duplicate
                ddate$State[w+1] <- "State Unassigned"
                ddate$Confirmed[w+1] <- unassigned.c
                ddate$Recovered[w+1] <- unassigned.r
                ddate$Deceased[w+1] <- unassigned.d
                ddate$Other[w+1] <- 0 # ignore
                ddate$Tested[w+1] <- 0 # ignore
                ## str(ddate[w+1, ])
            }
        }
        rownames(ddate) <- NULL
        ## drop states that have all 0 confirmed
        ddate <- subset(ddate, Confirmed + Other + Tested != 0)
        return(ddate)
    }
}

extractDistrictDataByDate <- function(D, jdata, STATE = "", ..., drop0 = FALSE)
{
    ldate <- 
        lapply(names(jdata),
               function(S)
               {
                   x <- jdata[[S]]$dates[[D]]
                   if (is.null(x)) NULL
                   else extractNumbers(x, ...)
               })
    names(ldate) <- names(jdata)
    ## str(ldate)
    ddate <- do.call(rbind, ldate)
    if (is.null(ddate))
        return(NULL)
    else {
        ## str(list(Date = D, State = STATE, District = rownames(ddate),
        ##          ddate = as.data.frame(ddate)))
        ddate <-
            cbind(Date = D, State = unname(STATE), District = rownames(ddate),
                  as.data.frame(ddate))
        rownames(ddate) <- NULL
        ## drop states that have all 0
        if (drop0)
            ddate <- subset(ddate, Confirmed + Recovered + Other + Tested != 0)
        return(ddate)
    }
}


