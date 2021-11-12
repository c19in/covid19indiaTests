
## Helper functions

## Extract relevant numbers from a 'total' component. FIXME: Missing
## numbers usually mean 0, expect Tested is NA. Do that here or later?

extractNumbers <- function(x)
{
    if (is.null(x)) stop("Data not available, check before calling.")
    if (!length(x$total)) return(NULL) # FIXME: should this happen?
    FIELDS <- c("Confirmed", "Recovered", "Deceased", "Other", "Tested")
    fields <- tolower(FIELDS)
    ## not all names are always there. Replace others by 0
    ans <- unlist(x$total)[fields]
    ans[is.na(ans)] <- 0
    if (length(ans) == 0) str(x) # FIXME
    names(ans) <- FIELDS
    ans
}

extractStateDataByDate <- function(D, jdata, add.unassigned = TRUE, verbose = TRUE)
{
    ldate <- 
        lapply(STATE.CODES,
               function(S)
               {
                   x <- jdata[[S]]$dates[[D]]
                   if (is.null(x)) NULL
                   else extractNumbers(x)
               })
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

extractDistrictDataByDate <- function(D, jdata, STATE = "")
{
    ldate <- 
        lapply(names(jdata),
               function(S)
               {
                   x <- jdata[[S]]$dates[[D]]
                   if (is.null(x)) NULL
                   else extractNumbers(x)
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
        ddate <- subset(ddate, Confirmed + Recovered + Other + Tested != 0)
        return(ddate)
    }
}





## generate CSV API endpoint states.csv using JSON files

## Generate 'states.csv'

json2states <- function(src = NULL, dest = NULL, verbose = TRUE, columns = TRUE)
{
    op <- options(scipen = 5)
    on.exit(options(op))
    if (is.null(src))
        src <- Sys.getenv("JSON_API_DIR",
                          stop("'src' must be provided explicitly or via environment variable JSON_API_DIR"))
    if (is.null(dest))
        dest <- Sys.getenv("CSV_API_DIR",
                           stop("'dest' must be provided explicitly or via environment variable CSV_API_DIR"))

    jdata <- read_json(file.path(src, "timeseries.min.json"))

    ## Seems simplest to go by date. Components of jdata are state codes
    ## (including TT=India). Each such component has a (single) component
    ## named "dates" which is a list named by dates. Within these are
    ## components delta, delta7, and total. We only need to extract total.

    ## str(jdata$WB, max.level = 2)
    ## str(jdata$TT, max.level = 1)
    ## str(jdata$WB$dates[["2020-06-21"]])

    ## Dates start from "2020-01-30" and go upto today. Some date / state
    ## combinations may be missing, so check.

    DATES <- as.character(seq(as.Date("2020-01-30"), Sys.Date(), by = 1))

    states <- do.call(rbind,
                      lapply(DATES, extractStateDataByDate, jdata = jdata,
                             add.unassigned = TRUE, verbose = verbose))

    ## One special case handled here: Tested==0 really means NA. These
    ## were missing in the JSON, but to handle 'Other' we converted them
    ## to 0. But the Tested==0 values should become missing in the output,
    ## so:

    states <- within(states,
    {
        is.na(Tested) <- Tested == 0
    })

    write.csv(states[columns], file = file.path(dest, "states.csv"),
              row.names = FALSE, quote = FALSE, na = "")
    
}


## Generate 'districts.csv'

json2districts <- function(src = NULL, dest = NULL, verbose = TRUE, columns = TRUE)
{
    op <- options(scipen = 5)
    on.exit(options(op))
    if (is.null(src))
        src <- Sys.getenv("JSON_API_DIR",
                          stop("'src' must be provided explicitly or via environment variable JSON_API_DIR"))
    if (is.null(dest))
        dest <- Sys.getenv("CSV_API_DIR",
                           stop("'dest' must be provided explicitly or via environment variable CSV_API_DIR"))


    ## Loop through states and collect results in a list
    ## FIXME: What to do if no district-level data? Could use state data instead, with 

    ## Use dates starting from "2020-01-30", but many dates will be
    ## missing, so check

    DATES <- as.character(seq(as.Date("2020-01-30"), Sys.Date(), by = 1))

    ## exclude TT from state codes
    all.states <- 
        lapply(STATE.CODES[STATE.CODES != "TT"],
               function(SCODE) {
                   if (verbose) message("Reading file for ", SCODE)
                   file <- file.path(src, sprintf("timeseries-%s.min.json", SCODE))
                   jdata <- read_json(file)[[SCODE]][["districts"]]
                   do.call(rbind,
                           lapply(DATES, extractDistrictDataByDate,
                                  jdata = jdata,
                                  STATE = SCODE))
               })

    districts <- do.call(rbind, all.states)

    ## Sort by Date, then State (code), then District.
    o <- with(districts, order(Date, State, District))
    districts <- districts[o, ]

    ## Finally, replace state codes by state names. Also, Tested==0 really
    ## means NA and should be recorded accordingly, so modify.

    districts <- within(districts,
    {
        State  <-  STATE.NAMES[State]
        is.na(Tested) <- Tested == 0
    })

    ## Includes some rows like:
    ##
    ## 2020-04-26,Rajasthan,Other State,0,2,2,0,
    ## 2020-04-26,Tamil Nadu,Unknown,0,25,-1,0,
    ##
    ## which are not included in covid19india.org's output. These all seem
    ## to have Confirmed==0, so just skip those.

    ## districts <- subset(districts, Confirmed != 0)

    write.csv(districts[columns], file = file.path(dest, "districts.csv"),
              row.names = FALSE, quote = FALSE, na = "")
}
