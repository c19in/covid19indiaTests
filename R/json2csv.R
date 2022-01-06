


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
    ## 
    ## FIXME: What to do if no district-level data? Could use state
    ## data instead, with district name 'Other' (or Delhi / Chandigarh
    ## / Lakshadweep for these three states to be consistent with covid19india)

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



    

