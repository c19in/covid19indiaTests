# covid19indiaTests

R package to test accuracy and consistency of covid19india API endpoints

This package will eventually contain tools for testing the accuracy and integrity of data provided by the incovid19 project, although they could be used by other similar projects as well, as long as they use the API formats developed by covid19india.org to provide COVID-19 data for India.

What follows is a list the types of tests it could be useful to implement. These fall into two broad types, accuracy and consistency.

## Accuracy

This set of checks involve verifying the accuracy of data by comparing with manually vetted source data. This requires external data of course, as well as an agreed upon format in which the data should be in for comparison.

A natural format is to have one CSV file for each day and state. External data must be in this form to enable comparison. Data in the covid19india API format will also need to be automatically converted to this form (for which we need code).

A specific proposal for this "standard" CSV format is the following:

### C,R,D and testing data

Filenames are of the form `<data-folder>/<date>/<state-code>_CRD.csv`.

Rows represent districts. Columns are district name, confirmed, recovered, deceased, tested. All values should be cumulative.

Extra columns may be needed, e.g., covid19india gives source url for testing data. Most bulletins would also give a state total for some or all columns. These should be in a special row whose district name is `"_Total_"`. Whenever possible, only data available in the bulletin should be filled in, and others should be inferred.

States for which no district-level data are available should have only the `_Total_` row. The district names to be finally used may depend on the state (e.g., state name for Delhi, Chandigarh, "Unknown" for UP, Goa). We should follow covid19india conventions.

There may be discrepancies between the total row and district total. We will worry about this later.

We will need

- Code to create daily CSV files from API endpoints
- Code to create daily CSV files from the easy sources (i.e., HTML)
- Code to convert daily CSV files to aggregated API endpoints
- Code to check whether these mappings are inverses of each other

### Vaccination data

There is really only one source for this data, namely the CoWIN API, so it's unlikely that anything can go wrong here. Still, we don't know if past data ever changes (not improbable for one or two days earlier), so it might be useful to keep a record of daily vaccination totals in a similar format. This will be useful to investigate whether past data changes, and also to generate JSON and CSV endpoints as a cross-check.

We need to be careful to check whether all districts map to C,R,D districts, and remap if necessary.

Filenames could be of the form `<data-folder>/<date>/<state-code>_VAC.csv`.

Rows represent districts. Columns could be district names, #vaccination1, #vaccination2 (cumulative). Additional rows may be useful to have (e.g., giving gender / age-wise breakups).

We will need:

- Code to create such daily files (going back to the past) by calling the CoWIN API
- Code to integrate data from these files into the workflow to produce final JSON and CSV endpoints

## Internal consistency

Where the same information is stored in multiple places, they should be consistent. There may be inconsistencies in the sources themselves (especially because not all source data is properly time stamped, and e.g., state totals reported by mygov.in may not match state reports). But these should also be flagged for manual verification.

Specific tests that should be done on JSON / CSV API endpoints:

- For each day, district totals in state JSON files (`timeseries-XX.min.json`) should add up to state total given in the same file. This includes the `TT` state code, which represents India, with state-wise totals as data.

- For each day, state totals in the aggregated JSON file (`timeseries.min.json`) should add up to the India total in the same file.

- State totals in state files (e.g., `timeseries-XX.min.json`) should add up to state totals in the  aggregated JSON file (`timeseries.min.json`).

- Similar checks for the CSV files.

- CSV files generated from JSON files (independently) should match the project-provided CSV files.

- JSON files generated from CSV files (independently) should match the project-provided JSON files.


## Specific tools

### Generating daily data

There are two possible pathways, starting either from JSON endpoints or from CSV endpoints. We should have two corresponding scripts, e.g., json2dailycsv and csv2dailycsv. These may involve multiple files, so one way to control source and destination locations is through environment variables, say DAILY_CSV_DIR, JSON_API_DIR, and CSV_API_DIR.

We should also be able to convert to / from any project-specific CSV formats used.

### Conversion between daily data, JSON and CSV formats

The data in JSON and CSV formats should be consistent. To check these, various subsets of the following combinations should be sufficient:

- JSON <-> daily
- daily <-> CSV
- JSON <-> CSV

Ideally, we should try to implement all six.

