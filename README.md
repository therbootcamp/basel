### Key Functions

Install `basel` with 

```
# install basel
devtools::install_github("therbootcamp/basel")
``


`simulate_basel()` contains the code to create data

`write_basel()` is a wrapper function that runs `simulate_basel()` and then saves the results to both an `.RData` file, as well as a comma-separated text file in `inst/extdata/basel.txt` (https://raw.githubusercontent.com/therbootcamp/basel/master/inst/extdata/basel.txt)
