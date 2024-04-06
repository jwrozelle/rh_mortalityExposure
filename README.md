## Code for mortality exposure paper

This is the code that was used to generate the analysis for the article titled " Fatalism or vigilance?  
Familial death & subsequent use of maternal health services in Malawi ", submitted to the Journal of World Development.

In order to use this code, you must download all Malawi data from the 2015-2016 survey, downloaded in Stata format.

You must also create a .Renviron file in the top level of the project directory

It should include a system variable with the path to the location of the unzipped STATA files, as follows:

```
DATA_DIRECTORY = "<path/to/unzipped/DHS/data>"
```

The original codebase was written to accommodate analysis of multiple countries simultaneously, and more data can be added to the lists in `readData.R`. However, be warned that the cleaning functions should be examined if using any data other than Malawi 2014-2015 DHS data.


R code is in .qmd format for readability. The sequence of files to execute can be found in `_quarto.yml`. Assuming your data is organized and .Renviron is set up, you can also run: 

```console
quarto render --to html
```

Be aware that the code may take several hours to run in full. I recommend running code in the individual .qmd documents instead.

Code should be run in the order of the `_quarto.yml` document, but models, tables, and figures featured in the manuscript are produced in the `RandR1_TablesFigures.qmd` document.
