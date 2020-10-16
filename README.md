# ST558Project2
Repository for Project 2 in ST 558

**Purpose:**  
This repo contains analysis on bike sharing data on separate weekdays between 2011 to 2012. The purpose of the analysis was to bring in the data, filter it by unique days of the week, split it into training and test sets, do some statistical summaries on the data and produce some plots to visualize the data on the training set, and create two models to predict the response variable, cnt. Cnt represents the count of bike rentals for each day based on several variables, such as weather, temperature, whether the day is a working day or holiday, wind speed, humidity, and season. The two models used to fit this data were a regression tree model and a boosted tree model. Details about the fit were printed in the analysis as well, along with details about the tuning parameters used in the final model. The fit of the model was judged by statistics such as RMSE, MAE, and R-squared values to see which model predicted the cnt variable more accurately. To get all this analysis for each day of the week, an automation was done within an R Markdown document to replicate the results for each day. The sub-documents are listed below.

**Links for Each Day's Analysis of the Bike Sharing Data:**  
The analysis for [Monday is available here](MondayAnalysis.md)  
The analysis for [Tuesday is available here](TuesdayAnalysis.md)  
The analysis for [Wednesday is available here](WednesdayAnalysis.md)  
The analysis for [Thursday is available here](ThursdayAnalysis.md)  
The analysis for [Friday is available here](FridayAnalysis.md)  
The analysis for [Saturday is available here](SaturdayAnalysis.md)  
The analysis for [Sunday is available here](SundayAnalysis.md)  

**Required Packages for Analysis:**  
The following packages were called with the `library()` function within the R Markdown file: `tidyverse, caret, knitr, rpart, rattle, forcats, rmarkdown`.

**Code Used to Automate Replication Process:**  
```{r Parameters, eval=FALSE, include=FALSE}  
dayNames <- unique(bike$weekday)  
output_file <- paste0(dayNames, "Analysis.md")  
params = lapply(dayNames, FUN = function(x){list(day = x)})  
reports <- tibble(output_file, params)  
```  

```{r render, eval=FALSE, include=FALSE}
apply(reports, MARGIN = 1, 
            FUN = function(x){
                render(input = "Project_2.Rmd", output_file = x[[1]], params = x[[2]])
                })
```
