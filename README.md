### IMDB Rating Score Prediction

#Data cleaning and linear regression models used to predict imdb rating scores

The purpose of this analysis is to provide movie theaters and television networks infomation to better slot their movies/shows along with figuring out how many showings they should provide. The linear regression formula created gives the theaters and networks a "calculator" they can use to predict how good of a score a movie/show gets. The better the score, the better the indication they have to show it at better times and have more showings. This analysis was accomplished by downloading an imdb dataset from Kaggle, removing duplicates in Microsoft Excel, implemening pivot tables to create categorical binary columns for certain features, and running linear regression models in RStudio.

https://www.kaggle.com/datasets/mazenramadan/imdb-most-popular-films-and-series

##Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

1. Download all csv files, workbooks, and r script
2. Open all csv files and workbooks with Microsft Excel
3. Open imdb.r with RStudio
4. Within RStudio, set working directory to wherever you completed step 1

##Prerequisites
1. R 3.3.2
2. RStudio 1.0.135
3. Packages to install: dplyr, caret, metrics.

##Breakdown
1. Kaggle_IMDB.xlsx contains original dataset, removed duplicate dataset, and pivot tables to produce all csv files produced for imdb.r
2. imdb.r uses imput of all csv files and produces Predicted.csv, Predicted2.csv, Predicted3.csv along with linear regression formulas, prediction accuracies, min-max accuracies, mean absolute percentage errors, mean absloute errors, root mean squared errors, and mean squared errors. 
