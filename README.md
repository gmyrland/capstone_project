Capstone Project
================

This repository contains source code and related documents for my capstone project the Data Analytics, Big Data, and Predictive Analytics certificate program at Ryerson University.

## Abstract  
Automotive accidents result in over 30,000 fatalities in the United States annually. The National Automotive Sampling System (NASS) provides a nationally representative sample of police reported collisions and is made available to researchers and the general public.
The research question is to identify and quantify factors which impact the survivability of various crash types (Rear-end, Sideswipe, etc) using R.
Techniques will include web-scraping, xml-parsing and data cleaning of real-world dataset, exploratory analysis to identify relevant factors, feature engineering, and linear regression.

## Dataset
The data using for this project can be found at http://www.nhtsa.gov/NASS, under the link *NASS CDS Case Viewer - XML Viewer (2004-Present)*.

## Project Structure

### 01_abstract
Deliverables are contained on numbered folders.
The abstract briefly describes the research question and methods that are used.

### 02_data_review
The data review deliverable provides an overview of the data and the approach taken.

### 03_final_results
The final deliverable provides an overview of the data, approach, and regression.

### data
Contains any data produced, including the files with case id information and the individual cases.
Individual cases are stored in data/cases as XML in \*.txt files, one case per file, with the filename set to the case id.
Note: Due to the size of the files involved, case data is not shared in the repository.
However, a sqlite database (db.s3db) with processed data is stored in the data folder and can be used in lieu of re-scraping the case files.
Additionally, the cleaned database, df.Rdata, is included.

### doc
Documentation related to the project, including any written instructions provided.

### R
Contains functions to declutter analysis.R.

#### R/cleaning.R
Contains code to build the cleaned dataset.

#### R/databse.R
Contains custom functions for reading and writing from SQLite.

#### R/global.R
Contains several shared functions.

#### R/machine_learning.R
Contains code snippets for machine learning.

#### R/parse.R
Contains function to parse XML into a single data frame.

#### R/partitioning.R
Contains functions to assist with partitioning data.

#### R/scrape.R
Contains all scripting related to web-scraping.

### Analysis.R
The main R source file containing the code for data harvesting and processing.

## Results

The results of the analysis are summarized below.

- 49,345 cases were scraped, with over 700 attributes in 10 tables
- The table were joined to form a dataset with each row representing a collision occupant.
- This resulted in 105,296 rows containing 704 attributes.  However, the mortality was not known for all occupants, so the dataset was filtered to only include occupants with a mortality of 'Fatal' or 'Not Fatal', leaving 84,277 records.
- 20 features were selected for machine learning: compartment\_integrity\_loss, alcohol\_test, posture, age, entrapment, seatbelt\_used, race, alcohol\_present, avoidance\_maneuver, damage\_plane, crash\_config, fire, rollover\_qtr\_turns, posted\_speed, eyewear, airbag\_deployment, seat\_orientation, roadway\_alignment, preimpact\_location, and sex.
- A binary logistic regression classifier was fitted using a 70/30 split for training and test data.
- The confusion matrix for classification of the test cases is shown below.

|   |     0|   1|
|:--|-----:|---:|
|0  | 24163| 196|
|1  |   601| 323|

- Precision: 0.62
- Recall: 0.35
- Accuracy: 0.97
- Optimum cut-off threshold for accuracy was slightly higher than 0.5.
- Area under the ROC curve: 0.955
- Area under the Recall-Precision curve: 0.496
- The generalized classifier performed reasonably well for all crash configurations, with rollover collisions performing the worst.
- The classification was repeated in AWS for all attributes in the cleaned dataset using a 70/30 split. This resulted in the following confusion matrix and similar values for precision, recall, and accuracy.

|   |     0|   1|
|:--|-----:|---:|
|0  | 23866| 141|
|1  |   677| 306|

## Conclusions

Overall, the binary classifier performed well, with an accuracy of 97%.
This is largely due to prevalence of a single class (Non-fatal), however, the model outperforms the naive approach of classifying every response as Non-fatal, which would have an accuracy of only 96.3%.
The model predicts 35% of fatal outcomes correctly, and 62% of cases classified as fatal are indeed fatal.

The model had a very high area under the ROC curve of 0.955.
Typically, this is considered very good performance for a binary classifier.
However, the class imbalance in the response variable manifested itself in a lower area under the RP curve than would be ideal, of 0.496.

Replicating the analysis in AWS for all features of the cleaned dataset showed additional features beyond the 20 chosen did not add substantial value, however, additional feature engineering might result in improved model performance.
