Capstone Project
================

This repository contains source code and related documents for my capstone project the Data Analytics, Big Data, and Predictive Analytics certificate program at Ryerson University.

## Abstract  
Automotive accidents result in over 30,000 fatalities in the United States annually. The National Automotive Sampling System (NASS) provides a nationally representative sample of police reported collisions and is made available to researchers and the general public.
The research question is to identify and quantify factors which impact the survivability of various crash types (Rear-end, Sideswipe, etc) using R, and potentially create a web app using the shiny package to predict survivability for given inputs using regression.
Techniques will include web-scraping, xml-parsing and data cleaning of real-world dataset, exploratory analysis to identify relevant factors, feature engineering, and linear regression.

## Project Structure

### 01_abstract
Deliverables are contained on numbered folders.
The abstract briefly describes the research question and methods that are used.

### 02_data_review
The data review deliverable provides an overview of the data and the approach taken.

### data
Contains any data produced, including the files with case id information and the individual cases.
Individual cases are stored as XML in \*.txt files, one case per file, with the filename set to the case id.
Note: Due to the size of the files involved, case data is not shared in the repository.

### doc
Documentation related to the project, including any written instructions provided.

### R
Contains functions to declutter analysis.R.

#### R/scrape.R
Contains all scripting related to web-scraping.

### Analysis.R
The main R source file containing the code for data harvesting and analysis.
