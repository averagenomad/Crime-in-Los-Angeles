# **Prediction of Crime Occurrence by Category**  

## **Overview**  
This project aims to predict the occurrence of crime by category in the City of Los Angeles using crime data from the LAPD and socio-economic status (SES) data from the US Census Bureau. The analysis combines supervised and unsupervised machine learning methods to identify key drivers of crime and evaluate the best-performing models for prediction.  

**Key Models Used:**  
- Backward Stepwise Regression  
- Multinomial Logistic Regression  
- Random Forest (best performer)  
- Neural Networks  
- K-means Clustering  
- Ensemble Models  

The study concludes that **Random Forest** achieved the highest predictive accuracy (48.9%), with weapon type, victim age, and victim ethnicity being significant predictors.  

---

## **Repository Structure**  

├── Code/
│ ├── Neural, Ensemble, K-means.R
│ └── Regression, Logit, RF, Corr.R
│
├── Output/
│ ├── 486 FINAL PROJECT PPT_JA...
│ └── SPE 486 Final Paper_Team Crispy Crime Donought.pdf
│
└── Tableau/
└── 486 Final project.twbx


- **Code/** – R scripts showing how models were trained. (Note: These scripts require data files that are not included in this repository.)  
- **Output/** – Final deliverables: research paper and presentation.  
- **Tableau/** – Tableau workbook with visualizations.  

---

## **Data Sources**  
- **Crime Data:** Los Angeles Police Department (LAPD) Crime Dataset (2011–2017)  
- **Socio-Economic Data:** US Census Bureau data (poverty, income, education, migration, etc.)  

These datasets were merged at the ZIP code level to form the modeling dataset.  

> ⚠️ **Data is not included in this repository** due to size and privacy constraints. Users must obtain the LAPD crime data and US Census SES data separately and preprocess it to replicate the analysis.  

---

## **Methodology**  

1. **Exploratory Data Analysis (EDA):**  
   - Mapped crime frequency by ZIP code.  
   - K-means clustering for understanding spatial and categorical patterns.  
   - Correlation analysis of SES variables and crime frequency.  

2. **Supervised Models:**  
   - Stepwise Regression (predict total crime counts).  
   - Multinomial Logistic Regression & Random Forest (predict crime by category).  

3. **Unsupervised Models:**  
   - Neural Networks (single hidden layer).  
   - Ensemble models combining multiple learners.  

---

## **Results**  
- **Random Forest** performed best with ~49% accuracy and an AUC of 62.2%.  
- Weapon type, victim age, victim ethnicity, and area location were the most important predictors.  
- Policy recommendations include focusing on weapon control, targeting high-crime areas, and considering migration and demographics in crime prevention strategies.  

---

## **How to Use This Repository**  
- Review the **Output/** folder for the final research paper and presentation.  
- Explore the **Code/** folder for R scripts illustrating the modeling process.  
- Open the **Tableau/** folder to view interactive visualizations (requires Tableau).  

> ⚠️ The R scripts will not run as-is because data and cleaning scripts are not included.  

---

## **Tech Stack**  
- **Languages:** R  
- **Packages:** `caret`, `nnet`, `randomForest`, `glmnet`, `kmeans`  
- **Visualization:** Tableau  

---

## **License**  
This project is available for academic and educational purposes only.  
