# Advanced-Statistical-Analysis-of-Heart-Disease-in-Pakistan

[![R Version](https://img.shields.io/badge/R-4.3.1+-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## 📌 Project Overview
Comprehensive statistical analysis of cardiovascular disease patterns in Pakistan using real-world clinical data. Identified key risk factors, mortality predictors, and patient clusters to enable data-driven healthcare interventions.

**Data Source:**  
[Heart Patients in Pakistan Dataset](https://opendata.com.pk/dataset/heart-patients-in-pakistan/resource/70aec9b8-7674-492f-a390-1bd72666744f)

**Supervisors:**  
Dr. Faridoon Khan & Sir Irfan ul Haq  
*Department of Creative Technologies, Air University Islamabad*

**Team:**
- Ayema Amir (232543)
- Malik Usama Arif (233105)
- Muhammad Abdullah Younas (232534)
- Halima Hanif (232553)

## 🧠 Key Objectives
1. Identify demographic/clinical predictors of heart disease severity
2. Develop mortality risk stratification models
3. Analyze comorbidity clusters (diabetes, hypertension, renal dysfunction)
4. Evaluate gender-specific risk patterns
5. Create clinical decision support tools

## ⚙️ Technical Implementation
### 🔧 Tools & Technologies
- **Programming Language:** R
- **Key Packages:** 
  - `dplyr`, `tidyr` (Data Wrangling)
  - `ggplot2`, `plotly` (Visualization)
  - `randomForest`, `nnet` (ML Modeling)
  - `shiny`, `shinydashboard` (Interactive Dashboard)
  - `cluster` (Patient Segmentation)

### 📊 Methodology
1. **Data Preprocessing**
   - Symptom standardization
   - Missing value imputation
   - Feature engineering

2. **Statistical Analysis**
   - Shapiro-Wilk normality tests
   - Chi-square associations
   - Mann-Whitney U/Kruskal-Wallis tests
   - Spearman correlations

3. **Predictive Modeling**
   - Severity Classification (Multinomial Logistic Regression + Random Forest)
   - Mortality Prediction (Logistic Regression with stepAIC)
   - Risk Stratification (K-Means Clustering + PCA)

4. **Visual Analytics**
   - Interactive Shiny dashboard
   - Cluster visualization
   - Risk score distributions

## 📂 Repository Structure
