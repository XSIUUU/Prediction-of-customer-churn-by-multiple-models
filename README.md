# Customer Churn Prediction 🚀

## Overview
This project predicts customer churn for a Dutch energy supplier using multiple machine learning models. Churn prediction helps identify customers likely to leave, allowing for better targeted retention strategies.

---

## Table of Contents
1. [Business Problem](#business-problem)
2. [Dataset](#dataset)
3. [Project Steps](#project-steps)
4. [Models Developed](#models-developed)
5. [Results](#results)
6. [Key Findings](#key-findings)
7. [Repository Contents](#repository-contents)

---

## Business Problem 📊
The primary goal is to predict which customers are most likely to churn and provide actionable insights for retention strategies.

---

## Dataset 📑
The dataset consists of **20,000 customers** with 14 variables:
- **Customer Demographics**: Age, Gender, Income
- **Contract Information**: Contract Length, Start Channel
- **Home Attributes**: Home Age, Energy Label
- **Energy Usage**: Yearly Electricity and Gas Consumption
- **Churn Status**: Target variable (1 = churned, 0 = retained)

---

## Project Steps 🛠
1. **Defining the Business Problem**
2. **Research Design**: Hypothesis formulation
3. **Data Preparation**: 
   - Data cleaning, transformations, and handling outliers.
4. **Exploratory Analysis**:
   - Visualizations, statistical tests, and correlation analysis.
5. **Model Development**:
   - Logistic Regression
   - Stepwise Regression
   - CART Trees
   - Bagging, Boosting
   - Random Forest
   - Support Vector Machines (SVM)
6. **Model Validation**:
   - Metrics: Hit Rate, Top-Decile Lift, Gini Coefficient
7. **Managerial Recommendations**

---

## Models Developed ✨
- **Logistic Regression**: Baseline model
- **Tree-Based Models**: CART, Random Forest (Tuned)
- **Ensemble Methods**: Bagging, Boosting
- **Support Vector Machines** (SVM)

---

## Results 📈

| **Model**                | **Hit-Rate** | **TDL** | **Gini** |
|--------------------------|-------------:|--------:|---------:|
| Logistic Regression      | 71.2%       | 1.82    | 0.576    |
| Boosting                 | **76.83%**  | **1.92**| **0.698**|
| Random Forest (Tuned)    | 75.0%       | 1.87    | 0.65     |
| Support Vector Machines  | 74.49%      | 1.79    | 0.63     |

---

## Key Findings 🔍
1. **Top Predictors**:
   - Electricity Usage
   - Contract Type and Length
   - Gas Usage
   - Income
2. Customers with **flexible contracts**, **high energy usage**, and **low income** are more likely to churn.
3. High accuracy and reliability were achieved using ensemble models (e.g., Boosting).

---
## 📁 Repository Contents  

| **File Name**                       | **Description**                                                                 |
|-------------------------------------|-------------------------------------------------------------------------------|
| **`assignment 1 churn prediction.R`** | R script containing code for data cleaning, transformation, exploratory analysis, model building, and validation for churn prediction. |
| **`Churn data.csv`**                 | Dataset used for the churn prediction project, containing relevant customer details.  |
| **`Final Report.pdf`**               | Comprehensive report detailing the methodology, analysis, findings, and model evaluations. |
| **`Assignment requirement Churn prediction.pdf`** | Document outlining the assignment requirements, objectives, and deliverables. |

