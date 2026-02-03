# Ad vs PSA A/B Testing & Conversion Prediction
This project analyzes a randomized marketing experiment comparing **Ads** vs **Public Service Announcements (PSAs)** to determine which campaign type leads to higher conversions. It also builds a predictive model to estimate conversion likelihood using campaign exposure and timing features.

---

## Objective

- Compare conversion rates between Ad and PSA campaigns  
- Validate results using statistical hypothesis testing  
- Estimate Average Treatment Effect (ATE)  
- Train and evaluate a logistic regression conversion model  
- Deliver actionable insights for marketing decisions

---

## Key Findings

###  A/B Test Results
- Dataset size: **588,101 users**  
- **Ad conversion rate:** 2.55%  
- **PSA conversion rate:** 1.79% (~42% higher for Ads)  
- Statistical testing confirmed significance (**p < 0.001**)  
- ATE estimate (~–0.77 percentage points) with 95% CI fully below zero

### Predictive Modeling
- Built a **logistic regression model** using:
  - Campaign type  
  - Number of ads seen  
  - Peak day/hour of exposure  
- Model achieved **ROC-AUC = 0.854**, indicating strong performance

---

## 🛠 Tech Stack

- **Python** – Data processing & modeling  
- **Pandas / NumPy** – Data manipulation  
- **Scikit-learn** – Modeling & evaluation  
- **Matplotlib / Seaborn** – Visualizations  
- **Jupyter Notebook** – Analysis environment

---
