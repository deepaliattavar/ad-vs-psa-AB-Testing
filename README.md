# ad-vs-psa-AB-Testing
This project evaluates the effectiveness of sales-driven advertisements (Ads) versus Public Service Announcements (PSAs) in driving user conversions. Using A/B testing and predictive modeling, we measure causal impact and build a model to predict conversion likelihood.

A/B Test Results
Dataset of 588,101 users from a randomized marketing experiment
Ad conversion rate: 2.55%
PSA conversion rate: 1.79%
Ads achieved ~42% higher conversion than PSAs
Statistical testing confirmed the difference was significant (p < 0.001)

Causal Impact
Estimated Average Treatment Effect (ATE) of PSA vs Ad ≈ –0.77 percentage points
95% confidence interval fully below zero → PSAs reduce conversions relative to Ads

Predictive Modeling
Built a logistic regression model using:
Campaign type
Number of ads seen
Peak day and hour of exposure
Model performance: ROC-AUC = 0.854, indicating strong predictive power
