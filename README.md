[![Build Status](https://travis-ci.com/belzaina/housing.svg?token=speK6yitTLXioyfS2PKH&branch=master)](https://travis-ci.com/belzaina/housing)     
     
[![SHINYAPPS.IO Status](https://img.shields.io/badge/shinyapps.io-up-green?style=for-the-badge)](https://belgadazainab.shinyapps.io/housing/)

# Penalized Logistic Tree Regression (PLTR) - Housing Dataset (shinyapp)

In this project we implement the Penalized Logistic Tree Regression (PLTR) algorithm described by Dumitrescu et al. (2020)<sup id="a1">[1](#f1)</sup>. According to the authors:

> ...(PLTR) uses information from decision trees to improve the performance of logistic regression. Formally, rules extracted from various short-depth decision trees built with pairs of predictive variables are used as predictors in a penalised logistic regression model. PLTR allows us to capture non-linear effects that can arise in credit scoring data while preserving the intrinsic interpretability of the logistic regression model.

We compare the performance of PLTR to that of:

  - Random Forest
  - SVM with radial kernel
  - SVM with polynomial kernel
  - Linear Logistic Regression
  - Penalized Linear Logistic Regression (RIDGE)
  - Penalized Linear Logistic Regression (LASSO)
  - Penalized Linear Logistic Regression (Adaptive LASSO)
  - Penalized Non-Linear Logistic Regression<sup id="a2">[2](#f2)</sup> (RIDGE)
  - Penalized Non-Linear Logistic Regression<sup id="a2">[2](#f2)</sup> (LASSO)
  - Penalized Non-Linear Logistic Regression<sup id="a2">[2](#f2)</sup> (Adaptive LASSO)
  

### Useful Links

  - Shiny Application: https://belgadazainab.shinyapps.io/housing/
  - Our PLTR implementation code: https://github.com/belzaina/housing/blob/master/scripts/pltr_learner.R

### Robustness Check

Following Dumitrescu et al. (2020)<sup id="a1">[1](#f1)</sup>, we use the so called N x 2-fold cross-validation of Dietterich (1998) which involves randomly dividing the dataset into two sub-samples of equal size:
  - The first (second) part is used to build the model;
  - The second (first) part is used for evaluation.

This procedure is repeated N times, and the evaluation metrics are averaged. We set N = 5 for computational reasons. Our results are as follow (sorted by AUC):


| LEARNING ALGORITHM | AUC | GINI | PCC | BS | KS |
|--------------------|-----|------|-----|----|----|
| RANDOM FOREST | 0.955 | 0.910 | 0.912 | 0.0668 | 0.785 |
| PLTR (RIDGE) | 0.911 | 0.823 | 0.890 | 0.0817 | 0.689 |
| PLTR (LASSO) | 0.911 | 0.821 | 0.891 | 0.0810 | 0.689 |
| PLTR (ADAPTIVE LASSO) | 0.907 | 0.815 | 0.889 | 0.0826 | 0.687 |
| SVM (RADIAL) | 0.823 | 0.646 | 0.812 | 0.0918 | 0.597 |
| NON-LINEAR LR (LASSO) | 0.821 | 0.642 | 0.857 | 0.110 | 0.498 |
| NON-LINEAR LR (RIDGE) | 0.819 | 0.638 | 0.852 | 0.112 | 0.494 |
| LINEAR LR | 0.792 | 0.583 | 0.836 | 0.123 | 0.447 |
| LINEAR LR (RIDGE) | 0.792 | 0.583 | 0.834 | 0.123 | 0.450 |
| LINEAR LR (LASSO) | 0.791 | 0.583 | 0.836 | 0.123 | 0.443 |
| NON-LINEAR LR (ADAPTIVE LASSO) | 0.789 | 0.579 | 0.844 | 0.119 | 0.447 |
| LINEAR LR (ADAPTIVE LASSO) | 0.786 | 0.573 | 0.834 | 0.124 | 0.425 |
| SVM (POLYNOMIAL) | 0.783 | 0.566 | 0.839 | 0.111 | 0.499 |


### About

This project was conducted by [ZAINAB BELGADA](https://fr.linkedin.com/in/za%C3%AFnab-belgada-b1175b1ab) <sup id="a3">[3](#f3)</sup> and [ZINSOU DAMIEN MEZONLIN](https://www.linkedin.com/in/zinsou-damien-m-7073861b6/) <sup id="a4">[4](#f4)</sup> under the supervision of Professor [TOKPAVI SESSI](http://www.leo-univ-orleans.fr/fr/membres/#sessi.tokpavi@univ-orleans.fr) <sup id="a5">[5](#f5)</sup> for Autumn 2020 [Master ESA](https://www.univ-orleans.fr/deg/masters/ESA/) Big Data Analytics class at [University of Orléans](https://www.univ-orleans.fr/fr/univ).


![Master ESA](www/Logo-couleur-MasterESA-RVB.jpg "Master ESA")     

     
### Footnotes:
     

<b id="f1">1. </b> Elena Dumitrescuy, Sullivan Hué, Christophe Hurlinx, and Sessi Tokpavi "Machine Learning or Econometrics for Credit Scoring: Let's Get the Best of Both Worlds", October 15, 2020. [↩](#a1)    
     
<b id="f2">2. </b> Includes as additional variables quadratic and interaction terms. [↩](#a2)    
     
<b id="f3">3. </b> zainab.belgada@etu.univ-orleans.fr [↩](#a3)     
     
<b id="f4">4. </b> zinsou.mezonlin@etu.univ-orleans.fr [↩](#a4)     
     
<b id="f5">5. </b> sessi.tokpavi@univorleans.fr [↩](#a5)     

