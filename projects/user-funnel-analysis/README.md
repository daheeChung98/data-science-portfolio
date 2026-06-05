# 🚀 User Funnel & Retention Analysis

## 📌 Project Overview

This project analyzes user behavior in an e-commerce platform to understand how users move through the purchase funnel and identify key drop-off points.

The goal is to uncover insights that can improve conversion rates and enhance user experience.

---

## 📊 Dataset

* Source: E-commerce behavior dataset (Kaggle)
* Data includes user interactions such as:

  * view
  * cart
  * purchase
* Contains timestamp, user_id, session, and product/category information

---

## 🎯 Key Questions

* How many users convert from view → cart → purchase?
* Where do users drop off in the funnel?
* How long does it take for users to make a purchase?
* Which product categories perform best?

---

## 🔍 Analysis

### 1. Funnel Analysis

* Calculated conversion rates between:

  * View → Cart
  * Cart → Purchase
* Identified major drop-off points in the funnel

---

### 2. Time Analysis

* Most users complete purchases within a short time window (~3–4 minutes median)
* Indicates impulsive buying behavior for a large portion of users
* A smaller segment takes significantly longer, suggesting deeper consideration

---

### 3. Session Analysis

* Most sessions contain only a few actions
* Suggests low engagement and early drop-off for many users

---

### 4. Category Analysis

* Utility-driven products (e.g., tools) show higher purchase frequency
* Some categories attract high views but low conversions
* Indicates a gap between user interest and actual purchase behavior

---

## 💡 Key Insights

* High drop-off between view and purchase indicates conversion inefficiency
* Users tend to make quick purchase decisions, but engagement is generally low
* Different product categories show significantly different conversion behaviors
* There is a clear mismatch between popular (viewed) and profitable (purchased) categories

---

## 🚀 Business Recommendations

* Improve UX in checkout flow to reduce drop-off
* Focus marketing on high-conversion categories
* Optimize product pages (pricing, reviews, info) for low-conversion categories
* Personalize recommendations for high-intent users

---

## 🛠️ Tools & Skills

* Python (Pandas, Matplotlib)
* Data Analysis
* Funnel Analysis
* Exploratory Data Analysis (EDA)

---

## 📈 Future Work

* Build churn prediction model
* Implement recommendation system
* Apply machine learning to predict conversion probability

