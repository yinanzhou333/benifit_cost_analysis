# Benefit-Cost Analysis & Forecasting Projects

A collection of decision science and financial forecasting models for capital project evaluation and cash flow prediction.

---

## 1. Benefit-Cost Analysis

Simulation-based decision model using Monte Carlo techniques to select the best capital project among alternatives.

### Overview

This project implements a Monte Carlo simulation framework to evaluate and compare capital project alternatives using benefit-cost analysis. The analysis compares the ratio of total projected benefits to total projected costs for each project to determine the most favorable investment.

### Project Context

JET Corporation is evaluating two dam construction projects:
- **Dam #1** (southwest Georgia)
- **Dam #2** (North Carolina)

Six benefit categories identified: improved navigation, hydroelectric power, fish and wildlife, recreation, flood control, and commercial development.

### Methodology

**Simulation Approach:**
- 10,000 Monte Carlo iterations
- Triangular distribution for benefit and cost estimation
- Three estimates per category: minimum, mode (most likely), maximum

**Analysis Steps:**
1. Generated random variates using triangular distribution
2. Calculated benefit-cost ratios for each iteration
3. Fitted theoretical distributions (Normal, Gamma, Log-normal) to results
4. Compared probability distributions between projects

### Key Results

**Recommendation: Dam #1**

**Supporting Statistics:**
- Higher mean and median benefit-cost ratios
- Lower variance and standard deviation (more stable)
- Better performance in extreme scenarios
- Probability of superiority: **55.16%**

### Implementation

- Simulation code in R (`benefit_cost_analysis_solution.R`)
- Distribution fitting and probability analysis
- Risk assessment and recommendation framework

---

## 2. Cash Flow Forecasting

Developed a time-series forecasting model using Python to predict monthly cash flows and analyze budget variance patterns.

### Overview

This module implements advanced time-series forecasting techniques to predict monthly cash flows. Multiple forecasting methods are combined to identify seasonal patterns, trends, and budget variance metrics for improved financial planning.

### Forecasting Methods

- **Exponential Smoothing** - Adaptive forecasting with seasonal adjustment
- **Autoregressive (AR) Models** - Temporal pattern capture with lagged features
- **Moving Averages** - Trend smoothing with multiple window sizes
- **Seasonal Decomposition** - Separation of trend, seasonal, and residual components

### Key Features

**Python Module** (`cash_flow_forecasting.py`):
- CashFlowForecaster class with comprehensive methods
- Multiple forecasting algorithms
- Budget variance analysis and tracking
- 12-month forward forecasting
- Automatic seasonal pattern detection

**SQL Analytics** (`cash_flow_forecasting.sql`):
- Budget variance analysis queries
- Trend analysis across time periods
- Seasonal pattern identification
- Forecast accuracy metrics
- Risk classification for variance

### Data Format

**Required CSV Structure:**
```
month, actual_cash_flow, budgeted_cash_flow
2023-01, 43555.52, 42677.53
2023-02, 39893.71, 38564.03
...
```

**Required Columns:**
- `month` - Date in YYYY-MM format
- `actual_cash_flow` - Realized monthly cash flow
- `budgeted_cash_flow` - Planned/budgeted cash flow

### Running the Model

```bash
# Install dependencies
pip install pandas numpy scikit-learn statsmodels

# Run forecasting
python cash_flow_forecasting.py
```

### Example Usage

```python
from cash_flow_forecasting import CashFlowForecaster
import pandas as pd

# Load data
df = pd.read_csv('cash_flows.csv')

# Initialize and run forecasting
forecaster = CashFlowForecaster(df, train_ratio=0.8)

# Generate forecast for next 12 months
forecast = forecaster.forecast_next_n(n=12, method='exp_smooth')

# Analyze budget variance
variance_stats = forecaster.variance_analysis()

# Run complete report
report = forecaster.generate_report()
```

### Output Metrics

- RMSE (Root Mean Squared Error)
- MAE (Mean Absolute Error)
- R² Score
- Budget Variance Accuracy
- Seasonal patterns and trend direction

---

## 3. Inventory Management Decision Model

Developed and implemented a decision model to help optimize inventory management decisions through sensitivity analysis.

### Approach

Conducted what-if analyses using two-way tables in Excel to study the sensitivity of total cost to changes in model parameters.

### Key Finding

The optimization analysis identified the optimal order quantity that minimizes total inventory cost, balancing ordering costs against holding costs.

### Visualization

The Total Cost vs Order Quantity plot shows:
- Clear minimum point at optimal order quantity
- Sensitivity to parameter changes
- Trade-off between ordering and holding costs

---

## Supporting Figures

### Benefit-Cost Analysis Figures

**Figure 1: Benefit and Cost Categories**
![image](https://github.com/user-attachments/assets/34d7632c-66e5-40dc-a74b-7933b6e31236)

**Figure 2: Triangular Distribution Implementation**
![image](https://github.com/user-attachments/assets/c2f93b62-d2c3-43b6-8f9d-e009a24e5fc3)

**Figure 3: Distribution Fitting Analysis**
![image](https://github.com/user-attachments/assets/d625467c-4bcf-42c7-aacc-55226235fef7)

**Figure 4: Probability Comparison Results**
![image](https://github.com/user-attachments/assets/ff4706e4-aa48-4d94-a83b-052202d284af)

**Figure 5: Statistical Summary**
![image](https://github.com/user-attachments/assets/0a88863f-9179-40f8-ad93-651a5a350ab2)

**Figure 6: Distribution Parameters**
![image](https://github.com/user-attachments/assets/e30cd285-0e84-4a7c-bfc8-7d713939e230)

**Figure 7: Threshold Probability Analysis**
![image](https://github.com/user-attachments/assets/a1b171cb-6d40-4e16-91fd-7d0357b73c80)

**Figure 8: Final Recommendation Summary**
![image](https://github.com/user-attachments/assets/8f0df614-4232-4ed0-a926-d963e0ca4ee3)
