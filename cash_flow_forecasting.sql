-- ============================================================================
-- CASH FLOW FORECASTING & BUDGET VARIANCE ANALYSIS
-- SQL queries for financial forecasting and variance analysis
-- ============================================================================

-- 1. CASH FLOW SUMMARY STATISTICS
-- Monthly cash flow overview
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(cash_flow, 2) as 'Actual Cash Flow',
    ROUND(budgeted_cash_flow, 2) as 'Budgeted Cash Flow',
    ROUND((cash_flow - budgeted_cash_flow), 2) as 'Variance',
    ROUND(((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100), 2) as 'Variance %'
FROM cash_flows
ORDER BY month DESC;

-- 2. BUDGET VARIANCE ANALYSIS
-- Identify periods with significant variances
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(cash_flow, 2) as 'Actual',
    ROUND(budgeted_cash_flow, 2) as 'Budget',
    ROUND(ABS(cash_flow - budgeted_cash_flow), 2) as 'Abs Variance',
    ROUND(((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100), 2) as 'Variance %',
    CASE 
        WHEN ABS((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100) > 10 THEN 'High'
        WHEN ABS((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100) > 5 THEN 'Medium'
        ELSE 'Low'
    END as 'Risk Level'
FROM cash_flows
WHERE month >= DATE_SUB(CURDATE(), INTERVAL 12 MONTH)
ORDER BY 'Variance %' DESC;

-- 3. CASH FLOW TREND ANALYSIS
-- Year-over-year comparison
SELECT 
    MONTH(month) as 'Month Number',
    DATE_FORMAT(month, '%b') as 'Month Name',
    YEAR(month) as 'Year',
    ROUND(cash_flow, 2) as 'Cash Flow',
    ROUND(LAG(cash_flow) OVER (PARTITION BY MONTH(month) ORDER BY YEAR(month)), 2) as 'Prior Year',
    ROUND(((cash_flow - LAG(cash_flow) OVER (PARTITION BY MONTH(month) ORDER BY YEAR(month))) / 
           LAG(cash_flow) OVER (PARTITION BY MONTH(month) ORDER BY YEAR(month)) * 100), 2) as 'YoY %'
FROM cash_flows
ORDER BY YEAR(month) DESC, MONTH(month);

-- 4. CUMULATIVE CASH FLOW TRACKING
-- Cumulative performance vs budget
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(SUM(cash_flow) OVER (ORDER BY month), 2) as 'Cumulative Actual',
    ROUND(SUM(budgeted_cash_flow) OVER (ORDER BY month), 2) as 'Cumulative Budget',
    ROUND((SUM(cash_flow) OVER (ORDER BY month) - 
           SUM(budgeted_cash_flow) OVER (ORDER BY month)), 2) as 'Cumulative Variance'
FROM cash_flows
WHERE month >= DATE_SUB(CURDATE(), INTERVAL 12 MONTH)
ORDER BY month;

-- 5. SEASONAL PATTERN ANALYSIS
-- Identify seasonal trends
SELECT 
    MONTH(month) as 'Month Number',
    DATE_FORMAT(month, '%b') as 'Month Name',
    ROUND(AVG(cash_flow), 2) as 'Average Cash Flow',
    ROUND(MIN(cash_flow), 2) as 'Min',
    ROUND(MAX(cash_flow), 2) as 'Max',
    ROUND(STDDEV(cash_flow), 2) as 'Std Dev',
    COUNT(*) as 'Years of Data'
FROM cash_flows
GROUP BY MONTH(month), DATE_FORMAT(month, '%b')
ORDER BY MONTH(month);

-- 6. MOVING AVERAGE FORECAST
-- 3-month and 12-month moving averages
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(cash_flow, 2) as 'Actual',
    ROUND(AVG(cash_flow) OVER (ORDER BY month ROWS BETWEEN 2 PRECEDING AND CURRENT ROW), 2) as 'MA3',
    ROUND(AVG(cash_flow) OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW), 2) as 'MA12',
    CASE 
        WHEN cash_flow > AVG(cash_flow) OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) 
        THEN 'Above Trend'
        ELSE 'Below Trend'
    END as 'Position'
FROM cash_flows
WHERE month >= DATE_SUB(CURDATE(), INTERVAL 24 MONTH)
ORDER BY month DESC;

-- 7. VARIANCE ACCURACY METRICS
-- Calculate forecasting accuracy improvement
SELECT 
    YEAR(month) as 'Year',
    ROUND(AVG(ABS((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100)), 2) as 'Mean Abs Variance %',
    ROUND(STDDEV(ABS((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100)), 2) as 'Std Dev Variance %',
    ROUND(100 - AVG(ABS((cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100)), 2) as 'Accuracy %',
    COUNT(*) as 'Months'
FROM cash_flows
GROUP BY YEAR(month)
ORDER BY YEAR(month) DESC;

-- 8. CASH FLOW VOLATILITY ANALYSIS
-- Measure cash flow stability
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(cash_flow, 2) as 'Cash Flow',
    ROUND(cash_flow - LAG(cash_flow) OVER (ORDER BY month), 2) as 'MoM Change',
    ROUND((cash_flow - LAG(cash_flow) OVER (ORDER BY month)) / 
          LAG(cash_flow) OVER (ORDER BY month) * 100, 2) as 'MoM % Change',
    ROUND(STDDEV(cash_flow) OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW), 2) as 'Rolling 12M Std Dev'
FROM cash_flows
WHERE month >= DATE_SUB(CURDATE(), INTERVAL 24 MONTH)
ORDER BY month DESC;

-- 9. FORECASTING ACCURACY BY QUARTER
-- Quarterly analysis
SELECT 
    YEAR(month) as 'Year',
    QUARTER(month) as 'Quarter',
    ROUND(SUM(cash_flow), 2) as 'Actual',
    ROUND(SUM(budgeted_cash_flow), 2) as 'Budget',
    ROUND(SUM(cash_flow - budgeted_cash_flow), 2) as 'Variance',
    ROUND(100 - ABS(SUM(cash_flow - budgeted_cash_flow)) / SUM(budgeted_cash_flow) * 100, 2) as 'Accuracy %'
FROM cash_flows
GROUP BY YEAR(month), QUARTER(month)
ORDER BY YEAR(month) DESC, QUARTER(month) DESC;

-- 10. CASH FLOW CLASSIFICATION
-- Categorize cash flows by performance
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(cash_flow, 2) as 'Cash Flow',
    ROUND(budgeted_cash_flow, 2) as 'Budget',
    ROUND(AVG(budgeted_cash_flow) OVER (), 2) as 'Avg Budget',
    CASE 
        WHEN cash_flow > PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY cash_flow) THEN 'High'
        WHEN cash_flow > PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY cash_flow) THEN 'Medium'
        ELSE 'Low'
    END as 'Performance Level'
FROM cash_flows
WHERE month >= DATE_SUB(CURDATE(), INTERVAL 12 MONTH)
ORDER BY month DESC;

-- 11. FORECAST EVALUATION METRICS
-- Compare forecasted vs actual for accuracy assessment
SELECT 
    ROUND(AVG(ABS(cash_flow - forecasted_cash_flow)), 2) as 'MAE (Mean Absolute Error)',
    ROUND(SQRT(AVG(POW(cash_flow - forecasted_cash_flow, 2))), 2) as 'RMSE (Root Mean Squared Error)',
    ROUND(1 - SUM(POW(cash_flow - forecasted_cash_flow, 2)) / 
              SUM(POW(cash_flow - AVG(cash_flow), 2)), 4) as 'R² Score',
    ROUND(AVG(ABS((cash_flow - forecasted_cash_flow) / cash_flow * 100)), 2) as 'MAPE %',
    COUNT(*) as 'Sample Size'
FROM cash_flow_forecasts
WHERE forecast_date >= DATE_SUB(CURDATE(), INTERVAL 12 MONTH);

-- 12. BUDGET VARIANCE IMPROVEMENT TRACKING
-- Track improvement in forecast accuracy over time
SELECT 
    DATE_FORMAT(month, '%Y-%m') as 'Month',
    ROUND(ABS(cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100, 2) as 'Variance %',
    ROUND(AVG(ABS(cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100) 
          OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW), 2) as '12M Avg Variance %',
    ROUND(100 - AVG(ABS(cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100) 
              OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW), 2) as '12M Accuracy %',
    CASE 
        WHEN ROUND(100 - AVG(ABS(cash_flow - budgeted_cash_flow) / budgeted_cash_flow * 100) 
                  OVER (ORDER BY month ROWS BETWEEN 11 PRECEDING AND CURRENT ROW), 2) > 85 
        THEN 'Improved'
        ELSE 'Monitor'
    END as 'Status'
FROM cash_flows
WHERE month >= DATE_SUB(CURDATE(), INTERVAL 24 MONTH)
ORDER BY month DESC;

-- ============================================================================
-- END OF CASH FLOW FORECASTING QUERIES
-- ============================================================================
