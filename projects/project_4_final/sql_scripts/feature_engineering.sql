 -- Feature engineering is the critical process of transforming raw data into meaningful input features 
 -- (variables) that improve machine learning model performance

SET search_path TO public;

 -- new variables
SELECT * 
FROM fact_orders_v5

CREATE TABLE fact_orders_v4 AS 
	SELECT *,
		CASE WHEN avg_review_score <= 2 THEN 1 ELSE 0 END AS low_rating_flag,
		CASE WHEN avg_review_score >= 4 THEN 1 ELSE 0 END AS high_rating_flag,
		CASE WHEN customer_order_number > 1 THEN 1 ELSE 0 END AS repeat_customer_flag, -- første ordre (0) vs har handlet før (1)
		EXTRACT(month FROM order_purchase_timestamp) AS order_month
	FROM (
		SELECT
			*,
			ROW_NUMBER() OVER (
			PARTITION BY customer_id
			ORDER BY order_purchase_timestamp
			) AS customer_order_number -- Hvilket nummer i rekken denne ordren er for kunden
		FROM fact_orders_v3
		) sub;

SELECT COUNT(*), COUNT(DISTINCT order_id)
FROM fact_orders_v;
	
CREATE VIEW aov AS 
		SELECT
			ROUND(SUM(total_payment) / COUNT(DISTINCT order_id), 2) AS avg_order_value
		FROM fact_orders_v4;

	-- monthly revenue
DROP VIEW montly_revenue

CREATE OR REPLACE VIEW montly_revenue AS
		SELECT
			DATE_TRUNC('month', order_purchase_timestamp) AS year_month,
			COUNT(DISTINCT(order_id)) AS order_volume,
			ROUND(SUM(total_order_value), 0) AS total_revenue,
			ROUND(AVG(total_order_value), 2) AS avg_revenue
		FROM fact_orders_v4
		GROUP BY year_month
		ORDER BY year_month;

		SELECT *
		FROM montly_revenue

	-- montly delay rate
DROP VIEW delay_rate_montly

CREATE VIEW delay_rate_montly AS
		SELECT
			DATE_TRUNC('month', order_purchase_timestamp) AS year_month,
	    	ROUND(
				SUM(CASE WHEN delay_flag = 1 THEN 1 ELSE 0 END)::numeric
				/ COUNT(DISTINCT order_id),
				4
				) AS delay_rate
		FROM fact_orders_v5
		GROUP BY year_month
		ORDER BY year_month;

SELECT 
	CORR(total_revenue, delay_rate) AS correlation
FROM montly_revenue
JOIN delay_rate_montly USING(year_month)

	-- AOV pr måned

CREATE OR REPLACE VIEW monthly_aov AS
SELECT
    DATE_TRUNC('month', order_purchase_timestamp) AS year_month,
    SUM(total_order_value) / COUNT(DISTINCT order_id) AS aov
FROM fact_orders_v4
GROUP BY 1
ORDER BY 1;

	-- SAMMENSLÅTT revenue, delay rate, low rating

CREATE OR REPLACE VIEW monthly_kpis AS
SELECT
    DATE_TRUNC('month', order_purchase_timestamp) AS year_month,
    COUNT(DISTINCT order_id) AS total_orders,
    SUM(total_order_value) AS total_revenue,
    SUM(total_order_value) / COUNT(DISTINCT order_id) AS aov,
    AVG(delay_flag::numeric) AS delay_rate,
    AVG(low_rating_flag::numeric) AS low_rating_rate
FROM fact_orders_v4
GROUP BY 1
ORDER BY 1;
	
 -- sjekke datakvalitet

 	-- manglende ratings
SELECT COUNT(avg_review_score)
FROM fact_orders_v4
WHERE avg_review_score IS NULL;

	-- ekstreme delivery_times
SELECT 
	MIN(delivery_time),
	MAX(delivery_time)
FROM fact_orders_v4

SELECT *
FROM montly_revenue