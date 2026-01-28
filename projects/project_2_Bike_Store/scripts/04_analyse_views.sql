-- Månedlig omsetning per butikk
CREATE OR REPLACE VIEW vw_montly_store_revenue AS
SELECT 
	s.store_name,
	EXTRACT(YEAR FROM order_date) AS year,
	EXTRACT(MONTH FROM order_date) AS month,
	SUM(gross_revenue) AS gross_revenue,
	SUM(net_revenue) AS net_revenue
FROM fact_sales f
	LEFT JOIN dim_stores s
	ON f.store_id = s.store_id
GROUP BY store_name, year, month
ORDER BY store_name, year, month;

-- vw_prodct_performance
-- top 10
SELECT 
	p.product_name,
	COUNT(*) AS order_lines,
	SUM(f.quantity) AS total_quantity,
	ROUND(SUM(gross_revenue), 0) AS gross_revenue,
	ROUND(SUM(net_revenue), 0) AS net_revenue
FROM fact_sales f
	LEFT JOIN dim_products p ON f.product_id = p.product_id
GROUP BY p.product_name
ORDER BY net_revenue DESC
LIMIT 10;

-- bottom 10
SELECT 
	p.product_name,
	COUNT(*) AS order_lines,
	SUM(f.quantity) AS total_quantity,
	ROUND(SUM(gross_revenue), 0) AS gross_revenue,
	ROUND(SUM(net_revenue), 0) AS net_revenue
FROM fact_sales f
	LEFT JOIN dim_products p ON f.product_id = p.product_id
GROUP BY p.product_name
ORDER BY net_revenue
LIMIT 10;

SELECT * 
FROM products;

-- Topp 10 produkter per butikk (basert på netto omsetning)
CREATE OR REPLACE VIEW vw_top_products_per_store AS
WITH ranked_products AS (
	SELECT 
		s.store_name,
		p.product_name,
		SUM(f.quantity) AS total_quantity,
		ROUND(SUM(gross_revenue), 0) AS gross_revenue,
		ROUND(SUM(net_revenue), 0) AS net_revenue,
		ROW_NUMBER() OVER (
	      PARTITION BY store_name
	      ORDER BY SUM(net_revenue) DESC
	    ) AS rn
	FROM fact_sales f
	LEFT JOIN dim_products p ON f.product_id = p.product_id
	LEFT JOIN dim_stores s ON f.store_id = s.store_id
	GROUP BY store_name, product_name
)
SELECT *
FROM ranked_products
WHERE rn <= 10


-- Slow movers: lav omsetning + høyt lager
WITH product_store_sales AS (
  SELECT
    f.store_id,
    f.product_id,
    SUM(f.net_revenue)        AS net_revenue,
    COALESCE(ds.quantity, 0)  AS stock_quantity
  FROM fact_sales f
  LEFT JOIN dim_stocks ds
    ON f.store_id  = ds.store_id
   AND f.product_id = ds.product_id
  GROUP BY
    f.store_id,
    f.product_id,
    ds.quantity
),
thresholds AS (
  SELECT
    PERCENTILE_CONT(0.2) WITHIN GROUP (ORDER BY net_revenue) AS revenue_p20,
    PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY stock_quantity) AS stock_median
  FROM product_store_sales
)
SELECT
  s.store_name,
  p.product_name,
  ps.net_revenue,
  ps.stock_quantity
FROM product_store_sales ps
CROSS JOIN thresholds t
JOIN dim_stores s   ON ps.store_id = s.store_id
JOIN dim_products p ON ps.product_id = p.product_id
WHERE
  ps.net_revenue <= t.revenue_p20
  AND ps.stock_quantity > t.stock_median
ORDER BY
  s.store_name,
  ps.net_revenue;










	