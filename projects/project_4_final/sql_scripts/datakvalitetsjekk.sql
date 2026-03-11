SET search_path TO public;

SELECT COUNT(*)
FROM facts_orders

SELECT *
FROM dim_customers

SELECT 
    schemaname,
    relname AS table_name,
    n_live_tup AS row_estimate
FROM pg_stat_user_tables
WHERE schemaname = 'public'
ORDER BY n_live_tup DESC;

SELECT COUNT(DISTINCT(order_id))
FROM dim_sellers	

-- SJEKKE DATAKVALITET
-- Mangler noen tabeller ordre 

SELECT COUNT(*) 
FROM dim_order_items oi
LEFT JOIN facts_orders o
ON oi.order_id = o.order_id
WHERE o.order_id IS NULL;


-- Referanseintegritet (orphan checks)

SELECT 'order_items → orders' AS check_type, COUNT(*) AS missing
FROM dim_order_items oi
LEFT JOIN facts_orders o ON oi.order_id = o.order_id
WHERE o.order_id IS NULL

UNION ALL

SELECT 'reviews → orders', COUNT(*)
FROM dim_order_reviews r
LEFT JOIN facts_orders o ON r.order_id = o.order_id
WHERE o.order_id IS NULL

UNION ALL

SELECT 'orders → customers', COUNT(*)
FROM facts_orders o
LEFT JOIN dim_customers c ON o.customer_id = c.customer_id
WHERE c.customer_id IS NULL

UNION ALL

SELECT 'order_items → products', COUNT(*)
FROM dim_order_items oi
LEFT JOIN dim_products p ON oi.product_id = p.product_id
WHERE p.product_id IS NULL

UNION ALL

SELECT 'order_items → sellers', COUNT(*)
FROM dim_order_items oi
LEFT JOIN dim_sellers s ON oi.seller_id = s.seller_id
WHERE s.seller_id IS NULL;

-- DUPLIKATKONTROLL

SELECT review_id, COUNT(*)
FROM public.dim_order_reviews
GROUP BY review_id
HAVING COUNT(*) > 1;

SELECT order_id, COUNT(*)
FROM facts_orders
GROUP BY order_id
HAVING COUNT(*) > 1;

-- Kritiske NULL-sjekker

SELECT
	SUM(CASE WHEN order_id IS NULL THEN 1 ELSE 0 END) AS null_order_id,
	SUM(CASE WHEN customer_id IS NULL THEN 1 ELSE 0 END) AS null_customer_id
FROM facts_orders

-- KPI-sanity check

SELECT
	COUNT(DISTINCT o.order_id) AS total_orders,
	SUM(oi.price + oi.freight_value) AS total_revenue
FROM facts_orders o
JOIN dim_order_items oi
ON o.order_id = oi.order_id


-- Har orders én rad per ordre? 
SELECT 
    COUNT(*) AS total_rows,
    COUNT(DISTINCT order_id) AS distinct_orders
FROM facts_orders;

-- Har order_items flere rader per ordre? 
SELECT 
    COUNT(*) AS total_rows,
    COUNT(DISTINCT order_id) AS distinct_orders
FROM public.dim_order_items;

	-- total_rows > distinct_orders
	-- Fordi én ordre kan ha flere varelinjer

	-- se distribusjon
SELECT order_id, COUNT(*) AS items_per_order
FROM public.dim_order_items
GROUP BY order_id
ORDER BY items_per_order DESC
LIMIT 10;

-- Har payments flere rader per ordre? 
SELECT 
    COUNT(*) AS total_rows,
    COUNT(DISTINCT order_id) AS distinct_orders
FROM public.dim_order_payments;

	-- Her er det normalt med flere betalinger per ordre (delbetalinger).
	
SELECT order_id, COUNT(*) AS payments_per_order
FROM public.dim_order_payments
GROUP BY order_id
ORDER BY payments_per_order DESC
LIMIT 10;

-- Har reviews alltid én per ordre?
SELECT 
    COUNT(*) AS total_rows,
    COUNT(DISTINCT order_id) AS distinct_orders
FROM public.dim_order_reviews;

SELECT order_id, COUNT(*) AS reviews_per_order
FROM public.dim_order_reviews
GROUP BY order_id
HAVING COUNT(*) > 1;

SELECT *
FROM dim_customers