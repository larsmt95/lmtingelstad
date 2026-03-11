SET search_path TO public;

 -- Aggregere order_items til ordre-nivå (SUM price, SUM freight_value, COUNT items))

SELECT COUNT(*), COUNT(DISTINCT order_id)
FROM (
    SELECT order_id
    FROM public.dim_order_items
    GROUP BY order_id
) t;

SELECT  
    order_id,
    SUM(price) AS total_price,
    SUM(freight_value) AS total_freight,
    COUNT(order_item_id) AS item_count
FROM public.dim_order_items
GROUP BY order_id;

 -- JOIN med orders, order_reviews, customers
DROP TABLE public.fact_orders_v2;

CREATE TABLE fact_orders_v2 AS 

WITH order_items_agg AS (
	SELECT  
	    order_id,
	    SUM(price) AS total_price,
	    SUM(freight_value) AS total_freight,
	    COUNT(order_item_id) AS item_count
	FROM public.dim_order_items
	GROUP BY order_id
),

payments_agg AS(
	SELECT
		order_id,
		SUM(payment_value) AS total_payment
	FROM dim_order_payments
	GROUP BY order_id
),

reviews_agg AS (
	SELECT
		order_id,
		AVG(review_score) AS avg_review_score
	FROM vw_reviews_clean
	GROUP BY order_id
)

SELECT 
	o.order_id,
	o.customer_id,
	o.order_status,
	o.order_purchase_timestamp,
	o.order_delivered_customer_date,
	o.order_estimated_delivery_date,

	oi.total_price,
	oi.total_freight,
	oi.item_count,

	p.total_payment,
	r.avg_review_score,

	(o.order_delivered_customer_date - o.order_purchase_timestamp) 
		AS delivery_time,

	(o.order_delivered_customer_date > o.order_estimated_delivery_date)
		AS delivery_late

FROM facts_orders o
LEFT JOIN order_items_agg oi USING(order_id)
LEFT JOIN payments_agg p USING(order_id)
LEFT JOIN reviews_agg r USING(order_id);

SELECT COUNT(*), COUNT(DISTINCT order_id)
FROM public.fact_orders_v2;

 -- Nye felt: total_order_value, delivery_time, estimated_delivery_time, delay_days, delay_flag
DROP TABLE public.fact_orders_v3;

CREATE TABLE fact_orders_v3 AS
	SELECT *,
	       total_price + total_freight AS total_order_value,
	
	       -- Faktisk leveringstid (hele dager)
	       (order_delivered_customer_date::date 
	        - order_purchase_timestamp::date) AS delivery_time_days,
	
	       -- Estimert leveringstid (hele dager)
	       (order_estimated_delivery_date::date 
	        - order_purchase_timestamp::date) AS estimated_delivery_days,
	
	       -- Forsinkelse i dager
	       (order_delivered_customer_date::date 
	        - order_estimated_delivery_date::date) AS delay_days,
	
	       -- Forsinket eller ikke
	       (order_delivered_customer_date::date 
	        > order_estimated_delivery_date::date)::int AS delay_flag
	
	FROM fact_orders_v2
	WHERE order_status = 'delivered'
	  AND order_delivered_customer_date >= order_purchase_timestamp;

SELECT *
FROM fact_orders_v5;

SELECT DISTINCT(order_status)
FROM facts_orders

 -- Filtrer bort: avbrutte ordre, ulogiske datoer

 	FROM fact_orders_v2
	WHERE order_status = 'delivered'
	  AND order_delivered_customer_date >= order_purchase_timestamp;

 -- Legge til kategori

SELECT*
FROM dim_product_category;

	-- Velg én kategori per ordre
CREATE TABLE fact_orders_v5 AS
SELECT *
FROM (
    SELECT
        f.*,
        pc.product_category_name_english,
        ROW_NUMBER() OVER (
            PARTITION BY f.order_id 
            ORDER BY oi.price DESC
        ) AS rn
    FROM fact_orders_v4 f
    JOIN dim_order_items oi 
        ON f.order_id = oi.order_id
    JOIN dim_products p
        ON oi.product_id = p.product_id
    JOIN dim_product_category pc
        ON p.product_category_name = pc.product_category_name
) t
WHERE rn = 1;

	-- Aggreger til én kategori-label
CREATE TABLE fact_orders_v5 AS;

SELECT
    f.order_id,
    f.low_rating_flag,
    f.delay_flag,
    STRING_AGG(DISTINCT pc.product_category_name_english, ', ') AS categories
FROM fact_orders_v4 f
JOIN dim_order_items oi 
    ON f.order_id = oi.order_id
JOIN dim_products p
    ON oi.product_id = p.product_id
JOIN dim_product_category pc
    ON p.product_category_name = pc.product_category_name
GROUP BY f.order_id, f.low_rating_flag, f.delay_flag;

-- Ny dim_category med sammenslåtte kategorier

CREATE TABLE dim_category AS
SELECT
    pc.product_category_name,
    pc.product_category_name_english,
    
    CASE
        
        WHEN pc.product_category_name_english IN (
            'computers','computers_accessories','electronics',
            'telephony','tablets_printing_image','consoles_games',
            'audio','fixed_telephony'
        ) THEN 'electronics_tech'
        
        WHEN pc.product_category_name_english IN (
            'fashion_male_clothing','fashion_female_clothing',
            'fashion_shoes','fashion_bags_accessories',
            'fashion_underwear_beach','fashion_childrens_clothes',
            'watches_gifts'
        ) THEN 'fashion'
        
        WHEN pc.product_category_name_english IN (
            'furniture_decor','furniture_living_room',
            'furniture_bedroom','housewares','home_appliances',
            'home_construction',
            'kitchen_dining_laundry_garden_furniture',
            'bed_bath_table','garden_tools'
        ) THEN 'home_furniture'
        
        WHEN pc.product_category_name_english IN (
            'health_beauty','perfumery','diapers_and_hygiene'
        ) THEN 'beauty_health'
        
        WHEN pc.product_category_name_english IN (
            'sports_leisure','toys','cool_stuff','musical_instruments'
        ) THEN 'sports_leisure'
        
        WHEN pc.product_category_name_english IN (
            'books_general_interest','books_technical',
            'books_imported','dvds_blu_ray',
            'cds_dvds_musicals','music'
        ) THEN 'books_media'
        
        WHEN pc.product_category_name_english IN (
            'food','food_drink','drinks'
        ) THEN 'food_drinks'
        
        ELSE 'other'
        
    END AS category_macro

FROM dim_product_category pc;

SELECT *
FROM dim_category