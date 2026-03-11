SET search_path TO public;

CREATE TABLE public.dim_geolocation (
	geolocation_zip_code_prefix INT,
	geolocation_lat INT,
	geolocation_lng INT,
	geolocation_city TEXT,
	geolocation_state TEXT
);

CREATE TABLE dim_customers (
    customer_id TEXT PRIMARY KEY,
    customer_unique_id TEXT,
    customer_zip_code_prefix INT,
    customer_city TEXT,
    customer_state TEXT
);

DROP TABLE public.dim_customers;


CREATE TABLE public.dim_order_items (
    order_id TEXT,
    order_item_id INT,
    product_id TEXT,
    seller_id TEXT,
    shipping_limit_date TIMESTAMP,
    price NUMERIC(10,2),
    freight_value NUMERIC(10,2)
);

CREATE TABLE public.dim_order_payments (
    order_id TEXT,
    payment_sequential INT,
    payment_type TEXT,
    payment_installments INT,
    payment_value NUMERIC(10,2)
);

-- Bruk surrogate key og fjern unik constraint på review_id:

CREATE TABLE public.dim_order_reviews (
    review_key SERIAL PRIMARY KEY,
    review_id TEXT,
    order_id TEXT,
    review_score INT,
    review_comment_title TEXT,
    review_comment_message TEXT,
    review_creation_date TIMESTAMP,
    review_answer_timestamp TIMESTAMP
);

SELECT order_id, COUNT(*)
FROM public.dim_order_reviews
GROUP BY order_id
HAVING COUNT(*) > 1;

SELECT review_id, COUNT(*)
FROM public.dim_order_reviews
GROUP BY review_id
LIMIT 10;

SELECT COUNT(*),
       COUNT(DISTINCT review_id)
FROM public.dim_order_reviews;


DROP TABLE public.dim_order_reviews;

-- clean view reviews uten duplikate (814)
CREATE VIEW public.vw_reviews_clean AS
SELECT DISTINCT ON (review_id)
       *
FROM public.dim_order_reviews
ORDER BY review_id, review_creation_date DESC;

SELECT * 
FROM vw_reviews_clean

CREATE TABLE public.facts_orders (
    order_id TEXT PRIMARY KEY,
    customer_id TEXT,
    order_status TEXT,
    order_purchase_timestamp TIMESTAMP,
    order_approved_at TIMESTAMP,
    order_delivered_carrier_date TIMESTAMP,
    order_delivered_customer_date TIMESTAMP,
    order_estimated_delivery_date TIMESTAMP
);

CREATE TABLE public.dim_products (
    product_id TEXT PRIMARY KEY,
    product_category_name TEXT,
    product_name_lenght INT,
    product_description_lenght INT,
    product_photos_qty INT,
    product_weight_g INT,
    product_length_cm INT,
    product_height_cm INT,
    product_width_cm INT
);

CREATE TABLE public.dim_sellers (
    seller_id TEXT PRIMARY KEY,
    seller_zip_code_prefix INT,
    seller_city TEXT,
    seller_state TEXT
);

CREATE TABLE public.dim_product_category (
    product_category_name TEXT PRIMARY KEY,
    product_category_name_english TEXT
);


-- endre på tabeller 

ALTER TABLE public.dim_geolocation
ADD COLUMN geolocation_id SERIAL PRIMARY KEY;

ALTER TABLE public.dim_geolocation
ALTER COLUMN geolocation_lat TYPE NUMERIC(10,6),
ALTER COLUMN geolocation_lng TYPE NUMERIC(10,6);

ALTER TABLE public.dim_order_items
ADD CONSTRAINT pk_order_items
PRIMARY KEY (order_id, order_item_id);

ALTER TABLE public.olist_customers_dataset
RENAME TO dim_customers;

SELECT *
FROM public.olist_customers_dataset;