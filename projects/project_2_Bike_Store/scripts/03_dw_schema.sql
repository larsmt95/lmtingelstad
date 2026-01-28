-- dim_products
SELECT p.product_id, 
		p.product_name, 
		c.category_name AS category, 
	 	b.brand_name AS brand, 
		p.list_price
FROM products p
	JOIN brands b USING (brand_id)
	JOIN categories c USING (category_id);


-- dim_stores
CREATE OR REPLACE VIEW dim_stores AS
SELECT 
	store_id,
	store_name, 
	city,
	state,
	CONCAT(city, ', ', state) AS location
FROM stores;

-- dim_staff
CREATE OR REPLACE VIEW dim_staff AS
SELECT 
	staff_id,
	first_name,
	last_name,
	staffs.email,
	store_id,
	manager_id,
	CONCAT(first_name, ' ', last_name) AS name,
	CASE
  		WHEN manager_id IS NULL THEN 'Manager'
  		ELSE 'Staff'
	END AS role
FROM staffs
	JOIN stores USING (store_id);

-- dim_customers
CREATE OR REPLACE VIEW dim_customers AS
SELECT 
	CONCAT(first_name, ' ', last_name) AS NAME,
	street,
	city,
	state,
	zip_code,
	CONCAT(state, '/', city)
FROM customers;

SELECT *
FROM order_items;

-- fact_sales
CREATE OR REPLACE VIEW fact_sales AS
SELECT 
  o.order_id,
  o.order_date,
  o.store_id,
  o.staff_id,
  o.customer_id,
  oi.product_id,
  oi.quantity,
  oi.list_price,
  oi.discount, 
  oi.quantity * oi.list_price AS gross_revenue,
  oi.quantity * oi.list_price * (1 - oi.discount) AS net_revenue
FROM orders o
JOIN order_items oi USING (order_id);


