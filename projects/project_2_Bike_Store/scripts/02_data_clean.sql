-- Undersøke databaser 
SELECT *
FROM orders;
SELECT *
FROM order_items;

-- Ordre uten order_item
SELECT o.order_id
FROM orders o 
LEFT JOIN order_items oi USING(order_id)
WHERE oi.order_id IS NULL;

-- order_items uten matchende orders
SELECT oi.order_id
FROM order_items oi 
LEFT JOIN orders o USING (order_id)
WHERE order_id IS NULL;

-- Produkter uten kategori eller merke
SELECT p.product_id, p.product_name
FROM products p 
LEFT JOIN categories c USING (category_id) 
LEFT JOIN brands b USING (brand_id) 
WHERE c.category_id IS NULL
	OR b.brand_id IS NULL;

-- Negative eller 0-verdier
-- Order items
SELECT *
FROM order_items
WHERE quantity <= 0
   OR list_price <= 0
   OR discount < 0;

-- Produkter
SELECT *
FROM products
WHERE list_price <= 0;
