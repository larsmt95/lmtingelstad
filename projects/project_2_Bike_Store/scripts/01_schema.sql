CREATE TABLE categories (
    category_id INT PRIMARY KEY,
    category_name TEXT
);

CREATE TABLE brands (
    brand_id INT PRIMARY KEY,
    brand_name TEXT
);

CREATE TABLE stores (
    store_id INT PRIMARY KEY,
    store_name TEXT,
    phone TEXT,
    email TEXT,
    street TEXT,
    city TEXT,
    state TEXT,
    zip_code TEXT
);

CREATE TABLE customers (
    customer_id INT PRIMARY KEY,
    first_name TEXT,
    last_name TEXT,
    phone TEXT,
    email TEXT,
    street TEXT,
    city TEXT,
    state TEXT,
    zip_code TEXT
);

CREATE TABLE staffs (
    staff_id INT PRIMARY KEY,
    first_name TEXT,
    last_name TEXT,
    email TEXT,
    phone TEXT,
    active INT,
    store_id INT,
    manager_id INT
);

CREATE TABLE products (
    product_id INT PRIMARY KEY,
    product_name TEXT,
    brand_id INT,
    category_id INT,
    model_year INT,
    list_price NUMERIC
);

CREATE TABLE orders (
    order_id INT PRIMARY KEY,
    customer_id INT,
    order_status INT,
    order_date DATE,
    required_date DATE,
    shipped_date DATE,
    store_id INT,
    staff_id INT
);

CREATE TABLE order_items (
    order_id INT,
    item_id INT,
    product_id INT,
    quantity INT,
    list_price NUMERIC,
    discount NUMERIC,
    PRIMARY KEY (order_id, item_id)
);

CREATE TABLE stocks (
    store_id INT,
    product_id INT,
    quantity INT,
    PRIMARY KEY (store_id, product_id)
);

SELECT table_schema, table_name
FROM information_schema.tables
WHERE table_type = 'BASE TABLE';

ALTER TABLE public.categories  SET SCHEMA bikestore;
ALTER TABLE public.brands      SET SCHEMA bikestore;
ALTER TABLE public.stores      SET SCHEMA bikestore;
ALTER TABLE public.customers   SET SCHEMA bikestore;
ALTER TABLE public.staffs      SET SCHEMA bikestore;
ALTER TABLE public.products    SET SCHEMA bikestore;
ALTER TABLE public.orders      SET SCHEMA bikestore;
ALTER TABLE public.order_items SET SCHEMA bikestore;
ALTER TABLE public.stocks      SET SCHEMA bikestore;

