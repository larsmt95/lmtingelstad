SELECT 
  name,
  type,
  EXTRACT(MONTH FROM start_date::timestamp) AS month,
  EXTRACT(YEAR  FROM start_date::timestamp) AS year
FROM public.activities
WHERE TYPE = 'Workout' AND EXTRACT(YEAR  FROM start_date::timestamp) = 2026
GROUP BY name, type, year, month 
ORDER BY year DESC, month DESC;


SELECT *
FROM public.activities


 SELECT DISTINCT type
 FROM public.activities;
  

-- ROUND(SUM(distance / 1000.0)::numeric, 2) AS total_distance_km