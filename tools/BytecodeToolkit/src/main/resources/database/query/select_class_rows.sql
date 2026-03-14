-- noinspection SqlNoDataSourceInspection

SELECT c.id,
       p.package_name,
       c.access_json,
       c.name,
       c.super_class,
       c.interfaces_json,
       c.full_name
FROM class_entity c
JOIN package_entity p ON p.id = c.package_id
ORDER BY c.full_name;
