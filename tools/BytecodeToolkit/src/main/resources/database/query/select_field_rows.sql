-- noinspection SqlNoDataSourceInspection

SELECT class_id,
       access_json,
       type_json,
       name,
       owner_type
FROM field_entity
ORDER BY id;
