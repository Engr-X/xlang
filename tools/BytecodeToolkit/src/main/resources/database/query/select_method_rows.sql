-- noinspection SqlNoDataSourceInspection

SELECT class_id,
       access_json,
       name,
       signature_json,
       owner_type
FROM method_entity
ORDER BY id;
