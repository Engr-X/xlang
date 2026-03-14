-- noinspection SqlNoDataSourceInspection

UPDATE class_entity
SET package_id = ?,
    access_json = ?,
    name = ?,
    full_name = ?,
    super_class = ?,
    interfaces_json = ?
WHERE id = ?;
