-- noinspection SqlNoDataSourceInspection

PRAGMA foreign_keys = ON;

CREATE TABLE IF NOT EXISTS schema_version (
    version INTEGER PRIMARY KEY,
    applied_at TEXT NOT NULL DEFAULT (datetime('now'))
);

INSERT OR IGNORE INTO schema_version(version) VALUES (1);


CREATE TABLE IF NOT EXISTS package_entity (
    id INTEGER PRIMARY KEY,
    package_name TEXT NOT NULL DEFAULT ''           -- e.g. java.lang
);

CREATE TABLE IF NOT EXISTS class_entity (
    id INTEGER PRIMARY KEY,
    package_id INTEGER NOT NULL REFERENCES package_entity(id) ON DELETE CASCADE,
    access_json TEXT NOT NULL,          -- Array<String> e.g. ["public", "static"]
    name TEXT NOT NULL,
    full_name TEXT NOT NULL UNIQUE,     -- e.g. java.lang.String
    super_class TEXT NOT NULL,          -- Array<String> e.g. ["java", "lang", "Object"]
    interfaces_json TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS field_entity (
    id INTEGER PRIMARY KEY,
    class_id INTEGER NOT NULL REFERENCES class_entity(id) ON DELETE CASCADE,
    access_json TEXT NOT NULL,          -- Array<String> e.g. ["public", "static"]
    type_json TEXT NOT NULL,            -- Array<String> e.g. ["java", "lang", "String"]
    name TEXT NOT NULL,
    owner_type TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS method_entity (
    id INTEGER PRIMARY KEY,
    class_id INTEGER NOT NULL REFERENCES class_entity(id) ON DELETE CASCADE,
    access_json TEXT NOT NULL,          -- Array<String> e.g. ["public", "static"]
    name TEXT NOT NULL,
    signature_json TEXT NOT NULL,       -- complicated json
    owner_type TEXT NOT NULL
);


CREATE INDEX IF NOT EXISTS idx_class_pkg_name
    ON class_entity(package_id, name);

CREATE INDEX IF NOT EXISTS idx_class_full_name
    ON class_entity(full_name);

CREATE INDEX IF NOT EXISTS idx_field_class_name
    ON field_entity(class_id, name);

CREATE INDEX IF NOT EXISTS idx_method_class_name
    ON method_entity(class_id, name);

CREATE UNIQUE INDEX IF NOT EXISTS ux_package_name
    ON package_entity(package_name);

CREATE UNIQUE INDEX IF NOT EXISTS ux_class_pkg_name
    ON class_entity(package_id, name);
