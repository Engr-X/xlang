package com.wangdi.bctoolkit.reader.database

import com.wangdi.bctoolkit.reader.JavaClassDto
import com.wangdi.bctoolkit.reader.JavaFieldDto
import com.wangdi.bctoolkit.reader.JavaMethodDto
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.sql.Connection
import java.sql.DriverManager
import java.sql.PreparedStatement


private data class DbClassRow(
    val id: Long,
    val packageName: String,
    val accessJson: String,
    val name: String,
    val superClassRaw: String,
    val interfacesJson: String
)

@Serializable
private data class MethodSignaturePayload(
    @SerialName("return_type") val returnType: List<String>,
    @SerialName("param_types") val paramTypes: List<List<String>>
)

private object DbSql
{
    val pragmaForeignKeysOn: String = this.load("/database/query/pragma_foreign_keys_on.sql")
    val insertPackageEntity: String = this.load("/database/query/insert_package_entity.sql")
    val selectPackageEntityIdByName: String = this.load("/database/query/select_package_entity_id_by_name.sql")
    val selectClassEntityIdByFullName: String = this.load("/database/query/select_class_entity_id_by_full_name.sql")
    val insertClassEntity: String = this.load("/database/query/insert_class_entity.sql")
    val updateClassEntityById: String = this.load("/database/query/update_class_entity_by_id.sql")
    val deleteFieldEntityByClassId: String = this.load("/database/query/delete_field_entity_by_class_id.sql")
    val deleteMethodEntityByClassId: String = this.load("/database/query/delete_method_entity_by_class_id.sql")
    val insertFieldEntity: String = this.load("/database/query/insert_field_entity.sql")
    val insertMethodEntity: String = this.load("/database/query/insert_method_entity.sql")
    val selectClassRows: String = this.load("/database/query/select_class_rows.sql")
    val selectFieldRows: String = this.load("/database/query/select_field_rows.sql")
    val selectMethodRows: String = this.load("/database/query/select_method_rows.sql")

    private fun load(resourcePath: String): String
    {
        val stream = DbSql::class.java.getResourceAsStream(resourcePath)
            ?: throw IllegalStateException("SQL resource not found: '$resourcePath'")
        return stream.bufferedReader(StandardCharsets.UTF_8).use { it.readText() }.trim()
    }
}

class DbMetadataStore(
    private val codec: Json = Json { this.ignoreUnknownKeys = true },
    private val bootstrap: DbBootstrap = DbBootstrap()
)
{
    /**
     * Save class metadata into an sqlite database file.
     *
     * @param dbPath target sqlite file path.
     * @param classes class metadata to persist.
     */
    fun saveClasses(dbPath: Path, classes: List<JavaClassDto>)
    {
        val normalized: Path = dbPath.toAbsolutePath().normalize()

        normalized.parent?.let {
            if (!Files.exists(it))
                Files.createDirectories(it)
        }

        this.bootstrap.bootstrap(normalized)

        DriverManager.getConnection("jdbc:sqlite:$normalized").use { conn ->
            conn.autoCommit = false
            try
            {
                saveClasses(conn, classes)
                conn.commit()
            }
            catch (e: Exception)
            {
                runCatching { conn.rollback() }
                throw IllegalStateException("Failed to persist class metadata into '$normalized'", e)
            }
            finally
            {
                conn.autoCommit = true
            }
        }
    }

    /**
     * Load class metadata from an sqlite database file.
     *
     * @param dbPath sqlite file path.
     * @return decoded class metadata list.
     */
    fun loadClasses(dbPath: Path): List<JavaClassDto>
    {
        val normalized: Path = dbPath.toAbsolutePath().normalize()

        DriverManager.getConnection("jdbc:sqlite:$normalized").use { conn ->
            return loadClasses(conn)
        }
    }

    /**
     * Save metadata using an existing db connection.
     *
     * @param conn database connection.
     * @param classes class metadata to persist.
     */
    private fun saveClasses(conn: Connection, classes: List<JavaClassDto>)
    {
        conn.createStatement().use { it.execute(DbSql.pragmaForeignKeysOn) }

        conn.prepareStatement(DbSql.insertPackageEntity).use { insertPackage ->
            conn.prepareStatement(DbSql.selectPackageEntityIdByName).use { selectPackageId ->
                conn.prepareStatement(DbSql.selectClassEntityIdByFullName).use { selectClassId ->
                    conn.prepareStatement(DbSql.insertClassEntity).use { insertClass ->
                        conn.prepareStatement(DbSql.updateClassEntityById).use { updateClass ->
                            conn.prepareStatement(DbSql.deleteFieldEntityByClassId).use { deleteFields ->
                                conn.prepareStatement(DbSql.deleteMethodEntityByClassId).use { deleteMethods ->
                                    conn.prepareStatement(DbSql.insertFieldEntity).use { insertField ->
                                        conn.prepareStatement(DbSql.insertMethodEntity).use { insertMethod ->
                                            classes.forEach { clazz ->
                                                val packageName: String = clazz.packagePath.joinToString(".")
                                                val className: String = clazz.name
                                                val fullName: String = if (packageName.isBlank()) className else "$packageName.$className"

                                                val packageId: Long = this.ensurePackageId(insertPackage, selectPackageId, packageName)
                                                val accessJson: String = this.codec.encodeToString(clazz.access)
                                                val superClassJson: String = this.codec.encodeToString(clazz.superClass)
                                                val interfacesJson: String = this.codec.encodeToString(clazz.interfaces)

                                                val existingClassId = this.findClassId(selectClassId, fullName)

                                                val classId = if (existingClassId == null)
                                                {
                                                    insertClass.setLong(1, packageId)
                                                    insertClass.setString(2, accessJson)
                                                    insertClass.setString(3, className)
                                                    insertClass.setString(4, fullName)
                                                    insertClass.setString(5, superClassJson)
                                                    insertClass.setString(6, interfacesJson)
                                                    insertClass.executeUpdate()
                                                    findClassId(selectClassId, fullName)
                                                        ?: throw IllegalStateException("Unable to read inserted class id for '$fullName'")
                                                }
                                                else
                                                {
                                                    updateClass.setLong(1, packageId)
                                                    updateClass.setString(2, accessJson)
                                                    updateClass.setString(3, className)
                                                    updateClass.setString(4, fullName)
                                                    updateClass.setString(5, superClassJson)
                                                    updateClass.setString(6, interfacesJson)
                                                    updateClass.setLong(7, existingClassId)
                                                    updateClass.executeUpdate()
                                                    existingClassId
                                                }

                                                deleteFields.setLong(1, classId)
                                                deleteFields.executeUpdate()

                                                deleteMethods.setLong(1, classId)
                                                deleteMethods.executeUpdate()

                                                (clazz.staticField + clazz.instanceField).forEach { field ->
                                                    insertField.setLong(1, classId)
                                                    insertField.setString(2, codec.encodeToString(field.access))
                                                    insertField.setString(3, codec.encodeToString(field.type))
                                                    insertField.setString(4, field.name)
                                                    insertField.setString(5, field.ownerType)
                                                    insertField.executeUpdate()
                                                }

                                                (clazz.staticMethod + clazz.instanceMethod).forEach { method ->
                                                    val payload = MethodSignaturePayload(
                                                        returnType = method.returnType,
                                                        paramTypes = method.params
                                                    )
                                                    insertMethod.setLong(1, classId)
                                                    insertMethod.setString(2, codec.encodeToString(method.access))
                                                    insertMethod.setString(3, method.name)
                                                    insertMethod.setString(4, codec.encodeToString(payload))
                                                    insertMethod.setString(5, method.ownerType)
                                                    insertMethod.executeUpdate()
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Load metadata using an existing db connection.
     *
     * @param conn database connection.
     * @return decoded class metadata list.
     */
    private fun loadClasses(conn: Connection): List<JavaClassDto>
    {
        val classRows: MutableList<DbClassRow> = mutableListOf()

        conn.prepareStatement(DbSql.selectClassRows).use { st ->
            st.executeQuery().use { rs ->
                while (rs.next())
                {
                    classRows.add(
                        DbClassRow(
                            id = rs.getLong("id"),
                            packageName = rs.getString("package_name"),
                            accessJson = rs.getString("access_json"),
                            name = rs.getString("name"),
                            superClassRaw = rs.getString("super_class"),
                            interfacesJson = rs.getString("interfaces_json")
                        )
                    )
                }
            }
        }

        val staticFieldsByClass: MutableMap<Long, MutableList<JavaFieldDto>> = mutableMapOf()
        val instanceFieldsByClass: MutableMap<Long, MutableList<JavaFieldDto>> = mutableMapOf()

        conn.prepareStatement(DbSql.selectFieldRows).use { st ->
            st.executeQuery().use { rs ->
                while (rs.next())
                {
                    val classId: Long = rs.getLong("class_id")
                    val access: List<String> = this.decodeStringList(rs.getString("access_json"))
                    val dto = JavaFieldDto(
                        access = access,
                        type = this.decodeStringList(rs.getString("type_json")),
                        name = rs.getString("name"),
                        ownerType = rs.getString("owner_type")
                    )

                    if (hasStaticAccess(access))
                        staticFieldsByClass.getOrPut(classId) { mutableListOf() }.add(dto)
                    else
                        instanceFieldsByClass.getOrPut(classId) { mutableListOf() }.add(dto)
                }
            }
        }

        val staticMethodsByClass: MutableMap<Long, MutableList<JavaMethodDto>> = mutableMapOf()
        val instanceMethodsByClass: MutableMap<Long, MutableList<JavaMethodDto>> = mutableMapOf()

        conn.prepareStatement(DbSql.selectMethodRows).use { st ->
            st.executeQuery().use { rs ->
                while (rs.next())
                {
                    val classId: Long = rs.getLong("class_id")
                    val access: List<String> = this.decodeStringList(rs.getString("access_json"))
                    val signature: MethodSignaturePayload = this.decodeMethodSignature(rs.getString("signature_json"))
                    val dto = JavaMethodDto(
                        access = access,
                        returnType = signature.returnType,
                        params = signature.paramTypes,
                        name = rs.getString("name"),
                        ownerType = rs.getString("owner_type")
                    )

                    if (hasStaticAccess(access))
                        staticMethodsByClass.getOrPut(classId) { mutableListOf() }.add(dto)
                    else
                        instanceMethodsByClass.getOrPut(classId) { mutableListOf() }.add(dto)
                }
            }
        }

        return classRows.map { row ->
            JavaClassDto(
                packagePath = decodePackagePath(row.packageName),
                access = decodeStringList(row.accessJson),
                name = row.name,
                superClass = decodeTypePath(row.superClassRaw),
                interfaces = decodeTypeMatrix(row.interfacesJson),
                staticField = staticFieldsByClass[row.id] ?: emptyList(),
                instanceField = instanceFieldsByClass[row.id] ?: emptyList(),
                staticMethod = staticMethodsByClass[row.id] ?: emptyList(),
                instanceMethod = instanceMethodsByClass[row.id] ?: emptyList()
            )
        }
    }

    /**
     * Ensure package row exists and return package id.
     *
     * @param insertPackage package insert statement.
     * @param selectPackageId package id lookup statement.
     * @param packageName package full name.
     * @return package id.
     */
    private fun ensurePackageId(
        insertPackage: PreparedStatement,
        selectPackageId: PreparedStatement,
        packageName: String
    ): Long
    {
        insertPackage.setString(1, packageName)
        insertPackage.executeUpdate()

        selectPackageId.setString(1, packageName)
        selectPackageId.executeQuery().use { rs ->
            if (rs.next())
                return rs.getLong(1)
        }
        throw IllegalStateException("Unable to resolve package id for '$packageName'")
    }

    /**
     * Find class id by full class name.
     *
     * @param selectClassId class id lookup statement.
     * @param fullName class full name.
     * @return class id or null if absent.
     */
    private fun findClassId(
        selectClassId: PreparedStatement,
        fullName: String
    ): Long?
    {
        selectClassId.setString(1, fullName)
        selectClassId.executeQuery().use { rs ->
            return if (rs.next()) rs.getLong(1) else null
        }
    }

    /**
     * Decode package path list from package text.
     *
     * @param packageName package full name.
     * @return package path segments.
     */
    private fun decodePackagePath(packageName: String): List<String> =
        if (packageName.isBlank()) emptyList() else packageName.split('.').filter { it.isNotBlank() }

    /**
     * Decode type path from stored json/raw text.
     *
     * @param raw raw db text.
     * @return type path segments.
     */
    private fun decodeTypePath(raw: String): List<String>
    {
        val text = raw.trim()

        if (text.isEmpty())
            return emptyList()
        if (text.startsWith("["))
            return decodeStringList(text)
        return text.replace('/', '.').split('.').filter { it.isNotBlank() }
    }

    /**
     * Decode list of string from json.
     *
     * @param raw raw db text.
     * @return decoded list.
     */
    private fun decodeStringList(raw: String): List<String>
    {
        val text: String = raw.trim()

        if (text.isEmpty())
            return emptyList()
        return runCatching { codec.decodeFromString<List<String>>(text) }
            .getOrElse {
                text.removePrefix("[").removeSuffix("]")
                    .split(',')
                    .map { item -> item.trim().trim('"') }
                    .filter { it.isNotEmpty() }
            }
    }

    /**
     * Decode list of type paths from json.
     *
     * @param raw raw db text.
     * @return decoded type matrix.
     */
    private fun decodeTypeMatrix(raw: String): List<List<String>>
    {
        val text: String = raw.trim()
        if (text.isEmpty())
            return emptyList()
        return runCatching { codec.decodeFromString<List<List<String>>>(text) }
            .getOrDefault(emptyList())
    }

    /**
     * Decode method signature payload from json.
     *
     * @param raw raw db text.
     * @return decoded method signature.
     */
    private fun decodeMethodSignature(raw: String): MethodSignaturePayload
    {
        val text: String = raw.trim()

        if (text.isEmpty())
            return MethodSignaturePayload(emptyList(), emptyList())
        return runCatching { codec.decodeFromString<MethodSignaturePayload>(text) }
            .getOrDefault(MethodSignaturePayload(emptyList(), emptyList()))
    }

    /**
     * Check whether access list contains static modifier.
     *
     * @param access access flags.
     * @return true if static.
     */
    private fun hasStaticAccess(access: List<String>): Boolean =
        access.any { it.equals("static", ignoreCase = true) }
}
