package com.diwang.bctoolkit.reader.database

import java.io.InputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.sql.Connection
import java.sql.DriverManager

class DbBootstrap(private val schemaResourcePath: String = "/database/schema.sql")
{
    /**
     * Auto-generated baseline docs for bootstrap.
     * Describes the intent and behavior of this function.
     *
     * @param dbPath parameter from function signature.
     */
    fun bootstrap(dbPath: Path)
    {
        val normalized: Path = dbPath.toAbsolutePath().normalize()
        val parent: Path? = normalized.parent

        if (parent != null && !Files.exists(parent))
            Files.createDirectories(parent)

        val jdbcUrl = "jdbc:sqlite:${normalized}"

        DriverManager.getConnection(jdbcUrl).use { conn ->
            bootstrap(conn)
        }
    }


    /**
     * Auto-generated baseline docs for bootstrap.
     * Describes the intent and behavior of this function.
     *
     * @param connection parameter from function signature.
     */
    fun bootstrap(connection: Connection)
    {
        val schemaSql: String = loadSchemaText()
        val statements: List<String> = splitSqlStatements(schemaSql)

        val oldAutoCommit: Boolean = connection.autoCommit

        connection.autoCommit = false

        try
        {
            statements.forEach { sql ->
                connection.createStatement().use { st ->
                    st.execute(sql)
                }
            }
            connection.commit()
        }
        catch (e: Exception)
        {
            runCatching { connection.rollback() }
            throw IllegalStateException("Failed to bootstrap database schema from '$schemaResourcePath'", e)
        }
        finally
        {
            connection.autoCommit = oldAutoCommit
        }
    }

    /**
     * Auto-generated baseline docs for loadSchemaText.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    private fun loadSchemaText(): String
    {
        val stream: InputStream = DbBootstrap::class.java.getResourceAsStream(schemaResourcePath)
            ?: throw IllegalStateException("Schema resource not found: '$schemaResourcePath'")

        return stream.use { input ->
            input.bufferedReader(StandardCharsets.UTF_8).use { it.readText() }
        }.trimStart('\uFEFF')
    }

    /**
     * Auto-generated baseline docs for splitSqlStatements.
     * Describes the intent and behavior of this function.
     *
     * @param sqlText parameter from function signature.
     * @return return value of this function.
     */
    private fun splitSqlStatements(sqlText: String): List<String>
    {
        val out: MutableList<String> = mutableListOf()
        val current = StringBuilder()

        var i = 0
        var inSingleQuote = false
        var inDoubleQuote = false
        var inLineComment = false
        var inBlockComment = false

        while (i < sqlText.length)
        {
            val c: Char = sqlText[i]
            val next: Char? = if (i + 1 < sqlText.length) sqlText[i + 1] else null

            if (inLineComment)
            {
                if (c == '\n')
                {
                    inLineComment = false
                    current.append(c)
                }
                i += 1
                continue
            }

            if (inBlockComment)
            {
                if (c == '*' && next == '/')
                {
                    inBlockComment = false
                    i += 2
                }
                else
                    i += 1
                continue
            }

            if (!inSingleQuote && !inDoubleQuote)
            {
                if (c == '-' && next == '-')
                {
                    inLineComment = true
                    i += 2
                    continue
                }
                if (c == '/' && next == '*')
                {
                    inBlockComment = true
                    i += 2
                    continue
                }
            }

            if (c == '\'' && !inDoubleQuote)
                inSingleQuote = !inSingleQuote
            else if (c == '"' && !inSingleQuote)
                inDoubleQuote = !inDoubleQuote

            if (c == ';' && !inSingleQuote && !inDoubleQuote)
            {
                val one: String = current.toString().trim()
                if (one.isNotEmpty())
                    out.add(one)
                current.setLength(0)
                i += 1
                continue
            }

            current.append(c)
            i += 1
        }

        val tail: String = current.toString().trim()
        if (tail.isNotEmpty())
            out.add(tail)

        return out
    }
}