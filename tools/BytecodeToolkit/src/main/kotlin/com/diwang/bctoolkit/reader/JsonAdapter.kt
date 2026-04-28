package com.diwang.bctoolkit.reader

import kotlinx.serialization.json.Json
import kotlinx.serialization.decodeFromString
import kotlinx.serialization.encodeToString


class JsonAdapter(private val codec: Json = prettyJson())
{
    companion object
    {
        /**
         * Auto-generated baseline docs for prettyJson.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun prettyJson(): Json = Json {
            prettyPrint = true
            prettyPrintIndent = "    "
        }
    }

    /**
     * Auto-generated baseline docs for encodeClasses.
     * Describes the intent and behavior of this function.
     *
     * @param classes parameter from function signature.
     * @return return value of this function.
     */
    fun encodeClasses(classes: List<JavaClassDto>): String =
        codec.encodeToString(ClassesEnvelope(classes))

    /**
     * Auto-generated baseline docs for decodeClasses.
     * Describes the intent and behavior of this function.
     *
     * @param jsonText parameter from function signature.
     * @return return value of this function.
     */
    fun decodeClasses(jsonText: String): List<JavaClassDto> =
        codec.decodeFromString<ClassesEnvelope>(jsonText).classes
}

