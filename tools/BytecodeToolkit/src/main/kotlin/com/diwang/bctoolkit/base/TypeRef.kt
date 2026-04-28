package com.diwang.bctoolkit.base


class TypeRef(private val name: MutableList<String>)
{
    constructor(vararg name: String) : this(name.toMutableList())

    companion object
    {
        val OBJECT = TypeRef("java", "lang", "Object")
        val OBJECT_FULL_NAME: String = OBJECT.getFullName("/")

        /**
         * Auto-generated baseline docs for safeCreate.
         * Describes the intent and behavior of this function.
         *
         * @param name parameter from function signature.
         * @return return value of this function.
         */
        fun safeCreate(name: MutableList<String>): TypeRef = if (name.isEmpty()) TypeRef("java", "lang", "Object") else TypeRef(name)
    }

    /**
     * Auto-generated baseline docs for getName.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun getName(): List<String> = this.name.toMutableList()

    /**
     * Auto-generated baseline docs for getLastName.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun getLastName(): String = this.name.last()

    /**
     * Auto-generated baseline docs for getFullName.
     * Describes the intent and behavior of this function.
     *
     * @param separator parameter from function signature.
     * @return return value of this function.
     */
    fun getFullName(separator: String): String = this.name.joinToString(separator = separator)

    /**
     * Auto-generated baseline docs for getPackage.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun getPackage(): MutableList<String> = this.name.subList(0, this.name.lastIndex)

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun toString(): String = this.name.joinToString(separator = ".")
}

