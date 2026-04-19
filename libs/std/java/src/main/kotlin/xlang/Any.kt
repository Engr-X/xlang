package xlang


open class Any : Cloneable
{
    override fun toString(): kotlin.String = super.toString()

    /**
     * xlang-level alias of Java's hashCode().
     */
    open fun hash(): kotlin.Int = this.hashCode()

    /**
     * Kept only for compatibility with old Object lifecycle APIs.
     * Prefer explicit resource management over finalization.
     */
    @Suppress("deprecation")
    open fun finalize() {}

    override fun clone(): Any = throw NotImplementedError("${this.javaClass.name}.clone() is not implemented.")
}
