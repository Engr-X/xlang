package com.wangdi.classgen.base


class Type(private val typeRef: TypeRef, private val arrayDimension: Int = 0)
{
    companion object
    {
        private val PRIM: Map<PrimaryType, String> = mapOf(
            PrimaryType.VOID to "V",
            PrimaryType.BOOL to "Z",
            PrimaryType.CHAR to "C",
            PrimaryType.I8 to "B",
            PrimaryType.I16 to "S",
            PrimaryType.I32 to "I",
            PrimaryType.I64 to "J",
            PrimaryType.F32 to "F",
            PrimaryType.F64 to "D"
        )

        val VOID = Type(PrimaryType.VOID)
        val BOOLEAN = Type(PrimaryType.BOOL)
        val CHAR = Type(PrimaryType.CHAR)

        val INT8 = Type(PrimaryType.I8)
        val INT16 = Type(PrimaryType.I16)
        val INT32 = Type(PrimaryType.I32)
        val INT64 = Type(PrimaryType.I64)

        val Float32 = Type(PrimaryType.F32)
        val Float64 = Type(PrimaryType.F64)
    }

    private var pt: PrimaryType? = null

    private constructor(pt: PrimaryType) : this(TypeRef())
    {
        this.pt = pt
    }

    fun isPrimitive(): Boolean = this.pt != null

    fun getName(): String = if (this.pt == null)
        "[".repeat(this.arrayDimension) + "L${this.typeRef.getFullName("/")};"
    else
        "[".repeat(this.arrayDimension) + PRIM[this.pt!!]!!
}
