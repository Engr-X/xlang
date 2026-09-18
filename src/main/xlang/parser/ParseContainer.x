/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package xlang.parser

/**
 * Represents a generic parser result container.
 *
 * <p>A {@code ParseContainer} stores an untyped result value together with a
 * kind identifier describing how the value should be interpreted.
 *
 * <p>The container does not copy or own the referenced value. The lifetime of
 * the stored object is managed by the code that created it.
 */
struct ParseContainer
{
    /**
     * Identifies a parse result whose value is an {@code ArrayList}.
     */
    static val ARRAY_LIST_KIND: int = 0


    /**
     * The kind identifier describing the stored parse result.
     */
    private var kind: int


    /**
     * A pointer to the value stored in this parse result.
     *
     * <p>The concrete type of the referenced value is determined by
     * {@code kind}.
     */
    private var value: pointer<*>


    /**
     * Creates a parse result container with the specified kind and value.
     *
     * <p>The supplied value is stored by reference and is not copied.
     *
     * @param kind              the kind identifier describing the stored value
     * @param value             a pointer to the parse result value
     */
    constructor(kind: int, value: pointer<*>)
    {
        this.kind = kind
        this.value = value
    }


    /**
     * Returns the kind identifier of this parse result.
     *
     * @return                  the kind identifier associated with the stored value
     */
    fun getKind() -> int = this.kind


    /**
     * Returns the value stored in this parse result.
     *
     * <p>The returned pointer is untyped. Its concrete type should be
     * interpreted according to {@code getKind()}.
     *
     * @return                  a pointer to the stored parse result value
     */
    fun getValue() -> pointer<*> = this.value


    /**
     * Checks whether this parse result has the specified kind.
     *
     * @param                   kind the kind identifier to compare against
     * @return                  {@code true} if this container has the specified kind;
     *                          {@code false} otherwise
     */
    fun isKind(kind: int) -> bool = this.kind == kind
}
