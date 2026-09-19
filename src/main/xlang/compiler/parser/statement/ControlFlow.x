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
 */

package xlang.compiler.parser.statement

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a {@code pass} statement in the statement abstract syntax tree.
 *
 * <p>A {@code PassStatement} represents a statement that performs no operation.
 * It can be used in places where the grammar requires a statement but no
 * executable behavior is needed.
 *
 * <p>The statement does not contain any child expressions or nested statements.
 * Its source information is therefore represented entirely by the additional
 * syntax tokens retained in {@code extraTokens}.
 *
 * <p>All stored tokens can be retrieved in lexical source order through
 * {@code getAllTokens()}.
 */
struct PassStatement
{
   /**
     * Additional syntax tokens associated with this pass statement.
     *
     * <p>The collection normally contains the lexical token representing the
     * {@code pass} keyword and may contain other syntax information retained by
     * the parser.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty pass statement.
     *
     * <p>A new empty token collection is allocated for syntax tokens associated
     * with the statement.
     */
    constructor():
        this.extraTokens = new ArrayList(sizeof(Token))


    /**
     * Adds an additional syntax token to this pass statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * statement.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code PassStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<PassStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this pass
     * statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Modifications performed through the returned list therefore affect the
     * same token collection referenced internally by this statement.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this pass statement.
     *
     * <p>The additional syntax tokens stored by the statement are copied into a
     * newly allocated result list.
     *
     * <p>The resulting collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This ensures that the returned tokens
     * follow their original lexical ordering even if they were inserted into the
     * statement in another order.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this pass statement in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this pass statement.
     *
     * <p>The representation consists solely of the {@code "pass"} keyword.
     *
     * <p>A newly allocated {@code StringBuilder} is returned. Modifying the
     * returned builder does not modify this statement or its stored tokens.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing {@code "pass"}
     */
    fun toString() -> pointer<StringBuilder> = new StringBuilder("pass")
}


/**
 * Represents a {@code break} statement in the statement abstract syntax tree.
 *
 * <p>A {@code BreakStatement} represents a control-flow statement that requests
 * termination of the enclosing breakable construct, such as a loop.
 *
 * <p>The statement does not contain child expressions or nested statements.
 * Its source-level information is stored entirely through the additional syntax
 * tokens retained by the node.
 *
 * <p>All tokens associated with the statement can be retrieved in lexical source
 * order through {@code getAllTokens()}.
 */
struct BreakStatement
{
    /**
     * Additional syntax tokens associated with this break statement.
     *
     * <p>The collection normally contains the lexical token representing the
     * {@code break} keyword and may contain other syntax information retained by
     * the parser.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty break statement.
     *
     * <p>A new empty token collection is allocated for syntax tokens associated
     * with the statement.
     */
    constructor():
        this.extraTokens = new ArrayList(sizeof(Token))


    /**
     * Adds an additional syntax token to this break statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * statement.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code BreakStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<BreakStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this break
     * statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code BreakStatement}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this break statement.
     *
     * <p>The additional syntax tokens stored by the statement are copied into a
     * newly allocated result list.
     *
     * <p>The resulting collection is sorted according to source position using
     * {@code TokenPosition.compareToken}, ensuring that all tokens are returned
     * in their original lexical order.
     *
     * <p>The returned list is newly allocated. The individual token objects are
     * referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this break statement in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this break statement.
     *
     * <p>The representation consists solely of the {@code "break"} keyword.
     *
     * <p>A newly allocated {@code StringBuilder} is returned. Modifying the
     * returned builder does not modify the underlying statement.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing {@code "break"}
     */
    fun toString() -> pointer<StringBuilder> = new StringBuilder("break")
}


/**
 * Represents a {@code continue} statement in the statement abstract syntax
 * tree.
 *
 * <p>A {@code ContinueStatement} represents a control-flow statement that skips
 * the remainder of the current iteration of an enclosing loop and transfers
 * execution to the next iteration according to that loop's semantics.
 *
 * <p>The statement contains no child expressions or nested statements. Its
 * source-level information is represented entirely by additional syntax tokens
 * retained during parsing.
 *
 * <p>All tokens associated with the statement can be retrieved in lexical source
 * order through {@code getAllTokens()}.
 */
struct ContinueStatement
{
    /**
     * Additional syntax tokens associated with this continue statement.
     *
     * <p>The collection normally contains the lexical token representing the
     * {@code continue} keyword and may contain other syntax information retained
     * by the parser.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty continue statement.
     *
     * <p>A new empty token collection is allocated for syntax tokens associated
     * with the statement.
     */
    constructor():
        this.extraTokens = new ArrayList(sizeof(Token))


    /**
     * Adds an additional syntax token to this continue statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * statement.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ContinueStatement} instance
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Adds an additional syntax token to this continue statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection for the
     * statement.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ContinueStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ContinueStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this
     * continue statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code ContinueStatement}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this continue statement.
     *
     * <p>The representation consists solely of the {@code "continue"} keyword.
     *
     * <p>A newly allocated {@code StringBuilder} is returned. Modifying the
     * returned builder does not modify the underlying statement.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing {@code "continue"}
     */
    fun toString() -> pointer<StringBuilder> = new StringBuilder("continue")
}
