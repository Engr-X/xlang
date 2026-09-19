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

package xlang.compiler.parser.stmtexpr

import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.statement.Statement
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents an if-else construct used as an expression-level AST node.
 *
 * <p>An {@code IfElseExpression} stores an optional condition expression, an
 * ordered collection of statements forming the if branch, an ordered collection
 * of statements forming the optional else branch, and additional syntax tokens
 * retained from the original source.
 *
 * <p>The condition may initially be absent and can later be assigned through
 * {@code setCondition()}.
 *
 * <p>The if-branch collection is always allocated internally by the available
 * constructors. The else-branch collection may either be allocated internally
 * or supplied directly by the caller.
 *
 * <p>Statements stored in the branch collections are referenced directly and
 * are not copied or cloned.
 *
 * <p>Additional syntax tokens may contain the {@code if} and {@code else}
 * keywords, branch separators, parentheses, punctuation, or other lexical
 * elements not directly owned by the condition or nested statements.
 *
 * <p>All tokens belonging to the conditional expression and its child AST nodes
 * can be collected in lexical source order using {@code getAllTokens()}.
 */
struct IfElseExpression
{
    /**
     * The optional condition expression controlling which branch is selected.
     *
     * <p>The expression is stored by reference and is not copied or cloned.
     *
     * <p>A {@code null} value indicates that no condition has currently been
     * assigned.
     */
    private var condition: pointer<Expression>

    /**
     * The ordered collection of statements forming the if branch.
     *
     * <p>This collection is allocated internally by every constructor.
     *
     * <p>The statements contained by the collection are stored by reference and
     * are not recursively copied or cloned.
     */
    private val ifStmts: pointer<ArrayList>

    /**
     * The ordered collection of statements forming the optional else branch.
     *
     * <p>An empty collection represents the absence of an effective else-body.
     *
     * <p>Depending on the constructor used, this collection is either allocated
     * internally or supplied directly by the caller.
     */
    private val elseStmts: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this if-else expression.
     *
     * <p>This collection may contain conditional keywords, separators,
     * parentheses, branch punctuation, or other lexical elements that are not
     * directly represented by the condition or nested statements.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty if-else expression.
     *
     * <p>The condition is initialized to {@code null}.
     *
     * <p>New empty collections are allocated for both the if branch and the else
     * branch.
     *
     * <p>A new empty collection is also allocated for additional syntax tokens.
     */
    constructor()
    {
        this.condition = null
        this.ifStmts = new ArrayList(sizeof(Statement))
        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an if-else expression with the specified else-body collection.
     *
     * <p>The condition is initialized to {@code null}, and a new empty
     * if-statement collection is allocated.
     *
     * <p>The supplied {@code elseStmts} collection is stored directly and is not
     * copied or cloned.
     *
     * <p>This constructor does not normalize a {@code null} else-body collection
     * to an empty list. Because methods such as {@code haveElseStatement()},
     * {@code getAllTokens()}, and {@code toString()} access
     * {@code elseStmts} directly, callers are expected to provide a valid
     * collection.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param elseStmts         a pointer to the ordered else-body statement
     *                          collection
     */
    constructor(elseStmts: pointer<ArrayList>)
    {
        this.condition = null
        this.ifStmts = new ArrayList(sizeof(Statement))
        this.elseStmts = elseStmts
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates an if-else expression containing an optional initial else
     * statement.
     *
     * <p>The condition is initialized to {@code null}.
     *
     * <p>New empty collections are allocated for the if branch, else branch, and
     * additional syntax tokens.
     *
     * <p>If {@code elseStatement} is not {@code null}, it is appended to the
     * newly created else-body collection. A {@code null} value therefore
     * produces an expression with an empty else branch.
     *
     * <p>The supplied statement is stored by reference and is not copied or
     * cloned.
     *
     * @param elseStatement     a pointer to the initial else-body statement, or
     *                          {@code null} to leave the else branch empty
     */
    constructor(elseStatement: pointer<Statement>)
    {
        this.condition = null
        this.ifStmts = new ArrayList(sizeof(Statement))
        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))

        if elseStatement != null:
            this.elseStmts.push(elseStatement)
    }


    /**
     * Replaces the condition expression associated with this if-else
     * expression.
     *
     * <p>The supplied expression is stored directly and is not copied or cloned.
     *
     * <p>Passing {@code null} removes the currently stored condition.
     *
     * @param expr              a pointer to the condition expression, or
     *                          {@code null} to clear the condition
     *
     * @return                  this {@code IfElseExpression} instance
     */
    fun setCondition(expr: pointer<Expression>) -> pointer<IfElseExpression>
    {
        this.condition = expr
        return this
    }


    /**
     * Appends a statement to the if branch.
     *
     * <p>If {@code statement} is {@code null}, no modification is performed.
     *
     * <p>A valid statement is appended to the end of the internal if-statement
     * collection, preserving insertion order.
     *
     * <p>The statement is stored by reference and is not copied or cloned.
     *
     * @param statement         a pointer to the statement to append to the if
     *                          branch
     *
     * @return                  this {@code IfElseExpression} instance
     */
    fun addIfStatement(statement: pointer<Statement>) -> pointer<IfElseExpression>
    {
        if statement != null:
            this.ifStmts.push(statement)

        return this
    }


    /**
     * Appends all statements from the specified collection to the if branch.
     *
     * <p>If {@code statement} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal if-statement
     * collection in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection itself is not modified, and the contained
     * statement objects are not recursively copied or cloned.
     *
     * <p>This method does not individually filter null entries that may already
     * exist inside the supplied collection. Such entries are skipped later by
     * {@code getAllTokens()} and {@code toString()}.
     *
     * @param statement         a pointer to the statement collection to append
     *                          to the if branch
     *
     * @return                  this {@code IfElseExpression} instance
     */
    fun addIfStatements(statement: pointer<ArrayList>) -> pointer<IfElseExpression>
    {
        if statement != null:
            this.ifStmts.pushAll(statement)

        return this
    }


    /**
     * Returns whether this expression contains at least one if-branch statement.
     *
     * <p>The result is determined exclusively from the length of the internal
     * if-statement collection.
     *
     * @return                  {@code true} if at least one if-branch statement
     *                          is stored; {@code false} otherwise
     */
    fun haveIfStatement() -> bool = this.ifStmts.length > 0


    /**
     * Returns whether this expression contains at least one else-branch
     * statement.
     *
     * <p>The result is determined exclusively from the length of the internal
     * else-statement collection.
     *
     * <p>This method assumes that {@code elseStmts} references a valid
     * collection.
     *
     * @return                  {@code true} if at least one else-branch
     *                          statement is stored; {@code false} otherwise
     */
    fun haveElseStatement() -> bool = this.elseStmts.length > 0


    /**
     * Returns the condition expression associated with this if-else expression.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Expression} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if no condition has been assigned.
     *
     * @return                  a pointer to the condition expression, or
     *                          {@code null} if no condition is available
     */
    fun getCondition() -> pointer<Expression> = this.condition


    /**
     * Returns the statement collection forming the if branch.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same if-statement
     * collection referenced internally by this {@code IfElseExpression}.
     *
     * @return                  a pointer to the internally stored if-branch
     *                          statement collection
     */
    fun getIfStatements() -> pointer<ArrayList> = this.ifStmts


    /**
     * Returns the statement collection forming the else branch.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same else-statement
     * collection referenced internally by this {@code IfElseExpression}.
     *
     * @return                  a pointer to the internally stored else-branch
     *                          statement collection
     */
    fun getElseStatements() -> pointer<ArrayList> = this.elseStmts


    /**
     * Adds an additional syntax token to this if-else expression.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection associated
     * with the conditional expression.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code IfElseExpression} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<IfElseExpression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Appends all tokens from the specified collection to this if-else
     * expression.
     *
     * <p>If {@code tokens} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal extra-token
     * collection in their existing order.
     *
     * <p>The supplied collection is not modified and the individual token
     * objects are not recursively cloned.
     *
     * @param tokens            a pointer to the syntax-token collection to
     *                          append
     *
     * @return                  this {@code IfElseExpression} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<IfElseExpression>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this
     * if-else expression.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the same token collection
     * referenced internally by this {@code IfElseExpression}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this if-else expression and its child
     * AST nodes.
     *
     * <p>If a condition expression is present, its token collection is obtained
     * through {@code Expression.getAllTokens()}. If the returned collection is
     * not {@code null}, its contents are appended to the result.
     *
     * <p>The if-branch statement collection is then traversed in its stored
     * order. Null statement entries are skipped.
     *
     * <p>For every valid if-branch statement,
     * {@code Statement.getAllTokens()} is invoked. If the returned token
     * collection is not {@code null}, its contents are appended.
     *
     * <p>The else-branch collection is processed using the same rules after the
     * if branch.
     *
     * <p>The additional syntax tokens stored directly by this expression are
     * then appended to the result.
     *
     * <p>The complete token collection is sorted according to source position
     * using {@code TokenPosition.compareToken}. This restores the original
     * lexical ordering even though tokens are gathered independently from the
     * condition, both branch collections, and the expression-level token
     * collection.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * <p>This method assumes that both {@code ifStmts} and {@code elseStmts}
     * reference valid collections.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this if-else expression in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.condition != null:
        {
            val tokens: pointer<ArrayList> = this.condition.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.ifStmts.length; i++):
        {
            val statement: pointer<Statement> = this.ifStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.elseStmts.length; i++):
        {
            val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this if-else expression.
     *
     * <p>If a condition expression is present, the representation begins with
     * {@code "(if "}, followed by the textual representation returned by
     * {@code Expression.toString()}.
     *
     * <p>The sequence {@code ":\n"} is then appended unconditionally, regardless
     * of whether a condition expression was available.
     *
     * <p>The if-branch statement collection is traversed in its stored order.
     * Null entries are skipped. Each valid statement contributes the textual
     * representation returned by {@code Statement.toString()}, followed by a
     * newline.
     *
     * <p>If {@code haveElseStatement()} returns {@code true}, the sequence
     * {@code "else:\n"} is appended. Valid else-branch statements are then
     * emitted in their stored order and are each followed by a newline.
     *
     * <p>Finally, the sequence {@code ")\n"} is always appended.
     *
     * <p>For a normal expression with a valid condition and both branches, the
     * generated representation generally has the form:
     *
     * <pre>
     * (if condition:
     * ifStatement1
     * ifStatement2
     * else:
     * elseStatement1
     * )
     * </pre>
     *
     * <p>If {@code condition} is {@code null}, the current implementation does
     * not emit the opening {@code "(if "} text but still emits the branch
     * separator and final closing parenthesis.
     *
     * <p>The current implementation does not add indentation before statements
     * in either branch.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying if-else-expression AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          if-else expression
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.condition != null:
        {
            sb.append("(if ")
            sb.append(this.condition.toString())
        }

        sb.append(":\n")

        for (var i = 0; i < this.ifStmts.length; i++):
        {
            val statement: pointer<Statement> = this.ifStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }


        if this.haveElseStatement():
        {
            sb.append("else:\n")

            for (var i = 0; i < this.elseStmts.length; i++):
            {
                val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

                if statement == null:
                    continue

                sb.append(statement.toString())
                sb.append("\n")
            }
        }

        sb.append(")\n")

        return sb
    }
}
