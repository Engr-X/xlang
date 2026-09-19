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

import xlang.compiler.lexer.Tokenizer
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.expression.Expression
import xlang.compiler.parser.stmtexpr.Block
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents the header portion of a C-style {@code for} statement.
 *
 * <p>A {@code ForHeader} stores the optional initialization statement,
 * condition expression, and step statement that appear inside the parentheses
 * of a {@code for} declaration.
 *
 * <p>Each of the three components may be {@code null}. This allows headers such
 * as:
 *
 * <pre>
 * for (; condition; step)
 * for (init; ; step)
 * for (init; condition; )
 * for (; ; )
 * </pre>
 *
 * <p>Additional syntax tokens are stored separately and may include the
 * semicolon separators or other lexical elements belonging to the header.
 *
 * <p>All tokens belonging to the header and its child AST nodes can be collected
 * in lexical source order through {@code getAllTokens()}.
 */
struct ForHeader
{
    /**
     * The optional initialization statement executed before loop evaluation
     * begins.
     *
     * <p>A {@code null} value indicates that no initialization statement was
     * specified in the header.
     */
    private var initStmt: pointer<Statement>

    /**
     * The optional condition expression controlling loop execution.
     *
     * <p>A {@code null} value represents an omitted condition. During
     * {@code ForStatement.expand()}, such a missing condition is replaced by an
     * automatically generated boolean {@code true} expression.
     */
    private var condition: pointer<Expression>

    /**
     * The optional step statement executed after the loop body.
     *
     * <p>A {@code null} value indicates that no step statement was specified.
     */
    private var stepStmt: pointer<Statement>

    /**
     * Additional syntax tokens associated with this for-loop header.
     *
     * <p>This collection may contain semicolon separators or other lexical
     * elements that are not directly owned by the initialization statement,
     * condition expression, or step statement.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates an empty for-loop header.
     *
     * <p>The initialization statement, condition expression, and step statement
     * are all initialized to {@code null}.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     */
    constructor()
    {
        this.initStmt = null
        this.condition = null
        this.stepStmt = null
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a for-loop header from the specified initialization statement,
     * condition expression, and step statement.
     *
     * <p>All three supplied AST nodes are stored directly and are not copied or
     * cloned. Any of them may be {@code null} to represent an omitted component
     * of the header.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param initStmt          a pointer to the initialization statement, or
     *                          {@code null} if no initialization is specified
     * @param condition         a pointer to the loop condition expression, or
     *                          {@code null} if the condition is omitted
     * @param stepStmt          a pointer to the step statement, or {@code null}
     *                          if no step statement is specified
     */
    constructor(
        initStmt: pointer<Statement>,
        condition: pointer<Expression>,
        stepStmt: pointer<Statement>)
    {
        this.initStmt = initStmt
        this.condition = condition
        this.stepStmt = stepStmt
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the initialization statement associated with this header.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Statement} and is not copied or cloned.
     *
     * <p>The result is {@code null} when no initialization statement was
     * specified.
     *
     * @return                  a pointer to the initialization statement, or
     *                          {@code null} if no initialization is present
     */
    fun getInitStatement() -> pointer<Statement> = this.initStmt


    /**
     * Returns the condition expression associated with this header.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Expression} and is not copied or cloned.
     *
     * <p>The result may be {@code null} when the source header omitted the
     * condition.
     *
     * @return                  a pointer to the loop condition expression, or
     *                          {@code null} if the condition is omitted
     */
    fun getCondition() -> pointer<Expression> = this.condition


    /**
     * Returns the step statement associated with this header.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code Statement} and is not copied or cloned.
     *
     * <p>The result is {@code null} when no step statement was specified.
     *
     * @return                  a pointer to the loop step statement, or
     *                          {@code null} if no step statement is present
     */
    fun getStepStatement() -> pointer<Statement> = this.stepStmt


    /**
     * Adds an additional syntax token to this for-loop header.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ForHeader} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ForHeader>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Appends all tokens from the specified collection to this for-loop header.
     *
     * <p>If {@code tokens} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all token entries are appended to the internal extra-token
     * collection in their existing order using {@code ArrayList.pushAll()}.
     *
     * <p>The supplied collection itself is not modified, and the contained token
     * objects are not recursively cloned.
     *
     * @param tokens            a pointer to the syntax-token collection to
     *                          append
     *
     * @return                  this {@code ForHeader} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ForHeader>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this
     * for-loop header.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes performed through the returned collection affect the same token
     * list referenced internally by this {@code ForHeader}.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Returns all tokens associated with this for-loop header.
     *
     * <p>If an initialization statement is present, its token collection is
     * obtained through {@code Statement.getAllTokens()} and appended when the
     * returned collection is not {@code null}.
     *
     * <p>If a condition expression is present, all tokens returned by
     * {@code Expression.getAllTokens()} are then appended.
     *
     * <p>If a step statement is present, its tokens are collected in the same
     * manner.
     *
     * <p>The tokens collected from the three structural header components are
     * combined with the additional syntax tokens stored directly by this
     * header.
     *
     * <p>The complete result is sorted according to source position using
     * {@code TokenPosition.compareToken}, restoring the original lexical order
     * regardless of the order in which tokens were gathered.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this for-loop header in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.initStmt != null:
        {
            val tokens: pointer<ArrayList> = this.initStmt.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.condition != null:
        {
            val tokens: pointer<ArrayList> = this.condition.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        if this.stepStmt != null:
        {
            val tokens: pointer<ArrayList> = this.stepStmt.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this for-loop header.
     *
     * <p>The initialization statement is emitted first when present, followed by
     * {@code "; "}.
     *
     * <p>The condition expression is emitted next when present, followed by a
     * second {@code "; "}.
     *
     * <p>The step statement is emitted last when present.
     *
     * <p>The two separators are always emitted even when one or more header
     * components are absent. This allows incomplete headers such as
     * {@code "; condition; step"} or {@code "; ;"} to retain the expected
     * three-part structure.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying header AST node.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          for-loop header
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.initStmt != null:
            sb.append(this.initStmt.toString())

        sb.append("; ")

        if this.condition != null:
            sb.append(this.condition.toString())

        sb.append("; ")

        if this.stepStmt != null:
            sb.append(this.stepStmt.toString())

        return sb
    }
}


/**
 * Represents a C-style {@code for} statement in the statement abstract syntax
 * tree.
 *
 * <p>A {@code ForStatement} consists of a {@code ForHeader}, an ordered
 * collection of body statements, an optional else-body collection, and
 * additional syntax tokens retained from the original source.
 *
 * <p>The header contains the initialization statement, condition expression, and
 * step statement. Missing headers are normalized to an empty
 * {@code ForHeader}. Missing body or else-body collections are normalized to
 * empty statement lists.
 *
 * <p>The statement can be desugared through {@code expand()} into an equivalent
 * block containing an optional initialization statement followed by a
 * {@code WhileStatement}. The step statement is appended to the generated while
 * body, and an omitted condition is replaced with an automatically generated
 * boolean {@code true} expression.
 *
 * <p>Additional syntax tokens from both the original for statement and its
 * header are transferred to the generated block during expansion.
 */
struct ForStatement
{
    /**
     * Creates the default condition used when a for-loop header omits its
     * condition expression.
     *
     * <p>An automatically generated token position is created first. A
     * synthetic {@code true} token using {@code Tokenizer.KW_TRUE} is then
     * constructed at that position.
     *
     * <p>The token is stored in a newly allocated atom-token collection and used
     * to construct a boolean immediate {@code Atom}. That atom is finally wrapped
     * in an {@code Expression}.
     *
     * <p>The resulting expression represents the boolean literal
     * {@code true}, allowing a for loop without an explicit condition to behave
     * as an unconditional loop during expansion.
     *
     * @return                  a newly created boolean {@code true} expression
     *                          using an automatically generated source position
     */
    private static fun makeDefaultCondition() -> pointer<Expression>
    {
        val position: pointer<TokenPosition> = TokenPosition.autoGenPos()
        val token: pointer<Token> = new Token(Tokenizer.KW_TRUE, position, "true")
        val tokens: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
        val resultItem: pointer<*> = token as pointer<*>

        tokens.push(resultItem.ref)
        return Expression.fromAtom(new Atom(Atom.BOOL_IMM_KIND, tokens))
    }


    /**
     * The header describing initialization, condition, and step components of
     * this for loop.
     *
     * <p>The constructors normalize a missing header to an empty
     * {@code ForHeader} instance.
     */
    private var header: pointer<ForHeader>

    /**
     * The ordered collection of statements forming the main loop body.
     *
     * <p>The collection is normalized to an empty statement list when no body
     * collection is supplied.
     */
    private val bodyStmts: pointer<ArrayList>

    /**
     * The ordered collection of statements forming the optional loop else
     * branch.
     *
     * <p>An empty collection represents a for loop without an else branch.
     */
    private val elseStmts: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this for statement.
     *
     * <p>This collection may contain the {@code for} keyword, parentheses,
     * branch delimiters, or other lexical elements not directly owned by the
     * header or child statements.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a for statement without an explicit else-body collection.
     *
     * <p>If {@code header} is {@code null}, a new empty {@code ForHeader} is
     * created. Otherwise, the supplied header is stored directly.
     *
     * <p>If {@code bodyStmts} is {@code null}, a new empty statement list is
     * allocated. Otherwise, the supplied body collection is stored directly and
     * is not copied or cloned.
     *
     * <p>A new empty else-body collection and a new empty extra-token collection
     * are allocated.
     *
     * @param header            a pointer to the for-loop header, or
     *                          {@code null} to create an empty header
     * @param bodyStmts         a pointer to the loop-body statement collection,
     *                          or {@code null} to create an empty body
     */
    constructor(
        header: pointer<ForHeader>,
        bodyStmts: pointer<ArrayList>)
    {
        this.header = if header == null:
            new ForHeader()
        else:
            header

        this.bodyStmts = if bodyStmts == null:
            new ArrayList(sizeof(Statement))
        else:
            bodyStmts

        this.elseStmts = new ArrayList(sizeof(Statement))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a for statement with explicit body and else-body collections.
     *
     * <p>If {@code header} is {@code null}, a new empty {@code ForHeader} is
     * allocated.
     *
     * <p>If {@code bodyStmts} or {@code elseStmts} is {@code null}, the
     * corresponding value is normalized to a newly allocated empty statement
     * collection.
     *
     * <p>Non-null objects and collections are stored directly and are not copied
     * or cloned.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param header            a pointer to the for-loop header, or
     *                          {@code null} to create an empty header
     * @param bodyStmts         a pointer to the loop-body statement collection,
     *                          or {@code null} to create an empty body
     * @param elseStmts         a pointer to the else-body statement collection,
     *                          or {@code null} to create an empty else body
     */
    constructor(
        header: pointer<ForHeader>,
        bodyStmts: pointer<ArrayList>,
        elseStmts: pointer<ArrayList>)
    {
        this.header = if header == null:
            new ForHeader()
        else:
            header

        this.bodyStmts = if bodyStmts == null:
            new ArrayList(sizeof(Statement))
        else:
            bodyStmts

        this.elseStmts = if elseStmts == null:
            new ArrayList(sizeof(Statement))
        else:
            elseStmts

        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the initialization statement stored in this for-loop header.
     *
     * <p>The request is delegated directly to
     * {@code ForHeader.getInitStatement()}.
     *
     * @return                  a pointer to the initialization statement, or
     *                          {@code null} if no initialization is present
     */
    fun getInitStatement() -> pointer<Statement> = this.header.getInitStatement()


    /**
     * Returns the condition expression stored in this for-loop header.
     *
     * <p>The request is delegated directly to
     * {@code ForHeader.getCondition()}. The returned value may be
     * {@code null} when the source header omitted its condition.
     *
     * @return                  a pointer to the condition expression, or
     *                          {@code null} if no condition is present
     */
    fun getCondition() -> pointer<Expression> = this.header.getCondition()


    /**
     * Returns the step statement stored in this for-loop header.
     *
     * <p>The request is delegated directly to
     * {@code ForHeader.getStepStatement()}.
     *
     * @return                  a pointer to the step statement, or
     *                          {@code null} if no step statement is present
     */
    fun getStepStatement() -> pointer<Statement> = this.header.getStepStatement()


    /**
     * Returns the statement collection forming the main body of this for loop.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Changes made through the returned list affect the body represented by
     * this {@code ForStatement}.
     *
     * @return                  a pointer to the internally stored loop-body
     *                          statement collection
     */
    fun getBodyStatements() -> pointer<ArrayList> = this.bodyStmts


    /**
     * Returns the statement collection forming the optional else branch.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * @return                  a pointer to the internally stored else-body
     *                          statement collection
     */
    fun getElseStatements() -> pointer<ArrayList> = this.elseStmts


    /**
     * Returns whether this for loop contains at least one else-body statement.
     *
     * <p>The result is determined exclusively from the length of the internal
     * else-statement collection.
     *
     * @return                  {@code true} if at least one else-body statement
     *                          is stored; {@code false} otherwise
     */
    fun haveElseStatement() -> bool = this.elseStmts.length > 0


    /**
     * Adds an additional syntax token to this for statement.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code ForStatement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<ForStatement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Appends all tokens from the specified collection to this for statement.
     *
     * <p>If {@code tokens} is {@code null}, no modification is performed.
     *
     * <p>Otherwise, all entries are appended to the internal extra-token
     * collection in their existing order.
     *
     * @param tokens            a pointer to the syntax-token collection to
     *                          append
     *
     * @return                  this {@code ForStatement} instance
     */
    fun addExtraTokens(tokens: pointer<ArrayList>) -> pointer<ForStatement>
    {
        if tokens != null:
            this.extraTokens.pushAll(tokens)

        return this
    }


    /**
     * Returns the additional syntax-token collection associated with this for
     * statement.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * @return                  a pointer to the internally stored additional
     *                          syntax-token collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens


    /**
     * Expands this for statement into lower-level block and while constructs.
     *
     * <p>A new statement collection is first created for the generated while
     * body. All original body statements are appended to this collection in
     * their existing order.
     *
     * <p>If the for-loop header contains a step statement, that statement is
     * appended after the original body statements. The generated while body
     * therefore has the conceptual form:
     *
     * <pre>
     * body statements
     * step statement
     * </pre>
     *
     * <p>The loop condition is then resolved. If the header contains an explicit
     * condition, that expression is reused directly. Otherwise,
     * {@code makeDefaultCondition()} creates a synthetic boolean {@code true}
     * expression, turning the generated while loop into an unconditional loop.
     *
     * <p>A new {@code WhileStatement} is created using the resolved condition,
     * generated body collection, and the original else-body collection.
     *
     * <p>A new outer statement list is then created. If an initialization
     * statement is present, it is placed before the generated while statement.
     * The generated structure is therefore conceptually:
     *
     * <pre>
     * {
     *     init
     *     while (condition):
     *         body
     *         step
     * }
     * </pre>
     *
     * <p>The resulting statements are wrapped in a new {@code Block}. Additional
     * tokens stored by both the original {@code ForHeader} and this
     * {@code ForStatement} are copied into that generated block.
     *
     * <p>The block is then wrapped as a block expression, converted to an
     * {@code ExprStatement}, and finally wrapped as a general {@code Statement}.
     * The returned collection contains exactly that single generated statement.
     *
     * <p>The original AST nodes are reused by reference during expansion and are
     * not recursively cloned.
     *
     * @return                  a newly allocated statement list containing the
     *                          single block statement produced by desugaring
     *                          this for loop
     */
    fun expand() -> pointer<ArrayList>
    {
        val whileBodyStmts: pointer<ArrayList> = new ArrayList(sizeof(Statement))
        whileBodyStmts.pushAll(this.bodyStmts)

        if this.header.getStepStatement() != null:
            whileBodyStmts.push(this.header.getStepStatement())

        val condition: pointer<Expression> = if this.header.getCondition() == null:
            ForStatement.makeDefaultCondition()
        else:
            this.header.getCondition()

        val whileStmt: pointer<WhileStatement> =
            new WhileStatement(condition, whileBodyStmts, this.elseStmts)

        val resultStmts: pointer<ArrayList> = new ArrayList(sizeof(Statement))

        if this.header.getInitStatement() != null:
            resultStmts.push(this.header.getInitStatement())

        resultStmts.push(Statement.fromWhileStatement(whileStmt))

        val block: pointer<Block> = new Block(resultStmts)
        val headerTokens: pointer<ArrayList> = this.header.getExtraTokens()

        for (var i: int = 0; i < headerTokens.length; i++):
            block.addExtraToken(headerTokens.get(i) as pointer<Token>)

        for (var i: int = 0; i < this.extraTokens.length; i++):
            block.addExtraToken(this.extraTokens.get(i) as pointer<Token>)

        val blockExpr: pointer<Expression> = Expression.fromBlockExpr(block)
        val blockStmt: pointer<Statement> = Statement.fromExprStatement(new ExprStatement(blockExpr))
        val result: pointer<ArrayList> = new ArrayList(sizeof(Statement))
        result.push(blockStmt)
        return result
    }


    /**
     * Returns all tokens associated with this for statement and its child AST
     * nodes.
     *
     * <p>The complete header token collection is obtained first through
     * {@code ForHeader.getAllTokens()} and appended when it is not
     * {@code null}.
     *
     * <p>The main body collection is then traversed in order. Null statements
     * are ignored. For every valid statement, its token collection is appended
     * when that collection is not {@code null}.
     *
     * <p>The else-body collection is processed in the same manner.
     *
     * <p>After tokens from the header, body, and else branch have been
     * collected, the additional tokens stored directly by this for statement
     * are appended.
     *
     * <p>The complete collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the original lexical
     * ordering across all components of the for statement.
     *
     * <p>A new result list is allocated. The individual token objects are
     * referenced rather than recursively cloned.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this for statement in source
     *                          order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))
        val headerTokens: pointer<ArrayList> = this.header.getAllTokens()

        if headerTokens != null:
            result.pushAll(headerTokens)

        for (var i: int = 0; i < this.bodyStmts.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            val tokens: pointer<ArrayList> = statement.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i: int = 0; i < this.elseStmts.length; i++):
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
     * Returns the textual representation of this for statement.
     *
     * <p>The representation begins with {@code "for ("}, followed by the
     * textual representation of the header and the sequence {@code "):\n"}.
     *
     * <p>The main body statements are then emitted in their stored order. Null
     * statements are skipped, and each valid statement is followed by a
     * newline.
     *
     * <p>If at least one else-body statement exists, an {@code "else:\n"}
     * section is appended. Valid else-body statements are emitted in their
     * stored order and are also followed by newlines.
     *
     * <p>The method does not currently add indentation before statements in
     * either the main body or the else branch.
     *
     * <p>A typical representation therefore has the form:
     *
     * <pre>
     * for (init; condition; step):
     * body1
     * body2
     * else:
     * elseBody1
     * </pre>
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying for-statement AST.
     *
     * @return                  a pointer to a newly created {@code StringBuilder}
     *                          containing the textual representation of this
     *                          for statement
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        sb.append("for (")
        sb.append(this.header.toString())
        sb.append("):\n")

        for (var i: int = 0; i < this.bodyStmts.length; i++):
        {
            val statement: pointer<Statement> = this.bodyStmts.get(i) as pointer<Statement>

            if statement == null:
                continue

            sb.append(statement.toString())
            sb.append("\n")
        }

        if this.haveElseStatement():
        {
            sb.append("else:\n")

            for (var i: int = 0; i < this.elseStmts.length; i++):
            {
                val statement: pointer<Statement> = this.elseStmts.get(i) as pointer<Statement>

                if statement == null:
                    continue

                sb.append(statement.toString())
                sb.append("\n")
            }
        }

        return sb
    }
}
