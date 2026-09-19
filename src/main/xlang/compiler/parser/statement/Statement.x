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

import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a generic statement node using a tagged statement
 * representation.
 *
 * <p>A {@code Statement} combines an integer statement kind with an untyped
 * {@code root} pointer. The {@code kind} value determines the concrete AST node
 * type referenced by {@code root}.
 *
 * <p>Statement instances are normally created through one of the static
 * {@code from...()} factory methods. Each factory associates the supplied
 * concrete statement node with the corresponding statement-kind constant.
 *
 * <p>The currently supported statement kinds include expression statements,
 * expression-list statements, variable declarations, groups of variable
 * declarations, while loops, for loops, return statements, break statements,
 * continue statements, and pass statements.
 *
 * <p>The {@code root} pointer is stored by reference and is not copied or
 * cloned. Correct operation therefore depends on the statement kind matching the
 * actual concrete type referenced by {@code root}.
 *
 * <p>Some statement kinds represent intermediate parser structures that can be
 * expanded into lower-level statement sequences through {@code expand()}.
 * Other statement kinds remain unchanged during expansion.
 *
 * <p>Additional syntax tokens may be stored directly by the wrapper. These
 * tokens are combined with tokens supplied by the concrete statement node when
 * {@code getAllTokens()} is called.
 */
struct Statement
{
    // Identifies a statement whose root is an {@code ExprStatement}.
    static val EXPRESSION_TYPE: int = 0

    /**
     * Identifies a statement whose root is an {@code ExprListStatement}.
     *
     * <p>This statement kind can be expanded into separate expression
     * statements through {@code expand()}.
     */
    static val EXPRESSION_LIST_TYPE: int = 1

    // Identifies a statement whose root is a {@code VariableDefine}.
    static val VARIABLE_DEFINE_TYPE: int = 2

    /**
     * Identifies a statement whose root is a {@code VariableDefines}.
     *
     * <p>This statement kind can be expanded through
     * {@code VariableDefines.expand()}.
     */
    static val VARIABLE_DEFINES_TYPE: int = 3

    // Identifies a statement whose root is a {@code WhileStatement}.
    static val WHILE_TYPE: int = 4

    /**
     * Identifies a statement whose root is a {@code ForStatement}.
     *
     * <p>This statement kind can be lowered into other statement forms through
     * {@code ForStatement.expand()}.
     */
    static val FOR_TYPE: int = 5

    // Identifies a statement whose root is a {@code ReturnStatement}.
    static val RETURN_TYPE: int = 6

    // Identifies a statement whose root is a {@code BreakStatement}.
    static val BREAK_TYPE: int = 7

    // Identifies a statement whose root is a {@code ContinueStatement}.
    static val CONTINUE_TYPE: int = 8

    // Identifies a statement whose root is a {@code PassStatement}.
    static val PASS_TYPE: int = 9


    /**
     * Creates a generic statement wrapper for an expression statement.
     *
     * <p>The supplied {@code ExprStatement} is stored by reference as the root
     * object and the resulting wrapper is tagged with
     * {@code EXPRESSION_TYPE}.
     *
     * <p>The supplied pointer is not copied or cloned. A {@code null} pointer is
     * permitted by the factory and results in a statement wrapper whose root is
     * {@code null}.
     *
     * @param expr              a pointer to the expression statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as an
     *                          expression statement
     */
    static fun fromExprStatement(expr: pointer<ExprStatement>) -> pointer<Statement> =
        new Statement(EXPRESSION_TYPE, expr)


    /**
     * Creates a generic statement wrapper for a variable declaration.
     *
     * <p>The supplied {@code VariableDefine} is stored by reference and the
     * wrapper is tagged with {@code VARIABLE_DEFINE_TYPE}.
     *
     * @param variableDefine    a pointer to the variable declaration to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          variable declaration
     */
    static fun fromExprListStatement(expr: pointer<ExprListStatement>) -> pointer<Statement> =
        new Statement(EXPRESSION_LIST_TYPE, expr)


    /**
     * Creates a generic statement wrapper for a group of variable declarations.
     *
     * <p>The supplied {@code VariableDefines} object is stored by reference and
     * the wrapper is tagged with {@code VARIABLE_DEFINES_TYPE}.
     *
     * <p>This statement kind participates in expansion through
     * {@code VariableDefines.expand()}.
     *
     * @param variableDefines   a pointer to the variable-declaration group to
     *                          wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          variable-declaration group
     */
    static fun fromVariableDefine(variableDefine: pointer<VariableDefine>) -> pointer<Statement> =
        new Statement(VARIABLE_DEFINE_TYPE, variableDefine)


    /**
     * Creates a generic statement wrapper for a return statement.
     *
     * <p>The supplied {@code ReturnStatement} is stored by reference and the
     * wrapper is tagged with {@code RETURN_TYPE}.
     *
     * @param statement         a pointer to the return statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          return statement
     */
    static fun fromVariableDefines(variableDefines: pointer<VariableDefines>) -> pointer<Statement> =
        new Statement(VARIABLE_DEFINES_TYPE, variableDefines)


    /**
     * Creates a generic statement wrapper for a while statement.
     *
     * <p>The supplied {@code WhileStatement} is stored by reference and the
     * wrapper is tagged with {@code WHILE_TYPE}.
     *
     * @param statement         a pointer to the while statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          while statement
     */
    static fun fromReturnStatement(statement: pointer<ReturnStatement>) -> pointer<Statement> =
        new Statement(RETURN_TYPE, statement)


    /**
     * Creates a generic statement wrapper for a for statement.
     *
     * <p>The supplied {@code ForStatement} is stored by reference and the
     * wrapper is tagged with {@code FOR_TYPE}.
     *
     * <p>For statements participate in expansion through
     * {@code ForStatement.expand()}.
     *
     * @param statement         a pointer to the for statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          for statement
     */
    static fun fromWhileStatement(statement: pointer<WhileStatement>) -> pointer<Statement> =
        new Statement(WHILE_TYPE, statement)


    /**
     * Creates a generic statement wrapper for a break statement.
     *
     * <p>The supplied {@code BreakStatement} is stored by reference and the
     * wrapper is tagged with {@code BREAK_TYPE}.
     *
     * @param statement         a pointer to the break statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          break statement
     */
    static fun fromForStatement(statement: pointer<ForStatement>) -> pointer<Statement> =
        new Statement(FOR_TYPE, statement)


    /**
     * Creates a generic statement wrapper for a continue statement.
     *
     * <p>The supplied {@code ContinueStatement} is stored by reference and the
     * wrapper is tagged with {@code CONTINUE_TYPE}.
     *
     * @param statement         a pointer to the continue statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          continue statement
     */
    static fun fromBreakStatement(statement: pointer<BreakStatement>) -> pointer<Statement> =
        new Statement(BREAK_TYPE, statement)


    /**
     * Creates a generic statement wrapper for a pass statement.
     *
     * <p>The supplied {@code PassStatement} is stored by reference and the
     * wrapper is tagged with {@code PASS_TYPE}.
     *
     * @param statement         a pointer to the pass statement to wrap
     *
     * @return                  a newly created {@code Statement} tagged as a
     *                          pass statement
     */
    static fun fromContinueStatement(statement: pointer<ContinueStatement>) -> pointer<Statement> =
        new Statement(CONTINUE_TYPE, statement)


    /**
     * The discriminator identifying the concrete statement type referenced by
     * {@code root}.
     *
     * <p>The value is expected to be one of the statement-kind constants
     * declared by this structure.
     */
    static fun fromPassStatement(statement: pointer<PassStatement>) -> pointer<Statement> =
        new Statement(PASS_TYPE, statement)


    /**
     * The discriminator identifying the concrete statement type referenced by
     * {@code root}.
     *
     * <p>The value is expected to be one of the statement-kind constants
     * declared by this structure.
     */
    private var kind: int

    /**
     * The concrete AST node represented by this generic statement wrapper.
     *
     * <p>The pointer is intentionally untyped. Its actual type is determined by
     * {@code kind} and is recovered through explicit casts during dispatch.
     *
     * <p>The root object is stored by reference and is not copied or cloned.
     */
    private var root: pointer<*>

    /**
     * Additional syntax tokens associated directly with this statement wrapper.
     *
     * <p>These tokens are independent from tokens stored by the concrete root
     * statement and are merged with root tokens by {@code getAllTokens()}.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a generic statement wrapper with the specified statement kind and
     * root object.
     *
     * <p>The supplied kind is stored without validation. The root pointer is also
     * stored directly and is not copied or cloned.
     *
     * <p>Correct dispatch therefore requires the supplied kind to correspond to
     * the actual concrete type referenced by {@code root}.
     *
     * <p>A new empty collection is allocated for statement-level additional
     * syntax tokens.
     *
     * @param kind              the statement-kind discriminator identifying the
     *                          concrete root type
     * @param root              an untyped pointer to the concrete statement
     *                          object, or {@code null}
     */
    private constructor(kind: int, root: pointer<*>)
    {
        this.kind = kind
        this.root = root
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Adds an additional syntax token directly to this statement wrapper.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Wrapper-level tokens are combined with tokens supplied by the concrete
     * root statement when {@code getAllTokens()} is called.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code Statement} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Statement>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the statement-kind discriminator stored by this wrapper.
     *
     * <p>The returned value identifies the concrete statement type expected to
     * be referenced by {@code root}.
     *
     * @return                  the integer statement-kind discriminator
     */
    fun getKind() -> int = this.kind


    /**
     * Returns the untyped root object represented by this statement.
     *
     * <p>The returned pointer refers directly to the internally stored root and
     * is not copied or cloned.
     *
     * <p>The caller must interpret the pointer according to the value returned by
     * {@code getKind()}.
     *
     * @return                  the internally stored untyped root pointer, or
     *                          {@code null} if no root object is stored
     */
    fun getRoot() -> pointer<*> = this.root


    /**
     * Expands this statement into its lower-level statement representation.
     *
     * <p>Only statement kinds that represent expandable parser structures are
     * dispatched to specialized expansion logic.
     *
     * <p>A {@code VARIABLE_DEFINES_TYPE} statement casts its root to
     * {@code VariableDefines} and returns the result of
     * {@code VariableDefines.expand()}.
     *
     * <p>An {@code EXPRESSION_LIST_TYPE} statement casts its root to
     * {@code ExprListStatement} and returns the result of
     * {@code ExprListStatement.expand()}.
     *
     * <p>A {@code FOR_TYPE} statement casts its root to {@code ForStatement} and
     * returns the result of {@code ForStatement.expand()}.
     *
     * <p>Every other statement kind is already treated as a non-expandable
     * statement. A new statement collection is created and this wrapper itself
     * is inserted into that collection.
     *
     * <p>The current implementation does not check whether {@code root} is
     * {@code null} before dispatching the three expandable statement kinds.
     * Those kinds therefore assume that their root pointer references a valid
     * object of the corresponding concrete type.
     *
     * @return                  the expanded statement collection, or a newly
     *                          allocated single-element collection containing
     *                          this statement when no expansion is required
     */
    fun expand() -> pointer<ArrayList> =
        if this.kind == VARIABLE_DEFINES_TYPE:
        {
            val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
            statement.expand()
        }
        elif this.kind == EXPRESSION_LIST_TYPE:
        {
            val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
            statement.expand()
        }
        elif this.kind == FOR_TYPE:
        {
            val statement: pointer<ForStatement> = this.root as pointer<ForStatement>
            statement.expand()
        }
        else: new ArrayList(sizeof(Statement)).push(this)


    /**
     * Returns all tokens associated with this statement wrapper and its concrete
     * root statement.
     *
     * <p>A new token collection is allocated and the additional syntax tokens
     * stored directly by this wrapper are appended first.
     *
     * <p>If {@code root} is not {@code null}, the statement kind is inspected and
     * the root pointer is cast to the corresponding concrete statement type.
     * Token collection is then delegated to that statement's
     * {@code getAllTokens()} implementation.
     *
     * <p>The currently supported dispatch targets are {@code ExprStatement},
     * {@code ExprListStatement}, {@code VariableDefine},
     * {@code VariableDefines}, {@code WhileStatement}, {@code ForStatement},
     * {@code ReturnStatement}, {@code BreakStatement},
     * {@code ContinueStatement}, and {@code PassStatement}.
     *
     * <p>If the kind is not recognized, no root token collection is produced and
     * only the wrapper-level additional tokens remain in the result.
     *
     * <p>If the dispatched concrete statement returns {@code null}, no tokens are
     * appended from that root.
     *
     * <p>After all available tokens have been collected, the result is sorted
     * according to source position using {@code TokenPosition.compareToken}.
     *
     * <p>A new list is allocated for the result. Individual token objects are
     * referenced rather than recursively cloned.
     *
     * <p>Correct dispatch assumes that {@code kind} matches the actual concrete
     * type referenced by {@code root}.
     *
     * @return                  a newly allocated list containing all tokens
     *                          associated with this statement in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)

        if this.root != null:
        {
            var tokens: pointer<ArrayList> = if this.kind == EXPRESSION_TYPE:
            {
                val statement: pointer<ExprStatement> = this.root as pointer<ExprStatement>
                statement.getAllTokens()
            }
            elif this.kind == EXPRESSION_LIST_TYPE:
            {
                val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
                statement.getAllTokens()
            }
            elif this.kind == VARIABLE_DEFINE_TYPE:
            {
                val statement: pointer<VariableDefine> = this.root as pointer<VariableDefine>
                statement.getAllTokens()
            }
            elif this.kind == VARIABLE_DEFINES_TYPE:
            {
                val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
                statement.getAllTokens()
            }
            elif this.kind == WHILE_TYPE:
            {
                val statement: pointer<WhileStatement> = this.root as pointer<WhileStatement>
                statement.getAllTokens()
            }
            elif this.kind == FOR_TYPE:
            {
                val statement: pointer<ForStatement> = this.root as pointer<ForStatement>
                statement.getAllTokens()
            }
            elif this.kind == RETURN_TYPE:
            {
                val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
                statement.getAllTokens()
            }
            elif this.kind == BREAK_TYPE:
            {
                val statement: pointer<BreakStatement> = this.root as pointer<BreakStatement>
                statement.getAllTokens()
            }
            elif this.kind == CONTINUE_TYPE:
            {
                val statement: pointer<ContinueStatement> = this.root as pointer<ContinueStatement>
                statement.getAllTokens()
            }
            elif this.kind == PASS_TYPE:
            {
                val statement: pointer<PassStatement> = this.root as pointer<PassStatement>
                statement.getAllTokens()
            }
            else: null


            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of the concrete statement represented
     * by this wrapper.
     *
     * <p>If {@code root} is {@code null}, a newly allocated empty
     * {@code StringBuilder} is returned.
     *
     * <p>Otherwise, the statement kind is inspected and the root pointer is cast
     * to the corresponding concrete statement type. Text generation is then
     * delegated directly to that statement's {@code toString()} method.
     *
     * <p>The currently recognized statement kinds dispatch to
     * {@code ExprStatement}, {@code ExprListStatement},
     * {@code VariableDefine}, {@code VariableDefines},
     * {@code WhileStatement}, {@code ForStatement},
     * {@code ReturnStatement}, {@code BreakStatement},
     * {@code ContinueStatement}, or {@code PassStatement}.
     *
     * <p>The {@code StringBuilder} returned by the concrete statement is returned
     * directly by this wrapper; it is not copied into another builder.
     *
     * <p>If {@code kind} is not recognized, a newly allocated empty
     * {@code StringBuilder} is returned.
     *
     * <p>Correct dispatch assumes that {@code kind} corresponds to the actual
     * concrete type referenced by {@code root}.
     *
     * @return                  the textual representation returned by the
     *                          concrete statement, or a newly allocated empty
     *                          {@code StringBuilder} if the root is absent or
     *                          the statement kind is not recognized
     */
    fun toString() -> pointer<StringBuilder> = 
        if this.root == null:
            new StringBuilder()
        elif this.kind == EXPRESSION_TYPE:
        {
            val statement: pointer<ExprStatement> = this.root as pointer<ExprStatement>
            statement.toString()
        }
        elif this.kind == EXPRESSION_LIST_TYPE:
        {
            val statement: pointer<ExprListStatement> = this.root as pointer<ExprListStatement>
            statement.toString()
        }
        elif this.kind == VARIABLE_DEFINE_TYPE:
        {
            val statement: pointer<VariableDefine> = this.root as pointer<VariableDefine> 
            statement.toString()
        }
        elif this.kind == VARIABLE_DEFINES_TYPE:
        {
            val statement: pointer<VariableDefines> = this.root as pointer<VariableDefines>
            statement.toString()
        }
        elif this.kind == WHILE_TYPE:
        {
            val statement: pointer<WhileStatement> = this.root as pointer<WhileStatement>
            statement.toString()
        }
        elif this.kind == FOR_TYPE:
        {
            val statement: pointer<ForStatement> = this.root as pointer<ForStatement>
            statement.toString()
        }
        elif this.kind == RETURN_TYPE:
        {
            val statement: pointer<ReturnStatement> = this.root as pointer<ReturnStatement>
            statement.toString()
        }
        elif this.kind == BREAK_TYPE:
        {
            val statement: pointer<BreakStatement> = this.root as pointer<BreakStatement>
            statement.toString()
        }
        elif this.kind == CONTINUE_TYPE:
        {
            val statement: pointer<ContinueStatement> = this.root as pointer<ContinueStatement>
            statement.toString()
        }
        elif this.kind == PASS_TYPE:
        {
            val statement: pointer<PassStatement> = this.root as pointer<PassStatement>
            statement.toString()
        }
        else: new StringBuilder()
}
