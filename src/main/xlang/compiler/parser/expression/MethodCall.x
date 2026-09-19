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

package xlang.compiler.parser.expression

import xlang.Operation
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Represents a method or function call expression.
 *
 * <p>A {@code MethodCall} stores an optional host expression, the name of the
 * function or method being invoked, an ordered collection of argument
 * expressions, and any additional syntax tokens associated with the call.
 *
 * <p>If {@code host} is not {@code null}, the call represents a member-style
 * invocation such as {@code host.method(...)}. If no host is present, the call
 * represents a direct function or intrinsic invocation such as
 * {@code function(...)}.
 *
 * <p>Argument expressions and the host expression are stored by reference.
 * Additional syntax tokens may be retained to preserve source information such
 * as parentheses, commas, member-access operators, and other delimiters.
 */
struct MethodCall
{
    /**
     * The optional host expression on which the method is invoked.
     *
     * <p>This value may be {@code null} for direct function calls, intrinsic
     * operations, or other calls that do not require an explicit receiver.
     */
    private var host: pointer<Expression>

    /**
     * The null-terminated name of the method or function being invoked.
     *
     * <p>Depending on the constructor used, this value may either be duplicated
     * from an explicitly supplied name or obtained from an {@code Operation}.
     */
    private var callName: pointer<char>

    /**
     * The ordered collection of argument expressions supplied to this call.
     *
     * <p>Each entry stores a pointer to an {@code Expression}. Arguments are
     * retained in the same order in which they are added or provided by an
     * {@code ExpressionTuple}.
     */
    private var arguments: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with this method call.
     *
     * <p>This collection may contain tokens such as parentheses, commas,
     * member-access operators, or other syntax elements that are not directly
     * owned by the host or argument expressions.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a method call using the specified host expression and call name.
     *
     * <p>The host expression is stored by reference and is not copied.
     *
     * <p>The supplied call name is duplicated using {@code String.strdup}, so
     * the internally stored name does not depend on the lifetime of the
     * original character sequence.
     *
     * <p>A new empty argument list and a new empty extra-token collection are
     * allocated for the call.
     *
     * @param host              a pointer to the host expression, or {@code null} if this is
     *                          a direct function or intrinsic call
     * @param callName          a pointer to the null-terminated name of the method or
     *                          function to invoke
     */
    constructor(host: pointer<Expression>, callName: pointer<char>)
    {
        this.host = host
        this.callName = String.strdup(callName)
        this.arguments = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a method call using the function name associated with an
     * operation.
     *
     * <p>The host expression is stored by reference and is not copied.
     *
     * <p>The call name is obtained from {@code Operation.getFunctionName()}.
     * This allows operators that have been desugared into function calls to use
     * their corresponding intrinsic or runtime function names.
     *
     * <p>A new empty argument list and a new empty extra-token collection are
     * allocated for the call.
     *
     * @param host              a pointer to the host expression, or {@code null} if this is
     *                          a direct operation call
     * @param op                a pointer to the operation that provides the function name
     */
    constructor(host: pointer<Expression>, op: pointer<Operation>)
    {
        this.host = host
        this.callName = op.getFunctionName()
        this.arguments = new ArrayList(sizeof(pointer<Expression>))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Appends an argument expression to this method call.
     *
     * <p>The argument pointer is stored in the internal argument list and the
     * expression itself is not copied or cloned.
     *
     * <p>Arguments are preserved in insertion order and are later used by token
     * collection and textual representation operations.
     *
     * @param argument          a pointer to the argument expression to append
     *
     * @return                  this {@code MethodCall} instance
     */
    fun addArgument(argument: pointer<Expression>) -> pointer<MethodCall>
    {
        this.arguments.push(argument.ref)
        return this
    }


    /**
     * Adds an additional syntax token to this method call.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is stored by reference and is not cloned. These additional
     * tokens are included when {@code getAllTokens()} collects the complete
     * source representation of the call.
     *
     * @param token             a pointer to the syntax token to add
     *
     * @return                  this {@code MethodCall} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<MethodCall>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Replaces the argument collection using the specified expression tuple.
     *
     * <p>The expression list contained by {@code arguments} becomes the
     * argument list of this method call. The list is stored by reference and is
     * not copied.
     *
     * <p>Any additional syntax tokens retained by the tuple are appended to this
     * call's own extra-token collection. This preserves tokens such as
     * parentheses and commas that originally belonged to the parsed argument
     * tuple.
     *
     * @param arguments         a pointer to the expression tuple containing the new
     *                          argument expressions and associated syntax tokens
     *
     * @return                  this {@code MethodCall} instance
     */
    fun setArguments(arguments: pointer<ExpressionTuple>) -> pointer<MethodCall>
    {
        this.arguments = arguments.getList()
        this.extraTokens.pushAll(arguments.getExtraTokens())
        return this
    }
    

    /**
     * Returns the host expression associated with this method call.
     *
     * <p>The returned expression is the same object referenced internally and
     * is not copied or cloned.
     *
     * <p>The result may be {@code null} when this call represents a direct
     * function or intrinsic invocation without an explicit receiver.
     *
     * @return                  a pointer to the host expression, or {@code null} if no host is
     *                          associated with this call
     */
    fun getHost() -> pointer<Expression> = this.host


    /**
     * Returns the name of the method or function being invoked.
     *
     * <p>The internally stored call name is duplicated using
     * {@code String.strdup}. The returned character sequence therefore does not
     * directly reference the internal name buffer.
     *
     * @return                  a pointer to a duplicated null-terminated call name
     */
    fun getCallName() -> pointer<char> = String.strdup(this.callName)


    /**
     * Returns the argument expression stored at the specified index.
     *
     * <p>The argument list stores expression pointers indirectly. This method
     * retrieves the corresponding pointer slot and returns the referenced
     * expression.
     *
     * <p>If the stored slot itself is {@code null}, this method returns
     * {@code null}. The expression is returned by reference and is not copied.
     *
     * @param index             the zero-based index of the argument to retrieve
     *
     * @return                  a pointer to the argument expression, or {@code null} if the
     *                          corresponding slot is invalid
     */
    fun getArgument(index: int) -> pointer<Expression>
    {
        val slot: pointer<pointer<Expression>> = this.arguments.get(index) as pointer<pointer<Expression>>

        if slot == null:
            return null

        return slot.deref
    }


    /**
     * Returns the number of argument slots stored by this method call.
     *
     * <p>The returned value corresponds directly to the length of the internal
     * argument list.
     *
     * @return                  the number of arguments stored by this call
     */
    fun argumentsCount() -> int = this.arguments.length


    /**
     * Returns all tokens associated with this method call.
     *
     * <p>If a host expression is available, all tokens belonging to that
     * expression are collected first.
     *
     * <p>The tokens of each valid argument expression are then collected in
     * argument order. Null argument entries and null token collections are
     * ignored.
     *
     * <p>The collected host and argument tokens are combined with the additional
     * syntax tokens stored by this call. These additional tokens may include
     * parentheses, commas, member-access operators, or other delimiters.
     *
     * <p>The final token collection is sorted according to source position using
     * {@code TokenPosition.compareToken}, ensuring that the returned list
     * reflects the original lexical order of the complete call expression.
     *
     * @return                  a newly allocated list containing all tokens associated with this
     *                          method call in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.host != null:
        {
            val tokens: pointer<ArrayList> = this.host.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        for (var i = 0; i < this.arguments.length; i++):
        {
            val argument: pointer<Expression> = this.getArgument(i)

            if argument == null:
                continue

            val tokens: pointer<ArrayList> = argument.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this method call.
     *
     * <p>If a host expression is present, its textual representation is emitted
     * first and is followed by a member-access separator and the call name. The
     * resulting form is:
     *
     * <pre>
     * host.callName(argument1, argument2, ...)
     * </pre>
     *
     * <p>If no host expression is present, only the call name and argument list
     * are emitted:
     *
     * <pre>
     * callName(argument1, argument2, ...)
     * </pre>
     *
     * <p>Argument expressions are emitted in their stored order and are
     * separated by {@code ", "}. Null argument entries are skipped and do not
     * produce separators.
     *
     * <p>The returned string is constructed independently and modifying it does
     * not change the underlying method-call structure.
     *
     * @return                  a pointer to a newly created {@code StringBuilder} containing the
     *                          textual representation of this method call
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.host != null:
        {
            sb.append(this.host.toString())
            sb.append('.')
        }

        sb.append(this.callName)
        sb.append('(')

        var appendedArgument: bool = false

        for (var i = 0; i < this.arguments.length; i++):
        {
            val argument: pointer<Expression> = this.getArgument(i)

            if argument == null:
                continue

            if appendedArgument:
                sb.append(", ")

            sb.append(argument.toString())
            appendedArgument = true
        }

        sb.append(')')

        return sb
    }
}
