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

package xlang.compiler.parser.program

import xlang.compiler.Type
import xlang.compiler.parser.expression.Expression
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.HashSet
import xlang.util.string.StringBuilder


/**
 * Represents a single parameter declared by a function.
 *
 * <p>A {@code FunctionParam} stores the parameter name, optional declared type,
 * mutability state, and any additional syntax tokens associated with the
 * parameter declaration.
 *
 * <p>The mutability state reuses the same modifier values used by {@code Field}.
 * Parameters are immutable by default and may later be marked as mutable using
 * {@code markAsMut()}.
 *
 * <p>The parameter name and type are stored by reference when the instance is
 * created. The declared type can be retrieved through {@code getParamType()},
 * which returns a cloned type rather than exposing the internally stored type
 * object directly.
 */
struct FunctionParam
{
    /**
     * The internal mutability modifier of this parameter.
     *
     * <p>The value is initialized using {@code Field.constModifier()} and may
     * later be changed to {@code Field.mutModifier()}.
     */
    private var modifier: int

    /**
     * The null-terminated name of this function parameter.
     *
     * <p>The supplied pointer is stored directly and is not duplicated or
     * cloned by the constructor.
     */
    private var paramName: pointer<char>

    /**
     * The optional declared type of this function parameter.
     *
     * <p>The type is stored by reference internally. External callers obtain a
     * cloned type through {@code getParamType()}.
     */
    private var paramType: pointer<Type>

    /**
     * Additional syntax tokens associated with this parameter declaration.
     *
     * <p>This collection may contain mutability keywords, punctuation, type
     * separators, or other tokens that are not directly owned by the parameter
     * type.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a function parameter with the specified name and declared type.
     *
     * <p>The parameter is initialized as immutable using
     * {@code Field.constModifier()}.
     *
     * <p>The supplied name and type are stored by reference and are not copied
     * or cloned during construction.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param paramName			a pointer to the null-terminated parameter name
     * @param paramType			a pointer to the declared parameter type, or
     * 					        {@code null} if no explicit type is available
     */
    constructor(paramName: pointer<char>, paramType: pointer<Type>)
    {
        this.modifier = Field.constModifier()
        this.paramName = paramName
        this.paramType = paramType
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the name of this function parameter.
     *
     * <p>The returned pointer refers directly to the character sequence stored
     * internally by this parameter. The name is not duplicated or cloned.
     *
     * <p>The result may be {@code null} if the parameter was constructed without
     * a valid name.
     *
     * @return				    a pointer to the internally stored parameter name,
     * 					        or {@code null} if no name is available
     */
    fun getParamName() -> pointer<char> = this.paramName


    /**
     * Returns a copy of the declared type associated with this parameter.
     *
     * <p>If no parameter type is stored, this method returns {@code null}.
     *
     * <p>When a type is available, {@code Type.clone()} is used to create the
     * returned value. The caller therefore receives a separate type object
     * rather than direct access to the internally stored type reference.
     *
     * @return				    a pointer to a cloned parameter type, or
     * 					        {@code null} if no type is available
     */
    fun getParamType() -> pointer<Type> =
        if this.paramType == null:
            null
        else:
            this.paramType.clone()


    /**
     * Marks this function parameter as mutable.
     *
     * <p>The internal modifier is replaced with the value returned by
     * {@code Field.mutModifier()}.
     *
     * <p>After this operation, {@code canModified()} returns {@code true}.
     * No other parameter properties are changed.
     *
     * @return				    this {@code FunctionParam} instance
     */
    fun markAsMut() -> pointer<FunctionParam>
    {
        this.modifier = Field.mutModifier()
        return this
    }


    /**
     * Marks this function parameter as immutable.
     *
     * <p>The internal modifier is replaced with the value returned by
     * {@code Field.constModifier()}.
     *
     * <p>After this operation, {@code canModified()} returns {@code false}.
     * No other parameter properties are changed.
     *
     * @return				    this {@code FunctionParam} instance
     */
    fun markAsConst() -> pointer<FunctionParam>
    {
        this.modifier = Field.constModifier()
        return this
    }


    /**
     * Returns whether this function parameter is currently marked as mutable.
     *
     * <p>The parameter is considered mutable only when its internal modifier is
     * equal to the value returned by {@code Field.mutModifier()}.
     *
     * @return				    {@code true} if this parameter is mutable;
     * 					        {@code false} otherwise
     */
    fun canModified() -> bool = this.modifier == Field.mutModifier()


    /**
     * Adds an additional syntax token to this function parameter.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is appended to the internal extra-token collection and is
     * stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} collects the complete source token set of the
     * parameter.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code FunctionParam} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<FunctionParam>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * modifications to the returned list structure do not directly replace or
     * resize the list stored by this parameter.
     *
     * <p>The individual {@code Token} objects referenced by the collection are
     * not recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this parameter
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this function parameter.
     *
     * <p>If a declared parameter type is available, all tokens returned by
     * {@code Type.getAllTokens()} are included in the result.
     *
     * <p>The type tokens are combined with the additional syntax tokens stored
     * directly by this parameter. These additional tokens may include the
     * parameter identifier, mutability syntax, type separator, or other tokens
     * retained during parsing.
     *
     * <p>The resulting collection is sorted according to source position using
     * {@code TokenPosition.compareToken}, restoring the original lexical order
     * of tokens collected from the different parameter components.
     *
     * <p>A new list is allocated for the result. The contained token objects are
     * referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this parameter in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()
        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this function parameter.
     *
     * <p>If a parameter name is available, it is emitted first.
     *
     * <p>If a declared type is available, the type is appended using the form:
     *
     * <pre>
     * name: Type
     * </pre>
     *
     * <p>If no type is available, only the parameter name is emitted. Likewise,
     * if the name is {@code null}, the representation may contain only the
     * declared type portion.
     *
     * <p>The current implementation does not emit the internal mutability state
     * represented by {@code modifier}; that information is retained separately
     * by the AST node.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying parameter node.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the textual representation of this
     * 					        function parameter
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.paramName != null:
            sb.append(this.paramName)

        if this.paramType != null:
        {
            sb.append(": ")
            sb.append(this.paramType.toString())
        }

        return sb
    }
}


/**
 * Represents an ordered collection of function parameters.
 *
 * <p>A {@code FunctionParams} groups zero or more {@code FunctionParam}
 * instances together with syntax tokens associated with the complete parameter
 * list.
 *
 * <p>The parameter collection preserves insertion order. Additional syntax
 * tokens may contain parentheses, commas, or other delimiters that belong to
 * the parameter-list syntax rather than to an individual parameter.
 *
 * <p>The structure provides utility operations for appending individual
 * parameters, merging another parameter collection, retrieving parameters by
 * index, collecting source tokens, and producing a comma-separated textual
 * representation.
 */
struct FunctionParams
{
    /**
     * The ordered collection of function parameters.
     *
     * <p>The collection is allocated when this structure is constructed and is
     * retained for the lifetime of the instance.
     */
    private val params: pointer<ArrayList>

    /**
     * Additional syntax tokens associated with the complete parameter list.
     *
     * <p>This collection may contain parentheses, commas, or other delimiters
     * that are not directly owned by the individual parameters.
     */
    private val extraTokens: pointer<ArrayList>


    /**
     * Creates an empty function-parameter collection.
     *
     * <p>A new empty parameter list and a new empty extra-token collection are
     * allocated.
     */
    constructor()
    {
        this.params = new ArrayList(sizeof(FunctionParam))
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Creates a function-parameter collection containing an initial parameter.
     *
     * <p>New parameter and extra-token collections are allocated first. The
     * supplied parameter is then added through {@code push()}.
     *
     * <p>If {@code param} is {@code null}, the resulting collection remains
     * empty.
     *
     * @param param			    a pointer to the initial function parameter, or
     * 					        {@code null} to create an empty collection
     */
    constructor(param: pointer<FunctionParam>)
    {
        this.params = new ArrayList(sizeof(FunctionParam))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.push(param)
    }


    /**
     * Appends a function parameter to this collection.
     *
     * <p>If {@code param} is {@code null}, no modification is performed.
     *
     * <p>A valid parameter is appended to the end of the internal collection,
     * preserving the order in which parameters are added.
     *
     * @param param			    a pointer to the function parameter to append
     *
     * @return				    this {@code FunctionParams} instance
     */
    fun push(param: pointer<FunctionParam>) -> pointer<FunctionParams>
    {
        if param != null:
            this.params.push(param)

        return this
    }


    /**
     * Appends all parameters and additional syntax tokens from another
     * parameter collection.
     *
     * <p>If {@code params} is {@code null}, or if its internal parameter list is
     * unavailable, no modification is performed.
     *
     * <p>All parameter entries from the supplied collection are appended to the
     * current parameter list in their existing order.
     *
     * <p>If the source collection contains additional syntax tokens, those
     * tokens are also appended to this instance's extra-token collection.
     *
     * <p>The source collection itself is not modified.
     *
     * @param params			a pointer to the parameter collection to append
     *
     * @return				    this {@code FunctionParams} instance
     */
    fun pushAll(params: pointer<FunctionParams>) -> pointer<FunctionParams>
    {
        if params != null && params.params != null:
        {
            this.params.pushAll(params.params)

            if params.extraTokens != null:
                this.extraTokens.pushAll(params.extraTokens)
        }

        return this
    }


    /**
     * Adds an additional syntax token to this parameter collection.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is appended to the internal extra-token collection and is
     * included by {@code getAllTokens()} when the complete parameter-list token
     * set is collected.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code FunctionParams} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<FunctionParams>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns the number of parameters currently stored in this collection.
     *
     * <p>The returned value corresponds directly to the length of the internal
     * parameter list.
     *
     * @return				    the number of stored function parameters
     */
    fun length() -> int = this.params.length


    /**
     * Returns the function parameter stored at the specified index.
     *
     * <p>The index is validated before accessing the internal collection. If the
     * index is negative or greater than or equal to the number of stored
     * parameters, this method returns {@code null}.
     *
     * <p>For a valid index, the corresponding parameter entry is returned
     * directly and is not cloned.
     *
     * @param index			    the zero-based index of the parameter to retrieve
     *
     * @return				    a pointer to the parameter at the specified index,
     * 					        or {@code null} if the index is outside the valid
     * 					        range
     */
    fun get(index: int) -> pointer<FunctionParam>
    {
        if index < 0 || index >= this.params.length:
            return null

        return this.params.get(index) as pointer<FunctionParam>
    }


    /**
     * Returns a copy of the internal parameter collection.
     *
     * <p>The {@code ArrayList} itself is cloned before being returned, allowing
     * callers to modify the returned collection structure without directly
     * replacing or resizing the internal list.
     *
     * <p>The individual parameter objects contained by the collection are not
     * recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the stored
     * 					        function parameters
     */
    fun getParams() -> pointer<ArrayList> = this.params.clone()


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned. The
     * individual token objects referenced by the collection are not recursively
     * cloned.
     *
     * @return				    a pointer to a cloned list containing the syntax
     * 					        tokens associated with this parameter collection
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this function-parameter collection.
     *
     * <p>The stored parameters are traversed in order. For each valid parameter,
     * all tokens returned by {@code FunctionParam.getAllTokens()} are appended
     * to the result.
     *
     * <p>Null parameter entries and null token collections are ignored.
     *
     * <p>After all parameter tokens have been collected, the additional syntax
     * tokens stored by this collection are appended. These tokens may include
     * parentheses, commas, or other delimiters belonging to the complete
     * parameter list.
     *
     * <p>The final collection is sorted according to source position using
     * {@code TokenPosition.compareToken}, ensuring that tokens gathered from
     * multiple parameter nodes are restored to their original lexical order.
     *
     * <p>A new list is allocated for the result. The individual token objects
     * are referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this parameter collection in source
     * 					        order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.params.length; i++):
        {
            val param: pointer<FunctionParam> = this.get(i)

            if param == null:
                continue

            val tokens: pointer<ArrayList> = param.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this function-parameter collection.
     *
     * <p>Valid parameters are emitted in their stored order using each
     * parameter's {@code toString()} representation.
     *
     * <p>Parameters are separated by {@code ", "}. Null parameter entries are
     * skipped and do not produce additional separators.
     *
     * <p>This method emits only the contents of the parameter list and does not
     * add surrounding parentheses. Parentheses are emitted by the surrounding
     * function representation.
     *
     * <p>If no valid parameters are present, an empty {@code StringBuilder} is
     * returned.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the comma-separated parameter
     * 					        representation
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedParam: bool = false

        for (var i = 0; i < this.params.length; i++):
        {
            val param: pointer<FunctionParam> = this.get(i)

            if param == null:
                continue

            if appendedParam:
                sb.append(", ")

            sb.append(param.toString())
            appendedParam = true
        }

        return sb
    }
}


/**
 * Represents an optional function-parameter collection.
 *
 * <p>{@code FunctionParamsMaybe} provides a small normalization wrapper for
 * parser productions in which a parameter collection may be absent.
 *
 * <p>A missing parameter collection is represented internally by an empty
 * {@code FunctionParams} instance rather than by a {@code null} pointer. This
 * allows later parser and AST stages to work with a parameter object without
 * repeatedly checking whether the collection itself exists.
 */
struct FunctionParamsMaybe
{
    /**
     * The normalized function-parameter collection.
     *
     * <p>This field always receives either the supplied parameter collection or
     * a newly allocated empty {@code FunctionParams} instance.
     */
    private var params: pointer<FunctionParams>


    /**
     * Creates an optional parameter wrapper from the supplied collection.
     *
     * <p>If {@code params} is {@code null}, a new empty
     * {@code FunctionParams} instance is allocated. Otherwise, the supplied
     * object is stored directly and is not cloned.
     *
     * @param params			a pointer to the function-parameter collection, or
     * 					        {@code null} to represent an empty collection
     */
    constructor(params: pointer<FunctionParams>):
        this.params =
            if params == null:
                new FunctionParams()
            else:
                params


    /**
     * Returns the normalized function-parameter collection.
     *
     * <p>The returned pointer refers directly to the object stored internally
     * and is not copied or cloned.
     *
     * <p>Because the constructor converts a {@code null} input into an empty
     * {@code FunctionParams}, this method normally returns a valid parameter
     * collection even when no parameters were present in the parsed source.
     *
     * @return				    a pointer to the internally stored function-parameter
     * 					        collection
     */
    fun toFunctionParams() -> pointer<FunctionParams> = this.params
}


/**
 * Represents a function declaration in the program abstract syntax tree.
 *
 * <p>A {@code Function} stores its annotations, declaration modifiers, function
 * name, parameter list, optional return type, optional body expression, and
 * additional syntax tokens retained from the original source.
 *
 * <p>Annotations and declaration modifiers are maintained as ordered
 * collections. The parameter list and body expression are stored by reference.
 * The declared return type is also stored by reference internally, but
 * {@code getReturnType()} returns a cloned type.
 *
 * <p>The function can represent a declaration whose body is absent. The current
 * textual representation still emits the assignment separator {@code " = "}
 * even when no body expression is available.
 *
 * <p>All source tokens belonging to the function and its child AST nodes can be
 * collected in lexical order using {@code getAllTokens()}.
 */
struct Function
{
    /**
     * The ordered collection of annotations attached to this function.
     */
    private var annotations: pointer<ArrayList>


    /**
     * The set of declaration modifiers attached to this function.
     */
    private var modifiers: pointer<HashSet>


    /**
     * The null-terminated name of this function.
     *
     * <p>The supplied pointer is stored directly and is not duplicated by the
     * constructor.
     */
    private var functionName: pointer<char>


    /**
     * The parameter collection associated with this function.
     *
     * <p>The collection is stored by reference and may be {@code null}.
     */
    private var params: pointer<FunctionParams>


    /**
     * The optional declared return type of this function.
     *
     * <p>The type is stored by reference internally. External callers obtain a
     * clone through {@code getReturnType()}.
     */
    private var returnType: pointer<Type>


    /**
     * The optional body expression of this function.
     *
     * <p>A {@code null} value indicates that the function currently has no body
     * expression.
     */
    private var bodyExpr: pointer<Expression>


    /**
     * Additional syntax tokens associated with this function declaration.
     *
     * <p>This collection may contain the {@code fun} keyword, punctuation,
     * delimiters, assignment separators, or other tokens not directly owned by
     * the child AST nodes.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a function declaration with the specified name, parameter
     * collection, and body expression.
     *
     * <p>New empty annotation and declaration-modifier collections are
     * allocated.
     *
     * <p>The function name, parameter collection, and body expression are stored
     * by reference and are not copied or cloned.
     *
     * <p>The return type is initialized to {@code null}. A return type may later
     * be assigned using {@code setReturnType()}.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param functionName		a pointer to the null-terminated function name
     * @param params			a pointer to the function-parameter collection, or
     * 					        {@code null} if no parameter object is available
     * @param bodyExpr			a pointer to the function body expression, or
     * 					        {@code null} if the function has no body
     */
    constructor(functionName: pointer<char>, params: pointer<FunctionParams>, bodyExpr: pointer<Expression>)
    {
        this.annotations = new ArrayList(sizeof(Annotation))
        this.modifiers = new HashSet(sizeof(Modifier), Modifier.compareModifier)
        this.functionName = functionName
        this.params = params
        this.returnType = null
        this.bodyExpr = bodyExpr
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns whether this function currently contains a body expression.
     *
     * <p>The function is considered to have a body whenever
     * {@code bodyExpr} is not {@code null}.
     *
     * @return				    {@code true} if a body expression is present;
     * 					        {@code false} otherwise
     */
    fun haveBody() -> bool = this.bodyExpr != null


 /**
     * Returns the annotation collection attached to this function.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not cloned.
     *
     * <p>Modifications performed through the returned list therefore affect the
     * same annotation collection referenced by this function.
     *
     * @return				    a pointer to the internally stored annotation
     * 					        collection
     */
    fun getAnnotations() -> pointer<ArrayList> = this.annotations


    /**
     * Replaces the annotation collection attached to this function.
     *
     * <p>If {@code annotations} is {@code null}, a new empty annotation list is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * @param annotations		a pointer to the new annotation collection, or
     * 					        {@code null} to replace it with an empty list
     *
     * @return				    this {@code Function} instance
     */
    fun setAnnotations(annotations: pointer<ArrayList>) -> pointer<Function>
    {
        this.annotations = if annotations == null:
                new ArrayList(sizeof(Annotation))
            else:
                annotations

        return this
    }


    /**
     * Returns the declaration-modifier collection attached to this function.
     *
     * <p>The returned pointer refers directly to the internally stored set and
     * is not copied or cloned.
     *
     * @return				    a pointer to the internally stored modifier
     * 					        collection
     */
    fun getModifiers() -> pointer<HashSet> = this.modifiers


    /**
     * Adds a declaration modifier to this function.
     *
     * <p>The specified modifier is added directly to the internally stored
     * modifier set. Duplicate modifier keywords are ignored by the set.
     *
     * <p>This operation modifies the current function instance and returns the
     * same instance, allowing method chaining.
     *
     * @param modifier          a pointer point to modifier to add
     * @return                  a pointer to this function
     */
    fun addModifier(modifier: pointer<Modifier>) -> pointer<Function>
    {
        if modifier != null:
            this.modifiers.add(modifier)

        return this
    }


    /**
     * Replaces the declaration-modifier collection attached to this function.
     *
     * <p>If {@code modifiers} is {@code null}, a new empty modifier set is
     * allocated. Otherwise, the supplied set is stored directly.
     *
     * @param modifiers			a pointer to the new declaration-modifier
     * 					        collection, or {@code null} to use an empty list
     *
     * @return				    this {@code Function} instance
     */
    fun setModifiers(modifiers: pointer<HashSet>) -> pointer<Function>
    {
        this.modifiers = if modifiers == null:
                new HashSet(sizeof(Modifier), Modifier.compareModifier)
            else:
                modifiers

        return this
    }


    /**
     * Returns the name of this function.
     *
     * <p>The returned pointer refers directly to the character sequence stored
     * internally and is not duplicated or cloned.
     *
     * @return				    a pointer to the internally stored null-terminated
     * 					        function name
     */
    fun getFunctionName() -> pointer<char> = this.functionName


    /**
     * Returns the parameter collection associated with this function.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code FunctionParams} object and is not copied or cloned.
     *
     * <p>The result may be {@code null} if the function was constructed without
     * a parameter collection.
     *
     * @return				    a pointer to the function-parameter collection, or
     * 					        {@code null} if no collection is available
     */
    fun getParams() -> pointer<FunctionParams> = this.params


    /**
     * Returns a copy of the declared return type of this function.
     *
     * <p>If no return type is currently stored, this method returns
     * {@code null}.
     *
     * <p>When a return type is available, {@code Type.clone()} is used to
     * create the returned value so that callers do not directly receive the
     * internal type object.
     *
     * @return				    a pointer to a cloned return type, or {@code null}
     * 					        if no return type is declared
     */
    fun getReturnType() -> pointer<Type> =
        if this.returnType == null:
            null
        else:
            this.returnType.clone()


    /**
     * Sets the declared return type of this function.
     *
     * <p>The supplied type is stored by reference and is not copied or cloned.
     *
     * <p>Passing {@code null} removes the currently declared return type.
     *
     * @param returnType	    a pointer to the return type, or {@code null}
     * 					        to remove the current return type
     *
     * @return				    this {@code Function} instance
     */
    fun setReturnType(returnType: pointer<Type>) -> pointer<Function>
    {
        this.returnType = returnType
        return this
    }


    /**
     * Returns the body expression associated with this function.
     *
     * <p>The returned pointer refers directly to the expression stored
     * internally and is not copied or cloned.
     *
     * <p>The result is {@code null} when the function does not currently have a
     * body expression.
     *
     * @return				    a pointer to the function body expression, or
     * 					        {@code null} if no body is present
     */
    fun getBodyExpr() -> pointer<Expression> = this.bodyExpr


    /**
     * Adds an additional syntax token to this function declaration.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The token is appended to the internal extra-token collection and is
     * stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included by
     * {@code getAllTokens()} when collecting the complete function token set.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code Function} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Function>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal list is cloned before being returned, so structural
     * changes to the returned collection do not directly modify the list stored
     * by this function.
     *
     * <p>The individual token objects are not recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this function
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this function declaration.
     *
     * <p>Tokens belonging to all valid annotations are collected first. Null
     * annotation entries are ignored.
     *
     * <p>The declaration modifiers are then traversed and tokens belonging to
     * each valid modifier are appended to the result.
     *
     * <p>If a parameter collection is available, all tokens returned by
     * {@code FunctionParams.getAllTokens()} are included.
     *
     * <p>If a return type is available, all tokens belonging to that type are
     * appended. Likewise, if a body expression is present, all tokens returned
     * by {@code Expression.getAllTokens()} are included.
     *
     * <p>The tokens collected from these structural components are then combined
     * with the additional syntax tokens stored directly by this function.
     *
     * <p>The final collection is sorted according to source position using
     * {@code TokenPosition.compareToken}. This restores the lexical ordering of
     * tokens gathered independently from annotations, modifiers, parameters,
     * the return type, the body expression, and the function itself.
     *
     * <p>A new result list is allocated. The contained token objects are
     * referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this function declaration in source
     * 					        order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        if this.annotations != null:
        {
            for (var i = 0; i < this.annotations.length; i++):
            {
                val annotation: pointer<Annotation> = this.annotations.get(i) as pointer<Annotation>

                if annotation != null:
                    result.pushAll(annotation.getAllTokens())
            }
        }

        if this.modifiers != null:
        {
            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier != null:
                    result.pushAll(modifier.getAllTokens())
            }
        }

        if this.params != null:
            result.pushAll(this.params.getAllTokens())

        if this.returnType != null:
            result.pushAll(this.returnType.getAllTokens())

        if this.bodyExpr != null:
            result.pushAll(this.bodyExpr.getAllTokens())

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this function declaration.
     *
     * <p>Annotations are emitted first in their stored order. Each valid
     * annotation is followed by a newline.
     *
     * <p>Declaration modifiers are emitted next. Valid modifiers are separated
     * by a single space, and an additional space is inserted after the final
     * modifier before the function keyword.
     *
     * <p>The {@code fun} keyword and function name are then emitted, followed by
     * parentheses containing the textual representation of the parameter
     * collection. If {@code params} is {@code null}, the parentheses remain
     * empty.
     *
     * <p>If a return type is available, it is emitted using the form:
     *
     * <pre>
     * -> ReturnType
     * </pre>
     *
     * <p>The method then always appends {@code " = "}. If a body expression is
     * available, its textual representation follows the assignment separator.
     * Consequently, a function without a body currently produces a
     * representation ending in {@code " = "}.
     *
     * <p>The resulting representation generally follows the form:
     *
     * <pre>
     * @Annotation
     * modifier1 modifier2 fun name(param1: Type1, param2: Type2) -> ReturnType = body
     * </pre>
     *
     * <p>Null annotations and modifiers are skipped. Optional components such as
     * the return type and body expression are omitted according to the behavior
     * described above.
     *
     * <p>The returned {@code StringBuilder} is newly allocated and modifying its
     * contents does not modify the underlying function AST node.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the textual representation of this
     * 					        function declaration
     */
    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()

        if this.annotations != null && this.annotations.length > 0:
        {
            for (var i = 0; i < this.annotations.length; i++):
            {
                val annotation: pointer<Annotation> = this.annotations.get(i) as pointer<Annotation>

                if annotation != null:
                {
                    sb.append(annotation.toString())
                    sb.newline()
                }
            }
        }

        if this.modifiers != null && this.modifiers.length > 0:
        {
            var appendedModifier: bool = false

            for (var i = 0; i < this.modifiers.length; i++):
            {
                val modifier: pointer<Modifier> = this.modifiers.get(i) as pointer<Modifier>

                if modifier == null:
                    continue

                if appendedModifier:
                    sb.append(' ')

                sb.append(modifier.toString())
                appendedModifier = true
            }

            if appendedModifier:
                sb.append(' ')
        }

        sb.append("fun ")

        if this.functionName != null:
            sb.append(this.functionName)

        sb.append('(')

        if this.params != null:
            sb.append(this.params.toString())

        sb.append(")")

        if this.returnType != null:
        {
            sb.append(" -> ")
            sb.append(this.returnType.toString())
        }

        sb.append(" = ")

        if this.bodyExpr != null:
            sb.append(this.bodyExpr.toString())

        return sb
    }
}
