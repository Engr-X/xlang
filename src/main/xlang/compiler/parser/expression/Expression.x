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

package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.compiler.parser.stmtexpr.Block
import xlang.compiler.parser.stmtexpr.IfExpression
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


/**
 * Represents a general expression node in the abstract syntax tree.
 *
 * <p>An {@code Expression} acts as a tagged wrapper around one of the concrete
 * expression structures supported by the compiler. The {@code kind} field
 * identifies the actual type of object referenced by {@code root}.
 *
 * <p>Expressions may additionally contain syntax tokens that are not directly
 * owned by the underlying expression node. These tokens are stored in
 * {@code extraTokens} and are included when collecting the complete source
 * representation of the expression.
 *
 * <p>The expression may also store a type determined during semantic analysis
 * or type inference.
 */
struct Expression
{
    // Identifies an expression whose root is an {@code Atom}.
    static val ATOM_KIND: int = 1

    // Identifies a statement-based expression.
    static val STATEMENT_KIND: int = 2

    // Identifies a field-access expression.
    static val FIELD_ACCESS_KIND: int = 3

    /**
     * Identifies a method-call expression.
     */
    static val METHOD_CALL_KIND: int = 4

    /**
     * Identifies an index-access expression.
     */
    static val INDEX_ACCESS_KIND: int = 5

    /**
     * Identifies an explicit type-cast expression.
     */
    static val TYPE_CAST_KIND: int = 6

    /**
     * Identifies an assignment expression.
     */
    static val ASSIGNMENT_KIND: int = 7

    /**
     * Identifies an expression that creates a new identifier-based object.
     */
    static val NEW_IDENTIFIER_KIND: int = 8

    /**
     * Identifies an expression that creates a new function-based object.
     */
    static val NEW_FUNCTION_KIND: int = 9

    /**
     * Identifies a block expression.
     */
    static val BLOCK_EXPR_KIND: int = 10

    // static val IF_BRANCH_KIND: int = 11

    /**
     * Identifies an if-else expression.
     */
    static val IFELSE_BRANCH_KIND: int = 12


    /**
     * Creates an expression from an atomic expression.
     *
     * @param atom              a pointer to the atom to wrap
     *
     * @return                  a new expression of kind {@code ATOM_KIND}
     */
    static fun fromAtom(atom: pointer<Atom>) -> pointer<Expression> = new Expression(ATOM_KIND, atom)


    /**
     * Creates a field-access expression.
     *
     * @param host              a pointer to the expression containing the field
     * @param fieldName         a pointer to the null-terminated field name
     *
     * @return                  a new expression of kind {@code FIELD_ACCESS_KIND}
     */
    inline static fun fromFieldAccess(host: pointer<Expression>, fieldName: pointer<char>) -> pointer<Expression>
    {
        val access: pointer<FieldAccess> = new FieldAccess(host, fieldName)
        return new Expression(FIELD_ACCESS_KIND, access)
    }


    /**
     * Creates a method-call expression.
     *
     * @param method            a pointer to the method-call node
     * @return                  a new expression of kind {@code METHOD_CALL_KIND}
     */
    inline static fun fromMethodCall(method: pointer<MethodCall>) -> pointer<Expression> = new Expression(METHOD_CALL_KIND, method) 


    /**
     * Creates an index-access expression.
     *
     * @param host              a pointer to the expression being indexed
     * @param indices           a pointer to the list of index expressions
     *
     * @return                  a new expression of kind {@code INDEX_ACCESS_KIND}
     */
    inline static fun fromIndexAccess(host: pointer<Expression>, indices: pointer<ListLiteral>) -> pointer<Expression> = 
        new Expression(INDEX_ACCESS_KIND, new IndexAccess(host, indices))


    /**
     * Creates an explicit type-cast expression.
     *
     * @param expression        a pointer to the expression being cast
     * @param targetType        a pointer to the destination type
     *
     * @return                  a new expression of kind {@code TYPE_CAST_KIND}
     */
    inline static fun fromTypeCast(expression: pointer<Expression>, targetType: pointer<Type>) -> pointer<Expression> = 
        new Expression(TYPE_CAST_KIND, new TypeCast(expression, targetType))


    /**
     * Creates an assignment expression.
     *
     * @param target            a pointer to the assignment target
     * @param value             a pointer to the value being assigned
     *
     * @return                  a new expression of kind {@code ASSIGNMENT_KIND}
     */
    inline static fun fromAssignment(target: pointer<Expression>, value: pointer<Expression>) -> pointer<Expression> = 
        new Expression(ASSIGNMENT_KIND, new Assignment(target, value))
    

    /**
     * Creates an expression from a new-identifier node.
     *
     * @param ident             a pointer to the new-identifier node
     *
     * @return                  a new expression of kind {@code NEW_IDENTIFIER_KIND}
     */
    inline static fun fromNewIdentifier(ident: pointer<NewIdentifier>) -> pointer<Expression> = new Expression(NEW_IDENTIFIER_KIND, ident)


    /**
     * Creates an expression from a new-function node.
     *
     * @param function          a pointer to the new-function node
     *
     * @return                  a new expression of kind {@code NEW_FUNCTION_KIND}
     */
    inline static fun fromNewFunction(function: pointer<NewFunction>) -> pointer<Expression> = new Expression(NEW_FUNCTION_KIND, function)


    /**
     * Creates an expression from a block.
     *
     * @param block             a pointer to the block represented as an expression
     *
     * @return                  a new expression of kind {@code BLOCK_EXPR_KIND}
     */
    inline static fun fromBlockExpr(block: pointer<Block>) -> pointer<Expression> = new Expression(BLOCK_EXPR_KIND, block)


    // inline static fun fromIfExpr(branch: pointer<IfExpression>) -> pointer<Expression> = new Expression(IF_BRANCH_KIND, branch)


    /**
     * Creates an if-else expression.
     *
     * @param branch            a pointer to the if-else expression node
     *
     * @return                  a new expression of kind {@code IFELSE_BRANCH_KIND}
     */
    inline static fun fromIfElseExpr(branch: pointer<IfElseExpression>) -> pointer<Expression> = new Expression(IFELSE_BRANCH_KIND, branch)


    /**
     * The kind identifier describing the concrete expression represented by
     * this wrapper.
     */
    private var kind: int


    /**
     * A pointer to the concrete expression node represented by this wrapper.
     *
     * <p>The actual pointer type is determined by {@code kind}. For example,
     * {@code ATOM_KIND} indicates an {@code Atom}, while
     * {@code METHOD_CALL_KIND} indicates a {@code MethodCall}.
     */
    private var root: pointer<*>

    /**
     * Additional syntax tokens associated with this expression.
     *
     * <p>These tokens are stored separately from the tokens owned by the root
     * expression node and are included by {@code getAllTokens()}.
     */
    private var extraTokens: pointer<ArrayList>

    /**
     * The type inferred or assigned to this expression.
     *
     * <p>This value may be {@code null} until semantic analysis or type
     * inference has determined the expression type.
     */
    private val inferredType: pointer<Type>


    /**
     * Creates an expression wrapper for the specified expression kind and root
     * object.
     *
     * <p>The root object is stored by reference and is not copied. A new empty
     * collection is created for additional syntax tokens.
     *
     * @param kind              the kind identifier of the expression
     * @param root              a pointer to the concrete expression node
     */
    private constructor(kind: int, root: pointer<*>)
    {
        this.kind = kind
        this.root = root
        this.extraTokens = new ArrayList(sizeof(Token))
    }

    /**
     * Returns the kind identifier of this expression.
     *
     * @return                  the expression kind
     */
    fun getKind() -> int = this.kind


    /**
     * Returns the concrete expression node wrapped by this expression.
     *
     * <p>The returned pointer is untyped. Its concrete type must be interpreted
     * according to {@code getKind()}.
     *
     * @return                  a pointer to the root expression node
     */
    fun getRoot() -> pointer<*> = this.root


    /**
     * Assigns an inferred type to this expression.
     *
     * <p>The supplied type is stored by reference and is not copied.
     *
     * @param                   inferredType a pointer to the type inferred for this expression
     *
     * @return                  this {@code Expression} instance
     */
    fun setType(inferredType: pointer<Type>) -> pointer<Atom>
    {
        this.inferredType = inferredType
        return this
    }


    /**
     * Adds an additional syntax token to this expression.
     *
     * <p>If {@code token} is {@code null}, no token is added.
     *
     * @param token             a pointer to the token to add
     *
     * @return                  this {@code Expression} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Expression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Creates a shallow copy of this expression.
     *
     * <p>The expression kind and root pointer are preserved. The root object
     * itself is not cloned.
     *
     * <p>A new extra-token collection is created and populated with the token
     * references stored by the current expression.
     *
     * @return                  a pointer to the cloned {@code Expression}
     */
    fun clone() -> pointer<Expression>
    {
        val result: pointer<Expression> = new Expression(this.kind, this.root)
        result.extraTokens.pushAll(this.extraTokens)
        return result
    }


    /**
     * Returns all tokens associated with this expression.
     *
     * <p>The tokens belonging to the concrete root expression are obtained by
     * dispatching to the appropriate {@code getAllTokens()} implementation
     * according to {@code kind}.
     *
     * <p>The root tokens are combined with this expression's additional tokens.
     * The combined collection is then sorted according to source position using
     * {@code TokenPosition.compareToken}.
     *
     * @return                  a newly allocated list containing all tokens associated with this
     *                          expression in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = this.extraTokens.clone()
        
        var rootTokens: pointer<ArrayList> = if this.kind == ATOM_KIND:
        {
            val atom: pointer<Atom> = this.root as pointer<Atom>
            atom.getAllTokens()
        }
        elif this.kind == METHOD_CALL_KIND:
        {
            val call: pointer<MethodCall> = this.root as pointer<MethodCall>
            call.getAllTokens()
        }
        elif this.kind == FIELD_ACCESS_KIND:
        {
            val access: pointer<FieldAccess> = this.root as pointer<FieldAccess>
            access.getAllTokens()
        }
        elif this.kind == INDEX_ACCESS_KIND:
        {
            val access: pointer<IndexAccess> = this.root as pointer<IndexAccess>
            access.getAllTokens()
        }
        elif this.kind == TYPE_CAST_KIND:
        {
            val cast: pointer<TypeCast> = this.root as pointer<TypeCast>
            cast.getAllTokens()
        }
        elif this.kind == ASSIGNMENT_KIND:
        {
            val assignment: pointer<Assignment> = this.root as pointer<Assignment>
            assignment.getAllTokens()
        }
        elif this.kind == NEW_IDENTIFIER_KIND:
        {
            val ident: pointer<NewIdentifier> = this.root as pointer<NewIdentifier>
            ident.getAllTokens()
        }
        elif this.kind == NEW_FUNCTION_KIND:
        {
            val function: pointer<NewFunction> = this.root as pointer<NewFunction>
            function.getAllTokens()
        }
        elif this.kind == BLOCK_EXPR_KIND:
        {
            val block: pointer<Block> = this.root as pointer<Block>
            block.getAllTokens()
        }
        // elif this.kind == IF_BRANCH_KIND:
        // {
        //     val branch: pointer<IfExpression> = this.root as pointer<IfExpression>
        //     branch.getAllTokens()
        // }
        elif this.kind == IFELSE_BRANCH_KIND:
        {
            val branch: pointer<IfElseExpression> = this.root as pointer<IfElseExpression>
            branch.getAllTokens()
        }
        else: null

        if rootTokens != null:
            result.pushAll(rootTokens)

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this expression.
     *
     * <p>The operation is delegated to the concrete root expression according
     * to {@code kind}.
     *
     * <p>If the expression kind is not recognized, an empty
     * {@code StringBuilder} is returned.
     *
     * @return                  a pointer to a {@code StringBuilder} containing the textual
     *                          representation of this expression
     */
    fun toString() -> pointer<StringBuilder> = if this.kind == ATOM_KIND:
        {
            val atom: pointer<Atom> = this.root as pointer<Atom>
            atom.toString()
        }
        elif this.kind == METHOD_CALL_KIND:
        {
            val call: pointer<MethodCall> = this.root as pointer<MethodCall>
            call.toString()
        }
        elif this.kind == FIELD_ACCESS_KIND:
        {
            val access: pointer<FieldAccess> = this.root as pointer<FieldAccess>
            access.toString()
        }
        elif this.kind == INDEX_ACCESS_KIND:
        {
            val access: pointer<IndexAccess> = this.root as pointer<IndexAccess>
            access.toString()
        }
        elif this.kind == TYPE_CAST_KIND:
        {
            val cast: pointer<TypeCast> = this.root as pointer<TypeCast>
            cast.toString()
        }
        elif this.kind == ASSIGNMENT_KIND:
        {
            val assignment: pointer<Assignment> = this.root as pointer<Assignment>
            assignment.toString()
        }
        elif this.kind == NEW_IDENTIFIER_KIND:
        {
            val ident: pointer<NewIdentifier> = this.root as pointer<NewIdentifier>
            ident.toString()
        }
        elif this.kind == NEW_FUNCTION_KIND:
        {
            val function: pointer<NewFunction> = this.root as pointer<NewFunction>
            function.toString()
        }
        elif this.kind == BLOCK_EXPR_KIND:
        {
            val block: pointer<Block> = this.root as pointer<Block>
            block.toString()
        }
        // elif this.kind == IF_BRANCH_KIND
        // {
        //     val branch: pointer<IfExpression> = this.root as pointer<IfExpression>
        //     branch.toString()
        // }
        elif this.kind == IFELSE_BRANCH_KIND:
        {
            val branch: pointer<IfElseExpression> = this.root as pointer<IfElseExpression>
            branch.toString()
        }
        else: new StringBuilder()
}
