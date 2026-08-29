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
@file.class("Expression") 
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.compiler.parser.stmtexpr.BlockExpression
import xlang.compiler.parser.stmtexpr.IfExpression
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Expression
{
    static val ATOM_KIND: int = 1
    static val STATEMENT_KIND: int = 2
    static val FIELD_ACCESS_KIND: int = 3
    static val METHOD_CALL_KIND: int = 4
    static val INDEX_ACCESS_KIND: int = 5
    static val TYPE_CAST_KIND: int = 6
    static val ASSIGNMENT_KIND: int = 7
    static val NEW_IDENTIFIER_KIND: int = 8
    static val NEW_FUNCTION_KIND: int = 9
    static val BLOCK_EXPR_KIND: int = 10
    // static val IF_BRANCH_KIND: int = 11
    static val IFELSE_BRANCH_KIND: int = 12


    static fun fromAtom(atom: pointer<Atom>) -> pointer<Expression> = new Expression(ATOM_KIND, atom)


    inline static fun fromFieldAccess(host: pointer<Expression>, fieldName: pointer<char>) -> pointer<Expression>
    {
        val access: pointer<FieldAccess> = new FieldAccess(host, fieldName)
        return new Expression(FIELD_ACCESS_KIND, access)
    }


    inline static fun fromMethodCall(method: pointer<MethodCall>) -> pointer<Expression> = new Expression(METHOD_CALL_KIND, method) 


    inline static fun fromIndexAccess(host: pointer<Expression>, indices: pointer<ListLiteral>) -> pointer<Expression> = 
        new Expression(INDEX_ACCESS_KIND, new IndexAccess(host, indices))


    inline static fun fromTypeCast(expression: pointer<Expression>, targetType: pointer<NormalType>) -> pointer<Expression> = 
        new Expression(TYPE_CAST_KIND, new TypeCast(expression, targetType))


    inline static fun fromAssignment(target: pointer<Expression>, value: pointer<Expression>) -> pointer<Expression> = 
        new Expression(ASSIGNMENT_KIND, new Assignment(target, value))

    
    inline static fun fromNewIdentifier(ident: pointer<NewIdentifier>) -> pointer<Expression> = new Expression(NEW_IDENTIFIER_KIND, ident)


    inline static fun fromNewFunction(function: pointer<NewFunction>) -> pointer<Expression> = new Expression(NEW_FUNCTION_KIND, function)


    inline static fun fromBlockExpr(block: pointer<BlockExpression>) -> pointer<Expression> = new Expression(BLOCK_EXPR_KIND, block)


    // inline static fun fromIfExpr(branch: pointer<IfExpression>) -> pointer<Expression> = new Expression(IF_BRANCH_KIND, branch)


    inline static fun fromIfElseExpr(branch: pointer<IfElseExpression>) -> pointer<Expression> = new Expression(IFELSE_BRANCH_KIND, branch)



    private var kind: int

    private var root: pointer<*>

    private var extraTokens: pointer<ArrayList>

    private val inferredType: pointer<NormalType>


    private fun __init__(kind: int, root: pointer<*>)
    {
        this.kind = kind
        this.root = root
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    fun getKind() -> int = this.kind


    fun getRoot() -> pointer<*> = this.root


    fun setType(inferredType: pointer<NormalType>) -> pointer<Atom>
    {
        this.inferredType = inferredType
        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<Expression>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun clone() -> pointer<Expression>
    {
        val result: pointer<Expression> = new Expression(this.kind, this.root)
        result.extraTokens.pushAll(this.extraTokens)
        return result
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))
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
            val block: pointer<BlockExpression> = this.root as pointer<BlockExpression>
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

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


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
            val block: pointer<BlockExpression> = this.root as pointer<BlockExpression>
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
