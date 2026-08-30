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
@file.class("ExpressionDesugar")
package xlang.compiler.parser.expression

import xlang.Operation
import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.TypeConvert
import xlang.util.string.String


struct ExpressionDesugar
{
    inline static fun fromPrefix(op: pointer<Operation>, exp: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp)
        return Expression.fromMethodCall(call)
    }


    inline static fun fromInfix(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp1).addArgument(exp2)
        return Expression.fromMethodCall(call)
    }


    inline static fun fromPostfix(op: pointer<Operation>, exp: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp)
        return Expression.fromMethodCall(call)
    }


    static fun fromAssign(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression> =
        ExpressionDesugar.fromAssignWith(op, exp1, exp2, null)


    static fun fromAssignWith(
        op: pointer<Operation>,
        exp1: pointer<Expression>,
        exp2: pointer<Expression>,
        subOp: pointer<Operation>) -> pointer<Expression>
    {
        var value: pointer<Expression> = exp2

        if subOp != null:
            value = ExpressionDesugar.fromInfix(subOp, exp1, exp2)

        return Expression.fromAssignment(exp1, value)
    }


    static fun fromCompare(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val functionName: pointer<char> = op.getFunctionName()

        return if String.streq(functionName, "greater"):
            ExpressionDesugar.compareSign(exp1, exp2, 1)
        elif String.streq(functionName, "less"):
            ExpressionDesugar.compareSign(exp1, exp2, -1)
        elif String.streq(functionName, "greaterEqual"):
        {
            val call: pointer<MethodCall> = new MethodCall(null, "logicalOr")
                .addArgument(ExpressionDesugar.compareSign(exp1, exp2, 1))
                .addArgument(ExpressionDesugar.compareSign(exp1, exp2, 0))

            Expression.fromMethodCall(call)
        }
        elif String.streq(functionName, "lessEqual"):
        {
            val call: pointer<MethodCall> = new MethodCall(null, "logicalOr")
                .addArgument(ExpressionDesugar.compareSign(exp1, exp2, -1))
                .addArgument(ExpressionDesugar.compareSign(exp1, exp2, 0))

            Expression.fromMethodCall(call)
        }
        else: null
    }


    static fun makeNotRefEqual(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression> =
        ExpressionDesugar.wrapNot(ExpressionDesugar.fromInfix(op, exp1, exp2))


    static fun makeNotEqual(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression> =
        ExpressionDesugar.wrapNot(ExpressionDesugar.fromInfix(op, exp1, exp2))


    static fun makeBitwiseNand(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val bitwiseAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(exp2)
        val invCall: pointer<MethodCall> = new MethodCall(null, "inv")
            .addArgument(Expression.fromMethodCall(bitwiseAndCall))

        return Expression.fromMethodCall(invCall)
    }


    static fun makeBitwiseNor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val bitwiseOrCall: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(exp1)
            .addArgument(exp2)
        val invCall: pointer<MethodCall> = new MethodCall(null, "inv")
            .addArgument(Expression.fromMethodCall(bitwiseOrCall))

        return Expression.fromMethodCall(invCall)
    }


    static fun makeBitwiseXor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val leftAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapInv(exp2))
        val rightAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(ExpressionDesugar.wrapInv(exp1))
            .addArgument(exp2)
        val orCall: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(Expression.fromMethodCall(leftAndCall))
            .addArgument(Expression.fromMethodCall(rightAndCall))

        return Expression.fromMethodCall(orCall)
    }


    static fun makeBitwiseXnor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val leftAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(exp2)
        val rightAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(ExpressionDesugar.wrapInv(exp1))
            .addArgument(ExpressionDesugar.wrapInv(exp2))
        val orCall: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(Expression.fromMethodCall(leftAndCall))
            .addArgument(Expression.fromMethodCall(rightAndCall))

        return Expression.fromMethodCall(orCall)
    }


    static fun makeImplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(ExpressionDesugar.wrapInv(exp1))
            .addArgument(exp2)

        return Expression.fromMethodCall(call)
    }


    static fun makeNimplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapInv(exp2))

        return Expression.fromMethodCall(call)
    }

    static fun makeIff(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val trueAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(exp2)
        val falseAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(ExpressionDesugar.wrapInv(exp1))
            .addArgument(ExpressionDesugar.wrapInv(exp2))
        val call: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(Expression.fromMethodCall(trueAndCall))
            .addArgument(Expression.fromMethodCall(falseAndCall))

        return Expression.fromMethodCall(call)
    }


    static fun makeNiff(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val leftAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapInv(exp2))
        val rightAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(ExpressionDesugar.wrapInv(exp1))
            .addArgument(exp2)
        val call: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(Expression.fromMethodCall(leftAndCall))
            .addArgument(Expression.fromMethodCall(rightAndCall))

        return Expression.fromMethodCall(call)
    }


    static fun makeLogicalXor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val leftAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapNot(exp2))
        val rightAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(ExpressionDesugar.wrapNot(exp1))
            .addArgument(exp2)
        val orCall: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(Expression.fromMethodCall(leftAndCall))
            .addArgument(Expression.fromMethodCall(rightAndCall))

        return Expression.fromMethodCall(orCall)
    }


    static fun makeLogicalXnor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val leftAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(exp2)
        val rightAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(ExpressionDesugar.wrapNot(exp1))
            .addArgument(ExpressionDesugar.wrapNot(exp2))
        val orCall: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(Expression.fromMethodCall(leftAndCall))
            .addArgument(Expression.fromMethodCall(rightAndCall))

        return Expression.fromMethodCall(orCall)
    }


    static fun makeLogicalNand(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val logicalAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(exp2)

        return ExpressionDesugar.wrapNot(Expression.fromMethodCall(logicalAndCall))
    }


    static fun makeLogicalImplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(ExpressionDesugar.wrapNot(exp1))
            .addArgument(exp2)

        return Expression.fromMethodCall(call)
    }


    static fun makeLogicalNimplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapNot(exp2))

        return Expression.fromMethodCall(call)
    }


    static fun makeLogicalIff(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val trueAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(exp2)
        val falseAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(ExpressionDesugar.wrapNot(exp1))
            .addArgument(ExpressionDesugar.wrapNot(exp2))
        val call: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(Expression.fromMethodCall(trueAndCall))
            .addArgument(Expression.fromMethodCall(falseAndCall))

        return Expression.fromMethodCall(call)
    }


    static fun makeLogicalNiff(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val leftAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapNot(exp2))
        val rightAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(ExpressionDesugar.wrapNot(exp1))
            .addArgument(exp2)
        val call: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(Expression.fromMethodCall(leftAndCall))
            .addArgument(Expression.fromMethodCall(rightAndCall))

        return Expression.fromMethodCall(call)
    }


    static fun makeLogicalNor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val logicalOrCall: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(exp1)
            .addArgument(exp2)

        return ExpressionDesugar.wrapNot(Expression.fromMethodCall(logicalOrCall))
    }


    private static fun wrapInv(expression: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "inv").addArgument(expression)
        return Expression.fromMethodCall(call)
    }


    private static fun wrapNot(expression: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "not").addArgument(expression)
        return Expression.fromMethodCall(call)
    }


    private static fun intLiteral(value: int) -> pointer<Expression>
    {
        val text: pointer<char> = System.allocMemory(16 * sizeof(char)) as pointer<char>
        TypeConvert.intToString(text, value, 10)

        val position: pointer<TokenPosition> = TokenPosition.autoGenPos()
        val token: pointer<Token> = new Token(Tokenizer.TK_INTEGER, position, text)
        val tokens: pointer<ArrayList> = new ArrayList(sizeof(pointer<*>))
        val resultItem: pointer<*> = token as pointer<*>

        tokens.push(resultItem.ref)

        return Expression.fromAtom(new Atom(Atom.INTEGER_IMM_KIND, tokens))
    }


    private static fun compareSign(exp1: pointer<Expression>, exp2: pointer<Expression>, sign: int) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "compareSign")
            .addArgument(exp1)
            .addArgument(exp2)
            .addArgument(ExpressionDesugar.intLiteral(sign))

        return Expression.fromMethodCall(call)
    }
}
