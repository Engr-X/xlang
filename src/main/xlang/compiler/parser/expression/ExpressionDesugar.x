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

import xlang.Operation
import xlang.System
import xlang.compiler.lexer.Tokenizer
import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.TypeConvert
import xlang.util.string.String


/**
 * Provides helper functions for desugaring expression operators into simpler
 * expression forms.
 *
 * <p>This structure converts prefix, infix, postfix, assignment, comparison,
 * logical, and bitwise operations into combinations of {@code MethodCall},
 * {@code Assignment}, and literal expressions.
 *
 * <p>The generated expressions use intrinsic operation names such as
 * {@code logicalAnd}, {@code logicalOr}, {@code bitwiseAnd}, {@code inv},
 * {@code not}, and {@code compareSign}.
 *
 * <p>These transformations reduce higher-level operator syntax into a smaller
 * set of expression nodes that can be processed uniformly by later compiler
 * stages.
 */
struct ExpressionDesugar
{
    /**
     * Converts a prefix operation into a method-call expression.
     *
     * <p>The operand is passed as the single argument of a method call
     * associated with the supplied operation.
     *
     * @param op                a pointer to the prefix operation
     * @param exp               a pointer to the operand expression
     *
     * @return                  the desugared method-call expression
     */
    inline static fun fromPrefix(op: pointer<Operation>, exp: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp)
        return Expression.fromMethodCall(call)
    }


    /**
     * Converts an infix operation into a method-call expression.
     *
     * <p>The left and right operands are passed as the first and second
     * arguments of a method call associated with the supplied operation.
     *
     * @param op                a pointer to the infix operation
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared method-call expression
     */
    inline static fun fromInfix(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp1).addArgument(exp2)
        return Expression.fromMethodCall(call)
    }


    /**
     * Converts a postfix operation into a method-call expression.
     *
     * <p>The operand is passed as the single argument of a method call
     * associated with the supplied operation.
     *
     * @param op                a pointer to the postfix operation
     * @param exp               a pointer to the operand expression
     *
     * @return                  the desugared method-call expression
     */
    inline static fun fromPostfix(op: pointer<Operation>, exp: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, op).addArgument(exp)
        return Expression.fromMethodCall(call)
    }


    /**
     * Converts an assignment operation into an assignment expression.
     *
     * <p>This method delegates to {@code fromAssignWith} without applying an
     * additional sub-operation.
     *
     * @param op                a pointer to the assignment operation
     * @param exp1              a pointer to the assignment target
     * @param exp2              a pointer to the assigned value
     *
     * @return the desugared assignment expression
     */
    static fun fromAssign(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression> =
        ExpressionDesugar.fromAssignWith(op, exp1, exp2, null)


    /**
     * Converts an assignment operation into an assignment expression,
     * optionally applying another operation to the assigned value.
     *
     * <p>If {@code subOp} is {@code null}, the resulting expression is
     * equivalent to:
     *
     * <pre>
     * exp1 = exp2
     * </pre>
     *
     * <p>If {@code subOp} is provided, the assigned value is first desugared as
     * an infix operation:
     *
     * <pre>
     * exp1 = subOp(exp1, exp2)
     * </pre>
     *
     * <p>This form can be used to represent compound assignments.
     *
     * @param op                a pointer to the assignment operation
     * @param exp1              a pointer to the assignment target
     * @param exp2              a pointer to the right-hand expression
     * @param subOp             a pointer to the operation applied before assignment,
     *                          or {@code null} for a plain assignment
     *
     * @return                  the desugared assignment expression
     */
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


    /**
     * Desugars a relational comparison operation.
     *
     * <p>The comparison is selected using the function name associated with
     * {@code op}.
     *
     * <p>The following operation names are currently recognized:
     * <ul>
     *     <li>{@code greater} - compares against a positive sign</li>
     *     <li>{@code less} - compares against a negative sign</li>
     *     <li>{@code greaterEqual} - combines positive and zero comparisons</li>
     *     <li>{@code lessEqual} - combines negative and zero comparisons</li>
     * </ul>
     *
     * <p>Unrecognized comparison operations produce {@code null}.
     *
     * @param op                a pointer to the comparison operation
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared comparison expression, or {@code null} if the
     *                          operation is not recognized
     */
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


    /**
     * Creates the negated form of a reference-equality operation.
     *
     * <p>The supplied equality operation is first converted into an infix
     * expression and then wrapped in a logical {@code not} operation.
     *
     * @param op                a pointer to the reference-equality operation
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the negated reference-equality expression
     */
    static fun makeNotRefEqual(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression> =
        ExpressionDesugar.wrapNot(ExpressionDesugar.fromInfix(op, exp1, exp2))


    /**
     * Creates the negated form of an equality operation.
     *
     * @param op                a pointer to the equality operation
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the negated equality expression
     */
    static fun makeNotEqual(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression> =
        ExpressionDesugar.wrapNot(ExpressionDesugar.fromInfix(op, exp1, exp2))


    /**
     * Creates a bitwise NAND expression.
     *
     * <p>The resulting expression is equivalent to:
     *
     * <pre>
     * inv(bitwiseAnd(exp1, exp2))
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared bitwise NAND expression
     */
    static fun makeBitwiseNand(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val bitwiseAndCall: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(exp2)
        val invCall: pointer<MethodCall> = new MethodCall(null, "inv")
            .addArgument(Expression.fromMethodCall(bitwiseAndCall))

        return Expression.fromMethodCall(invCall)
    }


    /**
     * Creates a bitwise NOR expression.
     *
     * <p>The resulting expression is equivalent to:
     *
     * <pre>
     * inv(bitwiseOr(exp1, exp2))
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared bitwise NOR expression
     */
    static fun makeBitwiseNor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val bitwiseOrCall: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(exp1)
            .addArgument(exp2)
        val invCall: pointer<MethodCall> = new MethodCall(null, "inv")
            .addArgument(Expression.fromMethodCall(bitwiseOrCall))

        return Expression.fromMethodCall(invCall)
    }


    /**
     * Creates a bitwise exclusive-OR expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 & inv(exp2)) | (inv(exp1) & exp2)
     * </pre>
     *
     * using the intrinsic {@code bitwiseAnd}, {@code bitwiseOr}, and
     * {@code inv} operations.
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared bitwise XOR expression
     */
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


    /**
     * Creates a bitwise equivalence, or XNOR, expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 & exp2) | (inv(exp1) & inv(exp2))
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared bitwise XNOR expression
     */
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


    /**
     * Creates a bitwise implication expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * inv(exp1) | exp2
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the antecedent expression
     * @param exp2              a pointer to the consequent expression
     *
     * @return                  the desugared implication expression
     */
    static fun makeImplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "bitwiseOr")
            .addArgument(ExpressionDesugar.wrapInv(exp1))
            .addArgument(exp2)

        return Expression.fromMethodCall(call)
    }


    /**
     * Creates a negated bitwise implication expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * exp1 & inv(exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the antecedent expression
     * @param exp2              a pointer to the consequent expression
     *
     * @return                  the desugared negated implication expression
     */
    static fun makeNimplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "bitwiseAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapInv(exp2))

        return Expression.fromMethodCall(call)
    }


    /**
     * Creates a bitwise logical-equivalence expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 & exp2) | (inv(exp1) & inv(exp2))
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared equivalence expression
     */
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


    /**
     * Creates a negated bitwise logical-equivalence expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 & inv(exp2)) | (inv(exp1) & exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared non-equivalence expression
     */
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


    /**
     * Creates a logical exclusive-OR expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 && !exp2) || (!exp1 && exp2)
     * </pre>
     *
     * using the intrinsic {@code logicalAnd}, {@code logicalOr}, and
     * {@code not} operations.
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared logical XOR expression
     */
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


    /**
     * Creates a logical XNOR expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 && exp2) || (!exp1 && !exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared logical XNOR expression
     */
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


    /**
     * Creates a logical NAND expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * !(exp1 && exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared logical NAND expression
     */
    static fun makeLogicalNand(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val logicalAndCall: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(exp2)

        return ExpressionDesugar.wrapNot(Expression.fromMethodCall(logicalAndCall))
    }


    /**
     * Creates a logical implication expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * !exp1 || exp2
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the antecedent expression
     * @param exp2              a pointer to the consequent expression
     *
     * @return                  the desugared logical implication expression
     */
    static fun makeLogicalImplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(ExpressionDesugar.wrapNot(exp1))
            .addArgument(exp2)

        return Expression.fromMethodCall(call)
    }


    /**
     * Creates a negated logical implication expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * exp1 && !exp2
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the antecedent expression
     * @param exp2              a pointer to the consequent expression
     *
     * @return                  the desugared negated implication expression
     */
    static fun makeLogicalNimplies(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "logicalAnd")
            .addArgument(exp1)
            .addArgument(ExpressionDesugar.wrapNot(exp2))

        return Expression.fromMethodCall(call)
    }


    /**
     * Creates a logical equivalence expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 && exp2) || (!exp1 && !exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     * @return                  the desugared logical equivalence expression
     */
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


    /**
     * Creates a logical non-equivalence expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * (exp1 && !exp2) || (!exp1 && exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return the desugared logical non-equivalence expression
     */
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


    /**
     * Creates a logical NOR expression.
     *
     * <p>The operation is expanded as:
     *
     * <pre>
     * !(exp1 || exp2)
     * </pre>
     *
     * @param op                the original operation metadata
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     *
     * @return                  the desugared logical NOR expression
     */
    static fun makeLogicalNor(op: pointer<Operation>, exp1: pointer<Expression>, exp2: pointer<Expression>) -> pointer<Expression>
    {
        val logicalOrCall: pointer<MethodCall> = new MethodCall(null, "logicalOr")
            .addArgument(exp1)
            .addArgument(exp2)

        return ExpressionDesugar.wrapNot(Expression.fromMethodCall(logicalOrCall))
    }


    /**
     * Wraps an expression in a bitwise inversion operation.
     *
     * <p>The returned expression is equivalent to:
     *
     * <pre>
     * inv(expression)
     * </pre>
     *
     * @param expression        a pointer to the expression to invert
     *
     * @return                  the generated bitwise inversion expression
     */
    private static fun wrapInv(expression: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "inv").addArgument(expression)
        return Expression.fromMethodCall(call)
    }


    /**
     * Wraps an expression in a logical negation operation.
     *
     * <p>The returned expression is equivalent to:
     *
     * <pre>
     * not(expression)
     * </pre>
     *
     * @param                   expression a pointer to the expression to negate
     *
     * @return                  the generated logical negation expression
     */
    private static fun wrapNot(expression: pointer<Expression>) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "not").addArgument(expression)
        return Expression.fromMethodCall(call)
    }


    /**
     * Creates an automatically generated integer literal expression.
     *
     * <p>The supplied value is converted to its decimal textual representation
     * and stored in an automatically generated integer token.
     *
     * <p>The generated token uses {@code TokenPosition.autoGenPos()} because it
     * does not directly correspond to an integer literal written in the source
     * program.
     *
     * @param value             the integer value represented by the generated expression
     *
     * @return                  the generated integer literal expression
     */
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


    /**
     * Creates a comparison expression using the sign of a comparison result.
     *
     * <p>The generated expression is equivalent to:
     *
     * <pre>
     * compareSign(exp1, exp2, sign)
     * </pre>
     *
     * <p>The {@code sign} argument indicates the comparison result to test:
     * a negative value represents less-than, zero represents equality, and a
     * positive value represents greater-than.
     *
     * @param exp1              a pointer to the left operand
     * @param exp2              a pointer to the right operand
     * @param sign              the comparison sign to test
     *
     * @return                  the generated comparison expression
     */
    private static fun compareSign(exp1: pointer<Expression>, exp2: pointer<Expression>, sign: int) -> pointer<Expression>
    {
        val call: pointer<MethodCall> = new MethodCall(null, "compareSign")
            .addArgument(exp1)
            .addArgument(exp2)
            .addArgument(ExpressionDesugar.intLiteral(sign))

        return Expression.fromMethodCall(call)
    }
}
