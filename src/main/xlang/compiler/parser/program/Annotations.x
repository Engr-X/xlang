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

#file.outerClass("Annotations")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Annotations
{
    private val annotations: pointer<ArrayList>


    constructor():
        this.annotations = new ArrayList(sizeof(Annotation))


    constructor(annotation: pointer<Annotation>)
    {
        this.annotations = new ArrayList(sizeof(Annotation))
        this.push(annotation)
    }


    fun push(annotation: pointer<Annotation>) -> pointer<Annotations>
    {
        if annotation != null:
            this.annotations.push(annotation)

        return this
    }


    fun pushAll(annotations: pointer<Annotations>) -> pointer<Annotations>
    {
        if annotations != null && annotations.annotations != null:
            this.annotations.pushAll(annotations.annotations)

        return this
    }


    fun length() -> int = this.annotations.length


    fun get(index: int) -> pointer<Annotation>
    {
        if index < 0 || index >= this.annotations.length:
            return null

        return this.annotations.get(index) as pointer<Annotation>
    }


    fun getAnnotations() -> pointer<ArrayList> = this.annotations.clone()


    fun getExtraTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.annotations.length; i++):
        {
            val annotation: pointer<Annotation> = this.get(i)

            if annotation == null:
                continue

            val tokens: pointer<ArrayList> = annotation.getExtraTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.annotations.length; i++):
        {
            val annotation: pointer<Annotation> = this.get(i)

            if annotation == null:
                continue

            val tokens: pointer<ArrayList> = annotation.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedAnnotation: bool = false

        for (var i = 0; i < this.annotations.length; i++):
        {
            val annotation: pointer<Annotation> = this.get(i)

            if annotation == null:
                continue

            if appendedAnnotation:
                sb.newline()

            sb.append(annotation.toString())
            appendedAnnotation = true
        }

        return sb
    }
}


struct AnnotationsMaybe
{
    private var annotations: pointer<Annotations>


    constructor(annotations: pointer<Annotations>):
        this.annotations = if annotations == null:
                new Annotations()
            else:
                annotations


    fun toAnnotations() -> pointer<Annotations> = this.annotations
}
