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

import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Member
{
    static val FIELD_TYPE: int = 1

    static val FUNCTION_TYPE: int = 2

    static val STRUCT_CONSTRUCTOR_TYPE: int = 3

    static val STRUCT_TYPE: int = 4



    static fun fromField(field: pointer<Field>) -> pointer<Member> =
        new Member(FIELD_TYPE, field)


    static fun fromFunction(function: pointer<Function>) -> pointer<Member> =
        new Member(FUNCTION_TYPE, function)


    static fun fromStructConstructor(structConstructor: pointer<StructConstructor>) -> pointer<Member> =
        new Member(STRUCT_CONSTRUCTOR_TYPE, structConstructor)


    static fun fromStruct(structDecl: pointer<Struct>) -> pointer<Member> =
        new Member(STRUCT_TYPE, structDecl)


    private var kind: int

    private var host: pointer<*>


    constructor(kind: int, host: pointer<*>)
    {
        this.kind = kind
        this.host = host
    }


    fun isField() -> bool = this.kind == FIELD_TYPE


    fun isFunction() -> bool = this.kind == FUNCTION_TYPE


    fun isStructConstructor() -> bool = this.kind == STRUCT_CONSTRUCTOR_TYPE


    fun isStruct() -> bool = this.kind == STRUCT_TYPE


    fun getKind() -> int = this.kind


    fun getHost() -> pointer<*> = this.host


    fun getAllTokens() -> pointer<ArrayList> =
        if this.host == null:
            new ArrayList(sizeof(Token))
        elif this.kind == FIELD_TYPE:
        {
            val field: pointer<Field> = this.host as pointer<Field>
            field.getAllTokens()
        }
        elif this.kind == FUNCTION_TYPE:
        {
            val function: pointer<Function> = this.host as pointer<Function>
            function.getAllTokens()
        }
        elif this.kind == STRUCT_CONSTRUCTOR_TYPE:
        {
            val structConstructor: pointer<StructConstructor> = this.host as pointer<StructConstructor>
            structConstructor.getAllTokens()
        }
        elif this.kind == STRUCT_TYPE:
        {
            val structDecl: pointer<Struct> = this.host as pointer<Struct>
            structDecl.getAllTokens()
        }
        else:
            new ArrayList(sizeof(Token))


    fun toString() -> pointer<StringBuilder> =
        if this.host == null:
            new StringBuilder()
        elif this.kind == FIELD_TYPE:
        {
            val field: pointer<Field> = this.host as pointer<Field>
            field.toString()
        }
        elif this.kind == FUNCTION_TYPE:
        {
            val function: pointer<Function> = this.host as pointer<Function>
            function.toString()
        }
        elif this.kind == STRUCT_CONSTRUCTOR_TYPE:
        {
            val structConstructor: pointer<StructConstructor> = this.host as pointer<StructConstructor>
            structConstructor.toString()
        }
        elif this.kind == STRUCT_TYPE:
        {
            val structDecl: pointer<Struct> = this.host as pointer<Struct>
            structDecl.toString()
        }
        else:
            new StringBuilder()
}
