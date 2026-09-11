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

#file.outerClass("Atoms")
package xlang.compiler.parser.expression

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct Atoms
{
    private val atoms: pointer<ArrayList>

    private var extraTokens: pointer<ArrayList>


    constructor(atom: pointer<Atom>)
    {
        this.atoms = new ArrayList(sizeof(pointer<Atom>))
        this.extraTokens = new ArrayList(sizeof(Token))
        this.push(atom)
    }


    fun push(atom: pointer<Atom>) -> pointer<Atoms>
    {
        if atom != null:
            this.atoms.push(atom.ref)

        return this
    }


    fun pushFront(atom: pointer<Atom>) -> pointer<Atoms>
    {
        if atom != null:
            this.atoms.pushFront(atom.ref)

        return this
    }


    fun pushAtoms(atoms: pointer<Atoms>) -> pointer<Atoms>
    {
        if atoms != null && atoms.atoms != null:
        {
            this.atoms.pushAll(atoms.atoms)
            this.extraTokens.pushAll(atoms.extraTokens)
        }

        return this
    }


    fun addExtraToken(token: pointer<Token>) -> pointer<Atoms>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    fun getAtoms() -> pointer<ArrayList> = this.atoms.clone()


    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.atoms.length; i++):
        {
            val slot: pointer<pointer<Atom>> = this.atoms.get(i) as pointer<pointer<Atom>>

            if slot == null || slot.deref == null:
                continue

            val atom: pointer<Atom> = slot.deref
            val tokens: pointer<ArrayList> = atom.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedAtom: bool = false

        for (var i = 0; i < this.atoms.length; i++):
        {
            val slot: pointer<pointer<Atom>> = this.atoms.get(i) as pointer<pointer<Atom>>

            if slot == null || slot.deref == null:
                continue

            if appendedAtom:
                sb.append(", ")

            val atom: pointer<Atom> = slot.deref
            sb.append(atom.toString())
            appendedAtom = true
        }

        return sb
    }
}
