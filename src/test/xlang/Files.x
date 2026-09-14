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
 *
 *
 */

#file.outerClass("Files")
package xlang

import xlang.System
import xlang.lexer.TokenList
import xlang.util.ArrayList
import xlang.util.IO
import xlang.util.string.String
import xlang.util.string.StringBuilder


val ALL_FILES: pointer<ArrayList> = collectFiles("D:/Coding/projects/Xlang/xlang/src")

val ALL_FILE_CONTENT: pointer<ArrayList> = collectFileContent(ALL_FILES)

val ALL_FILE_TOKENS: pointer<ArrayList> = createFileTokenCache(ALL_FILES)


private fun builderToString(builder: pointer<StringBuilder>) -> pointer<char>
{
    if builder == null:
        return null

    val result: pointer<char> = System.allocMemory((builder.length + 1) * sizeof(char)) as pointer<char>
    builder.toString(result)
    return result
}


private fun joinPath(parent: pointer<char>, child: pointer<char>) -> pointer<char>
{
    val builder: pointer<StringBuilder> = new StringBuilder(parent)
    val parentLength: int = String.strlen(parent)

    if parentLength > 0:
    {
        val last: char = parent[parentLength - 1]

        if last != '/' && last != '\\':
            builder.append('/')
    }

    builder.append(child)
    return builderToString(builder)
}


private fun copyPathPart(text: pointer<char>, start: int, end: int) -> pointer<char>
{
    val length: int = end - start
    val result: pointer<char> = System.allocMemory((length + 1) * sizeof(char)) as pointer<char>

    String.substring(result, text, start, length)
    return result
}


private fun collectFileContent(files: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(pointer<char>))

    if files == null:
        return result

    for (var i = 0; i < files.length; i++):
    {
        val pathSlot: pointer<pointer<char>> = files.get(i) as pointer<pointer<char>>

        // put("collect content ")

        // if pathSlot == null:
        //     putln("<null>")
        // else:
        //     putln(pathSlot.deref)

        val content: pointer<char> = if pathSlot == null:
                null
            else:
                IO.readFile(pathSlot.deref)

        result.push(content.ref)
    }

    return result
}


private fun createFileTokenCache(files: pointer<ArrayList>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(pointer<TokenList>))

    if files == null:
        return result

    for (var i = 0; i < files.length; i++):
    {
        val tokens: pointer<TokenList> = null
        result.push(tokens.ref)
    }

    return result
}


private fun collectFiles(root: pointer<char>) -> pointer<ArrayList>
{
    val result: pointer<ArrayList> = new ArrayList(sizeof(pointer<char>))

    if root == null:
        return result

    if IO.isFile(root):
    {
        // put("collect ")
        // putln(root)
        result.push(root.ref)
        return result
    }

    if !IO.isDirectory(root):
        return result

    val entries: pointer<char> = System.allocMemory(65536 * sizeof(char)) as pointer<char>
    val count: int = IO.subFiles(root, entries)

    if count <= 0:
        return result

    var start: int = 0
    var cursor: int = 0
    var collected: int = 0

    while collected < count && entries[cursor] != '\0':
    {
        while entries[cursor] != '\0' && entries[cursor] != '|':
            cursor++

        val childName: pointer<char> = copyPathPart(entries, start, cursor)
        val childPath: pointer<char> = joinPath(root, childName)
        val childFiles: pointer<ArrayList> = collectFiles(childPath)

        for (var i = 0; i < childFiles.length; i++):
        {
            val childFileSlot: pointer<pointer<char>> = childFiles.get(i) as pointer<pointer<char>>
            result.push(childFileSlot)
        }

        collected++

        if entries[cursor] == '\0':
            return result

        cursor++
        start = cursor
    }

    return result
}
