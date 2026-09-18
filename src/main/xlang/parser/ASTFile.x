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

package xlang.parser

import xlang.Diagnostic
import xlang.util.ArrayList



struct ASTFile
{
    private var path: pointer<char>

    private var program: pointer<*>

    private var errors: pointer<ArrayList>

    private var warnings: pointer<ArrayList>


    constructor(path: pointer<char>, program: pointer<*>)
    {
        this.path = path
        this.program = program
        this.errors = new ArrayList(sizeof(Diagnostic))
        this.warnings = new ArrayList(sizeof(Diagnostic))
    }


    /**
     * Adds a diagnostic to this AST file.
     *
     * <p>Before the diagnostic is stored, all source locations associated with it
     * are updated to use the path of this AST file.
     *
     * <p>The diagnostic is then classified by severity. Error diagnostics are
     * appended to {@code errors}, while warning diagnostics are appended to
     * {@code warnings}. Diagnostics that are neither errors nor warnings are
     * ignored.
     *
     * <p>The diagnostic is stored by reference and is not copied.
     *
     * @param diagnostics       a pointer to the diagnostic to add
     * @return                  this {@code ASTFile} instance
     */
    fun pushDiagnostic(diagnostics: pointer<Diagnostic>) -> pointer<ASTFile>
    {
        diagnostics.setFilePath(this.path)

        if diagnostics.isError():
            this.errors.push(diagnostics)
        elif diagnostics.isWarning():
            this.warnings.push(diagnostics)
        else:
            pass

        return this
    }


    /**
    * Adds all diagnostics from the specified list to this AST file.
    *
    * <p>Each diagnostic is passed to {@code pushDiagnostic}, which assigns the
    * source file path and classifies the diagnostic according to its severity.
    *
    * <p>Error diagnostics are stored in {@code errors}, warning diagnostics are
    * stored in {@code warnings}, and diagnostics with other severities are ignored.
    *
    * <p>The diagnostics are stored by reference and are not copied.
    *
    * @param diagnostics        a pointer to the list of diagnostics to add
    * @return                   this {@code ASTFile} instance
    */
    fun pushDiagnostics(diagnostics: pointer<ArrayList>) -> pointer<ASTFile>
    {
        for (var i = 0; i < diagnostics.length; i++):
        {
            val item: pointer<Diagnostic> = diagnostics.get(i) as pointer<Diagnostic>
            this.pushDiagnostic(item)
        }

        return this
    }


    /**
     * Reports or throws the diagnostics associated with this token file.
     *
     * <p>This method is intended to process lexical errors and warnings produced
     * while tokenizing the source file.
     *
     * <p>The current implementation performs no operation.
     */
    fun printDiagnostics()
    {
        for (var i = 0; i < this.warnings.length; i++):
        {
            val item: pointer<Diagnostic> = this.warnings.get(i) as pointer<Diagnostic>
            item.print()
            put("\n")
        }

        for (var i = 0; i < this.errors.length; i++):
        {
            val item: pointer<Diagnostic> = this.errors.get(i) as pointer<Diagnostic>
            item.print()
            put("\n")
        }
    }
}
