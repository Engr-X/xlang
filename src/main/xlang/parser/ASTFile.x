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


/**
 * Represents the abstract syntax tree and diagnostics associated with a source file.
 *
 * <p>An {@code ASTFile} stores the source file path, the root program node produced
 * by parsing, and the diagnostics generated while processing the file.
 *
 * <p>Error and warning diagnostics are maintained separately. Diagnostics added to
 * this instance are associated with the file path of this AST file before being
 * stored.
 */
struct ASTFile
{
    /**
     * The path of the source file represented by this AST file.
     *
     * <p>The path is stored by reference and is not duplicated.
     */
    private var path: pointer<char>


    /**
     * The root program node of the abstract syntax tree.
     *
     * <p>The concrete type of the program node depends on the parser and AST
     * implementation.
     */
    private var program: pointer<*>


    /**
     * The list of error diagnostics associated with this AST file.
     *
     * <p>Each element is expected to be a pointer to a {@code Diagnostic}
     * classified as an error.
     */
    private var errors: pointer<ArrayList>


    /**
     * The list of warning diagnostics associated with this AST file.
     *
     * <p>Each element is expected to be a pointer to a {@code Diagnostic}
     * classified as a warning.
     */
    private var warnings: pointer<ArrayList>


    /**
     * Creates an AST file for the specified source path and program node.
     *
     * <p>The source path and program node are stored by reference. Separate
     * diagnostic lists are created for errors and warnings.
     *
     * @param path              a pointer to the source file path
     * @param program           a pointer to the root program node of the abstract syntax tree
     */
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
     * @param diagnostic        a pointer to the diagnostic to add
     *
     * @return                  this {@code ASTFile} instance
     */
    fun pushDiagnostic(diagnostic: pointer<Diagnostic>) -> pointer<ASTFile>
    {
        if diagnostic == null:
            return this

        diagnostic.setFilePath(this.path)

        if diagnostic.isError():
            this.errors.push(diagnostic)
        elif diagnostic.isWarning():
            this.warnings.push(diagnostic)
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
    *
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
