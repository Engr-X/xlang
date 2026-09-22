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

package xlang.compiler.setting

import xlang.System
import xlang.compiler.parser.expression.Atom
import xlang.compiler.parser.program.PreprocessSetting
import xlang.util.IO
import xlang.util.string.StringBuilder


/**
 * Stores global configuration for the current compiler invocation.
 *
 * CompilerSettings contains options that affect compiler-wide behavior,
 * including the number of worker threads and the selected target platform.
 * These values may be configured before parsing, semantic analysis, or code
 * generation begins and are then shared across compiler components.
 *
 * A single shared instance is created for the compiler process and can be
 * accessed through {@link #getInstance()}.
 *
 * Unless explicitly changed, the compiler uses one worker thread and targets
 * a 64-bit Windows system.
 */
struct CompilerSettings
{
    static val OUTER_CLASS_POSTFIX: pointer<char> = "X"


    /**
     * Number of worker threads available to the compiler.
     *
     * A value of one causes compiler work to execute without additional
     * parallel worker threads.
     */
    private var thread: int

    /**
     * Bit width of the selected target system.
     *
     * The value is expected to correspond to one of the constants defined by
     * {@code SystemBits}.
     */
    private var systemBits: int

    /**
     * Operating-system family targeted by generated output.
     *
     * The value is expected to correspond to one of the constants defined by
     * {@code OperatingSystem}.
     */
    private var operatingSystem: int

    /**
     * The name of the enclosing class for this class.
     *
     * <p>This is used for nested classes to identify their outer class,
     * similarly to the enclosing class name stored in JVM class metadata.
     *
     * <p>The value is {@code null} if this class is not nested inside
     * another class.
     */
    private var outerClass: pointer<char>


    /**
     * Creates compiler settings using the default configuration.
     *
     * The default target is a 64-bit Windows system and the compiler is
     * configured to use a single worker thread.
     */
    constructor(filepath: pointer<char>)
    {
        this.thread = 1
        this.systemBits = SystemBits.BITS_64
        this.operatingSystem = OperatingSystem.WINDOWS

        val outerClassName: pointer<StringBuilder> = IO.getFileName(filepath)
        outerClassName.append(OUTER_CLASS_POSTFIX)

        this.outerClass = System.allocMemory((outerClassName.length + 1) * sizeof(char)) as pointer<char>
        outerClassName.toString(this.outerClass)
    }


    /**
     * Returns the shared compiler settings instance.
     *
     * All compiler components accessing this method receive the same settings
     * object.
     *
     * @return the global CompilerSettings instance
     */
    static fun getInstance() -> pointer<CompilerSettings> = new CompilerSettings("TODO")


    /**
     * Returns the configured outer class name.
     *
     * @return                  outer class name for the current source file
     */
    fun getOuterClass() -> pointer<char> = this.outerClass


    /**
     * Returns the configured number of worker threads.
     *
     * @return the number of worker threads available to the compiler
     */
    fun getThread() -> int = this.thread


    /**
     * Sets the number of worker threads available to the compiler.
     *
     * The method returns this instance so multiple compiler settings may be
     * configured using chained calls.
     *
     * @param thread number of worker threads to use
     * @return this CompilerSettings instance
     */
    fun setThread(thread: int) -> pointer<CompilerSettings>
    {
        this.thread = thread
        return this
    }


    /**
     * Returns the bit width of the selected target system.
     *
     * @return the target-system bit width
     */
    fun getSystemBits() -> int = this.systemBits


    /**
     * Sets the bit width of the target system.
     *
     * The supplied value should normally be one of the constants provided by
     * {@code SystemBits}.
     *
     * @param systemBits bit width of the target system
     * @return this CompilerSettings instance
     */
    fun setSystemBits(systemBits: int) -> pointer<CompilerSettings>
    {
        this.systemBits = systemBits
        return this
    }


    /**
     * Returns the operating-system family selected as the compilation target.
     *
     * @return the target operating-system family
     */
    fun getOperatingSystem() -> int = this.operatingSystem


    /**
     * Sets the operating-system family targeted by generated output.
     *
     * The supplied value should normally be one of the constants provided by
     * {@code OperatingSystem}.
     *
     * @param operatingSystem target operating-system family
     * @return this CompilerSettings instance
     */
    fun setOperatingSystem(operatingSystem: int) -> pointer<CompilerSettings>
    {
        this.operatingSystem = operatingSystem
        return this
    }


    /**
     * Applies a preprocessing setting to this compiler settings instance.
     *
     * <p>If {@code setting} is {@code null}, no changes are made and this
     * instance is returned unchanged.
     *
     * <p>The setting is identified by its qualified name. Recognized settings
     * are parsed and stored in the corresponding fields of this instance.
     *
     * <p>Currently, the following setting is supported:
     * <ul>
     *     <li>
     *         {@code file.outerClass} - Specifies the name of the outer class
     *         associated with the current source file. The value is obtained from
     *         the first token of the first value atom.
     *     </li>
     * </ul>
     *
     * <p>Unrecognized settings are ignored.
     *
     * @param setting           a pointer to the preprocessing setting to apply
     * @return                  this {@code CompilerSettings} instance
     */
    fun set(setting: pointer<PreprocessSetting>) -> pointer<CompilerSettings>
    {
        if setting == null:
            return this

        val name: pointer<QualifiedName> = setting.getName()
        val nameString: pointer<StringBuilder> = name.toString()

        if nameString.equals("file.outerClass"):
        {
            val atom: pointer<Atom> = setting.getValue().get(0) as pointer<Atom>
            val token: pointer<Token> = atom.getAllTokens().get(0) as pointer<Token>
            this.outerClass = token.text
        }

        return this
    }
}
