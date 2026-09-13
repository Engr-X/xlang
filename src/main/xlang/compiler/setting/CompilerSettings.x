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

#file.outerClass("CompilerSettings")
package xlang.compiler.setting


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
    /**
     * Shared settings instance used by compiler components.
     *
     * The instance is created once when CompilerSettings is initialized and is
     * returned by {@link #getInstance()}.
     */
    private static val instance: pointer<CompilerSettings> = new CompilerSettings()


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
     * Creates compiler settings using the default configuration.
     *
     * The default target is a 64-bit Windows system and the compiler is
     * configured to use a single worker thread.
     */
    constructor()
    {
        this.thread = 1
        this.systemBits = SystemBits.BITS_64
        this.operatingSystem = OperatingSystem.WINDOWS
    }


    /**
     * Returns the shared compiler settings instance.
     *
     * All compiler components accessing this method receive the same settings
     * object.
     *
     * @return the global CompilerSettings instance
     */
    static fun getInstance() -> pointer<CompilerSettings> = instance


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
}
