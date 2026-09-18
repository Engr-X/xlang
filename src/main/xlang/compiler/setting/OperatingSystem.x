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


/**
 * Defines the operating-system families supported as compilation targets.
 */
struct OperatingSystem
{
    /** Identifies Microsoft Windows targets. */
    static val WINDOWS: int = 0

    /** Identifies Linux targets. */
    static val LINUX: int = 1

    /** Identifies Apple macOS targets. */
    static val MACOS: int = 2
}
