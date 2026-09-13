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

#file.outerClass("SystemBits")
package xlang.compiler.setting


/**
 * Defines the supported target-system bit widths.
 */
struct SystemBits
{
    /** Number of bits contained in one byte. */
    static val BITS_PER_BYTE: int = 8

    /** Identifies a 64-bit target system. */
    static val BITS_64: int = 64

    /** Identifies a 32-bit target system. */
    static val BITS_32: int = 32

    /** Identifies a 16-bit target system. */
    static val BITS_16: int = 16
}
