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

package xlang.annotation

/**
 * Attaches compiler-level metadata to generated fields or functions.
 *
 * This annotation is stored in class files with BINARY retention and is
 * intended for tooling/runtime interop inside xlang's Java backend, not for
 * general source-level behavior changes.
 *
 * @property value opaque metadata payload encoded by the compiler.
 */

@Target(AnnotationTarget.FIELD, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.BINARY)
annotation class Metadata(val value: String)
