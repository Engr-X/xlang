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

@file.class("IO")
package xlang.util

@native("enable_ansi_color")
native inline fun enableANSIColor() -> int;


@native("colored_sprint")
native inline fun coloredSprint(dest: pointer<char>, value: pointer<char>, color: int) -> int;


@native("colored_sprintln")
native inline fun coloredSprintln(dest: pointer<char>, value: pointer<char>, color: int) -> int;
