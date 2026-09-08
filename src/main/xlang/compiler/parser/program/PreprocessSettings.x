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
#file.class("PreprocessSettings")
package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.StringBuilder


struct PreprocessSettings
{
    private val settings: pointer<ArrayList>


    fun __init__():
        this.settings = new ArrayList(sizeof(PreprocessSetting))


    fun __init__(setting: pointer<PreprocessSetting>)
    {
        this.settings = new ArrayList(sizeof(PreprocessSetting))
        this.push(setting)
    }


    fun push(setting: pointer<PreprocessSetting>) -> pointer<PreprocessSettings>
    {
        if setting != null:
            this.settings.push(setting)

        return this
    }


    fun pushAll(settings: pointer<PreprocessSettings>) -> pointer<PreprocessSettings>
    {
        if settings != null && settings.settings != null:
            this.settings.pushAll(settings.settings)

        return this
    }


    fun length() -> int = this.settings.length


    fun get(index: int) -> pointer<PreprocessSetting>
    {
        if index < 0 || index >= this.settings.length:
            return null

        return this.settings.get(index) as pointer<PreprocessSetting>
    }


    fun getSettings() -> pointer<ArrayList> = this.settings.clone()


    fun getExtraTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.settings.length; i++):
        {
            val setting: pointer<PreprocessSetting> = this.get(i)

            if setting == null:
                continue

            val tokens: pointer<ArrayList> = setting.getExtraTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        for (var i = 0; i < this.settings.length; i++):
        {
            val setting: pointer<PreprocessSetting> = this.get(i)

            if setting == null:
                continue

            val tokens: pointer<ArrayList> = setting.getAllTokens()

            if tokens != null:
                result.pushAll(tokens)
        }

        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    fun toString() -> pointer<StringBuilder>
    {
        val sb: pointer<StringBuilder> = new StringBuilder()
        var appendedSetting: bool = false

        for (var i = 0; i < this.settings.length; i++):
        {
            val setting: pointer<PreprocessSetting> = this.get(i)

            if setting == null:
                continue

            if appendedSetting:
                sb.newline()

            sb.append(setting.toString())
            appendedSetting = true
        }

        return sb
    }
}


struct PreprocessSettingsMaybe
{
    private var settings: pointer<PreprocessSettings>


    fun __init__(settings: pointer<PreprocessSettings>):
        this.settings = if settings == null:
                new PreprocessSettings()
            else:
                settings


    fun toPreprocessSettings() -> pointer<PreprocessSettings> = this.settings
}
