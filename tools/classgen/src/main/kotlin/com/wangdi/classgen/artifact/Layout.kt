package com.wangdi.classgen.artifact

import org.objectweb.asm.ClassWriter


open class Layout(private val cw: ClassWriter)
{
    fun generate(): Layout = this
}
