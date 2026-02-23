package com.wangdi.classgen.artifact

import org.objectweb.asm.ClassWriter


abstract class Layout(private val cw: ClassWriter)
{
    abstract fun generate(): Layout
}
