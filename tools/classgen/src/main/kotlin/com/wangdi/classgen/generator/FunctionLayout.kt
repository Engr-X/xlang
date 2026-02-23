package com.wangdi.classgen.generator

import com.wangdi.classgen.artifact.Layout
import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import org.objectweb.asm.ClassWriter


class FunctionLayout(
    cw: ClassWriter,
    private val access: Array<Access>,
    private val name: String,
    private val funParams: (Type, MutableList<Type>)
) : Layout(cw)
{
    override fun generate(): Layout = this.apply {

    }
}
