package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.generator.artifact.ClassArtifact
import com.wangdi.bctoolkit.base.Type
import com.wangdi.bctoolkit.base.TypeRef

import org.objectweb.asm.ClassWriter


class ClassEmitter(
    jvmTarget: Int,
    access: MutableList<Access>,
    clazz: TypeRef,
    signature: MutableList<String> = mutableListOf(),

    private val superClass: TypeRef = TypeRef.OBJECT,
    private val interfaces: MutableList<TypeRef> = ArrayList(),
) : ClassArtifact(jvmTarget, access, clazz, signature)
{
    private val attributes: MutableList<AttributeGenerator> = mutableListOf()

    private var clinitEmitter: ClinitEmitter? = null

    private val initEmitters: MutableList<InitEmitter> = mutableListOf()

    private var visited: Boolean = false

    /**
     * Auto-generated baseline docs for addAttribute.
     * Describes the intent and behavior of this function.
     *
     * @param attr parameter from function signature.
     * @return return value of this function.
     */
    fun addAttribute(attr: AttributeGenerator): ClassArtifact = this.apply { this.attributes.add(attr) }

    /**
     * Auto-generated baseline docs for setClinit.
     * Describes the intent and behavior of this function.
     *
     * @param clinit parameter from function signature.
     * @return return value of this function.
     */
    fun setClinit(clinit: ClinitEmitter): ClassEmitter = this.apply {
        this.ensureVisited()
        this.clinitEmitter = clinit
    }

    /**
     * Auto-generated baseline docs for addInit.
     * Describes the intent and behavior of this function.
     *
     * @param init parameter from function signature.
     * @return return value of this function.
     */
    fun addInit(init: InitEmitter): ClassEmitter = this.apply {
        this.ensureVisited()
        this.initEmitters.add(init)
    }

    /**
     * Auto-generated baseline docs for newClinit.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun newClinit(): ClinitEmitter {
        this.ensureVisited()
        return ClinitEmitter(this.cw)
    }

    /**
     * Auto-generated baseline docs for newInit.
     * Describes the intent and behavior of this function.
     *
     * @param access parameter from function signature.
     * @param params parameter from function signature.
     * @param signature parameter from function signature.
     * @param exceptions parameter from function signature.
     * @return return value of this function.
     */
    fun newInit(
        access: Access,
        params: MutableList<Type> = mutableListOf(),
        signature: MutableList<String> = mutableListOf(),
        exceptions: MutableList<Type> = mutableListOf(),
    ): InitEmitter
    {
        this.ensureVisited()
        return InitEmitter(this.cw, access, params, signature, exceptions)
    }

    /**
     * Auto-generated baseline docs for newMethod.
     * Describes the intent and behavior of this function.
     *
     * @param access parameter from function signature.
     * @param name parameter from function signature.
     * @param funParams parameter from function signature.
     * @param signature parameter from function signature.
     * @param exceptions parameter from function signature.
     * @return return value of this function.
     */
    fun newMethod(
        access: MutableList<Access>,
        name: String,
        funParams: Pair<Type, MutableList<Type>>,
        signature: MutableList<String> = mutableListOf(),
        exceptions: MutableList<Type> = mutableListOf(),
    ): MethodEmitter
    {
        this.ensureVisited()
        return MethodEmitter(this.cw, access, name, funParams, signature, exceptions)
    }

    /**
     * Auto-generated baseline docs for ensureVisited.
     * Describes the intent and behavior of this function.
     *
     * @param cw parameter from function signature.
     * @return return value of this function.
     */
    private fun ensureVisited(cw: ClassWriter = this.cw)
    {
        if (this.visited)
            return
        cw.visit(
            this.jvmTarget,
            this.accessOf(),
            this.clazz.getFullName("/"),
            this.genSignature(),
            this.superClass.getFullName("/"),
            this.interfaces.map { impl -> impl.getFullName("/") }.toTypedArray())
        this.visited = true
    }

    /**
     * Auto-generated baseline docs for init.
     * Describes the intent and behavior of this function.
     *
     * @param cw parameter from function signature.
     * @return return value of this function.
     */
    override fun init(cw: ClassWriter): ClassWriter = cw.also {
        this.ensureVisited(it)

        this.attributes.forEach { attr -> attr.generate() }

        val clinitEmitter = this.clinitEmitter ?: ClinitEmitter.getEmpty(cw)
        clinitEmitter.generate()
        val inits = if (this.initEmitters.isEmpty())
            mutableListOf(InitEmitter.getEmpty(cw))
        else
            this.initEmitters

        inits.forEach { `init` -> `init`.generate() }
        this.methods.forEach { method -> method.generate() }
    }
}

