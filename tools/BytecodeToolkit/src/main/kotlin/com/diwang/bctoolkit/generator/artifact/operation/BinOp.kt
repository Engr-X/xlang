package com.diwang.bctoolkit.generator.artifact.operation

import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes


// ---------- Int arithmetic ----------
class IAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IADD)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}iadd"
}


class ISub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.ISUB)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}isub"
}


class IMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IMUL)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}imul"
}


class IDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IDIV)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}idiv"
}


class IRem(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two ints, push remainder (a % b).
        it.visitInsn(Opcodes.IREM)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}irem"
}

class IAnd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IAND)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}iand"
}

class IOr(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IOR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ior"
}

class IXor(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IXOR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ixor"
}

class IShl(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.ISHL)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ishl"
}

class IShr(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.ISHR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}ishr"
}

class IUShr(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.IUSHR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}iushr"
}


// ---------- Long arithmetic ----------
class LAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LADD)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}ladd"
}


class LSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LSUB)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}lsub"
}


class LMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LMUL)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}lmul"
}


class LDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two longs, push their quotient.
        it.visitInsn(Opcodes.LDIV)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}ldiv"
}


class LRem(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        // English comment: pop two longs, push remainder (a % b).
        it.visitInsn(Opcodes.LREM)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}lrem"
}

class LAnd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LAND)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}land"
}

class LOr(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LOR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lor"
}

class LXor(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LXOR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lxor"
}

class LShl(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LSHL)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lshl"
}

class LShr(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LSHR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lshr"
}

class LUShr(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.LUSHR)
    }

    override fun toString(tabs: Int): String = "${this.indent(tabs)}lushr"
}


// ---------- Float arithmetic ----------
class FAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FADD)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}fadd"
}


class FSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FSUB)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}fsub"
}


class FMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FMUL)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}fmul"
}


class FDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.FDIV)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}fdiv"
}


// ---------- Double arithmetic ----------
class DAdd(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DADD)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}dadd"
}


class DSub(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DSUB)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}dsub"
}


class DMul(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DMUL)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}dmul"
}


class DDiv(mv: MethodVisitor, labels: MutableMap<Int, Label>) : Instruction(mv, labels)
{
    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun addOp(): MethodVisitor = this.mv.also {
        it.visitInsn(Opcodes.DDIV)
    }

    /**
     * Auto-generated baseline docs for toString.
     * Describes the intent and behavior of this function.
     *
     * @param tabs parameter from function signature.
     * @return return value of this function.
     */
    override fun toString(tabs: Int): String = "${this.indent(tabs)}ddiv"
}

