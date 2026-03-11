package com.wangdi.bctoolkit.generator

import com.wangdi.bctoolkit.artifact.Emitter
import com.wangdi.bctoolkit.artifact.operation.*
import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import xlang.annotation.OwnerTypeMetadata

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.MethodVisitor


open class MethodEmitter(
    private val cw: ClassWriter,
    private val access: MutableList<Access>,
    private val name: String,
    private val funParams: Pair<Type, MutableList<Type>>,
    private val signature: MutableList<String> = mutableListOf(),
    private val exceptions: MutableList<Type> = mutableListOf(),
) : Emitter()
{
    private var ownerType: String = OwnerTypeMetadata.CLASS

    protected val operations: MutableList<Instruction> = mutableListOf()

    open inner class Builder
    {
        private val innerOps: MutableList<Instruction> = mutableListOf()

        protected open fun innerAddOp(op: Instruction): MutableList<Instruction> =
            this.innerOps.also { it.add(op) }

        fun iadd(): Builder = this.apply { this.innerAddOp(IAdd(mv, labels)) }
        fun ladd(): Builder = this.apply { this.innerAddOp(LAdd(mv, labels)) }
        fun fadd(): Builder = this.apply { this.innerAddOp(FAdd(mv, labels)) }
        fun dadd(): Builder = this.apply { this.innerAddOp(DAdd(mv, labels)) }

        fun isub(): Builder = this.apply { this.innerAddOp(ISub(mv, labels)) }
        fun lsub(): Builder = this.apply { this.innerAddOp(LSub(mv, labels)) }
        fun fsub(): Builder = this.apply { this.innerAddOp(FSub(mv, labels)) }
        fun dsub(): Builder = this.apply { this.innerAddOp(DSub(mv, labels)) }

        fun imul(): Builder = this.apply { this.innerAddOp(IMul(mv, labels)) }
        fun lmul(): Builder = this.apply { this.innerAddOp(LMul(mv, labels)) }
        fun fmul(): Builder = this.apply { this.innerAddOp(FMul(mv, labels)) }
        fun dmul(): Builder = this.apply { this.innerAddOp(DMul(mv, labels)) }

        fun idiv(): Builder = this.apply { this.innerAddOp(IDiv(mv, labels)) }
        fun ldiv(): Builder = this.apply { this.innerAddOp(LDiv(mv, labels)) }
        fun fdiv(): Builder = this.apply { this.innerAddOp(FDiv(mv, labels)) }
        fun ddiv(): Builder = this.apply { this.innerAddOp(DDiv(mv, labels)) }

        fun irem(): Builder = this.apply { this.innerAddOp(IRem(mv, labels)) }
        fun lrem(): Builder = this.apply { this.innerAddOp(LRem(mv, labels)) }

        fun invokeStatic(fullName: MutableList<String>, signature: Pair<Type, MutableList<Type>>): Builder =
            this.apply { this.innerAddOp(InvokeStatic(mv, labels, fullName, signature)) }

        fun invokeSpecial(fullName: MutableList<String>, signature: Pair<Type, MutableList<Type>>): Builder =
            this.apply { this.innerAddOp(InvokeSpecial(mv, labels, fullName, signature)) }

        fun invokeVirtual(fullName: MutableList<String>, signature: Pair<Type, MutableList<Type>>): Builder =
            this.apply { this.innerAddOp(InvokeVirtual(mv, labels, fullName, signature)) }

        fun i2l(): Builder = this.apply { this.innerAddOp(I2L(mv, labels)) }
        fun i2f(): Builder = this.apply { this.innerAddOp(I2F(mv, labels)) }
        fun i2d(): Builder = this.apply { this.innerAddOp(I2D(mv, labels)) }

        fun l2i(): Builder = this.apply { this.innerAddOp(L2I(mv, labels)) }
        fun l2f(): Builder = this.apply { this.innerAddOp(L2F(mv, labels)) }
        fun l2d(): Builder = this.apply { this.innerAddOp(L2D(mv, labels)) }

        fun f2i(): Builder = this.apply { this.innerAddOp(F2I(mv, labels)) }
        fun f2l(): Builder = this.apply { this.innerAddOp(F2L(mv, labels)) }
        fun f2d(): Builder = this.apply { this.innerAddOp(F2D(mv, labels)) }

        fun d2i(): Builder = this.apply { this.innerAddOp(D2I(mv, labels)) }
        fun d2l(): Builder = this.apply { this.innerAddOp(D2L(mv, labels)) }
        fun d2f(): Builder = this.apply { this.innerAddOp(D2F(mv, labels)) }

        fun block(op: OpBlock): Builder = this.apply { this.innerAddOp(op) }

        fun goto(blockId: Int): Builder = this.apply { this.innerAddOp(Goto(mv, labels, blockId)) }

        @Suppress("FunctionName")
        fun if_icmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_icmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_icmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_icmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_icmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_icmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpge(mv, labels, blockId)) }

        @Suppress("FunctionName")
        fun if_lcmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_lcmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_lcmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_lcmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_lcmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_lcmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpge(mv, labels, blockId)) }

        @Suppress("FunctionName")
        fun if_fcmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_fcmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_fcmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_fcmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_fcmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_fcmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpge(mv, labels, blockId)) }

        @Suppress("FunctionName")
        fun if_dcmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_dcmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_dcmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_dcmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_dcmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        fun if_dcmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpge(mv, labels, blockId)) }


        fun `return`(): Builder = this.apply { this.innerAddOp(Return(mv, labels)) }
        fun ireturn(): Builder = this.apply { this.innerAddOp(IReturn(mv, labels)) }
        fun lreturn(): Builder = this.apply { this.innerAddOp(LReturn(mv, labels)) }
        fun freturn(): Builder = this.apply { this.innerAddOp(FReturn(mv, labels)) }
        fun dreturn(): Builder = this.apply { this.innerAddOp(DReturn(mv, labels)) }

        fun pop(): Builder = this.apply { this.innerAddOp(Pop(mv, labels)) }
        fun dup(): Builder = this.apply { this.innerAddOp(Dup(mv, labels)) }
        fun nop(): Builder = this.apply { this.innerAddOp(Nop(mv, labels)) }

        fun getstatic(name: MutableList<String>, type: Type): Builder =
            this.apply { this.innerAddOp(GetStatic(mv, labels, name, type)) }

        fun putstatic(name: MutableList<String>, type: Type): Builder =
            this.apply { this.innerAddOp(PutStatic(mv, labels, name, type)) }

        fun ineg(): Builder = this.apply { this.innerAddOp(INeg(mv, labels)) }
        fun lneg(): Builder = this.apply { this.innerAddOp(LNeg(mv, labels)) }
        fun fneg(): Builder = this.apply { this.innerAddOp(FNeg(mv, labels)) }
        fun dneg(): Builder = this.apply { this.innerAddOp(DNeg(mv, labels)) }

        fun inot(): Builder = this.apply { this.innerAddOp(IBitNot(mv, labels)) }
        fun lnot(): Builder = this.apply { this.innerAddOp(LBitNot(mv, labels)) }

        fun pos(): Builder = this.apply { this.innerAddOp(UPlus(mv, labels)) }

        fun iload(index: Int): Builder = this.apply { this.innerAddOp(ILoad(mv, labels, index)) }
        fun lload(index: Int): Builder = this.apply { this.innerAddOp(LLoad(mv, labels, index)) }
        fun fload(index: Int): Builder = this.apply { this.innerAddOp(FLoad(mv, labels, index)) }
        fun dload(index: Int): Builder = this.apply { this.innerAddOp(DLoad(mv, labels, index)) }
        fun aload(index: Int): Builder = this.apply { this.innerAddOp(ALoad(mv, labels, index)) }

        fun ipush(value: Int): Builder = this.apply { this.innerAddOp(IPush(mv, labels, value)) }
        fun lpush(value: Long): Builder = this.apply { this.innerAddOp(LPush(mv, labels, value)) }
        fun fpush(value: Float): Builder = this.apply { this.innerAddOp(FPush(mv, labels, value)) }
        fun dpush(value: Double): Builder = this.apply { this.innerAddOp(DPush(mv, labels, value)) }

        fun istore(index: Int): Builder = this.apply { this.innerAddOp(IStore(mv, labels, index)) }
        fun lstore(index: Int): Builder = this.apply { this.innerAddOp(LStore(mv, labels, index)) }
        fun fstore(index: Int): Builder = this.apply { this.innerAddOp(FStore(mv, labels, index)) }
        fun dstore(index: Int): Builder = this.apply { this.innerAddOp(DStore(mv, labels, index)) }

        open fun build(): Builder = this.apply {
            operations.addAll(this.innerOps)
            this.innerOps.clear()
        }

        fun getParent(): MethodEmitter = this@MethodEmitter
    }

    inner class BlockBuilder(private val id: Int) : Builder()
    {
        private val blockOps: MutableList<Instruction> = mutableListOf()

        override fun innerAddOp(op: Instruction): MutableList<Instruction> = this.blockOps.also { it.add(op) }

        override fun build(): Builder = throw IllegalStateException("use build block instead!")

        fun buildBlock(): Instruction = OpBlock(mv, labels, this.id, blockOps)
    }

    private val mv: MethodVisitor = this.cw.visitMethod(
        this.accessOf(this.access),
        this.name,
        this.getDescription(),
        this.genSignature(this.signature),
        this.genExceptions(this.exceptions)
    )

    private val labels: MutableMap<Int, Label> = mutableMapOf()

    fun addOp(op: Instruction): MethodEmitter = this.apply { this.operations.add(op) }

    fun setOwnerType(type: String): MethodEmitter =
        this.apply { this.ownerType = OwnerTypeMetadata.normalize(type) }

    private fun getDescription(): String =
        "(${this.funParams.second.joinToString(separator = "") { it.getName() }})${this.funParams.first.getName()}"

    override fun generate(): Emitter = this.apply {
        OwnerTypeMetadata.write(this.mv, this.ownerType)
        this.mv.visitCode()
        this.operations.forEach { it.addOp() }
        this.mv.visitMaxs(0, 0)
        this.mv.visitEnd()
    }
}

