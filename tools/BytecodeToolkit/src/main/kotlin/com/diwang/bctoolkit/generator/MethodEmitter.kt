package com.diwang.bctoolkit.generator

import com.wangdi.bctoolkit.generator.artifact.Emitter
import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import com.wangdi.bctoolkit.generator.artifact.operation.*
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

        /**
         * Auto-generated baseline docs for innerAddOp.
         * Describes the intent and behavior of this function.
         *
         * @param op parameter from function signature.
         * @return return value of this function.
         */
        protected open fun innerAddOp(op: Instruction): MutableList<Instruction> =
            this.innerOps.also { it.add(op) }

        /**
         * Auto-generated baseline docs for iadd.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun iadd(): Builder = this.apply { this.innerAddOp(IAdd(mv, labels)) }
        /**
         * Auto-generated baseline docs for ladd.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ladd(): Builder = this.apply { this.innerAddOp(LAdd(mv, labels)) }
        /**
         * Auto-generated baseline docs for fadd.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun fadd(): Builder = this.apply { this.innerAddOp(FAdd(mv, labels)) }
        /**
         * Auto-generated baseline docs for dadd.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun dadd(): Builder = this.apply { this.innerAddOp(DAdd(mv, labels)) }

        /**
         * Auto-generated baseline docs for isub.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun isub(): Builder = this.apply { this.innerAddOp(ISub(mv, labels)) }
        /**
         * Auto-generated baseline docs for lsub.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lsub(): Builder = this.apply { this.innerAddOp(LSub(mv, labels)) }
        /**
         * Auto-generated baseline docs for fsub.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun fsub(): Builder = this.apply { this.innerAddOp(FSub(mv, labels)) }
        /**
         * Auto-generated baseline docs for dsub.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun dsub(): Builder = this.apply { this.innerAddOp(DSub(mv, labels)) }

        /**
         * Auto-generated baseline docs for imul.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun imul(): Builder = this.apply { this.innerAddOp(IMul(mv, labels)) }
        /**
         * Auto-generated baseline docs for lmul.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lmul(): Builder = this.apply { this.innerAddOp(LMul(mv, labels)) }
        /**
         * Auto-generated baseline docs for fmul.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun fmul(): Builder = this.apply { this.innerAddOp(FMul(mv, labels)) }
        /**
         * Auto-generated baseline docs for dmul.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun dmul(): Builder = this.apply { this.innerAddOp(DMul(mv, labels)) }

        /**
         * Auto-generated baseline docs for idiv.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun idiv(): Builder = this.apply { this.innerAddOp(IDiv(mv, labels)) }
        /**
         * Auto-generated baseline docs for ldiv.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ldiv(): Builder = this.apply { this.innerAddOp(LDiv(mv, labels)) }
        /**
         * Auto-generated baseline docs for fdiv.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun fdiv(): Builder = this.apply { this.innerAddOp(FDiv(mv, labels)) }
        /**
         * Auto-generated baseline docs for ddiv.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ddiv(): Builder = this.apply { this.innerAddOp(DDiv(mv, labels)) }

        /**
         * Auto-generated baseline docs for irem.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun irem(): Builder = this.apply { this.innerAddOp(IRem(mv, labels)) }
        /**
         * Auto-generated baseline docs for lrem.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lrem(): Builder = this.apply { this.innerAddOp(LRem(mv, labels)) }

        /**
         * Auto-generated baseline docs for iand.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun iand(): Builder = this.apply { this.innerAddOp(IAnd(mv, labels)) }
        /**
         * Auto-generated baseline docs for ior.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ior(): Builder = this.apply { this.innerAddOp(IOr(mv, labels)) }
        /**
         * Auto-generated baseline docs for ixor.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ixor(): Builder = this.apply { this.innerAddOp(IXor(mv, labels)) }
        /**
         * Auto-generated baseline docs for land.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun land(): Builder = this.apply { this.innerAddOp(LAnd(mv, labels)) }
        /**
         * Auto-generated baseline docs for lor.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lor(): Builder = this.apply { this.innerAddOp(LOr(mv, labels)) }
        /**
         * Auto-generated baseline docs for lxor.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lxor(): Builder = this.apply { this.innerAddOp(LXor(mv, labels)) }
        /**
         * Auto-generated baseline docs for ishl.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ishl(): Builder = this.apply { this.innerAddOp(IShl(mv, labels)) }
        /**
         * Auto-generated baseline docs for ishr.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ishr(): Builder = this.apply { this.innerAddOp(IShr(mv, labels)) }
        /**
         * Auto-generated baseline docs for iushr.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun iushr(): Builder = this.apply { this.innerAddOp(IUShr(mv, labels)) }
        /**
         * Auto-generated baseline docs for lshl.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lshl(): Builder = this.apply { this.innerAddOp(LShl(mv, labels)) }
        /**
         * Auto-generated baseline docs for lshr.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lshr(): Builder = this.apply { this.innerAddOp(LShr(mv, labels)) }
        /**
         * Auto-generated baseline docs for lushr.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lushr(): Builder = this.apply { this.innerAddOp(LUShr(mv, labels)) }

        /**
         * Auto-generated baseline docs for invokeStatic.
         * Describes the intent and behavior of this function.
         *
         * @param fullName parameter from function signature.
         * @param signature parameter from function signature.
         * @return return value of this function.
         */
        fun invokeStatic(fullName: MutableList<String>, signature: Pair<Type, MutableList<Type>>): Builder =
            this.apply { this.innerAddOp(InvokeStatic(mv, labels, fullName, signature)) }

        /**
         * Auto-generated baseline docs for invokeSpecial.
         * Describes the intent and behavior of this function.
         *
         * @param fullName parameter from function signature.
         * @param signature parameter from function signature.
         * @return return value of this function.
         */
        fun invokeSpecial(fullName: MutableList<String>, signature: Pair<Type, MutableList<Type>>): Builder =
            this.apply { this.innerAddOp(InvokeSpecial(mv, labels, fullName, signature)) }

        /**
         * Auto-generated baseline docs for invokeVirtual.
         * Describes the intent and behavior of this function.
         *
         * @param fullName parameter from function signature.
         * @param signature parameter from function signature.
         * @return return value of this function.
         */
        fun invokeVirtual(fullName: MutableList<String>, signature: Pair<Type, MutableList<Type>>): Builder =
            this.apply { this.innerAddOp(InvokeVirtual(mv, labels, fullName, signature)) }

        /**
         * Auto-generated baseline docs for i2l.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun i2l(): Builder = this.apply { this.innerAddOp(I2L(mv, labels)) }
        /**
         * Auto-generated baseline docs for i2f.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun i2f(): Builder = this.apply { this.innerAddOp(I2F(mv, labels)) }
        /**
         * Auto-generated baseline docs for i2d.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun i2d(): Builder = this.apply { this.innerAddOp(I2D(mv, labels)) }

        /**
         * Auto-generated baseline docs for l2i.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun l2i(): Builder = this.apply { this.innerAddOp(L2I(mv, labels)) }
        /**
         * Auto-generated baseline docs for l2f.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun l2f(): Builder = this.apply { this.innerAddOp(L2F(mv, labels)) }
        /**
         * Auto-generated baseline docs for l2d.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun l2d(): Builder = this.apply { this.innerAddOp(L2D(mv, labels)) }

        /**
         * Auto-generated baseline docs for f2i.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun f2i(): Builder = this.apply { this.innerAddOp(F2I(mv, labels)) }
        /**
         * Auto-generated baseline docs for f2l.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun f2l(): Builder = this.apply { this.innerAddOp(F2L(mv, labels)) }
        /**
         * Auto-generated baseline docs for f2d.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun f2d(): Builder = this.apply { this.innerAddOp(F2D(mv, labels)) }

        /**
         * Auto-generated baseline docs for d2i.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun d2i(): Builder = this.apply { this.innerAddOp(D2I(mv, labels)) }
        /**
         * Auto-generated baseline docs for d2l.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun d2l(): Builder = this.apply { this.innerAddOp(D2L(mv, labels)) }
        /**
         * Auto-generated baseline docs for d2f.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun d2f(): Builder = this.apply { this.innerAddOp(D2F(mv, labels)) }

        /**
         * Auto-generated baseline docs for block.
         * Describes the intent and behavior of this function.
         *
         * @param op parameter from function signature.
         * @return return value of this function.
         */
        fun block(op: OpBlock): Builder = this.apply { this.innerAddOp(op) }

        /**
         * Auto-generated baseline docs for goto.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun goto(blockId: Int): Builder = this.apply { this.innerAddOp(Goto(mv, labels, blockId)) }

        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_icmpeq.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_icmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_icmpne.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_icmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_icmplt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_icmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_icmple.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_icmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_icmpgt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_icmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_icmpge.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_icmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_icmpge(mv, labels, blockId)) }

        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_lcmpeq.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_lcmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_lcmpne.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_lcmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_lcmplt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_lcmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_lcmple.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_lcmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_lcmpgt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_lcmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_lcmpge.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_lcmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_lcmpge(mv, labels, blockId)) }

        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_fcmpeq.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_fcmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_fcmpne.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_fcmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_fcmplt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_fcmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_fcmple.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_fcmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_fcmpgt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_fcmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_fcmpge.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_fcmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_fcmpge(mv, labels, blockId)) }

        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_dcmpeq.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_dcmpeq(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpeq(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_dcmpne.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_dcmpne(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpne(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_dcmplt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_dcmplt(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmplt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_dcmple.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_dcmple(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmple(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_dcmpgt.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_dcmpgt(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpgt(mv, labels, blockId)) }
        @Suppress("FunctionName")
        /**
         * Auto-generated baseline docs for if_dcmpge.
         * Describes the intent and behavior of this function.
         *
         * @param blockId parameter from function signature.
         * @return return value of this function.
         */
        fun if_dcmpge(blockId: Int): Builder = this.apply { this.innerAddOp(If_dcmpge(mv, labels, blockId)) }


        /**
         * Auto-generated baseline docs for `return`.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun `return`(): Builder = this.apply { this.innerAddOp(Return(mv, labels)) }
        /**
         * Auto-generated baseline docs for ireturn.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ireturn(): Builder = this.apply { this.innerAddOp(IReturn(mv, labels)) }
        /**
         * Auto-generated baseline docs for lreturn.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lreturn(): Builder = this.apply { this.innerAddOp(LReturn(mv, labels)) }
        /**
         * Auto-generated baseline docs for freturn.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun freturn(): Builder = this.apply { this.innerAddOp(FReturn(mv, labels)) }
        /**
         * Auto-generated baseline docs for dreturn.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun dreturn(): Builder = this.apply { this.innerAddOp(DReturn(mv, labels)) }

        /**
         * Auto-generated baseline docs for pop.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun pop(): Builder = this.apply { this.innerAddOp(Pop(mv, labels)) }
        /**
         * Auto-generated baseline docs for dup.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun dup(): Builder = this.apply { this.innerAddOp(Dup(mv, labels)) }
        /**
         * Auto-generated baseline docs for nop.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun nop(): Builder = this.apply { this.innerAddOp(Nop(mv, labels)) }

        /**
         * Auto-generated baseline docs for getstatic.
         * Describes the intent and behavior of this function.
         *
         * @param name parameter from function signature.
         * @param type parameter from function signature.
         * @return return value of this function.
         */
        fun getstatic(name: MutableList<String>, type: Type): Builder =
            this.apply { this.innerAddOp(GetStatic(mv, labels, name, type)) }

        /**
         * Auto-generated baseline docs for putstatic.
         * Describes the intent and behavior of this function.
         *
         * @param name parameter from function signature.
         * @param type parameter from function signature.
         * @return return value of this function.
         */
        fun putstatic(name: MutableList<String>, type: Type): Builder =
            this.apply { this.innerAddOp(PutStatic(mv, labels, name, type)) }

        /**
         * Auto-generated baseline docs for ineg.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun ineg(): Builder = this.apply { this.innerAddOp(INeg(mv, labels)) }
        /**
         * Auto-generated baseline docs for lneg.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun lneg(): Builder = this.apply { this.innerAddOp(LNeg(mv, labels)) }
        /**
         * Auto-generated baseline docs for fneg.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun fneg(): Builder = this.apply { this.innerAddOp(FNeg(mv, labels)) }
        /**
         * Auto-generated baseline docs for dneg.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun dneg(): Builder = this.apply { this.innerAddOp(DNeg(mv, labels)) }

        /**
         * Auto-generated baseline docs for pos.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun pos(): Builder = this.apply { this.innerAddOp(UPlus(mv, labels)) }

        /**
         * Auto-generated baseline docs for iload.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun iload(index: Int): Builder = this.apply { this.innerAddOp(ILoad(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for lload.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun lload(index: Int): Builder = this.apply { this.innerAddOp(LLoad(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for fload.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun fload(index: Int): Builder = this.apply { this.innerAddOp(FLoad(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for dload.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun dload(index: Int): Builder = this.apply { this.innerAddOp(DLoad(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for aload.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun aload(index: Int): Builder = this.apply { this.innerAddOp(ALoad(mv, labels, index)) }

        /**
         * Auto-generated baseline docs for ipush.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        fun ipush(value: Int): Builder = this.apply { this.innerAddOp(IPush(mv, labels, value)) }
        /**
         * Auto-generated baseline docs for lpush.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        fun lpush(value: Long): Builder = this.apply { this.innerAddOp(LPush(mv, labels, value)) }
        /**
         * Auto-generated baseline docs for fpush.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        fun fpush(value: Float): Builder = this.apply { this.innerAddOp(FPush(mv, labels, value)) }
        /**
         * Auto-generated baseline docs for dpush.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        fun dpush(value: Double): Builder = this.apply { this.innerAddOp(DPush(mv, labels, value)) }
        /**
         * Auto-generated baseline docs for apush.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        fun apush(value: String): Builder = this.apply { this.innerAddOp(APush(mv, labels, value)) }

        /**
         * Auto-generated baseline docs for istore.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun istore(index: Int): Builder = this.apply { this.innerAddOp(IStore(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for lstore.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun lstore(index: Int): Builder = this.apply { this.innerAddOp(LStore(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for fstore.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun fstore(index: Int): Builder = this.apply { this.innerAddOp(FStore(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for dstore.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun dstore(index: Int): Builder = this.apply { this.innerAddOp(DStore(mv, labels, index)) }
        /**
         * Auto-generated baseline docs for astore.
         * Describes the intent and behavior of this function.
         *
         * @param index parameter from function signature.
         * @return return value of this function.
         */
        fun astore(index: Int): Builder = this.apply { this.innerAddOp(AStore(mv, labels, index)) }

        /**
         * Auto-generated baseline docs for build.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        open fun build(): Builder = this.apply {
            operations.addAll(this.innerOps)
            this.innerOps.clear()
        }

        /**
         * Auto-generated baseline docs for getParent.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        fun getParent(): MethodEmitter = this@MethodEmitter
    }

    inner class BlockBuilder(private val id: Int) : Builder()
    {
        private val blockOps: MutableList<Instruction> = mutableListOf()

        /**
         * Auto-generated baseline docs for innerAddOp.
         * Describes the intent and behavior of this function.
         *
         * @param op parameter from function signature.
         * @return return value of this function.
         */
        override fun innerAddOp(op: Instruction): MutableList<Instruction> = this.blockOps.also { it.add(op) }

        /**
         * Auto-generated baseline docs for build.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
        override fun build(): Builder = throw IllegalStateException("use build block instead!")

        /**
         * Auto-generated baseline docs for buildBlock.
         * Describes the intent and behavior of this function.
         *
         * @return return value of this function.
         */
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

    /**
     * Auto-generated baseline docs for addOp.
     * Describes the intent and behavior of this function.
     *
     * @param op parameter from function signature.
     * @return return value of this function.
     */
    fun addOp(op: Instruction): MethodEmitter = this.apply { this.operations.add(op) }

    /**
     * Auto-generated baseline docs for setOwnerType.
     * Describes the intent and behavior of this function.
     *
     * @param type parameter from function signature.
     * @return return value of this function.
     */
    fun setOwnerType(type: String): MethodEmitter =
        this.apply { this.ownerType = OwnerTypeMetadata.normalize(type) }

    /**
     * Auto-generated baseline docs for getDescription.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    private fun getDescription(): String =
        "(${this.funParams.second.joinToString(separator = "") { it.getName() }})${this.funParams.first.getName()}"

    /**
     * Auto-generated baseline docs for generate.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    override fun generate(): Emitter = this.apply {
        OwnerTypeMetadata.write(this.mv, this.ownerType)
        this.mv.visitCode()
        this.operations.forEach { it.addOp() }
        this.mv.visitMaxs(0, 0)
        this.mv.visitEnd()
    }
}


