package com.wangdi.bctoolkit.generator

import com.alibaba.fastjson2.JSONArray
import com.alibaba.fastjson2.JSONObject

import com.wangdi.bctoolkit.base.Access
import com.wangdi.bctoolkit.base.Type
import com.wangdi.bctoolkit.base.TypeRef
import com.wangdi.bctoolkit.generator.artifact.operation.OpBlock
import xlang.annotation.OwnerTypeMetadata

import org.objectweb.asm.Opcodes


private typealias OpHandler = MethodEmitter.Builder.(JSONObject) -> Unit

class JsonAdapter(private val json: JSONObject)
{
    companion object
    {
        private const val JVM_TARGET_KEY = "jvm_target"         // this can be empty
        private const val CLASS_KEY = "class"
        private const val ACCESS_KEY = "access"
        private const val SIGNATURE_KEY = "signature"           // this can be empty
        private const val SUPER_CLASS_KEY = "super_class"       // this can be empty
        private const val INTERFACES_KEY = "interfaces"         // this can be empty

        private const val ATTRIBUTE_KEY = "attributes"          // this can be empty
        private const val CLINIT_KEY = "clinit"                 // this can be empty
        private const val INIT_KEY = "init"                     // this can be empty
        private const val METHODS_KEY = "methods"               // this can be empty

        private const val OP_NAME = "op_name"
        private const val FULL_NAME = "full_name"
        private const val FUNC_SIG = "func_sig"

        private const val FUN_RETURN = "func_return"
        private const val FUN_PARMS = "func_params"

        private const val BLOCK_ID = "block_id"
        private const val BLOCK_OPS = "block_ops"

        private const val ATTRIBUTE_NAME = "attr_name"
        private const val ATTRIBUTE_TYPE = "attr_type"
        private const val ATTRIBUTE_VALUE = "attr_value"

        private const val VAR_INDEX = "index"
        private const val OWNER_TYPE = "owner_type"
        private const val VALUE = "value"

        private const val METHOD_NAME = "name"
        private const val METHOD_RETURN = "return"
        private const val METHOD_PARAM_TYPES = "param_types"
        private const val METHOD_OPS = "ops"

        private const val MAIN_TYPE = "main_type"
        private const val MAIN_QNAME = "main_qname"

        /**
         * Auto-generated baseline docs for selectCtorAccess.
         * Describes the intent and behavior of this function.
         *
         * @param access parameter from function signature.
         * @return return value of this function.
         */
        private fun selectCtorAccess(access: MutableList<Access>): Access =
            access.firstOrNull { it == Access.Public || it == Access.Private || it == Access.Protected } ?: Access.Public


        inline fun <reified R> JSONObject.getOrDefault(key: String, default: R): R
        {
            val v: Any = this[key] ?: return default
            return (v as? R) ?: default
        }

        inline fun <reified R> JSONObject.getOrError(key: String): R
        {
            if (!this.containsKey(key))
                throw NoSuchElementException("Missing key: $key")

            val v: Any = get(key)

            return (v as? R) ?: throw ClassCastException(
                "Key '$key' expected ${R::class.java.name}, but was ${v::class.java.name}"
            )
        }

        inline fun <reified R : Any> JSONArray.toList(): MutableList<R> = this.map {
            val casted = (it as? R)

            if (casted != null)
                return@map casted

            throw ClassCastException("${R::class.java.name}, but was ${it::class.java.name}")
        }.toMutableList()

        /**
         * Auto-generated baseline docs for parseClassObjects.
         * Describes the intent and behavior of this function.
         *
         * @param jsonText parameter from function signature.
         * @return return value of this function.
         */
        fun parseClassObjects(jsonText: String): List<JSONObject>
        {
            val root: Any = JSONObject.parse(jsonText)

            val rootObj = root as? JSONObject
                ?: throw ClassCastException("Root JSON must be JSONObject, but was ${root::class.java.name}")

            val classesValue: Any = rootObj["classes"] ?: return listOf(rootObj)

            val classArray: JSONArray = classesValue as? JSONArray
                ?: throw ClassCastException("classes must be JSONArray, but was ${classesValue::class.java.name}")

            return classArray.map { item ->
                item as? JSONObject
                    ?: throw ClassCastException("classes item must be JSONObject, but was ${item::class.java.name}")
            }
        }

        private val ACCESS_TABLE_MAP: Map<String, Access> = mutableMapOf(
            "public" to Access.Public,
            "private" to Access.Private,
            "protected" to Access.Protected,
            "static" to Access.Static,
            "final" to Access.Final,
            "abstract" to Access.Abstract,
            "interface" to Access.Interface,
            "enum" to Access.Enum,
            "super" to Access.Super
        )

        /**
         * Auto-generated baseline docs for parseAccess.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        private fun parseAccess(value: String): Access =
            ACCESS_TABLE_MAP[value.trim().lowercase()] ?: throw IllegalArgumentException("Unknown access flag: $value")

        /**
         * Auto-generated baseline docs for parseAccessList.
         * Describes the intent and behavior of this function.
         *
         * @param json parameter from function signature.
         * @return return value of this function.
         */
        private fun parseAccessList(json: JSONObject): MutableList<Access> =
            json.getOrDefault<JSONArray>(ACCESS_KEY, JSONArray())
                .toList<String>()
                .map { parseAccess(it) }
                .toMutableList()

        private val JVM_TARGET_MAP = mapOf(
            8 to Opcodes.V1_8,
            9 to Opcodes.V9,
            10 to Opcodes.V10,
            11 to Opcodes.V11,
            12 to Opcodes.V12,
            13 to Opcodes.V13,
            14 to Opcodes.V14,
            15 to Opcodes.V15,
            16 to Opcodes.V16,
            17 to Opcodes.V17,
            18 to Opcodes.V18,
            19 to Opcodes.V19,
            20 to Opcodes.V20,
            21 to Opcodes.V21,
            22 to Opcodes.V22,
            23 to Opcodes.V23,
            24 to Opcodes.V24,
            25 to Opcodes.V25,
            26 to Opcodes.V26
        )

        /**
         * Auto-generated baseline docs for toJvmTarget.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        private fun toJvmTarget(value: Int): Int = JVM_TARGET_MAP[value] ?: value

        private val PRIMITIVE_TYPE_MAP: MutableMap<String, Type> = mutableMapOf(
            "void" to Type.VOID,
            "boolean" to Type.BOOLEAN,
            "bool" to Type.BOOLEAN,
            "char" to Type.CHAR,
            "byte" to Type.INT8,
            "i8" to Type.INT8,
            "int8" to Type.INT8,
            "short" to Type.INT16,
            "i16" to Type.INT16,
            "int16" to Type.INT16,
            "int" to Type.INT32,
            "i32" to Type.INT32,
            "int32" to Type.INT32,
            "long" to Type.INT64,
            "i64" to Type.INT64,
            "int64" to Type.INT64,
            "float" to Type.Float32,
            "f32" to Type.Float32,
            "float32" to Type.Float32,
            "double" to Type.Float64,
            "f64" to Type.Float64,
            "float64" to Type.Float64
        )

        /**
         * Auto-generated baseline docs for primitiveTypeOf.
         * Describes the intent and behavior of this function.
         *
         * @param raw parameter from function signature.
         * @return return value of this function.
         */
        private fun primitiveTypeOf(raw: String): Type? =
            PRIMITIVE_TYPE_MAP[raw.trim().lowercase()]

        /**
         * Auto-generated baseline docs for parseType.
         * Describes the intent and behavior of this function.
         *
         * @param value parameter from function signature.
         * @return return value of this function.
         */
        private fun parseType(value: JSONArray): Type
        {
            val parts: MutableList<String> = value.toList<String>()

            if (parts.isEmpty())
                throw IllegalArgumentException("Type parts must not be empty")

            val arrayDim: Int = parts.takeLastWhile { it == "[]" }.count()
            val baseParts: MutableList<String> = parts.dropLast(arrayDim).toMutableList()

            if (baseParts.isEmpty())
                throw IllegalArgumentException("Type base parts must not be empty")

            if (arrayDim == 0 && baseParts.size == 1)
            {
                val prim: Type? = primitiveTypeOf(baseParts[0])
                
                if (prim != null)
                    return prim
            }

            return Type(TypeRef(*baseParts.toTypedArray()), arrayDim)
        }

        /**
         * Auto-generated baseline docs for parseInvokeSpec.
         * Describes the intent and behavior of this function.
         *
         * @param json parameter from function signature.
         * @return return value of this function.
         */
        private fun parseInvokeSpec(json: JSONObject): Pair<MutableList<String>, Pair<Type, MutableList<Type>>> {
            val fullName: MutableList<String> = json.getJSONArray(FULL_NAME).toList<String>()
            require(fullName.isNotEmpty()) { "full_name must not be empty" }

            val functionSig: JSONObject = json.getJSONObject(FUNC_SIG)
            val returnType: Type = parseType(functionSig.getOrError(FUN_RETURN))
            val paramTypes: MutableList<Type> = functionSig
                .getOrDefault<JSONArray>(FUN_PARMS, JSONArray())
                .map { item ->
                    val arr = item as? JSONArray
                        ?: throw ClassCastException("func_parms item must be JSONArray, but was ${item::class.java.name}")
                    parseType(arr)
                }
                .toMutableList()

            return fullName to (returnType to paramTypes)
        }

        /**
         * Auto-generated baseline docs for parseFieldSpec.
         * Describes the intent and behavior of this function.
         *
         * @param json parameter from function signature.
         * @return return value of this function.
         */
        private fun parseFieldSpec(json: JSONObject): Pair<MutableList<String>, Type>
        {
            val fullName: MutableList<String> = json.getJSONArray(ATTRIBUTE_NAME).toList<String>()
            require(fullName.isNotEmpty()) { "attributeName must not be empty" }

            val typeValue: JSONArray = json.getOrError(ATTRIBUTE_TYPE)
            val type: Type = this.parseType(typeValue)

            return fullName to type
        }

        /**
         * Auto-generated baseline docs for normalizeOp.
         * Describes the intent and behavior of this function.
         *
         * @param json parameter from function signature.
         * @return return value of this function.
         */
        private fun normalizeOp(json: JSONObject): JSONObject
        {
            if (json.containsKey(OP_NAME))
                return json

            if (json.size != 1)
                throw IllegalArgumentException("op object must contain '$OP_NAME' or exactly one entry")

            val entry = json.entries.first()
            val opName = entry.key
            val payload = entry.value

            val normalized = JSONObject()
            normalized[OP_NAME] = opName
            when (payload)
            {
                is JSONObject -> payload.forEach { (k, v) -> normalized[k] = v }
                null -> Unit
                else -> normalized[VALUE] = payload
            }
            return normalized
        }

        private val OP_HANDLERS: MutableMap<String, OpHandler> = mutableMapOf(
            "iadd" to { iadd() },
            "ladd" to { ladd() },
            "fadd" to { fadd() },
            "dadd" to { dadd() },

            "isub" to { isub() },
            "lsub" to { lsub() },
            "fsub" to { fsub() },
            "dsub" to { dsub() },

            "imul" to { imul() },
            "lmul" to { lmul() },
            "fmul" to { fmul() },
            "dmul" to { dmul() },

            "idiv" to { idiv() },
            "ldiv" to { ldiv() },
            "fdiv" to { fdiv() },
            "ddiv" to { ddiv() },

            "irem" to { irem() },
            "lrem" to { lrem() },

            // no-op casts kept for compatibility with older lowering output
            "i2i" to { nop() },
            "l2l" to { nop() },
            "f2f" to { nop() },
            "d2d" to { nop() },

            "i2l" to { i2l() },
            "i2f" to { i2f() },
            "i2d" to { i2d() },

            "l2i" to { l2i() },
            "l2f" to { l2f() },
            "l2d" to { l2d() },

            "f2i" to { f2i() },
            "f2l" to { f2l() },
            "f2d" to { f2d() },

            "d2i" to { d2i() },
            "d2l" to { d2l() },
            "d2f" to { d2f() },

            "return" to { `return`() },
            "ireturn" to { ireturn() },
            "lreturn" to { lreturn() },
            "freturn" to { freturn() },
            "dreturn" to { dreturn() },

            "pop" to { pop() },
            "dup" to { dup() },
            "nop" to { nop() },

            "ineg" to { ineg() },
            "lneg" to { lneg() },
            "fneg" to { fneg() },
            "dneg" to { dneg() },

            "inot" to { inot() },
            "lnot" to { lnot() },

            "pos" to { pos() },

            "goto" to { json -> goto(json.getInteger(BLOCK_ID)) },

            "if_icmpeq" to { json -> if_icmpeq(json.getInteger(BLOCK_ID)) },
            "if_icmpne" to { json -> if_icmpne(json.getInteger(BLOCK_ID)) },
            "if_icmplt" to { json -> if_icmplt(json.getInteger(BLOCK_ID)) },
            "if_icmple" to { json -> if_icmple(json.getInteger(BLOCK_ID)) },
            "if_icmpgt" to { json -> if_icmpgt(json.getInteger(BLOCK_ID)) },
            "if_icmpge" to { json -> if_icmpge(json.getInteger(BLOCK_ID)) },

            "if_lcmpeq" to { json -> if_lcmpeq(json.getInteger(BLOCK_ID)) },
            "if_lcmpne" to { json -> if_lcmpne(json.getInteger(BLOCK_ID)) },
            "if_lcmplt" to { json -> if_lcmplt(json.getInteger(BLOCK_ID)) },
            "if_lcmple" to { json -> if_lcmple(json.getInteger(BLOCK_ID)) },
            "if_lcmpgt" to { json -> if_lcmpgt(json.getInteger(BLOCK_ID)) },
            "if_lcmpge" to { json -> if_lcmpge(json.getInteger(BLOCK_ID)) },

            "if_fcmpeq" to { json -> if_fcmpeq(json.getInteger(BLOCK_ID)) },
            "if_fcmpne" to { json -> if_fcmpne(json.getInteger(BLOCK_ID)) },
            "if_fcmplt" to { json -> if_fcmplt(json.getInteger(BLOCK_ID)) },
            "if_fcmple" to { json -> if_fcmple(json.getInteger(BLOCK_ID)) },
            "if_fcmpgt" to { json -> if_fcmpgt(json.getInteger(BLOCK_ID)) },
            "if_fcmpge" to { json -> if_fcmpge(json.getInteger(BLOCK_ID)) },

            "if_dcmpeq" to { json -> if_dcmpeq(json.getInteger(BLOCK_ID)) },
            "if_dcmpne" to { json -> if_dcmpne(json.getInteger(BLOCK_ID)) },
            "if_dcmplt" to { json -> if_dcmplt(json.getInteger(BLOCK_ID)) },
            "if_dcmple" to { json -> if_dcmple(json.getInteger(BLOCK_ID)) },
            "if_dcmpgt" to { json -> if_dcmpgt(json.getInteger(BLOCK_ID)) },
            "if_dcmpge" to { json -> if_dcmpge(json.getInteger(BLOCK_ID)) },

            "iload" to { json -> iload(json.getOrError(VAR_INDEX)) },
            "lload" to { json -> lload(json.getOrError(VAR_INDEX)) },
            "fload" to { json -> fload(json.getOrError(VAR_INDEX)) },
            "dload" to { json -> dload(json.getOrError(VAR_INDEX)) },
            "aload" to { json -> aload(json.getOrError(VAR_INDEX)) },

            "ipush" to { json -> ipush(json.getInteger(VALUE).toInt()) },
            "lpush" to { json -> lpush(json.getLong(VALUE).toLong()) },
            "fpush" to { json -> fpush(json.getFloat(VALUE).toFloat()) },
            "dpush" to { json -> dpush(json.getDouble(VALUE).toDouble()) },
            "apush" to { json -> apush(json.getOrError<String>(VALUE)) },

            "istore" to { json -> istore(json.getOrError(VAR_INDEX)) },
            "lstore" to { json -> lstore(json.getOrError(VAR_INDEX)) },
            "fstore" to { json -> fstore(json.getOrError(VAR_INDEX)) },
            "dstore" to { json -> dstore(json.getOrError(VAR_INDEX)) },
            "astore" to { json -> astore(json.getOrError(VAR_INDEX)) },

            "invokestatic" to { json ->
                val (fullName, sig) = parseInvokeSpec(json)
                invokeStatic(fullName, sig)
            },

            "invokespecial" to { json ->
                val (fullName, sig) = parseInvokeSpec(json)
                invokeSpecial(fullName, sig)
            },

            "invokevirtual" to { json ->
                val (fullName, sig) = parseInvokeSpec(json)
                invokeVirtual(fullName, sig)
            },

            "getstatic" to { json ->
                val (fullName, type) = parseFieldSpec(json)
                getstatic(fullName, type)
            },

            "putstatic" to { json ->
                val (fullName, type) = parseFieldSpec(json)
                putstatic(fullName, type)
            },

            "block" to { json ->
                val blockId: Int = json.getOrError(BLOCK_ID)
                val blockOps: JSONArray = json.getJSONArray(BLOCK_OPS)

                val blockBuilder: MethodEmitter.BlockBuilder = getParent().BlockBuilder(blockId)

                blockOps.forEach { item ->
                    val opJson = item as? JSONObject
                        ?: throw ClassCastException(
                            "block_ops item must be JSONObject, but was ${item::class.java.name}"
                        )
                    blockBuilder.addOp(opJson)
                }

                val opBlock: OpBlock = blockBuilder.buildBlock() as OpBlock
                block(opBlock)
            }
        )

        private fun MethodEmitter.Builder.addOp(json: JSONObject): MethodEmitter.Builder = this.apply {
            val opJson = normalizeOp(json)
            val opName = opJson.getString(OP_NAME)?.lowercase()
                ?: throw NoSuchElementException("Missing key: $OP_NAME")

            val handler = OP_HANDLERS[opName]
                ?: throw IllegalArgumentException("Unknown op_name: $opName")
            handler(this, opJson)
        }
    }

    constructor(json: String) : this(JSONObject.parse(json))

    /**
     * Auto-generated baseline docs for getClassEmitter.
     * Describes the intent and behavior of this function.
     *
     * @return return value of this function.
     */
    fun getClassEmitter(): ClassEmitter
    {
        val jvmTarget: Int = toJvmTarget(this.json.getOrDefault<Int>(JVM_TARGET_KEY, 8))

        val classParts: MutableList<String> = when (val raw = this.json.getOrError<Any>(CLASS_KEY)) {
            is JSONArray -> raw.toList<String>()
            is String -> mutableListOf(raw)
            else -> throw ClassCastException("Key '$CLASS_KEY' expected JSONArray or String, but was ${raw::class.java.name}")
        }
        val `class` = TypeRef(*classParts.toTypedArray())

        val access: MutableList<Access> = parseAccessList(this.json)

        val signature: MutableList<String> =
            this.json.getOrDefault<JSONArray>(SIGNATURE_KEY, JSONArray()).toList<String>()

        val superClass: TypeRef =
            TypeRef.safeCreate(this.json.getOrDefault<JSONArray>(SUPER_CLASS_KEY, JSONArray()).toList<String>())

        val interfaces: MutableList<TypeRef> = this.json
            .getOrDefault<JSONArray>(INTERFACES_KEY, JSONArray())
            .map { item ->
                val parts = (item as? JSONArray)?.toList<String>()
                    ?: throw ClassCastException("Interface entry must be JSONArray, but was ${item::class.java.name}")
                TypeRef(*parts.toTypedArray())
            }.toMutableList()

        val ce = ClassEmitter(
            jvmTarget,
            access,
            `class`,
            signature,
            superClass,
            interfaces
        )

        this.addAttribute(ce)
        this.addClinit(ce)
        this.addInit(ce)
        this.addMethods(ce)
        this.addMain(ce)
        return ce
    }

    /**
     * Auto-generated baseline docs for addAttribute.
     * Describes the intent and behavior of this function.
     *
     * @param ce parameter from function signature.
     */
    private fun addAttribute(ce: ClassEmitter)
    {
        val attrs: JSONArray = this.json.getOrDefault<JSONArray>(ATTRIBUTE_KEY, JSONArray())

        if (attrs.isEmpty())
            return

        attrs.forEach { item ->
            val attrJson = item as? JSONObject
                ?: throw ClassCastException("attributes item must be JSONObject, but was ${item::class.java.name}")

            val access: MutableList<Access> = parseAccessList(attrJson)

            val name: String = attrJson.getOrError(ATTRIBUTE_NAME)
            val type: Type = parseType(attrJson.getOrError(ATTRIBUTE_TYPE))
            val ownerType: String = OwnerTypeMetadata.normalize(attrJson.getString(OWNER_TYPE))

            val signatureRaw: String = attrJson.getString(SIGNATURE_KEY)
            val signature: String? = signatureRaw.takeIf { it.isNotBlank() }

            val value: Any? = attrJson.get(ATTRIBUTE_VALUE)

            ce.addAttribute(AttributeGenerator(ce.getCW(), access, name, type, signature, value).setOwnerType(ownerType))
        }
    }

    /**
     * Auto-generated baseline docs for addMethods.
     * Describes the intent and behavior of this function.
     *
     * @param ce parameter from function signature.
     */
    private fun addMethods(ce: ClassEmitter)
    {
        val methods: JSONArray = this.json.getJSONArray(METHODS_KEY) ?: JSONArray()

        if (methods.isEmpty())
            return

        methods.forEach { item ->
            val methodJson = item as? JSONObject
                ?: throw ClassCastException("methods item must be JSONObject, but was ${item::class.java.name}")

            val access: MutableList<Access> = parseAccessList(methodJson)
            val ownerType: String = OwnerTypeMetadata.normalize(methodJson.getString(OWNER_TYPE))
            val name: String = methodJson.getString(METHOD_NAME)
            val returnType: Type = parseType(methodJson.getOrError(METHOD_RETURN))
            val paramTypes: MutableList<Type> =
                (methodJson.getJSONArray(METHOD_PARAM_TYPES) ?: JSONArray())
                .map { item ->
                    val arr = item as? JSONArray
                        ?: throw ClassCastException("param_types item must be JSONArray, but was ${item::class.java.name}")
                    parseType(arr)
                }
                .toMutableList()

            val method = ce.newMethod(access, name, returnType to paramTypes)

            val ops: JSONArray = methodJson.getOrDefault<JSONArray>(METHOD_OPS, JSONArray())
            val builder: MethodEmitter.Builder = method.Builder()
            ops.forEach { opItem ->
                val opJson = opItem as? JSONObject
                    ?: throw ClassCastException("ops item must be JSONObject, but was ${opItem::class.java.name}")
                builder.addOp(opJson)
            }
            builder.build()
            ce.addMethod(method.setOwnerType(ownerType))
        }
    }

    /**
     * Auto-generated baseline docs for addClinit.
     * Describes the intent and behavior of this function.
     *
     * @param ce parameter from function signature.
     */
    private fun addClinit(ce: ClassEmitter)
    {
        val raw: JSONObject = this.json.getJSONObject(CLINIT_KEY) ?: return
        val ops: JSONArray = raw.getJSONArray(METHOD_OPS) ?: JSONArray()

        if (ops.isEmpty())
            return

        val clinit: ClinitEmitter = ce.newClinit()
        val builder: MethodEmitter.Builder = clinit.Builder()
        ops.forEach { opItem ->
            val opJson = opItem as? JSONObject
                ?: throw ClassCastException("clinit ops item must be JSONObject, but was ${opItem::class.java.name}")
            builder.addOp(opJson)
        }
        builder.build()
        ce.setClinit(clinit)
    }

    /**
     * Auto-generated baseline docs for addInit.
     * Describes the intent and behavior of this function.
     *
     * @param ce parameter from function signature.
     */
    private fun addInit(ce: ClassEmitter)
    {
        val raw: Any = this.json[INIT_KEY] ?: return

        val initItems: List<JSONObject> = when (raw)
        {
            is JSONArray -> raw.map { item ->
                item as? JSONObject
                    ?: throw ClassCastException("init item must be JSONObject, but was ${item::class.java.name}")
            }
            is JSONObject -> listOf(raw) // backward compatibility
            else -> throw ClassCastException("init must be JSONObject or JSONArray, but was ${raw::class.java.name}")
        }

        initItems.forEach { initJson ->
            val accessList: MutableList<Access> = parseAccessList(initJson)

            val paramTypes: MutableList<Type> = (initJson.getJSONArray(METHOD_PARAM_TYPES) ?: JSONArray())
                .map { item ->
                    val arr = item as? JSONArray
                        ?: throw ClassCastException("param_types item must be JSONArray, but was ${item::class.java.name}")
                    parseType(arr)
                }
                .toMutableList()

            val ops: JSONArray = initJson.getJSONArray(METHOD_OPS) ?: JSONArray()

            if (ops.isEmpty())
                return@forEach

            val init = ce.newInit(selectCtorAccess(accessList), paramTypes)
            val builder: MethodEmitter.Builder = init.Builder()
            ops.forEach { opItem ->
                val opJson = opItem as? JSONObject
                    ?: throw ClassCastException("init ops item must be JSONObject, but was ${opItem::class.java.name}")
                builder.addOp(opJson)
            }
            builder.build()
            ce.addInit(init)
        }
    }

    /**
     * Auto-generated baseline docs for addMain.
     * Describes the intent and behavior of this function.
     *
     * @param ce parameter from function signature.
     */
    private fun addMain(ce: ClassEmitter)
    {
        val mainType: Int = this.json.getOrDefault<Int>(MAIN_TYPE, -1)

        if (mainType == -1 || mainType == 2)
            return

        val main: MutableList<String> = this.json.getJSONArray(MAIN_QNAME).toList<String>()
        require(main.isNotEmpty()) { "$MAIN_QNAME must not be empty" }

        val stringArrayType = Type(TypeRef("java", "lang", "String"), 1)

        val mainMethod: MethodEmitter = ce.newMethod(
            mutableListOf(Access.Public, Access.Static),
            "main",
            Type.VOID to mutableListOf(stringArrayType),
            mutableListOf(),
            mutableListOf()
        )

        val builder: MethodEmitter.Builder = mainMethod.Builder()

        when (mainType)
        {
            // void main()
            0 -> {
                builder.invokeStatic(main, Type.VOID to mutableListOf())
                builder.`return`()
            }

            // int main()
            1 -> {
                builder.invokeStatic(main, Type.INT32 to mutableListOf())
                builder.invokeStatic(
                    mutableListOf("java", "lang", "System", "exit"),
                    Type.VOID to mutableListOf(Type.INT32))
                builder.`return`()
            }

            // int main(String[] args)
            3 -> {
                builder.aload(0)
                builder.invokeStatic(main, Type.INT32 to mutableListOf(stringArrayType))
                builder.invokeStatic(
                    mutableListOf("java", "lang", "System", "exit"),
                    Type.VOID to mutableListOf(Type.INT32))
                builder.`return`()
            }

            else -> throw IllegalArgumentException("unknown main method $mainType")
        }

        builder.build()
        ce.addMethod(mainMethod)
    }
}

