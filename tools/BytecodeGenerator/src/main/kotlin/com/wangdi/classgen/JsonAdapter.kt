package com.wangdi.classgen

import com.alibaba.fastjson2.JSONArray
import com.alibaba.fastjson2.JSONObject

import com.wangdi.classgen.base.Access
import com.wangdi.classgen.base.Type
import com.wangdi.classgen.base.TypeRef
import com.wangdi.classgen.artifact.operation.OpBlock
import com.wangdi.classgen.generator.AttributeGenerator
import com.wangdi.classgen.generator.ClassEmitter
import com.wangdi.classgen.generator.ClinitEmitter
import com.wangdi.classgen.generator.MethodEmitter

import org.objectweb.asm.Opcodes


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
        private const val VALUE = "value"

        private const val METHOD_NAME = "name"
        private const val METHOD_RETURN = "return"
        private const val METHOD_PARAM_TYPES = "param_types"
        private const val METHOD_OPS = "ops"

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

        private fun parseAccess(value: String): Access =
            when (value.trim().lowercase())
            {
                "public" -> Access.Public
                "private" -> Access.Private
                "protected" -> Access.Protected
                "static" -> Access.Static
                "final" -> Access.Final
                "abstract" -> Access.Abstract
                "interface" -> Access.Interface
                "enum" -> Access.Enum
                "super" -> Access.Super
                else -> throw IllegalArgumentException("Unknown access flag: $value")
            }

        private fun parseAccessList(json: JSONObject): MutableList<Access> =
            json.getOrDefault<JSONArray>(ACCESS_KEY, JSONArray())
                .toList<String>()
                .map { parseAccess(it) }
                .toMutableList()

        private fun toJvmTarget(value: Int): Int = when (value)
        {
            8 -> Opcodes.V1_8
            9 -> Opcodes.V9
            10 -> Opcodes.V10
            11 -> Opcodes.V11
            12 -> Opcodes.V12
            13 -> Opcodes.V13
            14 -> Opcodes.V14
            15 -> Opcodes.V15
            16 -> Opcodes.V16
            17 -> Opcodes.V17
            18 -> Opcodes.V18
            else -> value
        }

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

        private fun primitiveTypeOf(raw: String): Type? =
            PRIMITIVE_TYPE_MAP[raw.trim().lowercase()]

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

        private fun parseFieldSpec(json: JSONObject): Pair<MutableList<String>, Type>
        {
            val fullName: MutableList<String> = json.getJSONArray(ATTRIBUTE_NAME).toList<String>()
            require(fullName.isNotEmpty()) { "attributeName must not be empty" }

            val typeValue: JSONArray = json.getOrError(ATTRIBUTE_TYPE)
            val type: Type = this.parseType(typeValue)

            return fullName to type
        }

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

        private fun MethodEmitter.Builder.addOp(json: JSONObject): MethodEmitter.Builder = this.apply {
            val opJson = normalizeOp(json)
            val opName: String = opJson.getString(OP_NAME)
                ?: throw NoSuchElementException("Missing key: $OP_NAME")
            when (opName.lowercase())
            {
                "iadd" -> this.iadd()
                "ladd" -> this.ladd()
                "fadd" -> this.fadd()
                "dadd" -> this.dadd()

                "isub" -> this.isub()
                "lsub" -> this.lsub()
                "fsub" -> this.fsub()
                "dsub" -> this.dsub()

                "imul" -> this.imul()
                "lmul" -> this.lmul()
                "fmul" -> this.fmul()
                "dmul" -> this.dmul()

                "idiv" -> this.idiv()
                "ldiv" -> this.ldiv()
                "fdiv" -> this.fdiv()
                "ddiv" -> this.ddiv()

                "irem" -> this.irem()
                "lrem" -> this.lrem()

                "invokestatic" -> {
                    val (fullName, sig) = parseInvokeSpec(opJson)
                    this.invokeStatic(fullName, sig)
                }

                "invokespecial" -> {
                    val (fullName, sig) = parseInvokeSpec(opJson)
                    this.invokeSpecial(fullName, sig)
                }

                "invokevirtual" -> {
                    val (fullName, sig) = parseInvokeSpec(opJson)
                    this.invokeVirtual(fullName, sig)
                }

                "i2l" -> this.i2l()
                "i2f" -> this.i2f()
                "i2d" -> this.i2d()

                "l2i" -> this.l2i()
                "l2f" -> this.l2f()
                "l2d" -> this.l2d()

                "f2i" -> this.f2i()
                "f2l" -> this.f2l()
                "f2d" -> this.f2d()

                "d2i" -> this.d2i()
                "d2l" -> this.d2l()
                "d2f" -> this.d2f()

                "block" -> {
                    val blockId: Int = opJson.getOrError(BLOCK_ID)
                    val blockOps: JSONArray = opJson.getOrDefault<JSONArray>(BLOCK_OPS, JSONArray())

                    val blockBuilder: MethodEmitter.BlockBuilder = this.getParent().BlockBuilder(blockId)

                    blockOps.forEach { item ->
                        val opJson = item as? JSONObject
                            ?: throw ClassCastException("block_ops item must be JSONObject, but was ${item::class.java.name}")
                        blockBuilder.addOp(opJson)
                    }

                    val opBlock: OpBlock = blockBuilder.buildBlock() as OpBlock
                    this.block(opBlock)
                }

                "goto" -> {
                    val blockId: Int = opJson.getInteger(BLOCK_ID)
                    this.goto(blockId)
                }

                "ifeq" -> {
                    val blockId: Int = opJson.getInteger(BLOCK_ID)
                    this.ifeq(blockId)
                }

                "ifne" -> {
                    val blockId: Int = opJson.getInteger(BLOCK_ID)
                    this.ifne(blockId)
                }

                "return" -> this.`return`()
                "ireturn" -> this.ireturn()
                "lreturn" -> this.lreturn()
                "freturn" -> this.freturn()
                "dreturn" -> this.dreturn()

                "pop" -> this.pop()
                "dup" -> this.dup()
                "nop" -> this.nop()

                "getstatic" -> {
                    val (fullName, type) = parseFieldSpec(opJson)
                    this.getstatic(fullName, type)
                }

                "putstatic" -> {
                    val (fullName, type) = parseFieldSpec(opJson)
                    this.putstatic(fullName, type)
                }

                "ineg" -> this.ineg()
                "lneg" -> this.lneg()
                "fneg" -> this.fneg()
                "dneg" -> this.dneg()

                "inot" -> this.inot()
                "lnot" -> this.lnot()

                "pos" -> this.pos()

                "iload" -> this.iload(opJson.getOrError(VAR_INDEX))
                "lload" -> this.lload(opJson.getOrError(VAR_INDEX))
                "fload" -> this.fload(opJson.getOrError(VAR_INDEX))
                "dload" -> this.dload(opJson.getOrError(VAR_INDEX))
                "aload" -> this.aload(opJson.getOrError(VAR_INDEX))

                "ipush" -> this.ipush(opJson.getInteger(VALUE).toInt())
                "lpush" -> this.lpush(opJson.getLong(VALUE).toLong())
                "fpush" -> this.fpush(opJson.getFloat(VALUE).toFloat())
                "dpush" -> this.dpush(opJson.getDouble(VALUE).toDouble())

                "istore" -> this.istore(opJson.getOrError(VAR_INDEX))
                "lstore" -> this.lstore(opJson.getOrError(VAR_INDEX))
                "fstore" -> this.fstore(opJson.getOrError(VAR_INDEX))
                "dstore" -> this.dstore(opJson.getOrError(VAR_INDEX))
            }
        }
    }

    constructor(json: String) : this(JSONObject.parse(json))

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
        return ce
    }

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

            val signatureRaw: String = attrJson.getString(SIGNATURE_KEY)
            val signature: String? = signatureRaw.takeIf { it.isNotBlank() }

            val value: Any? = attrJson.get(ATTRIBUTE_VALUE)

            ce.addAttribute(AttributeGenerator(ce.getCW(), access, name, type, signature, value))
        }
    }

    private fun addMethods(ce: ClassEmitter)
    {
        val methods: JSONArray = this.json.getJSONArray(METHODS_KEY) ?: JSONArray()

        if (methods.isEmpty())
            return

        methods.forEach { item ->
            val methodJson = item as? JSONObject
                ?: throw ClassCastException("methods item must be JSONObject, but was ${item::class.java.name}")

            val access: MutableList<Access> = parseAccessList(methodJson)
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
            ce.addMethod(method)
        }
    }

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
}
