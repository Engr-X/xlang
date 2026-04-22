package com.wangdi.bctoolkit

import com.wangdi.bctoolkit.generator.JsonAdapter
import java.nio.file.Files
import java.nio.file.Path
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue

class JsonAdapterTest
{
    @Test
    /**
     * Auto-generated baseline docs for parseMinimalJsonAndSave.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun parseMinimalJsonAndSave()
    {
        val json: String = """
            {
                "class": ["TestA"],
                "methods": [
                    {
                        "access": ["public", "static"],
                        "name": "add1",
                        "return": ["int"],
                        "param_types": [["int"]],
                        "ops": [
                            { "op_name": "iload", "index": 0 },
                            { "op_name": "ipush", "value": 1 },
                            { "op_name": "iadd" },
                            { "op_name": "ireturn" }
                        ]
                    }
                ]
            }
        """.trimIndent()

        val adapter = JsonAdapter(json)
        val emitter: com.wangdi.bctoolkit.generator.ClassEmitter = adapter.getClassEmitter()
        val outDir: Path = Files.createTempDirectory("bcg-test-a")
        emitter.save(outDir)

        assertTrue(Files.exists(outDir.resolve("TestA.class")))
    }

    @Test
    /**
     * Auto-generated baseline docs for parseCompactMaskJsonAndSave.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun parseCompactMaskJsonAndSave()
    {
        val json: String = """
            {
                "class": ["TestCompact"],
                "access": 1,
                "methods": [
                    {
                        "access": 9,
                        "owner_type": 1,
                        "name": "add1",
                        "return": "i",
                        "param_types": ["i"],
                        "ops": [
                            { "op_name": "iload", "index": 0 },
                            { "op_name": "ipush", "value": 1 },
                            { "op_name": "iadd" },
                            { "op_name": "ireturn" }
                        ]
                    }
                ]
            }
        """.trimIndent()

        val adapter = JsonAdapter(json)
        val emitter: com.wangdi.bctoolkit.generator.ClassEmitter = adapter.getClassEmitter()
        val outDir: Path = Files.createTempDirectory("bcg-test-compact")
        emitter.save(outDir)

        assertTrue(Files.exists(outDir.resolve("TestCompact.class")))
    }

    @Test
    /**
     * Auto-generated baseline docs for parseShortOpFormatAndArrayParams.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun parseShortOpFormatAndArrayParams()
    {
        val json: String = """
            {
                "class": ["TestB"],
                "init": {
                    "access": ["public"],
                    "param_types": [],
                    "ops": [
                        { "op_name": "aload", "index": 0 },
                        {
                            "op_name": "invokespecial",
                            "full_name": ["java", "lang", "Object", "<init>"],
                            "func_sig": { "func_return": ["void"], "func_parms": [] }
                        },
                        { "op_name": "return" }
                    ]
                },
                "methods": [
                    {
                        "access": ["public", "static"],
                        "name": "main",
                        "return": ["void"],
                        "param_types": [["java", "lang", "String", "[]"]],
                        "ops": [
                            { "ipush": { "value": 1 } },
                            { "return": {} }
                        ]
                    }
                ]
            }
        """.trimIndent()

        val adapter = JsonAdapter(json)
        val emitter: com.wangdi.bctoolkit.generator.ClassEmitter = adapter.getClassEmitter()
        val outDir: Path = Files.createTempDirectory("bcg-test-b")
        emitter.save(outDir)

        assertTrue(Files.exists(outDir.resolve("TestB.class")))
    }

    @Test
    /**
     * Auto-generated baseline docs for unknownOpThrowsReadableError.
     * Describes the intent and behavior of this function.
     * @param Parameters are described by the function signature.`r`n * @return Return value follows the function signature.
     */
    fun unknownOpThrowsReadableError()
    {
        val json: String = """
            {
                "class": ["TestVStore"],
                "methods": [
                    {
                        "access": ["public", "static"],
                        "name": "callee",
                        "return": ["void"],
                        "param_types": [["int"]],
                        "ops": [
                            { "op_name": "return" }
                        ]
                    },
                    {
                        "access": ["public", "static"],
                        "name": "caller",
                        "return": ["void"],
                        "param_types": [],
                        "ops": [
                            { "op_name": "ipush", "value": 1 },
                            {
                                "op_name": "invokestatic",
                                "full_name": ["TestVStore", "callee"],
                                "func_sig": { "func_return": ["void"], "func_params": [["int"]] }
                            },
                            { "op_name": "vstore", "index": 0 },
                            { "op_name": "return" }
                        ]
                    }
                ]
            }
        """.trimIndent()

        val error = assertFailsWith<IllegalArgumentException> {
            JsonAdapter(json).getClassEmitter()
        }
        assertTrue(error.message?.contains("Unknown op_name: vstore") == true)
    }
}

