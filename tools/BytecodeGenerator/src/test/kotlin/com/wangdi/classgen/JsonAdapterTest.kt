package com.wangdi.classgen

import java.nio.file.Files
import java.nio.file.Path
import kotlin.test.Test
import kotlin.test.assertTrue

class JsonAdapterTest
{
    @Test
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
        val emitter: com.wangdi.classgen.generator.ClassEmitter = adapter.getClassEmitter()
        val outDir: Path = Files.createTempDirectory("bcg-test-a")
        emitter.save(outDir)

        assertTrue(Files.exists(outDir.resolve("TestA.class")))
    }

    @Test
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
        val emitter: com.wangdi.classgen.generator.ClassEmitter = adapter.getClassEmitter()
        val outDir: Path = Files.createTempDirectory("bcg-test-b")
        emitter.save(outDir)

        assertTrue(Files.exists(outDir.resolve("TestB.class")))
    }
}
