package com.wangdi.bctoolkit.reader

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable


@Serializable
data class ClassesEnvelope(
    val classes: List<JavaClassDto>)


@Serializable
data class JavaClassDto(
    @SerialName("package") val packagePath: List<String>,
    val access: List<String>,
    @SerialName("class") val name: String,
    @SerialName("super_class") val superClass: List<String>,
    val interfaces: List<List<String>>,
    @SerialName("static_fields") val staticField: List<JavaFieldDto>,
    @SerialName("instance_fields") val instanceField: List<JavaFieldDto>,
    @SerialName("static_methods") val staticMethod: List<JavaMethodDto>,
    @SerialName("instance_methods") val instanceMethod: List<JavaMethodDto>)


@Serializable
data class JavaFieldDto(
    val access: List<String>,
    val type: List<String>,
    val name: String,
    @SerialName("owner_type") val ownerType: String)


@Serializable
data class JavaMethodDto(
    val access: List<String>,
    @SerialName("return_type") val returnType: List<String>,
    val name: String,
    @SerialName("param_types") val params: List<List<String>>,
    @SerialName("owner_type") val ownerType: String)
