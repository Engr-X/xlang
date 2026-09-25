/*
 * Copyright (c) 2026 Di Wang
 * SPDX-License-Identifier: MIT
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package xlang.compiler.parser.program

import xlang.lexer.Token
import xlang.util.ArrayList
import xlang.util.HashSet
import xlang.util.string.StringBuilder


/**
 * Represents a member declaration contained by a program-level or structured
 * declaration.
 *
 * <p>A {@code Member} acts as a tagged wrapper around one of the concrete member
 * node types supported by the compiler. The {@code kind} field identifies the
 * actual type referenced by the untyped {@code host} pointer.
 *
 * <p>The currently supported member types are fields, functions, structure
 * constructors, and nested structure declarations.
 *
 * <p>Factory methods are provided for each supported member kind so callers do
 * not need to manually construct the wrapper with a matching kind value.
 *
 * <p>Operations such as {@code getAllTokens()} and {@code toString()} dispatch
 * to the concrete member implementation according to {@code kind}.
 */
struct Member
{
    /**
     * Identifies a member whose host object is a {@code Field}.
     *
     * <p>When a member has this kind, {@code host} is expected to contain a
     * pointer to a {@code Field} instance.
     */
    static val FIELD_TYPE: int = 1


    /**
     * Identifies a member whose host object is a {@code Function}.
     *
     * <p>When a member has this kind, {@code host} is expected to contain a
     * pointer to a {@code Function} instance.
     */
    static val FUNCTION_TYPE: int = 2

    /**
     * Identifies a member whose host object is a {@code StructConstructor}.
     *
     * <p>When a member has this kind, {@code host} is expected to contain a
     * pointer to a {@code StructConstructor} instance.
     */
    static val STRUCT_CONSTRUCTOR_TYPE: int = 3

    /**
     * Identifies a member whose host object is a nested {@code Struct}
     * declaration.
     *
     * <p>When a member has this kind, {@code host} is expected to contain a
     * pointer to a {@code Struct} instance.
     */
    static val STRUCT_TYPE: int = 4


    /**
     * Creates a member wrapper for a field declaration.
     *
     * <p>The supplied field pointer is stored by reference and is not copied or
     * cloned. The resulting member is tagged with {@code FIELD_TYPE}.
     *
     * <p>If {@code field} is {@code null}, the resulting member still retains
     * the field kind but contains a null host pointer.
     *
     * @param field			    a pointer to the field declaration to wrap, or
     * 					        {@code null} if no field object is available
     *
     * @return				    a newly created field member
     */
    static fun fromField(field: pointer<Field>) -> pointer<Member> =
        new Member(FIELD_TYPE, field)


    /**
     * Creates a member wrapper for a function declaration.
     *
     * <p>The supplied function pointer is stored by reference and is not copied
     * or cloned. The resulting member is tagged with {@code FUNCTION_TYPE}.
     *
     * <p>If {@code function} is {@code null}, the resulting member still retains
     * the function kind but contains a null host pointer.
     *
     * @param function			a pointer to the function declaration to wrap,
     * 					        or {@code null} if no function object is available
     *
     * @return				    a newly created function member
     */
    static fun fromFunction(function: pointer<Function>) -> pointer<Member> =
        new Member(FUNCTION_TYPE, function)


    /**
     * Creates a member wrapper for a structure constructor.
     *
     * <p>The supplied constructor pointer is stored by reference and is not
     * copied or cloned. The resulting member is tagged with
     * {@code STRUCT_CONSTRUCTOR_TYPE}.
     *
     * <p>If {@code structConstructor} is {@code null}, the resulting member
     * still retains the structure-constructor kind but contains a null host
     * pointer.
     *
     * @param structConstructor a pointer to the structure constructor to
     * 					        wrap, or {@code null} if no constructor object
     * 					        is available
     *
     * @return				    a newly created structure-constructor member
     */
    static fun fromStructConstructor(structConstructor: pointer<StructConstructor>) -> pointer<Member> =
        new Member(STRUCT_CONSTRUCTOR_TYPE, structConstructor)


    /**
     * Creates a member wrapper for a nested structure declaration.
     *
     * <p>The supplied structure pointer is stored by reference and is not copied
     * or cloned. The resulting member is tagged with {@code STRUCT_TYPE}.
     *
     * <p>If {@code structDecl} is {@code null}, the resulting member still
     * retains the structure kind but contains a null host pointer.
     *
     * @param structDecl	    a pointer to the structure declaration to
     * 					        wrap, or {@code null} if no structure object is
     * 					        available
     *
     * @return				a newly created structure member
     */
    static fun fromStruct(structDecl: pointer<Struct>) -> pointer<Member> =
        new Member(STRUCT_TYPE, structDecl)


    /**
     * The kind identifier describing the concrete member represented by this
     * wrapper.
     *
     * <p>The value determines how {@code host} must be interpreted before it is
     * cast to a concrete member type.
     */
    private var kind: int

    /**
     * A pointer to the concrete AST node represented by this member.
     *
     * <p>The pointer is intentionally untyped because different member kinds
     * reference different structures.
     *
     * <p>The actual pointer type must be determined from {@code kind} before the
     * value is cast or dereferenced.
     */
    private var host: pointer<*>


    /**
     * Creates a member wrapper using the specified kind and host object.
     *
     * <p>The kind and host pointer are stored directly. The host object is not
     * copied or cloned.
     *
     * <p>This constructor does not verify that {@code host} matches the concrete
     * type implied by {@code kind}. Callers are therefore responsible for
     * maintaining the relationship between the tag and the stored object.
     *
     * <p>An invalid kind can still be stored. Operations such as
     * {@code getAllTokens()} and {@code toString()} handle unrecognized kinds by
     * returning empty results.
     *
     * @param kind			    the kind identifier describing the member type
     * @param host			    a pointer to the concrete member object, or
     * 					        {@code null} if no host object is available
     */
    constructor(kind: int, host: pointer<*>)
    {
        this.kind = kind
        this.host = host
    }


    /**
     * Returns whether this member represents a field declaration.
     *
     * <p>The result is determined only from the stored kind identifier. The host
     * pointer itself is not inspected or validated.
     *
     * @return				    {@code true} if {@code kind} is
     * 					        {@code FIELD_TYPE}; {@code false} otherwise
     */
    fun isField() -> bool = this.kind == FIELD_TYPE


    /**
     * Returns whether this member represents a function declaration.
     *
     * <p>The result is determined only from the stored kind identifier. The host
     * pointer itself is not inspected or validated.
     *
     * @return				    {@code true} if {@code kind} is
     * 					        {@code FUNCTION_TYPE}; {@code false} otherwise
     */
    fun isFunction() -> bool = this.kind == FUNCTION_TYPE


    /**
     * Returns whether this member represents a structure constructor.
     *
     * <p>The result is determined only from the stored kind identifier. The host
     * pointer itself is not inspected or validated.
     *
     * @return				    {@code true} if {@code kind} is
     * 					        {@code STRUCT_CONSTRUCTOR_TYPE};
     * 					        {@code false} otherwise
     */
    fun isStructConstructor() -> bool = this.kind == STRUCT_CONSTRUCTOR_TYPE


    /**
     * Returns whether this member represents a nested structure declaration.
     *
     * <p>The result is determined only from the stored kind identifier. The host
     * pointer itself is not inspected or validated.
     *
     * @return				    {@code true} if {@code kind} is
     * 					        {@code STRUCT_TYPE}; {@code false} otherwise
     */
    fun isStruct() -> bool = this.kind == STRUCT_TYPE


    /**
     * Returns the kind identifier of this member.
     *
     * <p>The returned value can be used to determine the concrete type stored by
     * {@code getHost()}.
     *
     * <p>Known values include {@code FIELD_TYPE}, {@code FUNCTION_TYPE},
     * {@code STRUCT_CONSTRUCTOR_TYPE}, and {@code STRUCT_TYPE}.
     *
     * @return				    the kind identifier of this member
     */
    fun getKind() -> int = this.kind


    /**
     * Returns the concrete AST node wrapped by this member.
     *
     * <p>The returned pointer refers directly to the internally stored host and
     * is not copied or cloned.
     *
     * <p>The pointer is untyped. Callers should inspect {@code getKind()} or use
     * one of the {@code is...()} methods before casting it to a concrete member
     * type.
     *
     * <p>The result may be {@code null} if this member was constructed without a
     * valid host object.
     *
     * @return				    a pointer to the internally stored member object,
     * 					        or {@code null} if no host is available
     */
    fun getHost() -> pointer<*> = this.host


    /**
     * Returns all tokens associated with the concrete member represented by this
     * wrapper.
     *
     * <p>If {@code host} is {@code null}, a newly allocated empty token list is
     * returned because no concrete AST node is available from which tokens can
     * be collected.
     *
     * <p>For {@code FIELD_TYPE}, the host pointer is interpreted as a
     * {@code Field} and token collection is delegated to
     * {@code Field.getAllTokens()}.
     *
     * <p>For {@code FUNCTION_TYPE}, the host pointer is interpreted as a
     * {@code Function} and token collection is delegated to
     * {@code Function.getAllTokens()}.
     *
     * <p>For {@code STRUCT_CONSTRUCTOR_TYPE}, the host pointer is interpreted as
     * a {@code StructConstructor} and token collection is delegated to
     * {@code StructConstructor.getAllTokens()}.
     *
     * <p>For {@code STRUCT_TYPE}, the host pointer is interpreted as a
     * {@code Struct} and token collection is delegated to
     * {@code Struct.getAllTokens()}.
     *
     * <p>If the stored kind is not recognized, a newly allocated empty token
     * list is returned instead of attempting to interpret the host pointer.
     *
     * <p>For recognized kinds, the token collection returned by the underlying
     * AST node is returned directly. This method does not create an additional
     * copy or perform another sorting pass.
     *
     * @return				    the token collection produced by the wrapped member,
     * 					        or a newly allocated empty token list if the host is
     * 					        {@code null} or the kind is not recognized
     */
    fun getAllTokens() -> pointer<ArrayList> =
        if this.host == null:
            new ArrayList(sizeof(Token))
        elif this.kind == FIELD_TYPE:
        {
            val field: pointer<Field> = this.host as pointer<Field>
            field.getAllTokens()
        }
        elif this.kind == FUNCTION_TYPE:
        {
            val function: pointer<Function> = this.host as pointer<Function>
            function.getAllTokens()
        }
        elif this.kind == STRUCT_CONSTRUCTOR_TYPE:
        {
            val structConstructor: pointer<StructConstructor> = this.host as pointer<StructConstructor>
            structConstructor.getAllTokens()
        }
        elif this.kind == STRUCT_TYPE:
        {
            val structDecl: pointer<Struct> = this.host as pointer<Struct>
            structDecl.getAllTokens()
        }
        else:
            new ArrayList(sizeof(Token))


    /**
     * Returns the textual representation of the concrete member represented by
     * this wrapper.
     *
     * <p>If {@code host} is {@code null}, a newly allocated empty
     * {@code StringBuilder} is returned.
     *
     * <p>For {@code FIELD_TYPE}, the host pointer is interpreted as a
     * {@code Field} and textual representation is delegated to
     * {@code Field.toString()}.
     *
     * <p>For {@code FUNCTION_TYPE}, the host pointer is interpreted as a
     * {@code Function} and textual representation is delegated to
     * {@code Function.toString()}.
     *
     * <p>For {@code STRUCT_CONSTRUCTOR_TYPE}, the host pointer is interpreted as
     * a {@code StructConstructor} and textual representation is delegated to
     * {@code StructConstructor.toString()}.
     *
     * <p>For {@code STRUCT_TYPE}, the host pointer is interpreted as a
     * {@code Struct} and textual representation is delegated to
     * {@code Struct.toString()}.
     *
     * <p>If the stored kind is not recognized, a newly allocated empty
     * {@code StringBuilder} is returned instead of attempting an unsafe cast.
     *
     * <p>For recognized kinds, the builder returned by the concrete AST node is
     * returned directly. This wrapper does not add additional text around the
     * member representation.
     *
     * @return				    a pointer to the textual representation produced by
     * 					        the wrapped member, or a newly created empty
     * 					        {@code StringBuilder} if the host is {@code null}
     * 					        or the kind is not recognized
     */
    fun toString() -> pointer<StringBuilder> =
        if this.host == null:
            new StringBuilder()
        elif this.kind == FIELD_TYPE:
        {
            val field: pointer<Field> = this.host as pointer<Field>
            field.toString()
        }
        elif this.kind == FUNCTION_TYPE:
        {
            val function: pointer<Function> = this.host as pointer<Function>
            function.toString()
        }
        elif this.kind == STRUCT_CONSTRUCTOR_TYPE:
        {
            val structConstructor: pointer<StructConstructor> = this.host as pointer<StructConstructor>
            structConstructor.toString()
        }
        elif this.kind == STRUCT_TYPE:
        {
            val structDecl: pointer<Struct> = this.host as pointer<Struct>
            structDecl.toString()
        }
        else:
            new StringBuilder()
}


/**
 * Maintains a collection of members and provides utilities for querying and
 * filtering them according to their modifiers.
 *
 * <p>A {@code MemberRegistry} acts as a lightweight view over a group of
 * {@link Member} objects. Members may represent fields, functions, struct
 * constructors, structs, or other declaration kinds supported by the
 * compiler.</p>
 *
 * <p>The registry does not modify the declarations themselves. Operations
 * such as {@link #filtrate(int, int)} create a new registry containing only
 * the members that satisfy the requested conditions.</p>
 */
struct MemberRegistry
{
    /**
     * Filter value that accepts every value in the corresponding dimension.
     */
    static val ALL: int = 0

    /**
     * Internal identifier for members declared with the {@code private}
     * access modifier.
     */
    static val PRIVATE_MODIFIER: int = 1

    /**
     * Internal identifier for members declared with the {@code protected}
     * access modifier.
     *
     * @deprecated This constant appears to contain a spelling error.
     *             Use {@code PROTECTED_MODIFIER} instead.
     */
    static val PROTEXTED_MODIFIER: int = 2

    /**
     * Internal identifier for members declared with the {@code protected}
     * access modifier.
     */
    static val PROTECTED_MODIFIER: int = 2

    /**
     * Internal identifier for members with public accessibility.
     *
     * <p>Members without an explicit {@code private} or {@code protected}
     * modifier are currently treated as public.</p>
     */
    static val PUBLIC_MODIFIER: int = 3

    /**
     * Filter value for members declared with the {@code static} modifier.
     */
    static val STATIC_MODIFIER: int = 1

    /**
     * Filter value for members without the {@code static} modifier.
     */
    static val NON_STATIC_MODIFIER: int = 2

    /**
     * Members currently contained in this registry.
     */
    private var members: pointer<ArrayList>


    /**
     * Creates a new member registry backed by the specified member list.
     *
     * <p>If {@code members} is {@code null}, an empty member list is created
     * automatically.</p>
     *
     * @param members the list of members to register, or {@code null} to
     *                create an empty registry
     */
    constructor(members: pointer<ArrayList>)
    {
        this.members = if members == null:
                new ArrayList(sizeof(Member))
            else:
                members
    }


    /**
     * Returns the modifier set associated with the specified member.
     *
     * <p>The actual modifier set is obtained from the declaration represented
     * by the member. The declaration type is determined through the member
     * kind and then cast to the corresponding AST node type.</p>
     *
     * <p>The following member kinds are currently supported:</p>
     *
     * <ul>
     *     <li>fields</li>
     *     <li>functions</li>
     *     <li>struct constructors</li>
     *     <li>struct declarations</li>
     * </ul>
     *
     * <p>If the member is {@code null}, has no host declaration, or represents
     * an unsupported member kind, this function returns {@code null}.</p>
     *
     * @param member the member whose modifiers should be obtained
     *
     * @return the modifier set of the underlying declaration, or
     *         {@code null} if no modifier set is available
     */
    private fun getModifiers(member: pointer<Member>) -> pointer<HashSet> =
        if member == null || member.getHost() == null:
            null
        elif member.isField():
        {
            val field: pointer<Field> = member.getHost() as pointer<Field>
            field.getModifiers()
        }
        elif member.isFunction():
        {
            val function: pointer<Function> = member.getHost() as pointer<Function>
            function.getModifiers()
        }
        elif member.isStructConstructor():
        {
            val structConstructor: pointer<StructConstructor> = member.getHost() as pointer<StructConstructor>
            structConstructor.getModifiers()
        }
        elif member.isStruct():
        {
            val structDecl: pointer<Struct> = member.getHost() as pointer<Struct>
            structDecl.getModifiers()
        }
        else:
            null


    /**
     * Determines whether the specified member contains a particular modifier.
     *
     * <p>The modifier set is resolved from the declaration represented by the
     * member. If the member has no available modifier set, this function
     * returns {@code false}.</p>
     *
     * @param member the member to inspect
     * @param modifier the modifier to search for
     *
     * @return {@code true} if the member contains the specified modifier;
     *         {@code false} otherwise
     */
    private fun hasModifier(member: pointer<Member>, modifier: pointer<Modifier>) -> bool
    {
        val modifiers: pointer<HashSet> = this.getModifiers(member)

        return modifiers != null && modifiers.contains(modifier)
    }


    /**
     * Resolves the effective access level of the specified member.
     *
     * <p>Access modifiers are checked in the following order:</p>
     *
     * <ol>
     *     <li>{@code private}</li>
     *     <li>{@code protected}</li>
     *     <li>{@code public}</li>
     * </ol>
     *
     * <p>If neither {@code private} nor {@code protected} is explicitly
     * present, the member is treated as public.</p>
     *
     * @param member the member whose access level should be resolved
     *
     * @return {@link #PRIVATE_MODIFIER} if the member is private,
     *         {@link #PROTECTED_MODIFIER} if it is protected, or
     *         {@link #PUBLIC_MODIFIER} otherwise
     */
    private fun getAccessModifier(member: pointer<Member>) -> int =
        if this.hasModifier(member, Modifier.fromPrivate()):
            PRIVATE_MODIFIER
        elif this.hasModifier(member, Modifier.fromProtected()):
            PROTEXTED_MODIFIER
        else:
            PUBLIC_MODIFIER
    
    
    /**
     * Returns the underlying list of members contained in this registry.
     *
     * <p>The returned list is the list currently used by the registry rather
     * than a copy of it.</p>
     *
     * @return the member list maintained by this registry
     */
    fun getMembers() -> pointer<ArrayList> = this.members


    /**
     * Creates a new registry containing members that match the specified
     * access level and static-state filter.
     *
     * <p>If either filter is {@link #ALL}, that dimension accepts every
     * value.</p>
     *
     * <p>Null member entries are ignored. The current registry is not modified;
     * instead, a new {@code MemberRegistry} containing the matching members is
     * returned.</p>
     *
     * @param accessModifier the required access modifier, normally
     *                       {@link #ALL} or one of
     *                       {@link #PRIVATE_MODIFIER},
     *                       {@link #PROTECTED_MODIFIER}, or
     *                       {@link #PUBLIC_MODIFIER}
     * @param staticModifier the required static state, normally
     *                       {@link #ALL}, {@link #STATIC_MODIFIER}, or
     *                       {@link #NON_STATIC_MODIFIER}
     *
     * @return a new registry containing only members matching both filters
     */
    fun filtrate(accessModifier: int, staticModifier: int) -> pointer<MemberRegistry>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Member))

        for (var i = 0; i < this.members.length; i++):
        {
            val member: pointer<Member> = this.members.get(i) as pointer<Member>

            if member == null:
                continue

            if accessModifier != ALL && this.getAccessModifier(member) != accessModifier:
                continue

            val memberStaticModifier: int = if this.hasModifier(member, Modifier.fromStatic()):
                    STATIC_MODIFIER
                else:
                    NON_STATIC_MODIFIER

            if staticModifier != ALL && memberStaticModifier != staticModifier:
                continue

            result.push(member)
        }

        return new MemberRegistry(result)
    }
}
