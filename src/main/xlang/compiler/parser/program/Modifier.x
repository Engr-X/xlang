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
import xlang.lexer.TokenPosition
import xlang.util.ArrayList
import xlang.util.string.String
import xlang.util.string.StringBuilder


/**
 * Represents a declaration modifier in the program abstract syntax tree.
 *
 * <p>A {@code Modifier} stores the textual keyword of a declaration modifier
 * together with any additional syntax tokens associated with that modifier.
 *
 * <p>Common modifiers such as {@code private}, {@code protected},
 * {@code public}, {@code static}, {@code final}, {@code inline},
 * {@code native}, and {@code intrinsic} can be created through the provided
 * factory methods.
 *
 * <p>The modifier keyword is duplicated when the instance is created, so the
 * internally stored value does not depend on the lifetime of the character
 * sequence supplied to the constructor.
 *
 * <p>Additional syntax tokens are retained separately and may later be used for
 * source reconstruction, diagnostics, or source-location tracking.
 */
struct Modifier
{
    /**
     * Creates a {@code private} declaration modifier.
     *
     * <p>The returned modifier stores {@code "private"} as its keyword and has
     * an initially empty extra-token collection.
     *
     * @return				    a newly created {@code private} modifier
     */
    static fun fromPrivate() -> pointer<Modifier> = new Modifier("private")


    /**
     * Creates a {@code protected} declaration modifier.
     *
     * <p>The returned modifier stores {@code "protected"} as its keyword and has
     * an initially empty extra-token collection.
     *
     * @return				    a newly created {@code protected} modifier
     */
    static fun fromProtected() -> pointer<Modifier> = new Modifier("protected")


    /**
     * Creates a {@code public} declaration modifier.
     *
     * <p>The returned modifier stores {@code "public"} as its keyword and has an
     * initially empty extra-token collection.
     *
     * @return				a newly created {@code public} modifier
     */
    static fun fromPublic() -> pointer<Modifier> = new Modifier("public")


    /**
     * Creates a {@code static} declaration modifier.
     *
     * <p>The returned modifier stores {@code "static"} as its keyword and has an
     * initially empty extra-token collection.
     *
     * @return				a newly created {@code static} modifier
     */
    static fun fromStatic() -> pointer<Modifier> = new Modifier("static")


    /**
     * Creates a {@code final} declaration modifier.
     *
     * <p>The returned modifier stores {@code "final"} as its keyword and has an
     * initially empty extra-token collection.
     *
     * @return				a newly created {@code final} modifier
     */
    static fun fromFinal() -> pointer<Modifier> = new Modifier("final")


    /**
     * Creates an {@code inline} declaration modifier.
     *
     * <p>The returned modifier stores {@code "inline"} as its keyword and has an
     * initially empty extra-token collection.
     *
     * @return				a newly created {@code inline} modifier
     */
    static fun fromInline() -> pointer<Modifier> = new Modifier("inline")


    /**
     * Creates a {@code native} declaration modifier.
     *
     * <p>The returned modifier stores {@code "native"} as its keyword and has an
     * initially empty extra-token collection.
     *
     * @return				a newly created {@code native} modifier
     */
    static fun fromNative() -> pointer<Modifier> = new Modifier("native")


    /**
     * Creates an {@code intrinsic} declaration modifier.
     *
     * <p>The returned modifier stores {@code "intrinsic"} as its keyword and has
     * an initially empty extra-token collection.
     *
     * @return				a newly created {@code intrinsic} modifier
     */
    static fun fromIntrinsic() -> pointer<Modifier> = new Modifier("intrinsic")


    /**
     * The null-terminated keyword represented by this modifier.
     *
     * <p>The constructor duplicates the supplied character sequence before
     * storing it, so this field does not directly reference the original input
     * buffer.
     */
    private var keyword: pointer<char>

    /**
     * Additional syntax tokens associated with this modifier.
     *
     * <p>This collection may contain the lexical token corresponding to the
     * modifier keyword or other syntax information retained while parsing the
     * declaration.
     */
    private var extraTokens: pointer<ArrayList>


    /**
     * Creates a declaration modifier with the specified keyword.
     *
     * <p>The supplied keyword is duplicated using {@code String.strdup}, so the
     * internally stored character sequence is independent from the original
     * pointer supplied by the caller.
     *
     * <p>A new empty collection is allocated for additional syntax tokens.
     *
     * @param keyword			a pointer to the null-terminated modifier keyword
     */
    constructor(keyword: pointer<char>)
    {
        this.keyword = String.strdup(keyword)
        this.extraTokens = new ArrayList(sizeof(Token))
    }


    /**
     * Returns the keyword represented by this modifier.
     *
     * <p>The internally stored keyword is duplicated using
     * {@code String.strdup} before being returned. The returned character
     * sequence therefore does not directly reference the internal keyword
     * buffer.
     *
     * <p>Modifying or releasing the returned character sequence does not replace
     * the keyword pointer stored by this modifier.
     *
     * @return				    a pointer to a duplicated null-terminated modifier
     * 					        keyword
     */
    fun getKeyword() -> pointer<char> = String.strdup(this.keyword)


    /**
     * Adds an additional syntax token to this modifier.
     *
     * <p>If {@code token} is {@code null}, no modification is performed.
     *
     * <p>The supplied token is appended to the internal extra-token collection
     * and is stored by reference rather than cloned.
     *
     * <p>Tokens added through this method are included when
     * {@code getAllTokens()} constructs the complete token collection
     * associated with the modifier.
     *
     * @param token			    a pointer to the syntax token to add
     *
     * @return				    this {@code Modifier} instance
     */
    fun addExtraToken(token: pointer<Token>) -> pointer<Modifier>
    {
        if token != null:
            this.extraTokens.push(token)

        return this
    }


    /**
     * Returns a copy of the additional syntax-token collection.
     *
     * <p>The internal {@code ArrayList} is cloned before being returned, so
     * structural changes to the returned collection do not directly modify the
     * list stored by this modifier.
     *
     * <p>The individual {@code Token} objects referenced by the list are not
     * recursively cloned.
     *
     * @return				    a pointer to a cloned list containing the additional
     * 					        syntax tokens associated with this modifier
     */
    fun getExtraTokens() -> pointer<ArrayList> = this.extraTokens.clone()


    /**
     * Returns all tokens associated with this modifier.
     *
     * <p>The additional syntax tokens stored by this modifier are copied into a
     * newly allocated result list.
     *
     * <p>The result is then sorted according to source position using
     * {@code TokenPosition.compareToken}. This ensures that the returned token
     * sequence follows lexical source order even if tokens were added to the
     * modifier in a different order.
     *
     * <p>The keyword text stored in {@code keyword} does not independently
     * contribute a token to the result. Its lexical token must therefore be
     * present in {@code extraTokens} if it should be included in the token
     * collection.
     *
     * <p>The returned list is newly allocated. The individual token objects are
     * referenced rather than recursively cloned.
     *
     * @return				    a newly allocated list containing all tokens
     * 					        associated with this modifier in source order
     */
    fun getAllTokens() -> pointer<ArrayList>
    {
        val result: pointer<ArrayList> = new ArrayList(sizeof(Token))

        result.pushAll(this.extraTokens)
        result.setComparator(TokenPosition.compareToken)
        result.sort()
        return result
    }


    /**
     * Returns the textual representation of this modifier.
     *
     * <p>The generated representation consists only of the modifier keyword
     * stored by this instance.
     *
     * <p>A newly allocated {@code StringBuilder} is created from the internal
     * keyword. Modifying the returned builder does not modify the keyword stored
     * by this modifier.
     *
     * @return				    a pointer to a newly created {@code StringBuilder}
     * 					        containing the modifier keyword
     */
    fun toString() -> pointer<StringBuilder> = new StringBuilder(this.keyword)
}


/**
 * Represents an optional collection of declaration modifiers.
 *
 * <p>{@code ModifierListMaybe} is primarily used by parser productions in which
 * a modifier sequence may be absent.
 *
 * <p>Instead of preserving {@code null} to represent the absence of modifiers,
 * this structure normalizes the state to an empty {@code ArrayList}. Consumers
 * can therefore retrieve a modifier collection without repeatedly checking
 * whether the list itself exists.
 *
 * <p>If an existing modifier list is supplied to the constructor, that list is
 * stored directly and is not copied or cloned.
 */
struct ModifierListMaybe
{
    /**
     * The normalized collection of declaration modifiers.
     *
     * <p>This field contains either the list supplied to the constructor or a
     * newly allocated empty modifier list when no collection was provided.
     */
    private var list: pointer<ArrayList>


    /**
     * Creates an empty optional modifier collection.
     *
     * <p>A new empty {@code ArrayList} is allocated to represent the absence of
     * declaration modifiers without retaining a {@code null} list pointer.
     */
    constructor():
        this.list = new ArrayList(sizeof(Modifier))


    /**
     * Creates an optional modifier collection from the specified list.
     *
     * <p>If {@code list} is {@code null}, a new empty modifier collection is
     * allocated. Otherwise, the supplied list is stored directly and is not
     * copied or cloned.
     *
     * <p>This normalization ensures that the internal modifier collection is
     * represented by a valid {@code ArrayList} after construction.
     *
     * @param list			    a pointer to the modifier collection, or
     * 					        {@code null} to represent an empty collection
     */
    constructor(list: pointer<ArrayList>):
        this.list = if list == null:
                new ArrayList(sizeof(Modifier))
            else:
                list


    /**
     * Returns the normalized modifier collection represented by this wrapper.
     *
     * <p>The returned pointer refers directly to the internally stored
     * {@code ArrayList}. The collection is not copied or cloned.
     *
     * <p>Because both constructors normalize an absent modifier list to an empty
     * collection, this method normally returns a valid list even when no
     * modifiers were present in the parsed declaration.
     *
     * <p>Modifications performed through the returned list affect the same
     * collection referenced internally by this {@code ModifierListMaybe}
     * instance.
     *
     * @return				    a pointer to the internally stored modifier
     * 					        collection
     */
    fun toModifierList() -> pointer<ArrayList> = this.list
}
