module Lowing.JVMJson where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Bits ((.|.), shiftL)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Parse.ParserBasic (AccessModified(..), DeclFlag(..))
import Parse.SyntaxTree (Class(..))
import Semantic.NameEnv (QName)
import Semantic.TypeEnv (FunSig(..))

import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified IR.TAC as IR
import qualified Lowing.JVM as JVM
import qualified Parse.SyntaxTree as AST


opNameKey :: Key
opNameKey = Key.fromString "op_name"

varIndexKey :: Key
varIndexKey = Key.fromString "index"

valueKey :: Key
valueKey = Key.fromString "value"

attributeNameKey :: Key
attributeNameKey = Key.fromString "attr_name"

attributeTypeKey :: Key
attributeTypeKey = Key.fromString "attr_type"

fullNameKey :: Key
fullNameKey = Key.fromString "full_name"

funcSigKey :: Key
funcSigKey = Key.fromString "func_sig"

funcReturnKey :: Key
funcReturnKey = Key.fromString "func_return"

funcParamsKey :: Key
funcParamsKey = Key.fromString "func_params"

accessKey :: Key
accessKey = Key.fromString "access"

nameKey :: Key
nameKey = Key.fromString "name"

returnKey :: Key
returnKey = Key.fromString "return"

paramTypesKey :: Key
paramTypesKey = Key.fromString "param_types"

methodOpsKey :: Key
methodOpsKey = Key.fromString "ops"

ownerTypeKey :: Key
ownerTypeKey = Key.fromString "owner_type"

blockIdKey :: Key
blockIdKey = Key.fromString "block_id"

blockOpsKey :: Key
blockOpsKey = Key.fromString "block_ops"

signatureKey :: Key
signatureKey = Key.fromString "signature"

classKey :: Key
classKey = Key.fromString "class"

superClassKey :: Key
superClassKey = Key.fromString "super_class"

interfacesKey :: Key
interfacesKey = Key.fromString "interfaces"

attributesKey :: Key
attributesKey = Key.fromString "attributes"

clinitKey :: Key
clinitKey = Key.fromString "clinit"

initKey :: Key
initKey = Key.fromString "init"

methodsKey :: Key
methodsKey = Key.fromString "methods"

mainTypeKey :: Key
mainTypeKey = Key.fromString "main_type"

mainQNameKey :: Key
mainQNameKey = Key.fromString "main_qname"

jvmTargetKey :: Key
jvmTargetKey = Key.fromString "jvm_target"

classesKey :: Key
classesKey = Key.fromString "classes"


basicQNameMap :: Map Class QName
basicQNameMap = Map.fromList [
    (Int8T, ["int8"]),
    (Int16T, ["int16"]),
    (Int32T, ["int32"]),
    (Int64T, ["int64"]),
    (Float32T, ["float32"]),
    (Float64T, ["float64"]),
    (Bool, ["bool"]),
    (Char, ["char"]),
    (Void, ["void"])]


-- | Convert a language-level type to a QName used in JSON.
typeToQName :: Class -> QName
typeToQName (Class qn []) = qn
typeToQName cls@(Class _ _) = error ("typeToQName: generic class types not supported: " ++ show cls)
typeToQName ErrorClass = error "typeToQName: error class"
typeToQName cls =
    fromMaybe (error ("typeToQName: non-basic type not supported: " ++ show cls))
        (Map.lookup cls basicQNameMap)


accessStringMap :: Map AccessModified String
accessStringMap = Map.fromList [
    (Private, "private"),
    (Protected, "protected"),
    (Public, "public")]


-- | Render access modifier as JSON string.
accessToString :: AccessModified -> String
accessToString acc =
    fromMaybe (error ("accessToString: unsupported access: " ++ show acc))
        (Map.lookup acc accessStringMap)


flagStringMap :: Map DeclFlag String
flagStringMap = Map.fromList [
    (Static, "static"),
    (Final, "final")]


-- | Render declaration flag as JSON string.
flagToString :: DeclFlag -> String
flagToString flag =
    fromMaybe (error ("flagToString: unsupported flag: " ++ show flag))
        (Map.lookup flag flagStringMap)

-- | Merge access modifier and flags into the JSON access list.
declToAccessList :: (AccessModified, [DeclFlag]) -> [String]
declToAccessList (acc, flags) = accessToString acc : map flagToString flags


accessPublicBit :: Int
accessPublicBit = 0


accessPrivateBit :: Int
accessPrivateBit = 1


accessProtectedBit :: Int
accessProtectedBit = 2


accessStaticBit :: Int
accessStaticBit = 3


accessFinalBit :: Int
accessFinalBit = 4


accessInlineBit :: Int
accessInlineBit = 5


bitMask :: Int -> Int
bitMask b = 1 `shiftL` b


declToAccessMask :: (AccessModified, [DeclFlag]) -> Int
declToAccessMask (acc, flags) =
    let vis = case acc of
            Public -> bitMask accessPublicBit
            Private -> bitMask accessPrivateBit
            Protected -> bitMask accessProtectedBit
        stBits = if Static `elem` flags then bitMask accessStaticBit else 0
        finBits = if Final `elem` flags then bitMask accessFinalBit else 0
        inlBits = if Inline `elem` flags then bitMask accessInlineBit else 0
    in vis .|. stBits .|. finBits .|. inlBits


ownerTypeClassCode :: Int
ownerTypeClassCode = 0


ownerTypeTopLevelCode :: Int
ownerTypeTopLevelCode = 1


ownerTypeToCode :: String -> Int
ownerTypeToCode raw =
    if map toLower raw == "xlang-top-level"
        then ownerTypeTopLevelCode
        else ownerTypeClassCode


typeToCompactText :: Class -> String
typeToCompactText cls = case cls of
    ErrorClass -> "error"
    Class qn [] -> intercalate "." qn
    Class _ _ -> AST.classMangle cls
    _ -> AST.classMangle cls


-- | Encode function signature as JSON.
funSigToJSON :: FunSig -> Value
funSigToJSON sig =
    object [
        funcReturnKey .= typeToCompactText (funReturn sig),
        funcParamsKey .= map typeToCompactText (funParams sig)]


-- | Choose JVM op prefix for numeric types.
opPrefix :: Class -> String
opPrefix Int8T = "i"
opPrefix Int16T = "i"
opPrefix Int32T = "i"
opPrefix Int64T = "l"
opPrefix Float32T = "f"
opPrefix Float64T = "d"
opPrefix Float128T = "d"
opPrefix Bool = "i"
opPrefix Char = "i"
opPrefix Void = "v"
opPrefix (Pointer _) = error "opPrefix: pointer type is native-only and not supported on JVM target"
opPrefix (Class _ _) = "a"
opPrefix ErrorClass = error "opPrefix: error class"

-- | Choose JVM cmp prefix for compare/jump ops.
cmpPrefix :: Class -> String
cmpPrefix cls = case cls of
    Int64T -> "l"
    Float32T -> "f"
    Float64T -> "d"
    Float128T -> "d"
    Bool -> "i"
    Char -> "i"
    Int8T -> "i"
    Int16T -> "i"
    Int32T -> "i"
    _ -> error ("cmpPrefix: unsupported type " ++ show cls)


-- | Build JVM cast opcode name from (from,to) classes.
castOpToJson :: (Class, Class) -> String
castOpToJson (a, b) = concat [opPrefix a, "2", opPrefix b]


-- | Encode a single JVM op into JSON.
opToJSON :: JVM.JOP -> Value
opToJSON (JVM.Add t) = op0 (opPrefix t ++ "add")
opToJSON (JVM.Sub t) = op0 (opPrefix t ++ "sub")
opToJSON (JVM.Mul t) = op0 (opPrefix t ++ "mul")
opToJSON (JVM.Div t) = op0 (opPrefix t ++ "div")
opToJSON (JVM.Rem t) = op0 (opPrefix t ++ "rem")
opToJSON (JVM.And t) = op0 (opPrefix t ++ "and")
opToJSON (JVM.Or t) = op0 (opPrefix t ++ "or")
opToJSON (JVM.Xor t) = op0 (opPrefix t ++ "xor")
opToJSON (JVM.Shl t) = op0 (opPrefix t ++ "shl")
opToJSON (JVM.Shr t) = op0 (opPrefix t ++ "shr")
opToJSON (JVM.UShr t) = op0 (opPrefix t ++ "ushr")

opToJSON (JVM.InvokeStatic qn sig) = object [
    opNameKey .= ("invokestatic" :: String),
    fullNameKey .= qn,
    funcSigKey .= funSigToJSON sig]

opToJSON (JVM.Cast from to) = op0 $ castOpToJson (from, to)
opToJSON (JVM.Goto bid) = object [opNameKey .= ("goto" :: String), blockIdKey .= bid]
opToJSON (JVM.Ifne bid) = object [opNameKey .= ("ifne" :: String), blockIdKey .= bid]
opToJSON (JVM.IfcmpEq cls bid) =
    object [opNameKey .= (concat ["if_", cmpPrefix cls, "cmpeq"] :: String), blockIdKey .= bid]
opToJSON (JVM.IfcmpNe cls bid) =
    object [opNameKey .= (concat ["if_", cmpPrefix cls, "cmpne"] :: String), blockIdKey .= bid]
opToJSON (JVM.IfcmpLt cls bid) =
    object [opNameKey .= (concat ["if_", cmpPrefix cls, "cmplt"] :: String), blockIdKey .= bid]
opToJSON (JVM.IfcmpLe cls bid) =
    object [opNameKey .= (concat ["if_", cmpPrefix cls, "cmple"] :: String), blockIdKey .= bid]
opToJSON (JVM.IfcmpGt cls bid) =
    object [opNameKey .= (concat ["if_", cmpPrefix cls, "cmpgt"] :: String), blockIdKey .= bid]
opToJSON (JVM.IfcmpGe cls bid) =
    object [opNameKey .= (concat ["if_", cmpPrefix cls, "cmpge"] :: String), blockIdKey .= bid]
opToJSON JVM.Return = op0 "return"
opToJSON (JVM.ReturnWV t) = op0 (opPrefix t ++ "return")

opToJSON JVM.Nop = op0 "nop"
opToJSON JVM.Pop = op0 "pop"
opToJSON JVM.Dup = op0 "dup"

opToJSON (JVM.GetStatic t qn) = object [
    opNameKey .= ("getstatic" :: String),
    attributeTypeKey .= typeToCompactText t,
    attributeNameKey .= qn]

opToJSON (JVM.PutStatic t qn) = object [
    opNameKey .= ("putstatic" :: String),
    attributeTypeKey .= typeToCompactText t,
    attributeNameKey .= qn]

opToJSON (JVM.Neg t) = op0 (opPrefix t ++ "neg")
opToJSON (JVM.Not t) = op0 (opPrefix t ++ "not")

opToJSON (JVM.Store cls idx) = object [
    opNameKey .= (opPrefix cls ++ "store"),
    varIndexKey .= idx]

opToJSON (JVM.Load cls idx) = object [
    opNameKey .= (opPrefix cls ++ "load"),
    varIndexKey .= idx]

opToJSON (JVM.CPush (JVM.JI i)) = object [
    opNameKey .= ("ipush" :: String),
    valueKey .= i]
opToJSON (JVM.CPush (JVM.JL i)) = object [
    opNameKey .= ("lpush" :: String),
    valueKey .= i]
opToJSON (JVM.CPush (JVM.JF f)) = object [
    opNameKey .= ("fpush" :: String),
    valueKey .= f]
opToJSON (JVM.CPush (JVM.JD d)) = object [
    opNameKey .= ("dpush" :: String),
    valueKey .= d]
opToJSON (JVM.CPush (JVM.JString s)) = object [
    opNameKey .= ("apush" :: String),
    valueKey .= s]
opToJSON JVM.PushNull = op0 "aconst_null"


-- | Helper for ops that only carry a name.
op0 :: String -> Value
op0 name = object [opNameKey .= name]


-- | Encode a JVM command (op or block) into JSON.
jCommandToJSON :: JVM.JCommand -> Value
jCommandToJSON (JVM.OP op) = opToJSON op
jCommandToJSON (JVM.Label (bid, cmds)) =
    object [
        opNameKey .= ("block" :: String),
        blockIdKey .= bid,
        blockOpsKey .= map jCommandToJSON cmds
    ]


-- | Encode a JVM function into JSON.
jFunctionToJSON :: JVM.JFunction -> Value
jFunctionToJSON (JVM.JFunction decl fname sig ownerType body) = object [
    ownerTypeKey .= ownerTypeToCode ownerType,
    accessKey .= declToAccessMask decl,
    nameKey .= fname,
    returnKey .= typeToCompactText (funReturn sig),
    paramTypesKey .= map typeToCompactText (funParams sig),
    methodOpsKey .= map jCommandToJSON body]


-- | Encode a JVM field into JSON.
jFieldToJSON :: JVM.JField -> Value
jFieldToJSON (JVM.JField decl cls name ownerType) = object [
    ownerTypeKey .= ownerTypeToCode ownerType,
    accessKey .= declToAccessMask decl,
    attributeNameKey .= name,
    attributeTypeKey .= typeToCompactText cls,
    signatureKey .= ("" :: String)]


-- | Encode class static initializer (<clinit>) into JSON.
jClinitToJSON :: JVM.JClinit -> Value
jClinitToJSON (JVM.JClinit body) = object [
    methodOpsKey .= map jCommandToJSON body]


-- | Encode a constructor into JSON.
jInitToJSON :: JVM.JInit -> Value
jInitToJSON (JVM.JInit decl sig body) = object [
    accessKey .= declToAccessMask decl,
    paramTypesKey .= map typeToCompactText (funParams sig),
    methodOpsKey .= map jCommandToJSON body]


-- | Encode a class into JSON.
jClassToJSON :: Int -> JVM.JClass -> Value
jClassToJSON jvmTarget (JVM.JClass decl qname extendQ interfaces fields clinit inits methods mainKind) =
    object $ [
        jvmTargetKey .= jvmTarget,
        classKey .= qname,
        accessKey .= declToAccessMask decl,
        signatureKey .= ([] :: [String]),
        superClassKey .= extendQ,
        interfacesKey .= interfaces,
        attributesKey .= map jFieldToJSON fields,
        clinitKey .= jClinitToJSON clinit,
        initKey .= map jInitToJSON inits,
        methodsKey .= map jFunctionToJSON methods,
        mainTypeKey .= mainType
        ] ++ mainQNameField
    where
        (mainType, mainQName) = mainKindMeta mainKind
        mainQNameField = case mainQName of
            Nothing -> []
            Just qn -> [mainQNameKey .= qn]


ownerTypeFromIRMember :: IR.IRMemberType -> Int
ownerTypeFromIRMember memberType = case memberType of
    IR.MemberClass -> ownerTypeClassCode
    IR.MemberClassWrapped -> ownerTypeTopLevelCode


irFieldMetaToJSON :: IR.Attribute -> Value
irFieldMetaToJSON (decl, cls, name, memberType) = object [
    ownerTypeKey .= ownerTypeFromIRMember memberType,
    accessKey .= declToAccessMask decl,
    attributeNameKey .= name,
    attributeTypeKey .= typeToCompactText cls,
    signatureKey .= ("" :: String)]


irMethodMetaToJSON :: IR.IRFunction -> Value
irMethodMetaToJSON (IR.IRFunction decl fname sig _ _ memberType) = object [
    ownerTypeKey .= ownerTypeFromIRMember memberType,
    accessKey .= declToAccessMask decl,
    nameKey .= fname,
    returnKey .= typeToCompactText (funReturn sig),
    paramTypesKey .= map typeToCompactText (funParams sig)]


irClassMetaToJSON :: Int -> QName -> IR.IRClass -> Value
irClassMetaToJSON jvmTarget pkgSegs (IR.IRClass decl className attrs _ _ funs mainKind) =
    object $ [
        jvmTargetKey .= jvmTarget,
        classKey .= (pkgSegs ++ [className]),
        accessKey .= declToAccessMask decl,
        signatureKey .= ([] :: [String]),
        superClassKey .= ([] :: [String]),
        interfacesKey .= ([] :: [[String]]),
        attributesKey .= map irFieldMetaToJSON attrs,
        clinitKey .= object [methodOpsKey .= ([] :: [Value])],
        initKey .= ([] :: [Value]),
        methodsKey .= map irMethodMetaToJSON funs,
        mainTypeKey .= mainType
        ] ++ mainQNameField
  where
    (mainType, mainQName) = mainKindMeta mainKind
    mainQNameField = case mainQName of
        Nothing -> []
        Just qn -> [mainQNameKey .= qn]


patchObjectField :: Key -> Value -> Value -> Value
patchObjectField key val (Aeson.Object obj) = Aeson.Object (KeyMap.insert key val obj)
patchObjectField _ _ v = v


patchMethodMetaWithOps :: Value -> JVM.JFunction -> Value
patchMethodMetaWithOps metaVal (JVM.JFunction _ _ _ _ body) =
    patchObjectField methodOpsKey (toJSON (map jCommandToJSON body)) metaVal


patchClassMetaWithOps :: Int -> Value -> JVM.JClass -> Value
patchClassMetaWithOps jvmTarget metaVal cls@(JVM.JClass _ _ _ _ _ clinit inits methods _) =
    let metaMethods = case lookupObjectField methodsKey metaVal of
            Just (Aeson.Array arr) -> foldr (:) [] arr
            _ -> []
        patchedMethods = zipWith patchMethodMetaWithOps metaMethods methods
        extraMethods = drop (length metaMethods) (map jFunctionToJSON methods)
        withClinit = patchObjectField clinitKey (jClinitToJSON clinit) metaVal
        withInit = patchObjectField initKey (toJSON (map jInitToJSON inits)) withClinit
        withMethods = patchObjectField methodsKey (toJSON (patchedMethods ++ extraMethods)) withInit
    in case withMethods of
        Aeson.Object _ -> withMethods
        _ -> jClassToJSON jvmTarget cls


lookupObjectField :: Key -> Value -> Maybe Value
lookupObjectField key (Aeson.Object obj) = KeyMap.lookup key obj
lookupObjectField _ _ = Nothing


irClassMetasToJSON :: Int -> [IR.IRProgm] -> [Value]
irClassMetasToJSON jvmTarget progms = concatMap oneProgm progms
  where
    oneProgm :: IR.IRProgm -> [Value]
    oneProgm (IR.IRProgm pkgSegs classes) = map (irClassMetaToJSON jvmTarget pkgSegs) classes


zipMetaAndClasses :: Int -> [Value] -> [JVM.JClass] -> [Value]
zipMetaAndClasses jvmTarget metas classes = go metas classes
  where
    go :: [Value] -> [JVM.JClass] -> [Value]
    go (m:ms) (c:cs) = patchClassMetaWithOps jvmTarget m c : go ms cs
    go [] cs = map (jClassToJSON jvmTarget) cs
    go ms [] = ms


-- | Build JVM JSON from IR metadata skeleton, then patch method/clinit/init ops.
jProgmToJSONFromIR :: Int -> [IR.IRProgm] -> [JVM.JClass] -> Value
jProgmToJSONFromIR jvmTarget irProgms classes =
    let metas = irClassMetasToJSON jvmTarget irProgms
    in object [classesKey .= zipMetaAndClasses jvmTarget metas classes]


mainKindMeta :: IR.MainKind -> (Int, Maybe QName)
mainKindMeta kind = case kind of
    IR.NoMain -> (-1, Nothing)
    IR.MainVoid qn -> (0, Just qn)
    IR.MainInt qn -> (1, Just qn)
    IR.MainVoidArgs qn -> (2, Just qn)
    IR.MainIntArgs qn -> (3, Just qn)


-- | Encode a whole program (list of classes) into JSON.
jProgmToJSON :: Int -> [JVM.JClass] -> Value
jProgmToJSON jvmTarget classes =
    object [classesKey .= map (jClassToJSON jvmTarget) classes]
