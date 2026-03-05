module Lowing.JVMJson where

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key (Key)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Parse.ParserBasic (AccessModified(..), DeclFlag(..))
import Parse.SyntaxTree (Class(..))
import Semantic.NameEnv (QName)
import Semantic.TypeEnv (FunSig(..))

import qualified Data.Aeson.Key as Key
import qualified Data.Map.Strict as Map
import qualified Lowing.JVM as JVM


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
typeToQName (Array elemT dims) = typeToQName elemT ++ replicate dims "[]"
typeToQName cls@(Class _ _) = error ("typeToQName: class types not supported: " ++ show cls)
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


-- | Encode function signature as JSON.
funSigToJSON :: FunSig -> Value
funSigToJSON sig =
    object [
        funcReturnKey .= typeToQName (funReturn sig),
        funcParamsKey .= map typeToQName (funParams sig)]


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
opPrefix (Array _ _) = error "opPrefix: array type"
opPrefix (Class _ _) = error "opPrefix: class type"
opPrefix ErrorClass = error "opPrefix: error class"


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
opToJSON JVM.Return = op0 "return"
opToJSON (JVM.ReturnWV t) = op0 (opPrefix t ++ "return")

opToJSON JVM.Nop = op0 "nop"
opToJSON JVM.Pop = op0 "pop"
opToJSON JVM.Dup = op0 "dup"

opToJSON (JVM.GetStatic t qn) = object [
    opNameKey .= ("getstatic" :: String),
    attributeTypeKey .= typeToQName t,
    attributeNameKey .= qn]

opToJSON (JVM.PutStatic t qn) = object [
    opNameKey .= ("putstatic" :: String),
    attributeTypeKey .= typeToQName t,
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
jFunctionToJSON (JVM.JFunction decl fname sig body) = object [
    accessKey .= declToAccessList decl,
    nameKey .= fname,
    returnKey .= typeToQName (funReturn sig),
    paramTypesKey .= map typeToQName (funParams sig),
    methodOpsKey .= map jCommandToJSON body]


-- | Encode a JVM field into JSON.
jFieldToJSON :: JVM.JField -> Value
jFieldToJSON (JVM.JField decl cls name) = object [
    accessKey .= declToAccessList decl,
    attributeNameKey .= name,
    attributeTypeKey .= typeToQName cls,
    signatureKey .= ("" :: String)]


-- | Encode class static initializer (<clinit>) into JSON.
jClinitToJSON :: JVM.JClinit -> Value
jClinitToJSON (JVM.JClinit body) = object [
    methodOpsKey .= map jCommandToJSON body]


-- | Encode a constructor into JSON.
jInitToJSON :: JVM.JInit -> Value
jInitToJSON (JVM.JInit decl sig body) = object [
    accessKey .= declToAccessList decl,
    paramTypesKey .= map typeToQName (funParams sig),
    methodOpsKey .= map jCommandToJSON body]


-- | Encode a class into JSON.
jClassToJSON :: Int -> JVM.JClass -> Value
jClassToJSON jvmTarget (JVM.JClass decl qname extendQ interfaces fields clinit inits methods) =
    object [
        jvmTargetKey .= jvmTarget,
        classKey .= qname,
        accessKey .= declToAccessList decl,
        signatureKey .= ([] :: [String]),
        superClassKey .= extendQ,
        interfacesKey .= interfaces,
        attributesKey .= map jFieldToJSON fields,
        clinitKey .= jClinitToJSON clinit,
        initKey .= map jInitToJSON inits,
        methodsKey .= map jFunctionToJSON methods
    ]


-- | Encode a whole program (list of classes) into JSON.
jProgmToJSON :: Int -> [JVM.JClass] -> Value
jProgmToJSON jvmTarget classes =
    object [classesKey .= map (jClassToJSON jvmTarget) classes]
