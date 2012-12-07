module Parser where

import Control.Monad.Instances

-- forget happy - let's use parsec.

type Error = String

data Token =
  TokenLoad |
  TokenNewObject |
  TokenNewArray |
  TokenNewRegexp |
  TokenMov |
  TokenEq |
  TokenNeq |
  TokenStricteq |
  TokenNstricteq |
  TokenLess |
  TokenLesseq |
  TokenPreInc |
  TokenPreDec |
  TokenPostInc |
  TokenPostDec |
  TokenToJsnumber |
  TokenNegate |
  TokenAdd |
  TokenMul |
  TokenDiv |
  TokenMod |
  TokenSub |
  TokenLshift |
  TokenRshift |
  TokenUrshift |
  TokenBitand |
  TokenBitor |
  TokenBitnot |
  TokenNot |
  TokenInstanceof |
  TokenTypeof |
  TokenIn |
  TokenResolve |
  TokenResolveSkip |
  TokenGetScopedVar |
  TokenPutScopedVar |
  TokenResolveBase |
  TokenResolveWithBase |
  TokenResolveFunc |
  TokenGetById |
  TokenPutById |
  TokenDelById |
  TokenGetByVal |
  TokenPutByVal |
  TokenDelByVal |
  TokenPutByIndex |
  TokenLoop |
  TokenJmp |
  TokenLoopIfTrue |
  TokenJtrue |
  TokenJfalse |
  TokenLoopIfLess |
  TokenJless |
  TokenJnless |
  TokenSwitchImm |
  TokenSwitchChar |
  TokenSwitchString |
  TokenNewFunc |
  TokenNewFuncExp |
  TokenCallEval |
  TokenCall |
  TokenRet |
  TokenConstruct |
  TokenPushScope |
  TokenPopScope |
  TokenGetPnames |
  TokenNextPname |
  TokenJmpScopes |
  TokenCatch |
  TokenThrow |
  TokenNewError |
  TokenEnd |
  TokenPutGetter |
  TokenPutSetter |
  TokenJsr |
  TokenSret |
  TokenDebug deriving (Show, Eq) 


readToken :: String -> Either Error Token
readToken "load" = return TokenLoad
readToken "new_object" = return TokenNewObject
readToken "new_array" = return TokenNewArray
readToken "new_regexp" = return TokenNewRegexp
readToken "mov" = return TokenMov
readToken "eq" = return TokenEq
readToken "neq" = return TokenNeq
readToken "stricteq" = return TokenStricteq
readToken "nstricteq" = return TokenNstricteq
readToken "less" = return TokenLess
readToken "lesseq" = return TokenLesseq
readToken "pre_inc" = return TokenPreInc
readToken "pre_dec" = return TokenPreDec
readToken "post_inc" = return TokenPostInc
readToken "post_dec" = return TokenPostDec
readToken "to_jsnumber" = return TokenToJsnumber
readToken "negate" = return TokenNegate
readToken "add" = return TokenAdd
readToken "mul" = return TokenMul
readToken "div" = return TokenDiv
readToken "mod" = return TokenMod
readToken "sub" = return TokenSub
readToken "lshift" = return TokenLshift
readToken "rshift" = return TokenRshift
readToken "urshift" = return TokenUrshift
readToken "bitand" = return TokenBitand
readToken "bitor" = return TokenBitor
readToken "bitnot" = return TokenBitnot
readToken "not" = return TokenNot
readToken "instanceof" = return TokenInstanceof
readToken "typeof" = return TokenTypeof
readToken "in" = return TokenIn
readToken "resolve" = return TokenResolve
readToken "resolve_skip" = return TokenResolveSkip
readToken "get_scoped_var" = return TokenGetScopedVar
readToken "put_scoped_var" = return TokenPutScopedVar
readToken "resolve_base" = return TokenResolveBase
readToken "resolve_with_base" = return TokenResolveWithBase
readToken "resolve_func" = return TokenResolveFunc
readToken "get_by_id" = return TokenGetById
readToken "put_by_id" = return TokenPutById
readToken "del_by_id" = return TokenDelById
readToken "get_by_val" = return TokenGetByVal
readToken "put_by_val" = return TokenPutByVal
readToken "del_by_val" = return TokenDelByVal
readToken "put_by_index" = return TokenPutByIndex
readToken "loop" = return TokenLoop
readToken "jmp" = return TokenJmp
readToken "loop_if_true" = return TokenLoopIfTrue
readToken "jtrue" = return TokenJtrue
readToken "jfalse" = return TokenJfalse
readToken "loop_if_less" = return TokenLoopIfLess
readToken "jless" = return TokenJless
readToken "jnless" = return TokenJnless
readToken "switch_imm" = return TokenSwitchImm
readToken "switch_char" = return TokenSwitchChar
readToken "switch_string" = return TokenSwitchString
readToken "new_func" = return TokenNewFunc
readToken "new_func_exp" = return TokenNewFuncExp
readToken "call_eval" = return TokenCallEval
readToken "call" = return TokenCall
readToken "ret" = return TokenRet
readToken "construct" = return TokenConstruct
readToken "push_scope" = return TokenPushScope
readToken "pop_scope" = return TokenPopScope
readToken "get_pnames" = return TokenGetPnames
readToken "next_pname" = return TokenNextPname
readToken "jmp_scopes" = return TokenJmpScopes
readToken "catch" = return TokenCatch
readToken "throw" = return TokenThrow
readToken "new_error" = return TokenNewError
readToken "end" = return TokenEnd
readToken "put_getter" = return TokenPutGetter
readToken "put_setter" = return TokenPutSetter
readToken "jsr" = return TokenJsr
readToken "sret" = return TokenSret
readToken "debug" = return TokenDebug
readToken x = Left $ "Bad token: " ++ x

showToken :: Token -> String
showToken TokenLoad = "load"
showToken TokenNewObject = "new_object"
showToken TokenNewArray = "new_array"
showToken TokenNewRegexp = "new_regexp"
showToken TokenMov = "mov"
showToken TokenEq = "eq"
showToken TokenNeq = "neq"
showToken TokenStricteq = "stricteq"
showToken TokenNstricteq = "nstricteq"
showToken TokenLess = "less"
showToken TokenLesseq = "lesseq"
showToken TokenPreInc = "pre_inc"
showToken TokenPreDec = "pre_dec"
showToken TokenPostInc = "post_inc"
showToken TokenPostDec = "post_dec"
showToken TokenToJsnumber = "to_jsnumber"
showToken TokenNegate = "negate"
showToken TokenAdd = "add"
showToken TokenMul = "mul"
showToken TokenDiv = "div"
showToken TokenMod = "mod"
showToken TokenSub = "sub"
showToken TokenLshift = "lshift"
showToken TokenRshift = "rshift"
showToken TokenUrshift = "urshift"
showToken TokenBitand = "bitand"
showToken TokenBitor = "bitor"
showToken TokenBitnot = "bitnot"
showToken TokenNot = "not"
showToken TokenInstanceof = "instanceof"
showToken TokenTypeof = "typeof"
showToken TokenIn = "in"
showToken TokenResolve = "resolve"
showToken TokenResolveSkip = "resolve_skip"
showToken TokenGetScopedVar = "get_scoped_var"
showToken TokenPutScopedVar = "put_scoped_var"
showToken TokenResolveBase = "resolve_base"
showToken TokenResolveWithBase = "resolve_with_base"
showToken TokenResolveFunc = "resolve_func"
showToken TokenGetById = "get_by_id"
showToken TokenPutById = "put_by_id"
showToken TokenDelById = "del_by_id"
showToken TokenGetByVal = "get_by_val"
showToken TokenPutByVal = "put_by_val"
showToken TokenDelByVal = "del_by_val"
showToken TokenPutByIndex = "put_by_index"
showToken TokenLoop = "loop"
showToken TokenJmp = "jmp"
showToken TokenLoopIfTrue = "loop_if_true"
showToken TokenJtrue = "jtrue"
showToken TokenJfalse = "jfalse"
showToken TokenLoopIfLess = "loop_if_less"
showToken TokenJless = "jless"
showToken TokenJnless = "jnless"
showToken TokenSwitchImm = "switch_imm"
showToken TokenSwitchChar = "switch_char"
showToken TokenSwitchString = "switch_string"
showToken TokenNewFunc = "new_func"
showToken TokenNewFuncExp = "new_func_exp"
showToken TokenCallEval = "call_eval"
showToken TokenCall = "call"
showToken TokenRet = "ret"
showToken TokenConstruct = "construct"
showToken TokenPushScope = "push_scope"
showToken TokenPopScope = "pop_scope"
showToken TokenGetPnames = "get_pnames"
showToken TokenNextPname = "next_pname"
showToken TokenJmpScopes = "jmp_scopes"
showToken TokenCatch = "catch"
showToken TokenThrow = "throw"
showToken TokenNewError = "new_error"
showToken TokenEnd = "end"
showToken TokenPutGetter = "put_getter"
showToken TokenPutSetter = "put_setter"
showToken TokenJsr = "jsr"
showToken TokenSret = "sret"
showToken TokenDebug = "debug"


data Register = Register Int deriving (Show, Eq)
data Constant = Constant Int deriving (Show, Eq)
data RegExp = Regexp String deriving (Show, Eq)
data Identifier = Identifier String deriving (Show, Eq)
data Count = Count Int deriving (Show, Eq)
data Offset = Offset Int deriving (Show, Eq)
data Function = Function String deriving (Show, Eq)

data Instruction =
  InstrLoad Register Constant |
  InstrNewObject Register |
  InstrNewArray Register Register Count |
  InstrNewRegexp Register RegExp |
  InstrMov Register Register |
  InstrEq Register Register Register |
  InstrNeq Register Register Register |
  InstrStricteq Register Register Register |
  InstrNstricteq Register Register Register |
  InstrLess Register Register Register |
  InstrLesseq Register Register Register |
  InstrPreInc Register |
  InstrPreDec Register |
  InstrPostInc Register Register |
  InstrPostDec Register Register |
  InstrToJsnumber Register Register |
  InstrNegate Register Register |
  InstrAdd Register Register Register |
  InstrMul Register Register Register |
  InstrDiv Register Register Register |
  InstrMod Register Register Register |
  InstrSub Register Register Register |
  InstrLshift Register Register Register |
  InstrRshift Register Register Register |
  InstrUrshift Register Register Register |
  InstrBitand Register Register Register |
  InstrBitxor Register Register Register |
  InstrBitor Register Register Register |
  InstrBitnot Register Register |
  InstrNot Register Register Register |   -- possible doc error
  InstrInstanceof Register Register Register |
  InstrTypeof Register Register |
  InstrIn Register Register Register |
  InstrResolve Register Identifier |
  InstrResolveSkip Register Identifier Count |
  InstrGetScopedVar Register Count Count |
  InstrPutScopedVar Count Count Register |
  InstrResolveBase Register Identifier |
  InstrResolveWithBase Register Register Identifier |
  InstrResolveFunc Register Register Identifier |
  InstrGetById Register Register Identifier |
  InstrPutById Register Identifier Register |
  InstrDelById Register Register Identifier |
  InstrGetByVal Register Register Register |
  InstrPutByVal Register Register Register |
  InstrDelByVal Register Register Register |
  InstrPutByIndex Register Count Register |
  InstrLoop Offset |
  InstrJmp Offset |
  InstrLoopIfTrue Register Offset |
  InstrJtrue Register Offset |
  InstrJfalse Register Offset |
  InstrLoopIfLess Register Register Offset |
  InstrJless Register Register Offset |
  InstrJnless Register Register Offset |
  InstrSwitchImm Count Offset Register |
  InstrSwitchChar Count Offset Register |
  InstrSwitchString Count Offset Register |
  InstrNewFunc Register Function |
  InstrNewFuncExp Register Function |
  InstrCallEval Register Register Register Register Count |
  InstrCall Register Register Register Register Count |
  InstrRet Register |
  InstrConstruct Register Register Register Count |
  InstrPushScope Register |
  InstrPopScope |
  InstrGetPnames Register Register |
  InstrNextPname Register Register Offset |
  InstrJmpScopes Count Offset |
  InstrCatch Register |
  InstrThrow Register |
  InstrNewError Register Count Constant |
  InstrEnd Register |
  InstrPutGetter Register Identifier Register |
  InstrPutSetter Register Identifier Register |
  InstrJsr Register Offset |
  InstrSret Register |
  InstrDebug Count Count Count deriving (Show, Eq)


class Interpreter m where
  interpret :: m -> Instruction -> Either Error m 

