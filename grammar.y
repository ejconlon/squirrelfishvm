{
module Squirrelfish where
}

%name squirrelfish
%tokentype { Token }
%error { parseError }

%token
load { TokenLoad }
new_object { TokenNewObject }
new_array { TokenNewArray }
new_regexp { TokenNewRegexp }
mov { TokenMov }
eq { TokenEq }
neq { TokenNeq }
stricteq { TokenStricteq }
nstricteq { TokenNstricteq }
less { TokenLess }
lesseq { TokenLesseq }
pre_inc { TokenPreInc }
pre_dec { TokenPreDec }
post_inc { TokenPostInc }
post_dec { TokenPostDec }
to_jsnumber { TokenToJsnumber }
negate { TokenNegate }
add { TokenAdd }
mul { TokenMul }
div { TokenDiv }
mod { TokenMod }
sub { TokenSub }
lshift { TokenLshift }
rshift { TokenRshift }
urshift { TokenUrshift }
bitand { TokenBitand }
bitor { TokenBitor }
bitnot { TokenBitnot }
not { TokenNot }
instanceof { TokenInstanceof }
typeof { TokenTypeof }
in { TokenIn }
resolve { TokenResolve }
resolve_skip { TokenResolveSkip }
get_scoped_var { TokenGetScopedVar }
put_scoped_var { TokenPutScopedVar }
resolve_base { TokenResolveBase }
resolve_with_base { TokenResolveWithBase }
resolve_func { TokenResolveFunc }
get_by_id { TokenGetById }
put_by_id { TokenPutById }
del_by_id { TokenDelById }
get_by_val { TokenGetByVal }
put_by_val { TokenPutByVal }
del_by_val { TokenDelByVal }
put_by_index { TokenPutByIndex }
loop { TokenLoop }
jmp { TokenJmp }
loop_if_true { TokenLoopIfTrue }
jtrue { TokenJtrue }
jfalse { TokenJfalse }
loop_if_less { TokenLoopIfLess }
jless { TokenJless }
jnless { TokenJnless }
switch_imm { TokenSwitchImm }
switch_char { TokenSwitchChar }
switch_string { TokenSwitchString }
new_func { TokenNewFunc }
new_func_exp { TokenNewFuncExp }
call_eval { TokenCallEval }
call { TokenCall }
ret { TokenRet }
construct { TokenConstruct }
push_scope { TokenPushScope }
pop_scope { TokenPopScope }
get_pnames { TokenGetPnames }
next_pname { TokenNextPname }
jmp_scopes { TokenJmpScopes }
catch { TokenCatch }
throw { TokenThrow }
new_error { TokenNewError }
end { TokenEnd }
put_getter { TokenPutGetter }
put_setter { TokenPutSetter }
jsr { TokenJsr }
sret { TokenSret }
debug { TokenDebug }

%%

