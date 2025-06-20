%%
\+ "+"
\* "*"
\- "-"
/ "/"
\% "%"
\< "LT"
\<= "LTE"
> "GT"
>= "GTE"
== "EQ"
!= "NEQ"
or "||"
and "&&"
| "|"
& "&"
\( "("
\) ")"
\{ "{"
\} "}"
\[ "["
\] "]"
; ";"
: ":"
, ","
\. "."
! "!"
= "="
int "PRIM_INT"
bool "PRIM_BOOL"
char "PRIM_CHAR"
double "PRIM_DOUBLE"
float "PRIM_FLOAT"
null "PRIM_NULL"
in "IN"
out "OUT"
tuple "TUP"
void "VOID"
self "SELF_PARAM"
struct "STRUCT"
fun "FUN"
ref "REF"
if "IF"
is "IS"
else "ELSE"
return "RETURN"
don't "DONT"
while "WHILE"
the "THE"
true "TRUE"
false "FALSE"
emit "EMIT"
\-> "ARROW"
\<- "LARROW"
variant "VARIANT"
[_a-zA-Z][_a-zA-Z0-9]* "ID"
\"(\\.|[^\\\"\n])*\" "STRINGLIT"
\'([^\\']|\\([a-z]|\\|[0-9]+))\' "CHARLIT"
[\t \n]+ ;
\/\/[^\n]* ;
([0-9]+) "INT"
\$\w+ "REGLIT"
