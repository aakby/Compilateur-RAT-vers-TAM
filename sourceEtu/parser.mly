/* Imports. */

%{

open Type
open Ast.AstSyntax
%}

%token <string> TID
%token <int> ENTIER
%token <string> ID
%token RETURN
%token PV
%token AO
%token AF
%token PF
%token PO
%token EQUAL
%token CONST
%token PRINT
%token IF
%token ELSE
%token WHILE
%token BOOL
%token INT
%token RAT
%token CALL 
%token CO
%token CF
%token SLASH
%token NUM
%token DENOM
%token TRUE
%token FALSE
%token PLUS
%token MULT
%token INF
%token EOF
%token NEW
%token AND
%token NULL
%token VIRG
%token ENUM
%token SWITCH
%token CASE 
%token DP 
%token DEFAULT 
%token BREAK 

(* Type de l'attribut synthétisé des non-terminaux *)
%type <programme> prog
%type <instruction list> bloc
%type <fonction> fonc
%type <instruction list> is
%type <instruction> i
%type <typ> typ
%type <(typ*string) list> dp
%type <expression> e 
%type <expression list> cp
%type <affectable> aff
%type <enumeration> enum

(* Type et définition de l'axiome *)
%start <Ast.AstSyntax.programme> main

%%

main : lfi = prog EOF     {lfi}

prog :
| lenum = enum lfi = prog {let (Programme (lenum1,lf,li))=lfi in (Programme (lenum::lenum1,lf,li))}
| lf = fonc  lfi = prog   {let (Programme (lenum,lf1,li))=lfi in (Programme (lenum,lf::lf1,li))}
| ID li = bloc            {Programme ([],[],li)}

fonc : t=typ n=ID PO p=dp PF AO li=is RETURN exp=e PV AF {Fonction(t,n,p,li,exp)}

bloc : AO li = is AF      {li}

is :
|                         {[]}
| i1=i li=is              {i1::li}

i :
| t=typ n=ID EQUAL e1=e PV          {Declaration (t,n,e1)}
| a=aff EQUAL e1=e PV               {Affectation (a,e1)}
| CONST n=ID EQUAL e=ENTIER PV      {Constante (n,e)}
| PRINT e1=e PV                     {Affichage (e1)}
| IF exp=e li1=bloc ELSE li2=bloc   {Conditionnelle (exp,li1,li2)}
| WHILE exp=e li=bloc               {TantQue (exp,li)}
| SWITCH PO exp=e PF AO lcases=lcase AF {Switch (exp,lcases)}

lcase :
| cas= case lc = lcase {cas::lc}
|                      {[]}

case :
| CASE n=tc DP l=is bv = b {Case(n,l,bv)}
| DEFAULT DP l=is bv = b   {Default(l,bv)}

tc : 
| n=TID   {TCtid n}
| n=ENTIER {TCentier n}
| TRUE     {TCtrue}
| FALSE    {TCfalse}

b :
|           {false}
| BREAK PV  {true}

dp :
|                         {[]}
| t=typ n=ID lp=dp        {(t,n)::lp}

enum : ENUM n = TID AO lid=ids AF PV {Enumeration(n,lid)}

ids : 
| n = TID {n::[]}
| n = TID VIRG lid=ids {n::lid}

typ :
| BOOL       {Bool}
| INT        {Int}
| RAT        {Rat}
| t=typ MULT {Pointeur t}
| n = TID    {Enumere n}

e : 
| CALL n=ID PO lp=cp PF   {AppelFonction (n,lp)}
| CO e1=e SLASH e2=e CF   {Rationnel(e1,e2)}
| NUM e1=e                {Numerateur e1}
| DENOM e1=e              {Denominateur e1}
| a = aff                 {Acces a}
| TRUE                    {True}
| FALSE                   {False}
| e=ENTIER                {Entier e}
| PO e1=e PLUS e2=e PF    {Binaire (Plus,e1,e2)}
| PO e1=e MULT e2=e PF    {Binaire (Mult,e1,e2)}
| PO e1=e EQUAL e2=e PF   {Binaire (Equ,e1,e2)}
| PO e1=e INF e2=e PF     {Binaire (Inf,e1,e2)}
| PO exp=e PF             {exp}
| NULL                    {Vide}
| PO NEW t=typ PF         {Allocation t}
| AND n=ID                {Adresse n}
| n = TID                 {ValeurEnum n}

aff :
| n = ID                  {Ident n}
| PO MULT a=aff PF        {Valeur a}
cp :
|               {[]}
| e1=e le=cp    {e1::le}
