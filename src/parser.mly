
%{
open Syntax
open Common

(* simple variable substitution by marking each term as either a 
   regular term: `Term, or a variable: `Var, and then replacing 
   all instances with the corresponding concrete term *)

let var_map = Hashtbl.create 16
let cache = Hashtbl.create 16

let rec substitute_all term = 
    match term with 
    | `Term `Tzero -> Tzero
    | `Term `Tone -> Tone
    | `Term (`Ttest fv) -> Ttest fv 
    | `Term (`Tassign fv) -> Tassign fv 
    | `Term `Tdup -> Tdup
    | `Term `Tstart -> Tstart 
    | `Term (`Tneg a) -> Tneg (substitute_all a)    
    | `Term (`Tstar a) -> Tstar (substitute_all a)    
    | `Term (`Tand (a,b)) -> Tand (substitute_all a, substitute_all b)    
    | `Term (`Tor (a,b)) -> Tor (substitute_all a, substitute_all b)    
    | `Term (`Tseq (a,b)) -> Tseq (substitute_all a, substitute_all b)    
    | `Term (`Tplus (a,b)) -> Tplus (substitute_all a, substitute_all b)    
    | `Term (`Tlast a) -> Tlast (substitute_all a)    
    | `Term (`Twlast a) -> Twlast (substitute_all a)    
    | `Term (`Tever a) -> Tever (substitute_all a)    
    | `Term (`Talways a) -> Talways (substitute_all a)    
    | `Var x -> 
        try Hashtbl.find cache x 
        with _ ->
          try 
            let trm = Hashtbl.find var_map x in
            let res = substitute_all trm in 
            (* memoize to avoid exponential blowup *)
            Hashtbl.replace cache x res;
            res
          with 
            | Not_found -> raise (Failure ("unbound variable: " ^ x))
            | except -> raise except

%}


%token <string> ID, IP, NUM

%token SWITCH PORT SOURCE DEST BUCKET
%token ZERO ONE PLUS SEMI AND OR NEG STAR WLAST LAST EVER ALWAYS START DUP
%token SEMI LPAREN RPAREN ASSIGN EQUALS NEQUALS
%token COMMA EOF

%start top
%type <Syntax.tterm> top 

%nonassoc EQUALS NEQUALS ASSIGN
%left PLUS OR
%left SEMI AND
%left LAST WLAST EVER ALWAYS
%right NEG
%nonassoc STAR

%%

test:
    | START                     { `Term `Tstart }
    | ZERO                      { `Term `Tzero }
    | ONE                       { `Term `Tone }
    | field EQUALS ID           { `Term (`Ttest (to_field_val ($1, $3))) }
    | field EQUALS IP           { `Term (`Ttest (to_field_val ($1, $3))) }
    | field EQUALS NUM          { `Term (`Ttest (to_field_val ($1, $3))) }
    | test AND test             { `Term (`Tand ($1, $3)) }
    | test OR test              { `Term (`Tor ($1, $3)) }
    | NEG test                  { `Term (`Tneg $2) }
    | LAST LPAREN test RPAREN   { `Term (`Tlast $3) }
    | WLAST LPAREN test RPAREN  { `Term (`Twlast $3) }
    | EVER LPAREN test RPAREN   { `Term (`Tever $3) }
    | ALWAYS LPAREN test RPAREN { `Term (`Talways $3) }
    | LPAREN test RPAREN        { $2 }
;

term:
    | field ASSIGN ID           { `Term (`Tassign (to_field_val ($1,$3))) }
    | field ASSIGN IP           { `Term (`Tassign (to_field_val ($1,$3))) }
    | field ASSIGN NUM          { `Term (`Tassign (to_field_val ($1,$3))) }
    | term SEMI term            { `Term (`Tseq ($1, $3)) }
    | term PLUS term            { `Term (`Tplus ($1, $3)) }
    | term STAR                 { `Term (`Tstar $1) }
    | DUP                       { `Term `Tdup }
    | test                      { $1 }
    | LPAREN term RPAREN        { $2 }
    | ID                        { `Var $1 }
;

bindings:
    | ID EQUALS term COMMA bindings   { Hashtbl.add var_map $1 $3; $5 }
    | term EOF                        { $1 }
;

top:
    | bindings                  { substitute_all $1 }
;

field: 
    | SWITCH    { "sw" }
    | PORT      { "pt" }
    | SOURCE    { "src" }
    | DEST      { "dst" }
    | BUCKET    { "bucket" }
;

%% 


