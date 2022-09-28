-- `!!!!!!!!!!!!!!! Aexp`

-- Positive numeral
declare_syntax_cat adig
declare_syntax_cat apos

syntax "⋄" : adig
syntax "‡" : adig
syntax adig : apos
syntax apos adig : apos

-- Arithmetic Expression
declare_syntax_cat aexp

syntax apos : aexp
syntax "⌢" apos : aexp
syntax "(" aexp ")" : aexp
syntax aexp "⊹" : aexp
syntax aexp "⁻¹" : aexp
syntax:60 aexp:60 "+" aexp:61 : aexp
syntax:60 aexp:60 "-" aexp:61 : aexp
syntax:70 aexp:70 "*" aexp:71 : aexp
syntax:70 aexp:70 "/" aexp:71 : aexp
syntax:70 aexp:70 "%" aexp:71 : aexp

-- Continuation
declare_syntax_cat contelem
declare_syntax_cat cont
syntax "|" aexp "]" : contelem
syntax "&||" adig : contelem
syntax "&neg" : contelem
syntax "&⊹" : contelem
syntax "&⁻¹" : contelem
syntax "&+" : contelem
syntax "&-" : contelem
syntax "&*" : contelem
syntax "&/" : contelem
syntax "&/⁻¹" : contelem
syntax "&%" : contelem
syntax "&%⁻¹" : contelem
syntax contelem+ : cont

syntax "`[teval" cont : term


-- Decomposing expression into continuation
macro_rules
|`(`[teval |($p:aexp)] $ct*) => `(`[teval |$p:aexp] $ct*)
|`(`[teval |$p:aexp ⊹] $ct*) => `(`[teval |$p:aexp] &⊹ $ct*)
|`(`[teval |$p:aexp ⁻¹] $ct*) => `(`[teval |$p:aexp] &⁻¹ $ct*)
|`(`[teval |$p:aexp + $q:aexp] $ct*) => `(`[teval |$p:aexp] |$q:aexp] &+ $ct*)
|`(`[teval |$p:aexp - $q:aexp] $ct*) => `(`[teval |$p:aexp] |$q:aexp] &- $ct*)
|`(`[teval |$p:aexp * $q:aexp] $ct*) => `(`[teval |$p:aexp] |$q:aexp] &* $ct*)
|`(`[teval |$p:aexp / $q:aexp] $ct*) => `(`[teval |$p:aexp] |$q:aexp] &/ $ct*)
|`(`[teval |$p:aexp % $q:aexp] $ct*) => `(`[teval |$p:aexp] |$q:aexp] &% $ct*)

macro_rules
|`(`[teval |$p:apos] |$q:aexp] &+ $ct*) => `(`[teval |$q:aexp] |$p:apos] &+ $ct*)
|`(`[teval |⌢$p:apos] |$q:aexp] &+ $ct*) => `(`[teval |$q:aexp] |⌢$p:apos] &+ $ct*)
|`(`[teval $c1:contelem $c2:contelem &- $ct*) => `(`[teval $c2:contelem &neg $c1 &+ $ct*)
|`(`[teval |$p:apos] |$q:aexp] &* $ct*) => `(`[teval |$q:aexp] |$p:apos] &* $ct*)
|`(`[teval |⌢$p:apos] |$q:aexp] &* $ct*) => `(`[teval |$q:aexp] |⌢$p:apos] &* $ct*)
|`(`[teval |$p:apos] |$q:aexp] &/ $ct*) => `(`[teval |$q:aexp] |$p:apos] &/⁻¹ $ct*)
|`(`[teval |⌢$p:apos] |$q:aexp] &/ $ct*) => `(`[teval |$q:aexp] |⌢$p:apos] &/⁻¹ $ct*)
|`(`[teval |$p:apos] |$q:aexp] &% $ct*) => `(`[teval |$q:aexp] |$p:apos] &%⁻¹ $ct*)
|`(`[teval |⌢$p:apos] |$q:aexp] &% $ct*) => `(`[teval |$q:aexp] |⌢$p:apos] &%⁻¹ $ct*)

macro_rules
|`(`[teval |$p:apos] |$q:aexp] &/⁻¹ $ct*) => `(`[teval |$q:aexp] |$p:apos] &/ $ct*)
|`(`[teval |⌢$p:apos] |$q:aexp] &/⁻¹ $ct*) => `(`[teval |$q:aexp] |⌢$p:apos] &/ $ct*)
|`(`[teval |$p:apos] |$q:aexp] &%⁻¹ $ct*) => `(`[teval |$q:aexp] |$p:apos] &% $ct*)
|`(`[teval |⌢$p:apos] |$q:aexp] &%⁻¹ $ct*) => `(`[teval |$q:aexp] |⌢$p:apos] &% $ct*)

-- Executing continuation, ||, neg
macro_rules
|`(`[teval |$n:apos] &||$d:adig $ct*) => `(`[teval |$n:apos $d:adig] $ct*)
|`(`[teval |⌢$n:apos] &||$d:adig $ct*) => `(`[teval |⌢$n:apos $d:adig] $ct*)
|`(`[teval |$n:apos] &neg $ct*) => `(`[teval |⌢ $n] $ct*)
|`(`[teval |⌢$n:apos] &neg $ct*) => `(`[teval |$n:apos] $ct*)

-- Executing continuation, ⊹, ⁻¹
macro_rules
|`(`[teval |⋄] &⊹ $ct*) => `(`[teval |‡] $ct:contelem*)
|`(`[teval |‡] &⊹ $ct*) => `(`[teval |‡⋄] $ct:contelem*)
|`(`[teval |$n:apos ⋄] &⊹ $ct*) => `(`[teval |$n:apos ‡] $ct:contelem*)
|`(`[teval |$n:apos ‡] &⊹ $ct*) => `(`[teval |$n:apos] &⊹ &||⋄ $ct:contelem*)
|`(`[teval |⌢ $n:apos ⋄⋄] &⊹ $ct*) => `(`[teval |⌢ $n:apos ⋄] &⊹ &||‡ $ct:contelem*)
|`(`[teval |⌢ $n:apos ‡⋄] &⊹ $ct*) => `(`[teval |⌢ $n:apos ‡] &⊹ &||‡ $ct:contelem*)
|`(`[teval |⌢ ⋄] &⊹ $ct*) => `(`[teval |‡] $ct:contelem*)
|`(`[teval |⌢ ‡] &⊹ $ct*) => `(`[teval |⋄] $ct:contelem*)
|`(`[teval |⌢ ‡⋄] &⊹ $ct*) => `(`[teval |⌢ ‡] $ct:contelem*)
|`(`[teval |⌢ ⋄$p] &⊹ $ct*) => `(`[teval |⌢ $p:adig] &⊹ $ct:contelem*)
|`(`[teval |⌢ $n:apos ‡] &⊹ $ct*) => `(`[teval |⌢ $n:apos ⋄] $ct:contelem*)
|`(`[teval |$n:apos] &⁻¹ $ct*) => `(`[teval| ⌢$n:apos] &⊹ &neg $ct*)
|`(`[teval |⌢$n:apos] &⁻¹ $ct*) => `(`[teval| $n:apos] &⊹ &neg $ct*)

-- Executing continuation, +, -
macro_rules
|`(`[teval |$n:apos ⋄] |$m:apos $p:adig] &+ $ct*) => `(`[teval |$n:apos] |$m:apos] &+ &||$p $ct*)
|`(`[teval |$n:apos ‡] |$m:apos ⋄] &+ $ct*) => `(`[teval |$n:apos] |$m:apos] &+ &||‡ $ct*)
|`(`[teval |$n:apos ‡] |$m:apos ‡] &+ $ct*) => `(`[teval |$n:apos] |$m:apos] &+ &⊹ &||⋄ $ct*)
|`(`[teval |⋄] |$m:apos] &+ $ct*) => `(`[teval |$m:apos] $ct*)
|`(`[teval |‡] |$m:apos] &+ $ct*) => `(`[teval |$m:apos] &⊹ $ct*)
|`(`[teval |$m:apos] |$p:adig] &+ $ct*) => `(`[teval |$p:adig] |$m:apos] &+ $ct*)

macro_rules
|`(`[teval |⌢$n:apos ⋄] |$m:apos $p:adig] &+ $ct*) => `(`[teval |⌢$n:apos] |$m:apos] &+ &||$p $ct*)
|`(`[teval |⌢$n:apos ‡] |$m:apos ⋄] &+ $ct*) => `(`[teval |$n:apos] |$m:apos] &+ &||⋄ &⁻¹ $ct*)
|`(`[teval |⌢$n:apos ‡] |$m:apos ‡] &+ $ct*) => `(`[teval |⌢$n:apos] |$m:apos] &+ &||⋄ $ct*)
|`(`[teval |⌢⋄] |$m:apos] &+ $ct*) => `(`[teval |$m:apos] $ct*)
|`(`[teval |⌢‡] |$m:apos] &+ $ct*) => `(`[teval |$m:apos] &⁻¹ $ct*)
|`(`[teval |⌢$m:apos] |⋄] &+ $ct*) => `(`[teval |⌢$m:apos] $ct*)
|`(`[teval |⌢$m:apos] |‡] &+ $ct*) => `(`[teval |⌢$m:apos] &⊹ $ct*)

macro_rules
|`(`[teval |$p:apos] |⌢ $q:apos] &+ $ct*) => `(`[teval |⌢ $q:apos] |$p:apos] &+ $ct*)
|`(`[teval |⌢$p:apos] |⌢ $q:apos] &+ $ct*) => `(`[teval |$p:apos] |$q:apos] &+ &neg $ct*)

-- Executing continuation, *
macro_rules
|`(`[teval |$n:apos] |$m:apos ⋄] &* $ct*) => `(`[teval |$n:apos] |$m:apos] &* &||⋄ $ct*)
|`(`[teval |$n:apos] |$m:apos ‡] &* $ct*) => `(`[teval |$n:apos] |$m:apos] &* &||⋄ |$n:apos] &+ $ct*)
|`(`[teval |$_:apos] |⋄] &* $ct*) => `(`[teval |⋄] $ct*)
|`(`[teval |$n:apos] |‡] &* $ct*) => `(`[teval |$n:apos] $ct*)
|`(`[teval |⌢$n:apos] |$m:apos] &* $ct*) => `(`[teval |$n:apos] |$m:apos] &* &neg $ct*)
|`(`[teval |$n:apos] |⌢$m:apos] &* $ct*) => `(`[teval |$n:apos] |$m:apos] &* &neg $ct*)
|`(`[teval |⌢$n:apos] |⌢$m:apos] &* $ct*) => `(`[teval |$n:apos] |$m:apos] &* $ct*)

-- Executing continuation, /
-- Facilities for division
--                  dividend  divisor  quotient remainder
syntax "&divmod_pre(" apos "," apos "," apos "," apos ")" : contelem
syntax "&divmod_dat_chk(" apos "," apos "," apos ")" : contelem
syntax "&divmod_dat_gr_1(" apos "," apos "," apos ")" : contelem
syntax "&divmod_dat_gr_2(" apos "," apos "," apos ")" : contelem

macro_rules
|`(`[teval |$p:apos] |$q:apos] &/ $ct*) => `(`[teval &divmod_pre($p, $q, ⋄, ⋄) &/ $ct*)
|`(`[teval |$p:apos] |$q:apos] &% $ct*) => `(`[teval &divmod_pre($p, $q, ⋄, ⋄) &% $ct*)

macro_rules
|`(`[teval &divmod_dat_chk($_, $q, $_) &/ $ct*) => `(`[teval|$q:apos] $ct*)
|`(`[teval &divmod_dat_chk($_, $_, $r) &% $ct*) => `(`[teval|$r:apos] $ct*)

macro_rules
|`(`[teval &divmod_pre($p$i:adig, $d, $q, $r) $ct*) =>
 `(`[teval &divmod_pre($p, $d, $q, $r) &||$i $ct*)
|`(`[teval &divmod_pre($i:adig, $d, $q, $r) $ct*) =>
 `(`[teval &divmod_dat_chk($d, $q, $r) &||$i $ct*)
macro_rules
|`(`[teval &divmod_dat_chk($d, $q, $r) &||$i $ct*) =>
 `(`[teval |$r:apos$i] |$d:apos] &- &divmod_dat_chk($d, $q⋄, $r$i) $ct*)
macro_rules
|`(`[teval |$l:apos] &divmod_dat_gr_1($d, $q, $_) $ct*) =>
 `(`[teval |$q:apos] &⊹ &divmod_dat_gr_2($d, $q, $l) $ct*)
|`(`[teval |$l:apos] &divmod_dat_gr_2($d, $_, $r) $ct*) =>
 `(`[teval &divmod_dat_chk($d, $l, $r) $ct*)

macro_rules
|`(`[teval |$i:apos⋄] &divmod_dat_chk($d, $q, $r) $ct*) =>
 `(`[teval |$i:apos] &divmod_dat_chk($d, $q, $r) $ct*)
|`(`[teval |⌢$i:apos⋄] &divmod_dat_chk($d, $q, $r) $ct*) =>
 `(`[teval |⌢$i:apos] &divmod_dat_chk($d, $q, $r) $ct*)
|`(`[teval |$_:apos‡] &divmod_dat_chk($d, $q, $r) $ct*) =>
 `(`[teval |$r:apos] |$d:apos] &- &divmod_dat_gr_1($d, $q, $r) $ct*)
|`(`[teval |‡] &divmod_dat_chk($d, $q, $r) $ct*) =>
 `(`[teval |$r:apos] |$d:apos] &- &divmod_dat_gr_1($d, $q, $r) $ct*)
|`(`[teval |⌢$_:apos‡] &divmod_dat_chk($d, $q, $r) $ct*) =>
 `(`[teval &divmod_dat_chk($d, $q, $r) $ct*)
|`(`[teval |⌢‡] &divmod_dat_chk($d, $q, $r) $ct*) =>
 `(`[teval &divmod_dat_chk($d, $q, $r) $ct*)
|`(`[teval |⋄] &divmod_dat_chk($d, $q, $_) $ct*) =>
 `(`[teval |$q:apos] &⊹ &divmod_dat_gr_2($d, $q, ⋄) $ct*)
|`(`[teval |⌢⋄] &divmod_dat_chk($d, $q, $_) $ct*) =>
 `(`[teval |$q:apos] &⊹ &divmod_dat_gr_2($d, $q, ⋄) $ct*)

macro_rules
|`(`[teval |⌢$p:apos] |$q:apos] &/ $ct*) => `(`[teval |$p:apos] |$q:apos] &/ &neg $ct*)
|`(`[teval |$p:apos] |⌢$q:apos] &/ $ct*) => `(`[teval |$p:apos] |$q:apos] &/ &neg $ct*)
|`(`[teval |⌢$p:apos] |⌢$q:apos] &/ $ct*) => `(`[teval |$p:apos] |$q:apos] &/ $ct*)
|`(`[teval |⌢$p:apos] |$q:apos] &% $ct*) => `(`[teval |$p:apos] |$q:apos] &% &neg $ct*)
|`(`[teval |$p:apos] |⌢$q:apos] &% $ct*) => `(`[teval |$p:apos] |$q:apos] &% $ct*)
|`(`[teval |⌢$p:apos] |⌢$q:apos] &% $ct*) => `(`[teval |$p:apos] |$q:apos] &% &neg $ct*)

-- Turning apos or pneg into term
inductive Pnum where
| P? : Pnum
| P0 : Pnum → Pnum
| P1 : Pnum → Pnum
| P_ : Pnum → Pnum
open Pnum

macro_rules
|`(`[teval |⋄]) => `(P0 P?)
|`(`[teval |‡]) => `(P1 P?)
|`(`[teval |⌢ ⋄]) => `(P0 (P_ P?))
|`(`[teval |⌢ ‡]) => `(P1 (P_ P?))
|`(`[teval |$p:apos ⋄]) => `(P0 (`[teval |$p:apos]))
|`(`[teval |$p:apos ‡]) => `(P1 (`[teval |$p:apos]))
|`(`[teval |⌢ $p:apos ⋄]) => `(P0 (`[teval |⌢ $p:apos]))
|`(`[teval |⌢ $p:apos ‡]) => `(P1 (`[teval |⌢ $p:apos]))




#check `[teval| ⌢⋄]
#check `[teval| ‡⊹]
#check `[teval| ⋄⋄⊹]
#check `[teval| ⌢⋄⊹]
#check `[teval| ⌢‡⊹]
#check `[teval| ⌢‡⋄⊹]
#check `[teval| ⌢‡‡⊹]
#check `[teval| ⌢‡‡⋄⊹]
#check `[teval| ⌢‡‡‡⊹]
#check `[teval| ⌢‡‡⊹] &neg &⊹ &neg &neg
#check `[teval| (‡)⊹]
#check `[teval| (⋄‡⋄‡+(‡‡))⊹⁻¹] &⊹

#check `[teval| (⌢‡)-(⌢‡)]
#check `[teval| ‡‡-(⌢‡)]
#check `[teval| (⌢‡‡ * ⋄) + ‡‡ * ‡‡]

#check `[teval| ‡ / ‡‡]
set_option trace.Elab.step true in
#check `[teval| ((⌢‡‡‡‡ * ‡⋄ - ‡) % (⌢‡‡⋄))]
#check `[teval|⌢⋄⋄]&⊹





-- `!!!!!!!!!!!!!!! Bexp`

-- Boolean Expression
declare_syntax_cat bexp

syntax aexp "=" aexp : bexp
syntax aexp "<" aexp : bexp
syntax aexp ">" aexp : bexp
syntax:90 "¬" bexp : bexp
syntax:60 bexp:60 "||" bexp:61 : bexp
syntax:70 bexp:70 "&&" bexp:71 : bexp
syntax "(" bexp ")" : bexp

-- Continuation
syntax "|" aexp "]" : contelem
syntax "|" bexp "]" : contelem
syntax "&=" : contelem
syntax "&<" : contelem
syntax "&>" : contelem
syntax "&OR" : contelem
syntax "&AND" : contelem
syntax contelem+ : cont

syntax "`[beval" cont : term