(* ::Package:: *)

(*Implementation of SHA-3 hash function (i.e. a family of sponge functions using KECCAK-p permutation as
 the underlying function). For further information, refer to FIPS PUB 202, "SHA-3 Standard: Permutation-Based Hash
 and Extendable-Output Functions" at https://www.nist.gov/publications/sha-3-standard-permutation-based-hash-and-extendable-output-functions*)



(*Utilities*)

(*Generate pseudorandom hex string equivalent with an integer in the range {0,...,imax}*)
RandomHexString[imax_]:=IntegerString[RandomInteger[imax],16]

(* 
SHA-3 conversion function from hexadecimal string to the corresponding binary List.
INPUT:
H: input hex string; |H|=2m, for some positive integer m;
n: number of bits to take. It is a positive integer such that n\[LessEqual]8m.
OUTPUT:
bit string S such that l(S)=n.
*)
h2b[H_,n_]:=Module[{hexlist,tmp1,tmp2},

tmp1=IntegerDigits[FromDigits[H,16],16];
hexlist=Join[Table[0,{i,StringLength[H]-Length[tmp1]}],tmp1];
tmp2=Map[IntegerDigits[16*#[[1]]+#[[2]],2]&,Partition[hexlist,2]];
Take[Flatten@Map[Reverse@Join[Table[0,8-Length[#]],#]&,tmp2],n]
]

(*
SHA-3 conversion function from binary List to the corresponding hex string
INPUT:
S: input binary List (its length must be a multiple of 4).
OUTPUT:
hexadecimal string H with 2*upperfloor(n/8) digits.
*)
b2h[S_]:=Module[{n,m,tmp1,tmp2},

n = Length[S];
m=Length[S]/4;(*number of hex chars of the output*)
tmp1=Flatten@Map[
	Partition[Reverse[#],4]&, 
	Partition[Join[S,Table[0,Mod[-n,8]]],8](*make the List length divisible by 8*)
];
tmp2=IntegerString[FromDigits[tmp1,2],16];
(*Prepends the right amount of 0s that were ignored before*)
StringJoin[StringRepeat["0",m-StringLength[tmp2]],tmp2]
]

(*
Rotate the bits in the integer n to the right by m places
INPUT:
n: input number;
m: number of single bit right rotations;
w: number of significant bits to consider (must be greater than Log[2,n]);
*)
BitRotateRight[n_, m_Integer,w_] := BitShiftRight[n, m] + BitShiftLeft[Mod[n, 2^m], (w - m)]

(*
Rotate the bits in the integer n to the left by m places  
n: input number;
m: number of single bit left rotations;
w: number of significant bits to consider (must be greater than Log[2,n]); 
*)
BitRotateLeft[n_, m_Integer,w_] :=  Mod[BitShiftLeft[n, m], 2^w]+ BitShiftRight[n, (w - m)]
(*Rotate the binary bits in the integer n to the left one time*)
BitRotateLeft[n_,w_] := BitRotateLeft[n, 1, w]

(* Complement the bits in the integer n *)
ComplementBits[n_Integer]:=BitXor[n,2^IntegerPart[Log[2,n]+1]-1]

(*Convert a bit string (represented by a List) into a state array.
INPUT:
str: input bit string (represented by a List);
w: length of state lane;
OUTPUT:
A 5-by-5-by-w array representing the state.
*)
StringToStateArray[str_,w_]:=Partition[Map[FromDigits[#,2]&,Partition[str,w]],5,5]

(*Convert a state array back into a bit string (represented by a List).
INPUT:
state: 5-by-5-by-w input state array;
w: length of state lane.
OUTPUT:
Bit string equivalent to the state (represented by a List).
*)
StateArrayToString[state_,w_]:=Module[{pad},
pad = Table[0,w];
Flatten[Map[Take[Join[pad,IntegerDigits[#,2]],-w]&,Flatten[state]]]
]


(*Keccak-p permutation step mappings*)


(*
XOR each bit in the state with the parities of two columns in the state array
INPUT:
state: 5-by-5-by-w input state array;
w: length of state lane.
OUTPUT:
Updated state array.
*)
Theta[state_,w_] := Module[{CV, DV},
  (
   CV = Map[BitXor @@ # &, Transpose[state]];
   DV = MapThread[
     BitXor, {RotateLeft[CV], Map[BitRotateLeft[#,w]&, RotateRight[CV]]}];
   MapThread[BitXor, {state, DV}]
   )]


(*
Rotate the bits of each lane by an offset which depends on the fixed coordinates of the lane
INPUT:
state: 5-by-5-by-w input state array;
w: length of state lane.
OUTPUT:
Updated state array.
*)
Rho[state_,w_] := Module[{startingpoint, indices, tmp1, tmp2, tmp3, offsets},
  (
   startingpoint = {1, 0};
   
   indices = NestList[{#[[2]], Mod[2 #[[1]] + 3 #[[2]], 5]} &, startingpoint, 23];
   
   (*Associa agli indici corrispondenti dello stato (con gli indici che partono da 0) l'offset della bit rotation da compiere*)
   tmp1 = Map[{#[[1]], (#[[2]] + 1) (#[[2]] + 2)/2} &, Map[{#[[1]], #[[2]] - 1} &, MapIndexed[{#1, #2} &, indices]]];
   tmp2 = Sort[tmp1];
   
   (*La parola di coordinate (0,0) non viene shiftata*)
   tmp3 = PrependTo[tmp2, {{0, 0}, {0}}];
   offsets = Flatten@Map[Mod[#[[2]],w] &, tmp2];
   
   (*Ruota ogni parola a sinistra dell'offset previsto*)
   Partition[MapThread[BitRotateLeft[#1, #2, w] &, {Flatten[state], offsets}], 5]
   )]


(*
Rearrange the position of the lanes
INPUT:
state: 5-by-5-by-w input state array.
OUTPUT:
Updated state array.
*)
pi[state_] := Table[state[[##]] & @@ (1 + {Mod[x + 3 y , 5], x}), {x, 0, 4}, {y, 0, 4}]


(*
XOR each bit of the state with a nonlinear function
INPUT:
state: 5-by-5-by-w input state array.
OUTPUT:
Updated state array.
*)
Chi[state_]:=
	Table[BitXor[state[[x+1,y+1]], BitAnd[ComplementBits[state[[Mod[x+1,5]+1,y+1]]],state[[Mod[x+2,5]+1,y+1]]]],{x,0,4},{y,0,4}]



(*
LFSR upon which round constant function is built
INPUT:
R: 8-bit register.
OUTPUT:
Updated register.
*)
lfsr[R_]:=Module[{r8},
r8=R[[-1]];
Most[MapAt[BitXor[#,r8]&,Join[{0},R],{{1},{5},{6},{7}}]]
]

(*
Round constant function generator used by \iota permutation
*)
rc[t_]:=Module[{iter,R,i},
iter = Mod[t,255];
R= {1,0,0,0,0,0,0,0};
Nest[lfsr,R,iter][[1]]
]


(*
Modify some of the bits of the lane (1,1) ((0,0) in the original paper but Mathematica starts
indexing from 1) in a manner that depends on the round index i_r
INPUT:
state: 5-by-5-by-w input state array;
w: length of state lane;
l: Log[2,(b/25)]
ir: round index;
OUTPUT:
Updated state array.
*)
Iota[state_,w_,l_,ir_]:=Module[{RC,j},
RC=Table[0,w];

For[j=0,j<l,j++,
RC[[2^j]]= rc[j+7ir]
];

{MapAt[BitXor[#,FromDigits[RC,2]]&,state,{1,1}],w,l}
]


(* Round function of KECCAK-p *)
Rnd[{A_,w_,l_},ir_]:=Iota[Chi[pi[Rho[Theta[A,w],w]]],w,l,ir]


(*KECCAK-p[b,nr](S) implementation. Notice that KECCAK-p[b,12+2l] is equivalent to KECCAK-f[b].
INPUT:
b: state array dimension, b \in {25, 50, 100, 200, 400, 800, 1600};
nr: number of times the round function Rnd executes, nr = 12 + 2l with l=Log[2,b/25];
str: input bit List.
OUTPUT:
Bit List of length b.
*)
KECCAKp[b_,nr_][str_]:= Module[{w,l,state,roundindices},
w=b/25;
l=Log[2,b/25];
state=StringToStateArray[str,w];
roundindices=Table[i,{i,12+2l-nr,12+2l-1}];
StateArrayToString[Fold[Rnd,{state,w,l},roundindices][[1]],w]
]


(*
pad10*1 multirate padding rule
INPUT:
x: positive integer (length of the rate r)
m: nonnegative integer (length of the List to pad)
*)
pad[x_,m_]:=Join[{1},Table[0,Mod[-m-2,x]],{1}]


(* Sponge function 
INPUT:
f: sponge construction underlying function, of the form KECCAK-p[b,nr];
pad: padding rule used by the sponge function;
r: sponge rate (r<b);
b: state array dimension;
N: input bit List;
d: number of output bits expected, d\[GreaterEqual]0.
OUTPUT:
bit List such that l(Z)=d.
*)
Sponge[f_,pad_,r_,b_][N_,d_]:=Module[{P,c,tmp1,S,Z},
P=Join[N,pad[r,Length[N]]];
c=b-r;

(*Absorbing phase*)
tmp1=Map[Join[#,Table[0,c]]&,Partition[P,r]];
S=Fold[f[BitXor[#1,#2]]&,Table[0,b],tmp1];

(*Squeezing phase*)
Take[
	NestWhile[
		{Join[#[[1]],Take[#[[2]],r]],f[#[[2]]] }&,{Take[S,r],f[S]},d>Length[#[[1]]]& 
	][[1]],
d]
]


(* 
KECCAK family of sponge functions with the KECCAK-p[b, 12+2l] permutation as the underlying 
function and pad10*1 as the padding rule
c: capacity of the sponge function;
N: input bit List;
d: number of output bits expected, d\[GreaterEqual]0.
*)
KECCAK[c_][N_,d_]:=Sponge[KECCAKp[1600,24],pad,1600-c,1600][N,d]


(*
Standard SHA-3 hash functions. The suffix '01' is added to the input to distinguish inputs of
hash functions from inputs of XOFs (Extended Output Functions, not treated here).
INPUT:
M: input hex string that represent the message to hash
OUTPUT:
H(M).
*)
SHA3::unexpectedinput="Input hexadecimal string of even length expected";

SHA3224[M_]:= 
If[
EvenQ@StringLength[M],
b2h[KECCAK[448][Join[h2b[M,4*StringLength[M]],{0,1}],224]],
Message[SHA3::unexpectedinput]
]

SHA3256[M_]:= 
If[
EvenQ@StringLength[M],
b2h[KECCAK[512][Join[h2b[M,4*StringLength[M]],{0,1}],256]],
Message[SHA3::unexpectedinput]
]

SHA3384[M_]:= 
If[
EvenQ@StringLength[M],
b2h[KECCAK[768][Join[h2b[M,4*StringLength[M]],{0,1}],384]],
Message[SHA3::unexpectedinput]
]

SHA3512[M_]:= 
If[
EvenQ@StringLength[M],
b2h[KECCAK[1024][Join[h2b[M,4*StringLength[M]],{0,1}],512]],
Message[SHA3::unexpectedinput]
]
