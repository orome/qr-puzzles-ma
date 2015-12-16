(* Mathematica Package *)

(* :Title: GCHQ *)
(* :Context: GCHQ` *)
(* :Author: Roy Levien *)
(* :Date: 2015-12-15 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2015 Roy Levien *)
(* :Keywords: *)
(* :Discussion: *)


(* USAGE:

SetDirectory[NotebookDirectory[]];
Get["GCHQ`"];
showTable[constraints]
showTable[solve[constraints]]

*)

BeginPackage["GCHQ`"]
(* Exported symbols added here with SymbolName::usage *)

(* ::Package:: *)

(* ==================== Definitions *)

(* Some constants for use in display, etc. *)
unknown = "-";
cellGraphics = {
  1 -> Graphics[{Black, Rectangle[]}, ImageSize -> 20],
  0 -> Graphics[{White, Rectangle[]}, ImageSize -> 20],
  unknown -> Graphics[{GrayLevel[.95], Rectangle[]}, ImageSize -> 20]};
gridSpecs = Sequence[Frame -> None, Alignment -> Center, ItemSize -> {1.25, 1.25}, Spacings -> {0.2, 0.2}];

(* A function to display results *)
showTable[t_, {cr_, cc_}] := Grid[Join[
  Transpose@Join[ConstantArray["", {9, 9}], (Style[#, Bold]& /@ PadLeft[#, 9, ""]& /@ cc)],
  MapThread[Join, {(Style[#, Bold]& /@ PadLeft[#, 9, ""]& /@ cr), (t /. cellGraphics)}]
], gridSpecs];
showTable[t_]:=Grid[t/.cellGraphics, gridSpecs];


(* ==================== Problem statement *)

(* The "clues" along the sides of the puzzle *)
dim = 25; (* TBD - Needs to be generalized <<< *)
clues = {
  {{7, 3, 1, 1, 7}, {1, 1, 2, 2, 1, 1}, {1, 3, 1, 3, 1, 1, 3, 1}, {1, 3, 1, 1, 6, 1, 3, 1},
    {1, 3, 1, 5, 2, 1, 3, 1}, {1, 1, 2, 1, 1}, {7, 1, 1, 1, 1, 1, 7}, {3, 3}, {1, 2, 3, 1, 1, 3, 1, 1, 2},
    {1, 1, 3, 2, 1, 1}, {4, 1, 4, 2, 1, 2}, {1, 1, 1, 1, 1, 4, 1, 3}, {2, 1, 1, 1, 2, 5}, {3, 2, 2, 6, 3, 1},
    {1, 9, 1, 1, 2, 1}, {2, 1, 2, 2, 3, 1}, {3, 1, 1, 1, 1, 5, 1}, {1, 2, 2, 5}, {7, 1, 2, 1, 1, 1, 3},
    {1, 1, 2, 1, 2, 2, 1}, {1, 3, 1, 4, 5, 1}, {1, 3, 1, 3, 10, 2}, {1, 3, 1, 1, 6, 6},
    {1, 1, 2, 1, 1, 2}, {7, 2, 1, 2, 5}},
  {{7, 2, 1, 1, 7}, {1, 1, 2, 2, 1, 1}, {1, 3, 1, 3, 1, 3, 1, 3, 1}, {1, 3, 1, 1, 5, 1, 3, 1},
    {1, 3, 1, 1, 4, 1, 3, 1}, {1, 1, 1, 2, 1, 1}, {7, 1, 1, 1, 1, 1, 7}, {1, 1, 3}, {2, 1, 2, 1, 8, 2, 1},
    {2, 2, 1, 2, 1, 1, 1, 2}, {1, 7, 3, 2, 1}, {1, 2, 3, 1, 1, 1, 1, 1}, {4, 1, 1, 2, 6}, {3, 3, 1, 1, 1, 3, 1},
    {1, 2, 5, 2, 2}, {2, 2, 1, 1, 1, 1, 1, 2, 1}, {1, 3, 3, 2, 1, 8, 1}, {6, 2, 1}, {7, 1, 4, 1, 1, 3}, {1, 1, 1, 1, 4},
    {1, 3, 1, 3, 7, 1}, {1, 3, 1, 1, 1, 2, 1, 1, 4}, {1, 3, 1, 4, 3, 3}, {1, 1, 2, 2, 2, 6, 1}, {7, 1, 3, 2, 1, 1}}
};

(* Constraints (thusfar) in the puzzle) *)
constraints = ConstantArray[unknown, {dim, dim}];
(constraints[[Sequence @@ #]] = 1)& /@ {
  {4, 4}, {4, 5}, {4, 13}, {4, 14}, {4, 22},
  {9, 7}, {9, 8}, {9, 11}, {9, 15}, {9, 16}, {9, 19},
  {17, 7}, {17, 12}, {17, 17}, {17, 21},
  {22, 4}, {22, 5}, {22, 10}, {22, 11}, {22, 16}, {22, 21}, {22, 22}
};


(* ==================== Solutuon *)

(*
The approach is to progressively refine the constraints until they are free of unknowns.
- (spec: the clue, expressed as a list of all possible combinations of the runs of 0s and 1s)
- possible: All the possible strips matching a given clue.
- constraint: The possible expressed as pattern
*)

possible[clue_] := Module[{spec},
  spec = Module[{specN},
    specN[n_] := Switch[n, 1, #, -1, Join[{0}, #, {0}], 0, {Append[#, 0], Prepend[#, 0]}]& /@
        (Union @@ (Permutations /@ (IntegerPartitions[dim - Plus @@ clue, {Length[clue] + n}])));
    Riffle[#, clue]& /@ Union[specN[-1], Union @@ specN[0], specN[1]]];
  Flatten[{ConstantArray[0, #[[1]]], ConstantArray[1, #[[2]]]}& /@ Partition[Append[#, 0], 2]]& /@ spec]

constraint[_, const_] := const /; isDone[const];
constraint[poss_, const_] := Module[{constrainedPoss = Cases[poss, const /. unknown -> _]},
  Switch[#, Length[constrainedPoss], 1, 0, 0, _, unknown]& /@ (Thread[Total[#]&@constrainedPoss])]

isDone[strip_] := FreeQ[strip, unknown];


(* All the time here is taken up by the initialization of possRows and possCols; after that, the solution is fast *)
(* Has a possibly unnecessary refinement of sol using possColls if the preceeding possRow step has completed the
puzzle; not worth checking. *)

solve[const_,  {cr_, cc_}] := Module[{possRows = possible[#]& /@ cr, possCols = possible[#]& /@ cc, sol = const},
  While[Not@isDone@Flatten@sol,
    sol = Transpose@MapThread[constraint, {possRows, sol}];
    sol = Transpose@MapThread[constraint, {possCols, sol}];];
  sol]

(*showTable[constraints]*)

Begin["`Private`"]

End[] (* `Private` *)

EndPackage[]