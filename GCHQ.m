(* Mathematica Package *)

(* :Title: GCHQ *)
(* :Context: GCHQ` *)
(* :Author: Roy Levien *)
(* :Date: 2015-12-16 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2015 Roy Levien *)
(* :Keywords: *)
(* :Discussion: *)


(* USAGE:

SetDirectory[NotebookDirectory[]];
Get["GCHQ`"];
showTable[givensGCHQ, cluesGCHQ]
solutionGCHQ = solve[cluesGCHQ, givensGCHQ];
showTable[solutionGCHQ, cluesGCHQ]

*)

BeginPackage["GCHQ`"]
(* Exported symbols added here with SymbolName::usage *)

(* ::Package:: *)


(* ==================== Solution *)

(*
The approach is to progressively refine the constraints until they are free of unknowns.
Starting with clues, and optional givens, generated from knowns (see).
*)

(* All the time here is taken up by the initialization of poss; after that, the solution is fast *)
(* Has a possibly unnecessary refinement of sol using poss[[2]] if the preceeding poss[[1]] step has completed the
puzzle; not worth checking. *)

solve[clues_, given_] := Module[{poss = possibles[clues], sol},
  FixedPoint[(sol = Transpose@MapThread[constraint, {poss[[1]], #}];
  sol = Transpose@MapThread[constraint, {poss[[2]], sol}])&, given]];
solve[clues_] := solve[clues, unconstrained[Length/@clues]];

givens[dims_, knowns_:{{},{}}] := Module[{const =  unconstrained[dims]},
  (const[[Sequence @@ #]] = 1)& /@ knowns[[1]]; (const[[Sequence @@ #]] = 0)& /@knowns[[2]]; const
];


(* ==================== Display *)

(* A function to display results *)
showTable[t_, {cr_, cc_}] := Grid[Join[
  Transpose@Join[ConstantArray["", {9, 9}], (Style[#, Bold]& /@ PadLeft[#, 9, ""]& /@ cc)],
  MapThread[Join, {(Style[#, Bold]& /@ PadLeft[#, 9, ""]& /@ cr), (t /. cellGraphics)}]
], gridSpecs];
showTable[t_]:=Grid[t/.cellGraphics, gridSpecs];


(* ==================== Generation *)

(*
New puzzles can be generated from data. Clues directly from the data, and suggested (sufficient) knowns
follwing an attempted solution.

E.g.:

goalFromQR = ImageData[BarcodeImage["www.github.com/","QR",25]]
cluesFromQR = clues[1-goalFromQR];
givensFromQR = givens[Length/@cluesFromQR, knowns[goalFromQR,solve[cluesFromQR]]];
*)

clues[data_]:={(Length/@Select[Split[#],FreeQ[#,0]&])&/@data, (Length/@Select[Split[#],FreeQ[#,0]&])&/@Transpose@data}

knowns[goal_,const_] := Intersection[Position[goal,#] , Position[const,"-"]]&/@{1,0};

unconstrained[dims_] := ConstantArray[unknown, dims];

(* ==================== GCHQ Problem statement *)

(* Define values for GCHQ 2015 puzzle *)

(* The "clues" along the sides of the puzzle *)
cluesGCHQ = {
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

(* The initial constraints *)
givensGCHQ = givens[Length/@cluesGCHQ,
  { {{4, 4}, {4, 5}, {4, 13}, {4, 14}, {4, 22}, {9, 7}, {9, 8}, {9, 11}, {9, 15}, {9, 16}, {9, 19},
     {17, 7}, {17, 12}, {17, 17}, {17, 21}, {22, 4}, {22, 5}, {22, 10}, {22, 11}, {22, 16}, {22, 21}, {22, 22}},
    {}
  }];



Begin["`Private`"]

(* Some constants for use in display, etc. *)
unknown = "-";
cellGraphics = {
  1 -> Graphics[{Black, Rectangle[]}, ImageSize -> 20],
  0 -> Graphics[{White, Rectangle[]}, ImageSize -> 20],
  unknown -> Graphics[{GrayLevel[.95], Rectangle[]}, ImageSize -> 20]};
gridSpecs = Sequence[Frame -> None, Alignment -> Center, ItemSize -> {1.25, 1.25}, Spacings -> {0.2, 0.2}];

(* TBD - This needs some fixing to be made clearer and more efficient *)
(*spec: the clue, expressed as a list of all possible combinations of the runs of 0s and 1s *)
(*possible: All the possible strips matching a given clue. *)
possible[clue_, dim_] := Module[{spec},
  spec = Module[{specN},
    specN[n_] := Switch[n, 1, #, -1, Join[{0}, #, {0}], 0, {Append[#, 0], Prepend[#, 0]}]& /@
        (Union @@ (Permutations /@ (IntegerPartitions[dim - Plus @@ clue, {Length[clue] + n}])));
    Riffle[#, clue]& /@ Union[specN[-1], Union @@ specN[0], specN[1]]];
  Flatten[{ConstantArray[0, #[[1]]], ConstantArray[1, #[[2]]]}& /@ Partition[Append[#, 0], 2]]& /@ spec]
possibles[clues_] := With[{dims = Length/@clues},
  {possible[#, dims[[2]]]& /@ clues[[1]], possible[#, dims[[1]]]& /@ clues[[2]]}];

constraint[_, const_] := const /; FreeQ[const, unknown];
constraint[poss_, const_] := Module[{constrainedPoss = Cases[poss, const /. unknown -> _]},
  Switch[#, Length[constrainedPoss], 1, 0, 0, _, unknown]& /@ (Thread[Total[#]&@constrainedPoss])];



(*isDone[strip_] := FreeQ[strip, unknown];*)

End[] (* `Private` *)



EndPackage[]