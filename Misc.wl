(* ::Package:: *)

ClearAll[realNumberQ];
SetAttributes[realNumberQ,Listable]
realNumberQ[_Real|_Integer|_Rational]:=True
realNumberQ[_]:=False


ClearAll[sanitizeData];
sanitizeData[data_]:=Select[data,FreeQ[#,Indeterminate|Infinity|DirectedInfinity]&&(And@@realNumberQ[#])&]
