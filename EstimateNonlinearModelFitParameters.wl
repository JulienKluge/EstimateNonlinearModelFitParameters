(* ::Package:: *)

ClearAll[EstimateNonlinearModelFitParameters];
Options[EstimateNonlinearModelFitParameters]=Options[]={MaxIterations->Automatic,Assumptions->$Assumptions,TimeConstraint->Automatic}
EstimateNonlinearModelFitParameters[data_List,{form_,cons_},parameters_List,var_Symbol,opts:OptionsPattern[]]:=internalENMFP[
parseENMFPData[data],
form,
cons,
Splice[parseENMFPParameters[parameters]],
var,
opts
]
EstimateNonlinearModelFitParameters[data_List,form_,parameters_List,var_Symbol,opts:OptionsPattern[]]:=internalENMFP[
parseENMFPData[data],
form,
True,
Splice[parseENMFPParameters[parameters]],
var,
opts
]


parseENMFPData[data_List]:=Switch[data,List[List[_?NumericQ,_?NumericQ]..],data,List[(_?NumericQ)..],Transpose[{Range[Length@data],data}],_,$Failed]


parseENMFPParameters[params_]:=Transpose[If[Head[#]===List,If[MatchQ[#,List[_Symbol,_?NumericQ]],{#[[1]],{#[[2]]}},{$Failed,$Failed}],{#[[2]],{}}]&/@params]


internalENMFP[data_List[List[_?NumericQ,_?NumericQ]..],form_,cons_,parameters_List[(_Symbol)..],guesses_List[(List[]|List[_?NumericQ])..],var_Symbol]
internalENMFP[___]:=$Failed
