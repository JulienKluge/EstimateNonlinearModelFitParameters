(* ::Package:: *)

Get["Misc.wl"]


ClearAll[StripFrontHeads];
StripFrontHeads[pdata_,pmodel_,params_,var_,minDataPoints_,assump_]:=Module[
{data=pdata,model=pmodel,newModel},
	{data,model}=stripFrontHeadsWorker[{data,model},params,var,Max[{minDataPoints,Length[params]+1}]];
	{data,model}
]


ClearAll[stripFrontHeadsWorker];

(*Plus*)
stripFrontHeadsWorker[{data_,arg__Plus},params_,var_,minPoints_]:=Module[{argList=List@@arg,summ,newArgs,newData},
	{summ,newArgs}=({True,False}/.GroupBy[argList,FreeQ[Alternatives@@params],Apply[Plus]])/.{True->0,False->0};
	If[summ=!=0,
		newData=sanitizeData@Quiet[Check[{#1,#2-(summ/.{var->#1})},Nothing]&@@@data];
		If[Length[newData]>=minPoints,
			stripFrontHeadsWorker[{newData,Plus[newArgs]},params,var,minPoints]
		,
			{data,Plus[arg]} (*give up if we would have to sacrifice too many data points*)
		]			
	,
		{data,Plus[arg]}
	]
]

(*Times*)
stripFrontHeadsWorker[{data_,arg__Times},params_,var_,minPoints_]:=Module[{argList=List@@arg,factor,newArgs,newData},
	{factor,newArgs}=({True,False}/.GroupBy[argList,FreeQ[Alternatives@@params],Apply[Times]])/.{True->1,False->1};
	If[factor=!=1,
		newData=sanitizeData@Quiet[Check[{#1,#2/(factor/.{var->#1})},Nothing]&@@@data];
		If[Length[newData]>=minPoints,
			stripFrontHeadsWorker[{newData,Times[newArgs]},params,var,minPoints]
		,
			{data,Times[arg]} (*give up if we would have to sacrifice too many data points*)
		]			
	,
		{data,Times[arg]}
	]
]

(*Power*)
powerBaseInverse[{x_,y_},base_]:=If[TrueQ[base>0]&&TrueQ[base!=1]&&TrueQ[y>0],Check[{x,Log[base,y]},Nothing],Nothing]
powerExponentInverse[{x_,y_},exp_Integer]:=If[TrueQ[TrueQ[exp<0]&&TrueQ[y==0]],Nothing,If[EvenQ[exp],If[TrueQ[y>=0],{x,Surd[y,exp]},Nothing],{x,Surd[y,exp]}]]
powerExponentInverse[{x_,y_},exp_]:=If[TrueQ[TrueQ[exp<0]&&TrueQ[y==0]],Nothing,If[TrueQ[y>=0],{x,y^(1/exp)},Nothing]]
stripFrontHeadsWorker[{data_,arg__Power},params_,var_,minPoints_]:=Module[{argList=List@@arg,base,power,newData},
	base=First[argList];
	power=Power@@(argList[[2;;-1]]);
	If[FreeQ[base,Alternatives@@params],
		(*base is free*)
		newData=sanitizeData@Quiet[powerBaseInverse[{#1,#2},base/.{var->#1}]&@@@data];
		If[Length[newData]>=minPoints,
			stripFrontHeadsWorker[{newData,power},params,var,minPoints]
		,
			{data,Power[arg]} (*give up if we would have to sacrifice too many data points*)
		]
	,
		If[FreeQ[power,Alternatives@@params],
			(*exponent is free*)
			newData=sanitizeData@Quiet[powerExponentInverse[{#1,#2},power/.{var->#1}]&@@@data];
			If[FreeQ[power,var],If[IntegerQ[power],If[EvenQ[power],
				base=RealAbs[base];
				newData={#1,Abs[#2]}&@@@newData;
			]]];
			If[Length[newData]>=minPoints,
				stripFrontHeadsWorker[{newData,base},params,var,minPoints]
			,
				{data,Power[arg]} (*give up if we would have to sacrifice too many data points*)
			]
		,
			{data,Power[arg]} (*nothing to make here*)
		]
	]
]

(*Log*)
stripFrontHeadsWorker[{data_,Log[arg_]},params_,var_,minPoints_]:=stripFrontHeadsWorker[
{{#1,Exp[#2]}&@@@data,arg}, (*paradoxically, this is a very simple case*)
params,var,minPoints
]

(*
sin
cos
tan
asin
acos
atan
sinc
erf
inverf
erfc
inverfc
csc
sec
cosh
sinh
asinh
acosh
*)

stripFrontHeadsWorker[{data_,model_},params_,var_,minPoints_]:={data,model}
