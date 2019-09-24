(* ::Package:: *)

BeginPackage["tshift`"]

tshift::usage="tshift[dataxy] calculates Tm of x and y data from thermal shift assay"

Begin["Private`"]

tshift[in_] :=
 Module[
  {a, b,original,data,tm,list},
  original = in;
data = Mean/@GatherBy[original,First]; (*remove possible duplicates*)
  a = Interpolation[data,Method->"Hermite"];
  b =a';
  list = Table[{x, b[x]}, {x,Min[data[[1,1]]],Last[data][[1]]}];
  tm=SortBy[list, Last] // Take[#, -1] &;
Return@tm[[1]][[1]];
  ]

End[]

EndPackage[]
