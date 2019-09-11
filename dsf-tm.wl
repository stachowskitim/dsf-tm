#!/Applications/Mathematica.app/Contents/MacOS/MathematicaScript

in = ToString[$ScriptCommandLine[[2]]];

tshift[in_] :=
 Module[
  {a, b,data,tm},
  data = Import[in][[2;;,All]];
  a = Interpolation[data,Method->"Spline"];
  b = a';
  MaxValue[b[x], x];
  list = Table[{x, b[x]}, {x,Min[data],Max[data]}];
  tm=SortBy[list, Last] // Take[#, -1] &;
  Print@tm[[1]][[1]]
  ]

tshift[in]