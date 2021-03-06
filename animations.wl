(* ::Package:: *)

SetDirectory @ NotebookDirectory[]


<< SunTzu`


(* ::Section:: *)
(*Multiplication*)


{
  {81, 81},
  {1989, 64},
  {1024, 1024},
  {246, 97531}
} /.
  {x_, y_} :> MultiplyAnimation[x, y, Export -> True]


(* ::Section:: *)
(*Division*)


{
  {100, 6},
  {1989, 64},
  {6561, 9},
  {97531, 4}
} /.
  {x_, y_} :> DivideAnimation[x, y, Export -> True]
