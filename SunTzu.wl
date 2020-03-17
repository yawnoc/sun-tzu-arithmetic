(* ::Package:: *)

(* ::Text:: *)
(*SunTzu.wl (a Wolfram Language Package)*)


(* ::Section:: *)
(*Start of package*)


BeginPackage["SunTzu`"];


(* ::Section:: *)
(*Unprotect and clear existing definitions*)


Unprotect["SunTzu`*"];
ClearAll["SunTzu`*"];
ClearAll["SunTzu`*`*"];


(* ::Section:: *)
(*Mention non-private symbols*)


{
  IntExponent,
  MakeGridRowOptionsPattern,
  MakeGridRow,
  CellStyles,
  StyleGridCell,
  MakeGrid,
  ExportGif,
  AnimationOptionsPattern,
  MultiplyAnimation,
  DivideAnimation
};


(* ::Section:: *)
(*Private scope*)


(* ::Subsection:: *)
(*Start of private scope*)


Begin["`Private`"];


(* ::Subsection:: *)
(*IntExponent*)


IntExponent[integer_Integer] := Length @ IntegerDigits[integer] - 1;


(* ::Subsection:: *)
(*MakeGridRowOptionsPattern*)


MakeGridRowOptionsPattern = OptionsPattern @ {
  "HideLoneZero" -> True,
  "TrailingSpaces" -> 0
};


(* ::Subsection:: *)
(*MakeGridRow*)


MakeGridRow[
  gridColumns_Integer,
  digitList : List[___Integer],
  opts : MakeGridRowOptionsPattern
] :=
  Module[{trailingSpaces, hideLoneZero, displayedDigitList},
    trailingSpaces = OptionValue["TrailingSpaces"];
    hideLoneZero = OptionValue["HideLoneZero"];
    displayedDigitList = digitList;
    If[displayedDigitList == {0} && hideLoneZero,
      displayedDigitList = {}
    ];
    PadLeft[
      Join[
        displayedDigitList,
        ConstantArray[Null, trailingSpaces]
      ],
      gridColumns,
      Null
    ]
  ];


MakeGridRow[
  gridColumns_,
  integer_Integer,
  opts : MakeGridRowOptionsPattern
] :=
  MakeGridRow[gridColumns, IntegerDigits[integer], opts];


(* ::Subsection:: *)
(*CellStyles*)


CellStyles = Association[
  "none" -> Transparent,
  "current" -> Yellow,
  "finished" -> Gray,
  "fraction" -> Pink,
  "result" -> Green,
  "temporary" -> Cyan
];


(* ::Subsection:: *)
(*StyleGridCell*)


StyleGridCell[type_String, rowSpan_List, colSpan_List] :=
  {rowSpan, colSpan} -> CellStyles[type];


StyleGridCell[type_String, row_Integer, colSpec_] :=
  StyleGridCell[type, {row, row}, colSpec];


StyleGridCell[type_String, rowSpec_, col_Integer] :=
  StyleGridCell[type, rowSpec, {col, col}];


(* ::Subsection:: *)
(*MakeGrid*)


MakeGrid[gridRows__List, gridCellStyles___Rule] :=
  Grid[
    List[gridRows],
    Background -> {
      None,
      None,
      List[gridCellStyles]
    },
    Dividers -> {False, {2 -> True, -2 -> True}},
    ItemSize -> All,
    ItemStyle -> Large
  ];


(* ::Subsection:: *)
(*ExportGif*)


ExportGif[nameComponents__] :=
  Export[
    Riffle[ToString /@ {nameComponents}, "-"] <> ".gif",
    #,
    AnimationRepetitions -> Infinity,
    "DisplayDurations" -> 2
  ] &;


(* ::Subsection:: *)
(*AnimationOptionsPattern*)


AnimationOptionsPattern = OptionsPattern @ {
  Export -> False
};


(* ::Subsection:: *)
(*MultiplyAnimation*)


MultiplyAnimation[
  upper_Integer,
  lower_Integer,
  opts: AnimationOptionsPattern
] :=
  Module[
   {upperDigitList, lowerDigitList,
    pMax, qMax, n,
    middle,
    frameList,
    upperDigit, lowerDigit, temporary
   },
    (* Digit lists *)
    upperDigitList = IntegerDigits[upper];
    lowerDigitList = IntegerDigits[lower];
    (*
      Exponents (numbers of digits, minus one):
        pMax (upper)
        qMax (lower)
      Digits indexed from most to least significant:
        p = pMax, ..., 1, 0 (upper)
        q = qMax, ..., 1, 0 (lower)
    *)
    pMax = IntExponent[upper];
    qMax = IntExponent[lower];
    (*
      The number of columns of the grid, taken to be
      the upper bound for the number of digits of the result:
        n
    *)
    n = pMax + qMax + 2;
    (* Initialise middle row *)
    middle = 0;
    (* Reap frames for animation *)
    (*
      Each frame shall consist of a grid of 4 rows:
        1. Upper
        2. Temporary (result of product of digits)
        3. Middle
        4. Lower
     *)
    frameList =
      Last @ Last @ Reap[
        (* Loop through the upper indices *)
        Do[
          (* Initial *)
          Sow @ MakeGrid[
            MakeGridRow[n, upper],
            MakeGridRow[n, 0],
            MakeGridRow[n, middle],
            MakeGridRow[n, lower,
              "TrailingSpaces" -> p
            ],
            (* Finished upper digits *)
            StyleGridCell["finished", 1, {n - pMax, n - p - 1}]
          ];
          (* Loop through the lower indices *)
          Do[
            (* Highlight current digits *)
            Sow @ MakeGrid[
              MakeGridRow[n, upper],
              MakeGridRow[n, 0],
              MakeGridRow[n, middle],
              MakeGridRow[n, lower,
                "TrailingSpaces" -> p
              ],
              (* Finished upper digits *)
              StyleGridCell["finished", 1, {n - pMax, n - p - 1}],
              (* Current upper digit *)
              StyleGridCell["current", 1, n - p],
              (* Current lower digit *)
              StyleGridCell["current", 4, n - (p + q)]
            ];
            (* Multiply current digits *)
            upperDigit = upperDigitList[[pMax + 1 - p]];
            lowerDigit = lowerDigitList[[qMax + 1 - q]];
            temporary = upperDigit * lowerDigit;
            (* Highlight product of current digits *)
            Sow @ MakeGrid[
              MakeGridRow[n, upper],
              MakeGridRow[n, temporary,
                "HideLoneZero" -> False,
                "TrailingSpaces" -> p + q
              ],
              MakeGridRow[n, middle],
              MakeGridRow[n, lower,
                "TrailingSpaces" -> p
              ],
              (* Finished upper digits *)
              StyleGridCell["finished", 1, {n - pMax, n - p - 1}],
              (* Current upper digit *)
              StyleGridCell["current", 1, n - p],
              (* Current digits' product *)
              StyleGridCell[
                "temporary", 2, {
                  n - (p + q) - IntExponent[temporary],
                  n - (p + q)
                }
              ],
              (* Current lower digit *)
              StyleGridCell["current", 4, n - (p + q)]
            ];
            (* Update middle number *)
            middle += temporary * 10 ^ (p + q);
            (* Highlight current digits *)
            Sow @ MakeGrid[
              MakeGridRow[n, upper],
              MakeGridRow[n, 0],
              MakeGridRow[n, middle],
              MakeGridRow[n, lower,
                "TrailingSpaces" -> p
              ],
              (* Finished upper digits *)
              StyleGridCell["finished", 1, {n - pMax, n - p - 1}],
              (* Current upper digit *)
              StyleGridCell["current", 1, n - p],
              (* Current lower digit *)
              StyleGridCell["current", 4, n - (p + q)]
            ];
          , {q, qMax, 0, -1}];
        , {p, pMax, 0, -1}];
        (* Final *)
        Sow @ MakeGrid[
          MakeGridRow[n, upper],
          MakeGridRow[n, 0],
          MakeGridRow[n, middle],
          MakeGridRow[n, lower],
          (* Finished upper digits *)
          StyleGridCell["finished", 1, {n - pMax, n}],
          (* Result *)
          StyleGridCell["result", 3, {n - IntExponent[middle], n}],
          (* Finished lower digits *)
          StyleGridCell["finished", 4, {n - qMax, n}]
        ];
      ];
    (* Return result and list of frames *)
    {
      middle,
      frameList //
        If[OptionValue[Export],
          ExportGif["multiply", upper, lower],
          Identity
        ]
    }
  ];


(* ::Subsection:: *)
(*DivideAnimation*)


DivideAnimation[
  dividend_Integer,
  divisor_Integer,
  opts: AnimationOptionsPattern
] :=
  Module[
   {pMax, qMax, n,
    upper, middle,
    frameList,
    upperDigit, temporary,
    noRemainder
   },
    (*
      Exponents (numbers of digits, minus one):
        pMax (dividend)
        qMax (divisor)
      Digits indexed from most to least significant:
        p = pMax, ..., 1, 0 (upper)
        q = qMax, ..., 1, 0 (lower)
    *)
    pMax = IntExponent[dividend];
    qMax = IntExponent[divisor];
    (*
      The number of columns of the grid:
        n
    *)
    n = pMax + qMax + 1;
    (* Initialise upper and middle rows *)
    upper = 0;
    middle = dividend;
    (* Reap frames for animation *)
    (*
      Each frame shall consist of a grid of 4 rows:
        1. Upper
        2. Temporary (result of product to be removed)
        3. Middle
        4. Lower (divisor)
     *)
    frameList =
      Last @ Last @ Reap[
        (* Loop through the upper indices *)
        Do[
          (* Highlight divisor *)
          Sow @ MakeGrid[
            MakeGridRow[n, upper / 10 ^ (p + 1),
              "TrailingSpaces" -> p + 1
            ],
            MakeGridRow[n, 0],
            MakeGridRow[n, middle],
            MakeGridRow[n, divisor,
              "TrailingSpaces" -> p
            ],
            (* Divisor *)
            StyleGridCell["current", 4, {n - (p + qMax), n - p}]
          ];
          (* Determine upper digit *)
          upperDigit = Quotient[
            Quotient[middle, 10 ^ p],
            divisor
          ];
          (* Update upper number *)
          upper += upperDigit * 10 ^ p;
          (* Highlight current upper digit and divisor *)
          Sow @ MakeGrid[
            MakeGridRow[n, upper / 10 ^ p,
              "HideLoneZero" -> False,
              "TrailingSpaces" -> p
            ],
            MakeGridRow[n, 0],
            MakeGridRow[n, middle],
            MakeGridRow[n, divisor,
              "TrailingSpaces" -> p
            ],
            (* Current upper digit *)
            StyleGridCell["current", 1, n - p],
            (* Divisor *)
            StyleGridCell["current", 4, {n - (p + qMax), n - p}]
          ];
          (* Multiply current upper digit and divisor *)
          temporary = upperDigit * divisor;
          (* Highlight product to be removed *)
          Sow @ MakeGrid[
            MakeGridRow[n, upper / 10 ^ p,
              "HideLoneZero" -> False,
              "TrailingSpaces" -> p
            ],
            MakeGridRow[n, temporary,
              "HideLoneZero" -> False,
              "TrailingSpaces" -> p
            ],
            MakeGridRow[n, middle],
            MakeGridRow[n, divisor,
              "TrailingSpaces" -> p
            ],
            (* Current upper digit *)
            StyleGridCell["current", 1, n - p],
            (* Product to be removed *)
            StyleGridCell[
              "temporary", 2, {
                n - p - IntExponent[temporary],
                n - p
              }
            ],
            (* Divisor *)
            StyleGridCell["current", 4, {n - (p + qMax), n - p}]
          ];
          (* Update middle number *)
          middle -= temporary * 10 ^ p;
          (* Highlight current upper digit and divisor *)
          Sow @ MakeGrid[
            MakeGridRow[n, upper / 10 ^ p,
              "HideLoneZero" -> False,
              "TrailingSpaces" -> p
            ],
            MakeGridRow[n, 0],
            MakeGridRow[n, middle],
            MakeGridRow[n, divisor,
              "TrailingSpaces" -> p
            ],
            (* Current upper digit *)
            StyleGridCell["current", 1, n - p],
            (* Divisor *)
            StyleGridCell["current", 4, {n - (p + qMax), n - p}]
          ];
        , {p, pMax, 0, -1}];
        (* Final *)
        noRemainder = middle == 0;
        Sow @ MakeGrid[
          MakeGridRow[n, upper],
          MakeGridRow[n, 0],
          MakeGridRow[n, middle],
          MakeGridRow[n, divisor],
          (* Result (quotient) *)
          StyleGridCell["result", 1, {n - IntExponent[upper], n}],
          (* Result (remainder) *)
          StyleGridCell[
            If[noRemainder, "none", "fraction"],
            3, {n - IntExponent[middle], n}
          ],
          (* Result (divisor) *)
          StyleGridCell[
            If[noRemainder, "finished", "fraction"],
            4, {n - qMax, n}
          ]
        ];
      ];
    (* Return result and list of frames *)
    {
      If[noRemainder,
        upper,
        Inactive[Plus][
          upper,
          Inactive[Divide][middle, divisor]
        ]
      ],
      frameList //
        If[OptionValue[Export],
          ExportGif["divide", dividend, divisor],
          Identity
        ]
    }
  ];


(* ::Subsection:: *)
(*End of private scope*)


End[];


(* ::Section:: *)
(*Protect definitions*)


Protect["SunTzu`*"];


(* ::Section:: *)
(*End of package*)


EndPackage[];
