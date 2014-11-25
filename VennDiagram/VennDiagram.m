(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Nov 24, 2014 
   Author: Paco Jain (pacojain@gmail.com)
   Adapted from Wolfram Demonstrations Projects:
		"Interactive Venn Diagrams"
		  http://demonstrations.wolfram.com/InteractiveVennDiagrams/
		  Contributed by: Marc Brodie (Wheeling Jesuit University)
	  and
		"Venn Diagrams"
		  http://demonstrations.wolfram.com/VennDiagrams/
		  Contributed by: George Beck and Liz Kent
*)

BeginPackage["VennDiagram`"]
(* Exported symbols added here with SymbolName::usage *)

VennDiagram::usage= "Produces an interactive Venn diagram graphic"
VennDiagram1::usage= "Produces an interactive Venn diagram graphic"

Begin["`Private`"]
(* Implementation of the package *)

VennDiagram1= Module[
	{
		setNot, aOnly
	},
	Manipulate[
		aOnly= Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl21]],
			Button[Polygon[{
				Join[
					Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, k, 5 k}],
					Reverse[
						Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 4 k}]
					]
				]
			}],
			gl21 = 1.8 - gl21], 
			Inset[Text[Style["A", Black, Italic, Large],
				FormatType -> StandardForm], {-1.4, 0.8}
			]
		}];
		Pane[
			Column[{
				Show[aOnly, ImageSize -> {540, 300}],
				"",
				If[setNot, setNotations, Invisible[setNotations]]
			}, Alignment -> Center], 
			ImageSize -> {540, 460}
		],
		{{gl21, 1}, ControlType -> None},	
		{{setNot, True, "show set notation"}, {True, False}},
		Initialization :> 
			(k = 12;
   exp1 = a && ! b && ! c;
   exp2 = b && ! a && ! c;
   exp3 = c && ! a && ! b;
   exp4 = a && b && ! c;
   exp5 = a && c && ! b;
   exp6 = b && c && ! a;
   exp7 = a && b && c;
   exp8 = ! a && ! b && ! c;
   
   exp21 = a && ! b;
   exp22 = b && ! a;
   exp23 = a && b;
   exp24 = ! a && ! b;
   
   replacementsList = {"&&" -> "\[Intersection]", "||" -> "\[Union]", 
     "!A" -> "\!\(\*SuperscriptBox[\(A\), \(\[Prime]\)]\)", 
     "!B" -> "\!\(\*SuperscriptBox[\(B\), \(\[Prime]\)]\)", 
     "!C" -> "\!\(\*SuperscriptBox[\(C\), \(\[Prime]\)]\)", 
     "FALSE" -> "\[EmptySet]", "TRUE" -> "U"})
 ]
	]
]

VennDiagram= Manipulate[
 Switch[num, 2,
  expList = {exp21, exp22, exp23, exp24};
  glList = {gl21, gl22, gl23, gl24};
  expParts = Pick[expList, glList, 0.8];
  
  setStringDNF = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "DNF"]]],
    replacementsList];
  newSetStringsDNF = 
   Select[StringCases[setStringDNF, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsDNF = 
   StringReplace[
    newSetStringsDNF, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalDNF = 
   StringReplace[setStringDNF, 
    Table[newSetStringsDNF[[i]] -> newStringsDNF[[i]], {i, 
      Length[newSetStringsDNF]}]];
  
  setStringCNF = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "CNF"]]],
    replacementsList];
  newSetStringsCNF = 
   Select[StringCases[setStringCNF, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsCNF = 
   StringReplace[
    newSetStringsCNF, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalCNF = 
   StringReplace[setStringCNF, 
    Table[newSetStringsCNF[[i]] -> newStringsCNF[[i]], {i, 
      Length[newSetStringsCNF]}]];
  
  setStringOR = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "OR"]]],
    replacementsList];
  newSetStringsOR = 
   Select[StringCases[setStringOR, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsOR = 
   StringReplace[
    newSetStringsOR, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalOR = 
   StringReplace[setStringOR, 
    Table[newSetStringsOR[[i]] -> newStringsOR[[i]], {i, 
      Length[newSetStringsOR]}]];
  
  setStringAND = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "AND"]]],
    replacementsList];
  newSetStringsAND = 
   Select[StringCases[setStringAND, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsAND = 
   StringReplace[
    newSetStringsAND, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalAND = 
   StringReplace[setStringAND, 
    Table[newSetStringsAND[[i]] -> newStringsAND[[i]], {i, 
      Length[newSetStringsAND]}]];
  setNotations = 
   Text@Style[
     TableForm[
      DeleteDuplicates[{setStringFinalDNF, setStringFinalCNF, 
        setStringFinalOR, setStringFinalAND}], 
      TableSpacing -> {1.5, Automatic}], Italic, 24];
  
  aOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl21]], 
     Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, k, 5 k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 
            4 k}]]]
        }]                   , gl21 = 1.8 - gl21     ]   , 
     Inset[Text[Style["A", Black, Italic, Large], 
       FormatType -> StandardForm], {-1.4, 
       0.8}  ]                              }];
  bOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl22]], 
     Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, -k, k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, -2 k, 
            2 k}]]]
        }]                   , gl22 = 1.8 - gl22    ]   , 
     Inset[Text[Style["B", Black, Italic, Large], 
       FormatType -> StandardForm], {1.4, 
       0.8}  ]                               }];
  aAndb = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl23]], 
     Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, -k, k}],
         Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 
           4 k}]]
        }]                   , 
      gl23 = 1.8 - gl23    ]                               }];
  noneOfThem = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl24]], 
     Button[Polygon[{
        {-2, -2.3}, {2, -2.3}, {2, 1.5}, {-2, 1.5}, {-2, -2.3}
        }]                   , 
      gl24 = 1.8 - gl24     ]            ,     
     Inset[Text[Style["U", Black, Italic, Large], 
       FormatType -> StandardForm], {2.2, 
       1.4}  ]                      }];
  
  Pane[
   Column[{Show[noneOfThem, aOnly, bOnly, aAndb, 
      ImageSize -> {540, 300}],
     "",
     If[setNot, setNotations,
      Invisible[setNotations]]}, Alignment -> Center], 
   ImageSize -> {540, 460}],
  
  
  3,
  expList = {exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8};
  glList = {gl1, gl2, gl3, gl4, gl5, gl6, gl7, gl8};
  expParts = Pick[expList, glList, 0.8];
  setStringDNF = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "DNF"]]],
    replacementsList];
  newSetStringsDNF = 
   Select[StringCases[setStringDNF, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsDNF = 
   StringReplace[
    newSetStringsDNF, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalDNF = 
   StringReplace[setStringDNF, 
    Table[newSetStringsDNF[[i]] -> newStringsDNF[[i]], {i, 
      Length[newSetStringsDNF]}]];
  
  setStringCNF = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "CNF"]]],
    replacementsList];
  newSetStringsCNF = 
   Select[StringCases[setStringCNF, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsCNF = 
   StringReplace[
    newSetStringsCNF, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalCNF = 
   StringReplace[setStringCNF, 
    Table[newSetStringsCNF[[i]] -> newStringsCNF[[i]], {i, 
      Length[newSetStringsCNF]}]];
  
  setStringOR = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "OR"]]],
    replacementsList];
  newSetStringsOR = 
   Select[StringCases[setStringOR, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsOR = 
   StringReplace[
    newSetStringsOR, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalOR = 
   StringReplace[setStringOR, 
    Table[newSetStringsOR[[i]] -> newStringsOR[[i]], {i, 
      Length[newSetStringsOR]}]];
  
  setStringAND = 
   StringReplace[
    ToUpperCase[ToString[BooleanMinimize[Or @@ expParts, "AND"]]],
    replacementsList];
  newSetStringsAND = 
   Select[StringCases[setStringAND, "!(" ~~ __ ~~ ")", 
     Overlaps -> All], StringCount[#, "("] == 1 &];
  newStringsAND = 
   StringReplace[
    newSetStringsAND, {"!(" -> "(", 
     ")" -> "\!\(\*SuperscriptBox[\()\), \(\[Prime]\)]\)"}];
  setStringFinalAND = 
   StringReplace[setStringAND, 
    Table[newSetStringsAND[[i]] -> newStringsAND[[i]], {i, 
      Length[newSetStringsAND]}]];
  setNotations = 
   Text@Style[
     TableForm[
      DeleteDuplicates[{setStringFinalDNF, setStringFinalCNF, 
        setStringFinalOR, setStringFinalAND}], 
      TableSpacing -> {1.5, Automatic}], Italic, 18];
  
  
  aOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl1]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, k, 4 k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, 
            2 k, 3 k}]],
         Reverse[
          Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 
            3 k}]]]
        }]                   , gl1 = 1.8 - gl1     ]   ,   
     Inset[Text[Style["Z", Black, Italic, Large], 
       FormatType -> StandardForm], {-1.4, 
       0.8}  ]                          }];
  bOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl2]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, 0, k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, -k, 
            2 k}]], 
         Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, 0, 
           k}]]
        }]                  , gl2 = 1.8 - gl2     ]    , 
     Inset[Text[Style["B", Black, Italic, Large], 
       FormatType -> StandardForm], {1.4, 
       0.8}  ]                              }];
  cOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl3]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, 4 k, 
           5 k}],
         Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 4 k, 
           5 k}], Reverse[
          Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, 
            3 k, 6 k}]]]
        }]                  , gl3 = 1.8 - gl3     ]    , 
     Inset[Text[Style["C", Black, Italic, Large], 
       FormatType -> 
        StandardForm], {0.8, -1.8}  ]                             }];
  aAndbOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl4]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, 0, k}],
         Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 
           3 k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, k,
             2 k}]]]
        }]                   , 
      gl4 = 1.8 - gl4     ]                           }];
  aAndcOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl5]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, 4 k, 
           5 k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 3 k, 
            4 k}]],
         Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, 
           2 k, 3 k}]]
        }]                 , 
      gl5 = 1.8 - gl5     ]                                 }];
  bAndcOnly = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl6]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, 5 k, 
           6 k}],
         Reverse[
          Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, 0,
             k}]],
         Reverse[
          Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 4 k, 
            5 k}]]]
        }]                            , 
      gl6 = 1.8 - gl6     ]                             }];
  abAndc = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl7]], Button[Polygon[{
        Join[
         Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, 5 k, 
           6 k}],
         Table[{Cos[Pi/(3 k) i], Sin[Pi/(3 k) i] - Sqrt[3]/2}, {i, k, 
           2 k}],
         Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 3 k, 
           4 k}]]
        }]    , gl7 = 1.8 - gl7     ]           }];
  noneOfThem = 
   Graphics[{EdgeForm[Thick], Dynamic[GrayLevel[gl8]], Button[Polygon[{
        {-2, -2.3}, {2, -2.3}, {2, 1.5}, {-2, 1.5}, {-2, -2.3}
        }]                   , gl8 = 1.8 - gl8     ] ,     
     Inset[Text[Style["U", Black, Italic, Large], 
       FormatType -> StandardForm], {2.2, 
       1.4}  ]                            }];
  
  
	Pane[Column[{
		Show[noneOfThem, aOnly, bOnly, cOnly, aAndbOnly, aAndcOnly, bAndcOnly, abAndc, ImageSize -> {540, 300}],
		Show[noneOfThem, aOnly, bOnly, cOnly, aAndbOnly, aAndcOnly, bAndcOnly, abAndc, ImageSize -> {540, 300}],
		"",
		If[setNot, setNotations, Invisible[setNotations]]}, Alignment -> Center] , 
		ImageSize -> {540, 460}
	]
  ],
 
 
 {{gl1, 1}, ControlType -> None},
 {{gl2, 1}, ControlType -> None},
 {{gl3, 1}, ControlType -> None},
 {{gl4, 1}, ControlType -> None},
 {{gl5, 1}, ControlType -> None},
 {{gl6, 1}, ControlType -> None},
 {{gl7, 1}, ControlType -> None},
 {{gl8, 1}, ControlType -> None},
 {{gl21, 1}, ControlType -> None},
 {{gl22, 1}, ControlType -> None},
 {{gl23, 1}, ControlType -> None},
 {{gl24, 1}, ControlType -> None},
 {{num, 2, "number of sets"}, {2, 3}, ControlType -> RadioButtonBar},
 {{setNot, True, "show set notation"}, {True, False}},
 AutorunSequencing -> {13, 14},
 TrackedSymbols -> True,
 Initialization :> (k = 12;
   exp1 = a && ! b && ! c;
   exp2 = b && ! a && ! c;
   exp3 = c && ! a && ! b;
   exp4 = a && b && ! c;
   exp5 = a && c && ! b;
   exp6 = b && c && ! a;
   exp7 = a && b && c;
   exp8 = ! a && ! b && ! c;
   
   exp21 = a && ! b;
   exp22 = b && ! a;
   exp23 = a && b;
   exp24 = ! a && ! b;
   
   replacementsList = {"&&" -> "\[Intersection]", "||" -> "\[Union]", 
     "!A" -> "\!\(\*SuperscriptBox[\(A\), \(\[Prime]\)]\)", 
     "!B" -> "\!\(\*SuperscriptBox[\(B\), \(\[Prime]\)]\)", 
     "!C" -> "\!\(\*SuperscriptBox[\(C\), \(\[Prime]\)]\)", 
     "FALSE" -> "\[EmptySet]", "TRUE" -> "U"})
 ]

End[]

EndPackage[]