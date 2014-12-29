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
testFunc

Begin["`Private`"]
(* Implementation of the package *)

SetAttributes[testFunc, HoldAll]
testFunc[listAin_, listBin_] := With[
	{
		listAString = ToString[HoldForm[listAin]],
		listBString = ToString[HoldForm[listBin]]
	},
	Module[
		{},
		{listAString, listAin, listBString, listBin}
	]
]

VennDiagram[listA_List, listB_List, listUin:( _List | "All" | All)]:= Module[
	{
		i, k, seeExtra,
		n21, n22, n23, n24, n25,
		gl21, gl22, gl23, gl24, gl25,
		expList, glList, expParts,
		exp21, exp22, exp23, exp24, exp25,
		setNameA, setNameB, setNameU,
		setSizeA, setSizeB, setSizeU, setSizeAandB,
		aOnly, bOnly, aAndb, noneOfThem,
		showSetContents, showSetSizes,
		listU, lineThickness= 0.006
	},
	Column[{
	Manipulate[
		expList = {exp21, exp22, exp23, exp24, exp25};
		glList =  {gl21, gl22, gl23, gl24, gl25};
		expParts = Union @@ Pick[expList, glList, 0.8];
		(* Define graphical buttons *)
		aOnly= Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl21]],
			Button[Polygon[{ Join[
					Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, k, 5 k}],
					Reverse[ Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 4 k}] ]
				] }],
				gl21 = 1.8 - gl21
			], 
			Inset[Text[Style["A", Black, Italic, 17],
				FormatType -> StandardForm], {-1.4, 0.8}
			]
		}];
		bOnly = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl22]], 
			Button[Polygon[{ Join[
					Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, -k, k}],
					Reverse[ Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, -2 k, 2 k}] ]
				] }],
				gl22 = 1.8 - gl22
			],
			Inset[Text[Style["B", Black, Italic, 17], 
				FormatType -> StandardForm], {1.4, 0.8}
			]
		}];
		aAndb = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl23]], 
			Button[Polygon[{ Join[
					Table[{Cos[Pi/(3 k) i] - 1/2, Sin[Pi/(3 k) i]}, {i, -k, k}],
					Table[{Cos[Pi/(3 k) i] + 1/2, Sin[Pi/(3 k) i]}, {i, 2 k, 4 k}]
				] }],
				gl23 = 1.8 - gl23
			]
		}];
		noneOfThem = Graphics[{EdgeForm[Thickness[1.3 lineThickness]], Dynamic[GrayLevel[gl24]], 
			Button[Polygon[{{-2, -2.3}, {2, -2.3}, {2, 1.5}, {-2, 1.5}, {-2, -2.3}}],
				gl24 = 1.8 - gl24
			],
			Inset[	Text[Style["U", Black, Italic, 17],
					FormatType -> StandardForm], {-2.2, 1.8}
			]
		}];
		seeExtra = Sequence[
			Graphics[ { If[ gl25 != 1, EdgeForm[Thickness[0.6 lineThickness]], Unevaluated[Sequence[]] ],
				Dynamic[GrayLevel[gl25]],
				Polygon[{{2.1, -2.3}, {2.1, -1.9}, {2.5, -1.9}, {2.5, -2.3}, {2.1, -2.3}}],
				Inset[
					Text[Style[n25, Red, Bold, 14], FormatType -> StandardForm],
					{2.3, -2.15} 
				]
			}],
			Graphics[{Opacity[0],
				Button[Polygon[{{2.1, -2.3}, {2.1, -1.9}, {2.5, -1.9}, {2.5, -2.3}, {2.1, -2.3}}],
					gl25 = 1.8 - gl25
				]
			}]
		];

		(* Define display text *)
		exp21= Complement[listA, listB];
		exp22= Complement[listB, listA];
		exp23= Intersection[listA, listB];
		exp24= Complement[ listU, Union[listA, listB] ];
		exp25= Complement[ Union[listA, listB], listU ];
		{n21, n22, n23, n24, n25} = Length /@ {exp21, exp22, exp23, exp24, exp25};
		setSizeA= Graphics[ 
			Inset[
				Text[
					Style[n21, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{-1.2, 0.0}
			]
		];
		setSizeB= Graphics[ 
			Inset[
				Text[
					Style[n22, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{1.2, 0.0}
			]
		];
		setSizeAandB= Graphics[ 
			Inset[
				Text[
					Style[n23, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{0.0, 0}
			]
		];
		setSizeU= Graphics[ 
			Inset[
				Text[
					Style[n24, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{0.0, 1.2}
			]
		];
		setNameU= Graphics[ 
			Inset[
				Text[
					Style[" = ", Black, 17], 
					FormatType -> StandardForm
				],
				{-1.8, 1.8}
			]
		];
		setNameA= Graphics[
			Inset[
				Text[Style["A = ", Black, 17, Italic]],
				{-1.6, -1.6}
			]
		];
		setNameB= Graphics[
			Inset[
				Text[Style["B = ", Black, 17, Italic]],
				{-1.6, -2.1}
			]
		];
		Pane[
			Show[
				noneOfThem, aOnly, bOnly, aAndb, If[ n25 != 0, Unevaluated[seeExtra], Unevaluated[Sequence[]] ],
				If[showSetSizes,    Unevaluated[Sequence[setSizeA, setSizeB, setSizeU, setSizeAandB]], Unevaluated[Sequence[]] ],
				If[showSetContents, Unevaluated[Sequence[setNameA, setNameB, setNameU]], Unevaluated[Sequence[]] ],
				ImageSize -> {540, 300}
			], 
			ImageSize -> {540, 300}
		],

		(* Define controls and initialization *)
		{{gl21, 1}, ControlType -> None},
		{{gl22, 1}, ControlType -> None},
		{{gl23, 1}, ControlType -> None},
		{{gl24, 1}, ControlType -> None},
		{{gl25, 1}, ControlType -> None},
		{{showSetSizes, True, "show set sizes"}, {True, False}},
		{{showSetContents, True, "show set contents"}, {True, False}},
		TrackedSymbols -> True,
		Initialization :> (
			k = 12;
			If[ listUin === "All" || listUin === All, listU= Union[listA, listB], listU= listUin]
		)
	],
	Dynamic[expParts]
	}]
]

VennDiagram[listA_List, listB_List, listC_List, listUin:( _List | "All" | All)]:= Module[
	{
		i, k, seeExtra, d=-0.67,
		n1, n2, n3, n4, n5, n6, n7, n8, n9, 
		gl1, gl2, gl3, gl4, gl5, gl6, gl7, gl8, gl9,
		expList, glList, expParts,
		exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
		setNameA, setNameB, setNameC, setNameU,
		setSizeA, setSizeB, setSizeC, setSizeU, setSizeAandB,
		setSizeAandC, setSizeBandC, setSizeABC,
		aOnly, bOnly, cOnly, aAndb, bAndc, aAndc, 
		allOfThem, noneOfThem, showSetContents, showSetSizes,
		listU, lineThickness= 0.006
	},
	Column[{
	Manipulate[
		expList = {exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9};
		glList =  {gl1, gl2, gl3, gl4, gl5, gl6, gl7, gl8, gl9};
		expParts = Union @@ Pick[expList, glList, 0.8];
		(* Define graphical buttons *)
		aOnly= Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl1]],
			Button[Polygon[{ Join[
					Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, k, 4 k}],
					Reverse[Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, 2 k, 3 k}]],
					Reverse[Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, 2 k, 3 k}]]
				] }],
				gl1 = 1.8 - gl1
			], 
			Inset[Text[Style["A", Black, Italic, 17],
				FormatType -> StandardForm], {-1.4, d - 0.8}
			]
		}];
		bOnly = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl2]], 
			Button[Polygon[{ Join[
					Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, 0, k}],
					Reverse[ Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, -k, 2 k}] ],
					Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, 0, k}]
				] }],
				gl2 = 1.8 - gl2
			],
			Inset[Text[Style["B", Black, Italic, 17], 
				FormatType -> StandardForm], {1.4, d - 0.8}
			]
		}];
		cOnly = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl3]], 
			Button[Polygon[{ Join[
				Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, 4 k, 5 k}],
				Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, 4 k, 5 k}],
				Reverse[ Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, 3 k, 6 k}]]
				] }],
				gl3 = 1.8 - gl3
			], 
			Inset[Text[Style["C", Black, Italic, 17], 
				FormatType -> StandardForm], {0.8, d - (-1.8)}
			]
		}];
		aAndb = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl4]],
			Button[Polygon[{ Join[
				Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, 0, k}],
				Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, 2 k, 3 k}],
				Reverse[ Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, k, 2 k}]]
				] }],
				gl4 = 1.8 - gl4
			]
		}];
		aAndc = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl5]],
			Button[Polygon[{ Join[
				Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, 4 k, 5 k}],
				Reverse[ Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, 3 k, 4 k}]],
				Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, 2 k,3 k}]
				] }], 
				gl5 = 1.8 - gl5
			]
		}];
		bAndc = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl6]],
			Button[Polygon[{ Join[
				Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, 5 k, 6 k}],
				Reverse[ Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, 0, k}]],
				Reverse[ Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, 4 k, 5 k}]]
				] }], 
				gl6 = 1.8 - gl6
			]
		}];
		allOfThem = Graphics[{EdgeForm[Thickness[lineThickness]], Dynamic[GrayLevel[gl7]], 
			Button[Polygon[{ Join[
				Table[{Cos[Pi/(3 k) i] - 1/2, d - Sin[Pi/(3 k) i]}, {i, 5 k, 6 k}],
				Table[{Cos[Pi/(3 k) i], d - (Sin[Pi/(3 k) i] - Sqrt[3]/2)}, {i, k, 2 k}],
				Table[{Cos[Pi/(3 k) i] + 1/2, d - Sin[Pi/(3 k) i]}, {i, 3 k, 4 k}]
				] }],
				gl7 = 1.8 - gl7
			]
		}];
		noneOfThem = Graphics[{EdgeForm[Thickness[1.3 lineThickness]], Dynamic[GrayLevel[gl8]],
			Button[Polygon[{{-2.2, -3.3}, {2.2, -3.3}, {2.2, 1.5}, {-2.2, 1.5}, {-2.2, -3.3}}],
				gl8 = 1.8 - gl8
			],
			Inset[ Text[
					Style["U", Black, Italic, 17], 
					FormatType -> StandardForm
				], {-2.2, 1.8}  
			]
		}];
		seeExtra = Sequence[
			Graphics[ { If[ gl9 != 1, EdgeForm[Thickness[0.6 lineThickness]], Unevaluated[Sequence[]] ],
				Dynamic[GrayLevel[gl9]],
				Polygon[{{2.3, -3.3}, {2.3, -2.9}, {2.7, -2.9}, {2.7, -3.3}, {2.3, -3.3}}],
				Inset[
					Text[Style[n9, Red, Bold, 14], FormatType -> StandardForm],
					{2.5, -3.15} 
				]
			}],
			Graphics[{Opacity[0],
				Button[Polygon[{{2.3, -3.3}, {2.3, -2.9}, {2.7, -2.9}, {2.7, -3.3}, {2.3, -3.3}}],
					gl9 = 1.8 - gl9
				]
			}]
		];

		(* Define display text *)
		exp1= Complement[listA, Union[listB, listC]];
		exp2= Complement[listB, Union[listC, listA]];
		exp3= Complement[listC, Union[listA, listB]];
		exp4= Complement[Intersection[listA, listB], listC];
		exp5= Complement[Intersection[listA, listC], listB];
		exp6= Complement[Intersection[listB, listC], listA];
		exp7= Intersection[ listA, listB, listC ];
		exp8= Complement[ listU, Union[ listA, listB, listC ] ];
		exp9= Complement[ Union[listA, listB, listC], listU ];
		{n1, n2, n3, n4, n5, n6, n7, n8, n9} = Length /@ {exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9};
		setSizeA= Graphics[ 
			Inset[
				Text[
					Style[n1, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{-1.2, -0.7}
			]
		];
		setSizeB= Graphics[ 
			Inset[
				Text[
					Style[n2, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{1.2, -0.7}
			]
		];
		setSizeC= Graphics[ 
			Inset[
				Text[
					Style[n3, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{0.0, 0.9}
			]
		];
		setSizeAandB= Graphics[ 
			Inset[
				Text[
					Style[n4, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{0.0, -1.2}
			]
		];
		setSizeAandC= Graphics[ 
			Inset[
				Text[
					Style[n5, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{-0.69, 0.02}
			]
		];
		setSizeBandC= Graphics[ 
			Inset[
				Text[
					Style[n6, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{0.69, 0.02}
			]
		];
		setSizeABC= Graphics[ 
			Inset[
				Text[
					Style[n7, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{0.0, -0.38}
			]
		];
		setSizeU= Graphics[ 
			Inset[
				Text[
					Style[n8, Black, Bold, 14], 
					FormatType -> StandardForm
				],
				{-1.6, 1.2}
			]
		];
		setNameU= Graphics[ 
			Inset[
				Text[
					Style[" = ", Black, 17], 
					FormatType -> StandardForm
				],
				{-1.8, 1.8}
			]
		];
		setNameA= Graphics[
			Inset[
				Text[Style["A = ", Black, 17, Italic]],
				{-1.6, -2.1}
			]
		];
		setNameB= Graphics[
			Inset[
				Text[Style["B = ", Black, 17, Italic]],
				{-1.6, -2.6}
			]
		];
		setNameC= Graphics[
			Inset[
				Text[Style["C = ", Black, 17, Italic]],
				{-1.6, -3.1}
			]
		];
		Pane[
			Show[
				noneOfThem, aOnly, bOnly, cOnly, aAndb, bAndc, aAndc, allOfThem,
				If[ n9 != 0, Unevaluated[seeExtra], Unevaluated[Sequence[]] ],
				If[showSetSizes,    Unevaluated[Sequence[setSizeA, setSizeB, setSizeC, setSizeU, setSizeAandB, setSizeBandC, setSizeAandC, setSizeABC]], Unevaluated[Sequence[]] ],
				If[showSetContents, Unevaluated[Sequence[setNameA, setNameB, setNameC, setNameU]], Unevaluated[Sequence[]] ],
				ImageSize -> {540, 300}
			],
			ImageSize -> {540, 300}
		],
		{{gl1, 1}, ControlType -> None},
		{{gl2, 1}, ControlType -> None},
		{{gl3, 1}, ControlType -> None},
		{{gl4, 1}, ControlType -> None},
		{{gl5, 1}, ControlType -> None},
		{{gl6, 1}, ControlType -> None},
		{{gl7, 1}, ControlType -> None},
		{{gl8, 1}, ControlType -> None},
		{{gl9, 1}, ControlType -> None},
		{{showSetSizes, True, "show set sizes"}, {True, False}},
		{{showSetContents, True, "show set contents"}, {True, False}},
		TrackedSymbols -> True,
		Initialization :> (
			k = 12;
			If[ listUin === "All" || listUin === All, listU= Union[listA, listB, listC], listU= listUin]
		)
	],
	Dynamic[expParts]
	}]
]

End[]

EndPackage[]