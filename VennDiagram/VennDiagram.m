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

(*SetAttributes[VennDiagram1, HoldAll]*)
VennDiagram[listA_List, listB_List, listUin:( _List | "All" | All)]= Module[
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
		listU, lineThickness= 0.005
	},
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
		noneOfThem = Graphics[{EdgeForm[Thickness[1.5 lineThickness]], Dynamic[GrayLevel[gl24]], 
			Button[Polygon[{{-2, -2.3}, {2, -2.3}, {2, 1.5}, {-2, 1.5}, {-2, -2.3}}],
				gl24 = 1.8 - gl24
			],
			Inset[Text[Style["U", Black, Italic, 17], 
			FormatType -> StandardForm], {-2.2, 1.8}]
		}];
		seeExtra = Sequence[
			Graphics[ { If[ gl25 != 1, EdgeForm[Thickness[0.6 lineThickness]], Unevaluated[Sequence[]] ],
				Dynamic[GrayLevel[gl25]],
				Polygon[{{2.1, -2.3}, {2.1, -1.9}, {2.5, -1.9}, {2.5, -2.3}, {2.1, -2.3}}],
				Inset[Text[Style["U", Black, Italic, 17], 
				FormatType -> StandardForm], {-2.2, 1.8}]
			}],
			Graphics[ 
				Inset[
					Text[Style[n25, Red, Bold, 14], FormatType -> StandardForm],
					{2.3, -2.15} 
				]
			],
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
			Column[{
				"",
				Show[
					noneOfThem, aOnly, bOnly, aAndb, If[ n25 != 0, Unevaluated[seeExtra], Unevaluated[Sequence[]] ],
					If[showSetSizes,    Unevaluated[Sequence[setSizeA, setSizeB, setSizeU, setSizeAandB]], Unevaluated[Sequence[]] ],
					If[showSetContents, Unevaluated[Sequence[setNameA, setNameB, setNameU]], Unevaluated[Sequence[]] ],
					ImageSize -> {540, 300}
				],
				Short[expParts, 10]
			}, Alignment -> Center], 
			ImageSize -> {540, 600}
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
	]
]

End[]

EndPackage[]