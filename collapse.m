BeginPackage["collapse`"]

(* Config *)
$foldedExtensionLength = .000001;

(* Data *)
generateTwoBase[creaseType_] :=
    <|{
        "points" -> {
            "a" -> {-1, 0},
            "b" -> {1, 0},
            "u" -> {0, -1},
            "v" -> {0, 1}
        },
        "polygons" -> {
            "p" -> {"a", "b", "u"},
            "q" -> {"a", "b", "v"}
        },
        "creases" -> <|{
            "mountain" -> {
                If[creaseType == "mountain",
                    {"a", "b"},
                    Unevaluated[Sequence[]]
                ]
            },
            "valley" -> {
                If[creaseType == "valley",
                    {"a", "b"},
                    Unevaluated[Sequence[]]
                ]
            }
        }|>,
        "fractals" -> {}
    }|>

creasePatternLibrary =
    <|{
        "diamond_base" -> <|{
            "points" -> {
                "left_back" -> {-2, 0},
                "center_back" -> {0, 0},
                "right_back" -> {2, 0},
                "left_middle" -> {-2, 1},
                "center_middle" -> {0, 1},
                "right_middle" -> {2, 1},
                "left_forward" -> {-2, 2},
                "left_half_forward" -> {-1, 2},
                "right_half_forward" -> {1, 2},
                "right_forward" -> {2, 2},
                "left_front" -> {-2, 3},
                "left_half_front" -> {-1, 3},
                "center_front" -> {0, 3},
                "right_half_front" -> {1, 3},
                "right_front" -> {2, 3}
            },
            "polygons" -> <|{
                "left_back" -> {
                    "left_back", "center_back", "center_middle", "left_middle"
                },
                "right_back" -> {
                    "right_back", "center_back", "center_middle", "right_middle"
                },

                "left_middle" -> {
                    "left_middle", "center_middle", "left_half_forward",
                    "left_forward"
                },
                "right_middle" -> {
                    "right_middle", "center_middle", "right_half_forward",
                    "right_forward"
                },
                "left_center" -> {
                    "center_middle", "center_front", "left_half_forward"
                },
                "right_center" -> {
                    "center_middle", "center_front", "right_half_forward"
                },
                "left_front" -> {
                    "left_front", "left_half_front", "left_half_forward",
                    "left_forward"
                },
                "left_half_front" -> {
                    "left_half_front", "center_front", "left_half_forward"
                },
                "right_half_front" -> {
                    "right_half_front", "center_front", "right_half_forward"
                },
                "right_front" -> {
                    "right_front", "right_half_front", "right_half_forward",
                    "right_forward"
                }
            }|>,
            "creases" -> <|{
                "mountain" -> {
                    {"center_back", "center_middle"},
                    {"left_middle", "center_middle"},
                    {"center_middle", "right_middle"},
                    {"center_middle", "center_front"},
                    {"right_half_front", "right_half_forward"},
                    {"left_half_front", "left_half_forward"}
                },
                "valley" -> {
                    {"center_middle", "left_half_forward"},
                    {"center_middle", "right_half_forward"},
                    {"left_forward", "left_half_forward"},
                    {"right_half_forward", "right_forward"},
                    {"left_half_forward", "center_front"},
                    {"center_front", "right_half_forward"}
                }
            }|>,
            "fractals" -> {
            }
        }|>
    }|>

(* Geometry *)
trilaterate[{r1_, r2_, r3_}, {{x2_}, {x3_, y3_}}] :=
    Module[{d, i, j, x, y, z},
        d = x2;
        i = x3;
        j = y3;
        x = (r1^2 - r2^2 + d^2)/(2 d);
        y = (r1^2 - r3^2 + i^2 + j^2)/(2 j) - i/j x;
        z = Sqrt[r1^2 - x^2 - y^2];
        {x, y, z}
    ]

trilaterate[{p1 : {x1_, y1_, z1_}, radius1_}, {p2 : {x2_, y2_, z2_}, radius2_},
    {p3 : {x3_, y3_, z3_}, radius3_}] :=
    Module[{ehx, i, ehy, ehz, d, j, x, y, z},
        ehx = Normalize[p2 - p1];
        i = ehx.(p3 - p1);
        ehy = Normalize[p3 - p1 - i ehx];
        d = Norm[p2 - p1];
        j = ehy.(p3 - p1);
        ehz = Cross[ehx, ehy];
        {x, y, z} = trilaterate[{radius1, radius2, radius3}, {{d}, {i, j}}];
        {
            p1 + x ehx + y ehy + z ehz,
            p1 + x ehx + y ehy - z ehz
        }
    ]

calculatePointPlaneDistance[point0_, plane:{point1_, point2_, point3_}] :=
    Normalize[Cross[point2 - point1, point3 - point1]].(point0 - point1)

recast[{px_, py_, pz_}, {{x1_, y1_, z1_}, {x2_, y2_, z2_}, {x3_, y3_, z3_}}] :=
    Module[{x, y, z},
        {x, y, z} /. {
            x -> -((-pz x3 y2 + pz x2 y3 + py x3 z2 - px y3 z2 -
                py x2 z3 + px y2 z3)/(
                x3 y2 z1 - x2 y3 z1 - x3 y1 z2 + x1 y3 z2 + x2 y1 z3 -
                x1 y2 z3)),
            y -> -((pz x3 y1 - pz x1 y3 - py x3 z1 + px y3 z1 + py x1 z3 -
                px y1 z3)/(
                x3 y2 z1 - x2 y3 z1 - x3 y1 z2 + x1 y3 z2 + x2 y1 z3 -
                x1 y2 z3)),
            z -> -((pz x2 y1 - pz x1 y2 - py x2 z1 + px y2 z1 + py x1 z2 -
                px y1 z2)/(-x3 y2 z1 + x2 y3 z1 + x3 y1 z2 - x1 y3 z2 -
                x2 y1 z3 + x1 y2 z3))}
    ]

calculateBinormal[{p1_, p2_, p3_}] :=
    Normalize[Cross[calculateTangent[{p1, p2, p3}], p1 - p2]]

calculateTangent[{p1_, p2_, p3_}] :=
    Normalize[(p1 - p2) - (p3 - p2)]

calculateNormal[{p1_, p2_, p3_}] :=
    Normalize[
        Cross[calculateTangent[{p1, p2, p3}], calculateBinormal[{p1, p2, p3}]]
    ]

convertCreasePatternTo3D[creasePattern_, offset_:{0, 0, 0}] :=
    Module[{newCreasePattern},
        newCreasePattern = creasePattern;
        newCreasePattern["points"] =
            creasePattern[["points"]] /. {x_ /; NumberQ[x], y_ /; NumberQ[y]} :>
                ({x, y, 0} + offset);
        newCreasePattern
    ]

calculateUp[foldedTriangle_, triangle_] :=
    Module[{up},
        up = calculateBinormal[triangle];
        If[up[[3]] < 0,
            -calculateBinormal[foldedTriangle],
            calculateBinormal[foldedTriangle]
        ]
    ]

calculateFoldedVectors[polygonName_, foldedCreasePattern_, creasePattern_] :=
    Module[{points, triange, up},
        triangle = Take[creasePattern[["polygons"]][[polygonName]], 3] /.
            creasePattern[["points"]];
        foldedTriangle =
            Take[
                foldedCreasePattern[["polygons"]][[polygonName]],
                3
            ] /. foldedCreasePattern[["points"]];
        center = Mean[foldedTriangle];
        up = $foldedExtensionLength calculateUp[
            foldedTriangle,
            triangle
        ];
        down = -up;
        {
            center + up,
            center + down
        }
    ]

calculateCreaseDirection[polygonName1_, polygonName2_,
    foldedCreasePattern_, creasePattern_] :=
    Module[{points, triange1, triange2, up1, down1, up2, down2, center1,
        center2, upDistance, downDistance},
        {up1, down1} =
            calculateFoldedVectors[
                polygonName1,
                foldedCreasePattern,
                creasePattern
            ];
        {up2, down2} =
            calculateFoldedVectors[
                polygonName2,
                foldedCreasePattern,
                creasePattern
            ];
        upDistance = EuclideanDistance[up1, up2];
        downDistance = EuclideanDistance[down1, down2];
        If[upDistance < downDistance,
            {"valley", upDistance},
            {"mountain", downDistance}
        ]
    ]

(* Render *)
renderPolygon[polygon_, points_] :=
    Polygon[polygon /. points]

renderCrease[crease_, type_, points_] :=
    {
        If[type == "mountain", Red, Blue],
        Tube[(crease /. points)]
    }

renderCreasePattern[creasePattern_] :=
    Module[{points, polygons, creases},
        points = creasePattern[["points"]];
        polygons = creasePattern[["polygons"]];
        creases = creasePattern[["creases"]];

        {
            Table[
                renderPolygon[polygon, points],
                {polygon, polygons}
            ],
            Table[
                renderCrease[crease, type, points],
                {type, {"mountain", "valley"}},
                {crease, creases[[type]]}
            ]
        }
    ]

(* Test *)
testFlatTwoFoldCreasePattern =
    <|
        "points" -> {
            "a" -> {0, 1, 0},
            "b" -> {0, 0, 0},
            "c" -> {1, 1, 0},
            "u" -> {0, 1, 0},
            "v" -> {1, 1, 0},
            "w" -> {0, -1, 0}
        },
        "polygons" -> <|
            "q" -> {"a", "b", "c"},
            "r" -> {"u", "v", "w"}
        |>
    |>

testConvertFlatTwoFoldToFolded[type_, reversed_] :=
    Module[{creasePattern, flatCreasePattern, w},
        w =
            If[type == "mountain",
                {0, 2, -1},
                {1, 2, 1}
            ];
        creasePattern = testFlatTwoFoldCreasePattern;
        creasePattern["creases"] =
            <|
                "mountain" ->
                    {
                        If[type == "mountain",
                            {"a", "c"},
                            Unevaluated[Sequence[]]
                        ]
                    },
                "valley" ->
                    {
                        If[type == "valley",
                            {"a", "c"},
                            Unevaluated[Sequence[]]
                        ]
                    }
            |>;
        creasePattern["polygons"]["q"] =
            If[reversed,
                Reverse[creasePattern[["polygons"]][["q"]]],
                creasePattern[["polygons"]][["q"]]
            ];
        flatCreasePattern = creasePattern;
        If[type == "valley",
            creasePattern["points"] =
                {
                    "a" -> {0, 1, -1},
                    "b" -> {0, 0, -1},
                    "c" -> {1, 1, -1},
                    "u" -> {0, 1, -1},
                    "v" -> {1, 1, -1},
                    "w" -> {1, 2, 1}
                }
        ];
        If[type == "mountain",
            creasePattern["points"] =
                {
                    "a" -> {0, 1, 1},
                    "b" -> {0, 0, 1},
                    "c" -> {1, 1, 1},
                    "u" -> {0, 1, 1},
                    "v" -> {1, 1, 1},
                    "w" -> {0, 2, -1}
                }
        ];
        {creasePattern, flatCreasePattern}
    ]

EndPackage[]
