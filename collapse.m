(* ::Package:: *)

BeginPackage["collapse`"]

(* Data *)
generateTwoBase[creaseType_] :=
    <|
        "points" -> {
            "a" -> {-1, 0, 0},
            "b" -> {1, 0, 0},
            "u" -> {0, -1, 0},
            "v" -> {0, 1, 0}
        },
        "folded" -> {},
        "polygons" -> <|
            "p" -> {"a", "b", "u"},
            "q" -> {"a", "b", "v"}
        |>,
        "creases" -> <|
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
        |>,
        "fractals" -> {}
    |>

creasePatternLibrary =
    <|
        "diamond_base" -> <|
            "points" -> {
                "left_back" -> {-2, 0, 0},
                "center_back" -> {0, 0, 0},
                "right_back" -> {2, 0, 0},
                "left_middle" -> {-2, 1, 0},
                "center_middle" -> {0, 1, 0},
                "right_middle" -> {2, 1, 0},
                "left_forward" -> {-2, 2, 0},
                "left_half_forward" -> {-1, 2, 0},
                "right_half_forward" -> {1, 2, 0},
                "right_forward" -> {2, 2, 0},
                "left_front" -> {-2, 3, 0},
                "left_half_front" -> {-1, 3, 0},
                "center_front" -> {0, 3, 0},
                "right_half_front" -> {1, 3, 0},
                "right_front" -> {2, 3, 0}
            },
            "folded" -> {},
            "polygons" -> <|
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
            |>,
            "creases" -> <|
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
            |>,
            "fractals" -> {
            }
        |>
    |>

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
        {x, y, z} = Re /@ trilaterate[{radius1, radius2, radius3}, {{d}, {i, j}}];
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

(* Folding *)
resetFolded[origami_] :=
    Module[{points, newOrigami},
        points = Union[Values[<|origami[["points"]]|>]];
        newOrigami = origami;
        newOrigami["folded"] = Table[point -> N[point], {point, points}];
        newOrigami
    ]

getFolded[origami_] :=
    origami[["points"]] /. origami[["folded"]]

updateFolded[points_, origami_] :=
    Module[{newFolded, newOrigami},
        newFolded = <|origami[["folded"]]|>;
        Do[
            newFolded[Key[point[[1]] /. origami[["points"]]]] = point[[2]],
            {point, points}
        ];
        newOrigami = origami;
        newOrigami["folded"] = Normal[newFolded];
        newOrigami
    ]

calculateUp[foldedTriangle_, triangle_] :=
    Module[{up},
        up = calculateBinormal[triangle];
        If[up[[3]] < 0,
            -calculateBinormal[foldedTriangle],
            calculateBinormal[foldedTriangle]
        ]
    ]

calculateFoldedVectors[polygonName_, origami_] :=
    Module[{points, triange, up},
        triangle = Take[origami[["polygons"]][[polygonName]], 3] /.
            origami[["points"]];
        foldedTriangle =
            Take[
                origami[["polygons"]][[polygonName]],
                3
            ] /. getFolded[origami];
        center = Mean[foldedTriangle];
        up = calculateUp[
            foldedTriangle,
            triangle
        ];
        down = -up;
        {
            center + up,
            center + down
        }
    ]

calculatePolygonVectorAngle[polygonName1_, polygonName2_, origami_] :=
    Module[{creases1, creases2, axialCrease, axialMidpoint, binormal1,
        binormal2, points, foldedPoints},
        creases1 = origami[["polygons"]][[polygonName1]];
        creases2 = origami[["polygons"]][[polygonName2]];
        foldedPoints = getFolded[origami];
        axialCrease = Intersection[creases1, creases2][[1]];
        axialMidpoint = Mean[axialCrease /. foldedPoints];
        binormal1 = calculateBinormal[Take[creases1, 3]
            /. foldedPoints];
        binormal2 = calculateBinormal[Take[creases2, 3]
            /. foldedPoints];
        2 Pi - VectorAngle[binormal1, binormal2]
    ]

calculateCreaseDirection[polygonName1_, polygonName2_, origami_] :=
    Module[{up1, down1, up2, down2, upDistance, downDistance},
        {up1, down1} =
            calculateFoldedVectors[
                polygonName1,
                origami
            ];
        {up2, down2} =
            calculateFoldedVectors[
                polygonName2,
                origami
            ];
        upDistance = EuclideanDistance[up1, up2];
        downDistance = EuclideanDistance[down1, down2];

        If[upDistance < downDistance,
            "valley",
            "mountain"
        ]
    ]

convertPolygonToCreases[polygon_] :=
    Sort /@ Partition[polygon, 2, 1, {1, 1}]

rotatePolygonsInCreasePattern[polygonNames_, angle_, {creaseStart_, creaseEnd_},
    origami_] :=
    Module[{pointName, foldedPoints, points, point, rotation},
        points = <||>;
        rotation = RotationMatrix[
                angle,
                creaseEnd - creaseStart
            ];
        foldedPoints = getFolded[origami];
        Do[
            point = <|foldedPoints|>[[pointName]];
            points[pointName] =
                rotation.(point - creaseStart) + creaseStart,
            {polygonName, polygonNames},
            {pointName, origami[["polygons"]][[polygonName]]}
        ];
        updateFolded[Normal[points], origami]
    ]

getCreaseType[crease_, origami_] :=
    If[MemberQ[Sort /@ origami[["creases"]][["mountain"]], Sort[crease]],
        "mountain",
        "valley"
    ]

calculateCreaseDirectionalAngle[polygonName1_, polygonName2_, axialCrease_,
    origami_] :=
    Module[{creaseDirection, angle},
        creaseType = getCreaseType[axialCrease, origami];
        creaseDirection = calculateCreaseDirection[
            polygonName1,
            polygonName2,
            origami
        ];
        angle = calculatePolygonVectorAngle[
            polygonName1,
            polygonName2,
            origami 
        ];
        If[creaseDirection == creaseType,
            angle,
            -angle
        ]
    ]

manipulateCrease[polygonName1_, polygonName2_, polygons_, angle_, origami_] :=
    Module[{axialCrease, creaseStart, creaseEnd, origami1, origami2,
        creaseType, creaseDirection1, creaseDirection2, angle1, angle2},
        creases1 = convertPolygonToCreases[
            origami[["polygons"]][[polygonName1]]
        ];
        creases2 = convertPolygonToCreases[
            origami[["polygons"]][[polygonName2]]
        ];
        axialCrease = Intersection[creases1, creases2][[1]];
        {creaseStart, creaseEnd} = axialCrease /. getFolded[origami];
        origami1 = rotatePolygonsInCreasePattern[
            Append[polygons, polygonName1],
            angle,
            {creaseStart, creaseEnd},
            origami
        ];
        origami2 = rotatePolygonsInCreasePattern[
            Append[polygons, polygonName1],
            -angle,
            {creaseStart, creaseEnd},
            origami
        ];
        creaseType = getCreaseType[axialCrease, origami];

        angle1 = calculateCreaseDirectionalAngle[
            polygonName1,
            polygonName2,
            axialCrease,
            origami1
        ];
        angle2 = calculateCreaseDirectionalAngle[
            polygonName1,
            polygonName2,
            axialCrease, 
            origami2
        ];

        If[angle2 > angle1,
            origami1,
            origami2
        ]
    ]

findPointNeighbors[pointName_, polygons_] :=
    Flatten[Cases[polygons, polygon_ /; MemberQ[polygon, pointName]]]

findReferenceNeighbors[pointName_, origami_] :=
    Module[{neighborNames, referencePoints, point, referenceDistances,
        foldedPoints},
        foldedPoints = getFolded[origami];
        point = pointName /. foldedPoints;
        referenceNeighborNames = Intersection[
            findPointNeighbors[
                pointName,
                Values[origami[["polygons"]]]
            ],
            findKnownPoints[origami]
        ];
(*         Print[neighborNames];
        referenceNeighborNames = Cases[
            Keys[<|origami[["points"]]|>],
            (name_ -> point_) /; MemberQ[neighborNames, name] :> name
        ]; *)
        If[Length[referenceNeighborNames] >= 3,
            referencePoints = referenceNeighborNames /.
                foldedPoints;
            referenceDistances = EuclideanDistance[point, #]& /@
                (referenceNeighborNames /. origami[["points"]]);
            Take[
                Thread[
                        {
                            referencePoints,
                            referenceDistances
                        }
                    ],
                3
            ]
        ]
    ]

findKnownPoints[origami_] :=
    Cases[
        origami[["points"]],
        (name_ -> point_) /; MemberQ[Keys[<|origami[["folded"]]|>], point] :>
            name
    ]

findUnknownPoints[origami_] :=
    Complement[
        Keys[<|origami[["points"]]|>],
        findKnownPoints[origami]
    ]

findCalculablePoint[origami_] :=
    FirstCase[
        findUnknownPoints[origami],
        pointName_ /; findReferenceNeighbors[
                pointName,
                origami
            ] =!= Null :> {
                pointName,
                findReferenceNeighbors[
                    pointName,
                    origami
                ]
            }
    ]

calculateCreaseAngleAverage[origami_] :=
    Module[{creases, polygons, polygonName1, polygonName2,
        creasePolygons},
        creases = Union[
            origami[["creases"]][["mountain"]],
            origami[["creases"]][["valley"]]
        ];
        polygons = origami[["polygons"]];
        Mean[
            Table[
                creasePolygons = Cases[
                    Normal[polygons],
                    (name_ ->
                        polygon_ /; MemberQ[
                            convertPolygonToCreases[polygon],
                            Sort[crease]
                        ]) :>
                    name
                ];
                If[Length[creasePolygons] == 2,
                    {polygonName1, polygonName2} = creasePolygons;
                    calculateCreaseDirectionalAngle[
                        polygonName1,
                        polygonName2,
                        crease,
                        origami
                    ],
                    Unevaluated[Sequence[]]
                ],
                {crease, creases}
            ]
        ]
    ]

calculateAnyPoint[origami_, creasePattern_] :=
    Module[{reference, point, pointName, points, point1, point2,
        origami1, origami2},
        point = findCalculablePoint[origami];
        If[Head[point] =!= Missing,
            {pointName, reference} = point;
            {point1, point2} = trilaterate@@reference;
            points = <||>;
            points[pointName] = point1;
            origami1 = updateFolded[Normal[points], origami];
            origami1 = sizeCreasePattern[
                findKnownPoints[origami1],
                origami1,
                creasePattern    
            ];
            origami2 = origami1;
            points[pointName] = point2;
            origami2 = updateFolded[Normal[points], origami2];

            If[calculateCreaseAngleAverage[origami1] <
                calculateCreaseAngleAverage[origami2],
                origami1,
                origami2
            ]
        ]
    ]

fold[origami_, creasePattern_] :=
    Module[{lastOrigami, nextOrigami},
        lastOrigami = origami;
        nextOrigami = lastOrigami;
        While[nextOrigami =!= Null,
            lastOrigami = nextOrigami;
            nextOrigami = calculateAnyPoint[
                lastOrigami,
                creasePattern
            ]
        ];

        lastOrigami
    ]

sizeCreasePattern[pointNames_, origami_, creasePattern_] :=
    Module[{newCreasePattern, points, labels, folded},
        newCreasePattern = origami;

        points = <|creasePattern[["points"]]|>;

        folded = <|origami[["folded"]]|>;
        labels = points[[#]]& /@ pointNames;
        newCreasePattern["folded"] =
            # -> folded[[Key[#]]]& /@
                labels;

        newCreasePattern
    ]

(* Fractal *)
(* createDiamondBaseInstancePolygons[i_, terminal_, scale_, creasePattern_] :=
    Module[{id, parentID, leftMiddle, centerMiddle, rightMiddle, nextParentID},
        If[terminal,
            id = "_" <> ToString[scale] <> "_" <> ToString[i],
            id = "_" <> ToString[scale] <> "_" <> ToString[i],
        ];
        If[terminal,
            nextID = "_" <> ToString[scale] <> "_" <> ToString[i],
            nextID = "_" <> ToString[scale] <> "_" <> ToString[i + 1]
        ];
        If[scale == 1,
            parentID = "",
            If[terminal,
                parentID = "_" <> ToString[scale - 1] <> "_" <> ToString[IntegerPart[(i + 1)/2]],
                parentID = "_" <> ToString[scale - 1] <> "_" <> ToString[IntegerPart[(i + 1)/2]]
            ]
        ];
        If[scale == 1,
            nextParentID = "",
            If[terminal,
                nextParentID = "_" <> ToString[scale - 1] <> "_" <> ToString[IntegerPart[(i + 1)/2]],
                nextParentID = "_" <> ToString[scale - 1] <> "_" <> ToString[IntegerPart[(i + 1)/2] + 1]
            ]
        ];
        If[Mod[i, 2] == 0,
            side = "left";
            If[terminal,
                leftMiddle = "center_front" <> parentID;
                centerMiddle = "right_half_front" <> parentID;
                rightMiddle = "right_front" <> parentID,
                rightMiddle = "left_front" <> nextParentID;
                centerMiddle = "right_half_front" <> parentID;
                leftMiddle = "center_front" <> parentID
            ],
            side = "right";
            rightMiddle = "center_front" <> parentID;
            centerMiddle = "left_half_front" <> parentID;
            leftMiddle = "left_front" <> parentID
        ];
        Join[
            {
            "left_middle" <> id -> {
                leftMiddle, centerMiddle, "left_half_forward" <> id,
                "left_forward" <> id
            },
            "left_center" <> id -> {
                centerMiddle, "left_half_forward" <> id,
                "center_front" <> id
            },
            "right_center" <> id -> {
                centerMiddle, "right_half_forward" <> id,
                "center_front" <> id
            },
            "left_front" <> id -> {
                "left_front" <> id, "left_half_front" <> id, "left_half_forward" <> id,
                "left_forward" <> id
            },
            "left_half_front" <> id -> {
                "left_half_front" <> id, "center_front" <> id, "left_half_forward" <> id
            },
            "right_half_front" <> id -> {
                "right_half_front" <> id, "center_front" <> id, "right_half_forward" <> id
            }
        },
        If[terminal,
            {
                "right_front" <> id -> {
                    "right_front" <> id, "right_half_front" <> id, "right_half_forward" <> id,
                    "right_forward" <> id
                },
                "right_middle" <> id -> {
                    rightMiddle, centerMiddle, "right_half_forward" <> id,
                    "right_forward" <> id
                }
            },

            {
                "right_front" <> id -> {
                    "left_front" <> nextID, "right_half_front" <> id, "right_half_forward" <> id,
                    "left_forward" <> nextID
                },
                "right_middle" <> id -> {
                    rightMiddle, centerMiddle, "right_half_forward" <> id,
                    "left_forward" <> nextID
                }
            }
           ]
        ]
    ]

createDiamondBaseInstancePoints[i_, terminal_, scale_, creasePattern_] :=
    Module[{newCreasePattern, parent, parentPoints, suffix, side, step,
        parentName, referencePoint, newPoints},
        newCreasePattern = creasePattern;
        parentPoints = <|creasePattern[["points"]]|>;
        suffix = ToString[scale] <> "_" <> ToString[i - 1];
        If[Mod[i, 2] == 0,
            side = "left",
            side = "center"
        ];
        If[scale == 1,
            parentName = side <> "_front",
            parentName = side <> "_front_" <> ToString[scale - 1] <>
                "_" <> ToString[IntegerPart[i/2]]
        ];
        step = 1/(2^scale);
        referencePoint = parentPoints[[parentName]];
        If[terminal,
            {
                "right_forward_" <> suffix -> referencePoint + {4*step, step, 0},
                "right_front_" <> suffix -> referencePoint + {4*step, 2*step, 0}
            },
            {
                "right_half_forward_" <> suffix -> referencePoint +
                    {3*step, step, 0},
                "right_half_front_" <> suffix -> referencePoint +
                    {3*step, 2*step, 0},
                "left_forward_" <> suffix -> referencePoint + {0, step, 0},
                "left_half_forward_" <> suffix -> referencePoint +
                    {step, step, 0},
                "left_front_" <> suffix -> referencePoint + {0, 2*step, 0},
                "left_half_front_" <> suffix -> referencePoint +
                    {step, 2*step, 0},
                "center_front_" <> suffix -> referencePoint +
                    {2*step, 2*step, 0}
            }
        ]
    ]

createDiamondBaseRowPoints[scale_, creasePattern_] :=
    Module[{newCreasePattern, parent, lastIteration, parentPoints, suffix,
        side, step, parentName, referencePoint, newPoints},
        newCreasePattern = creasePattern;
        newPoints = Table[
            createDiamondBaseInstancePoints[i, False, scale, creasePattern],
            {i, 2, 2^scale + 1}
        ];
        newCreasePattern["points"] = Join[
            newCreasePattern[["points"]],
            Flatten[newPoints],
            createDiamondBaseInstancePoints[2^scale + 1, True, scale, creasePattern]
        ];
        newCreasePattern
    ]

createDiamondBaseRow[scale_, creasePattern_] :=
    Module[{newCreasePattern, newPolygons},
        newCreasePattern = createDiamondBaseRowPoints[scale, creasePattern];
        newPolygons = Table[
            createDiamondBaseInstancePolygons[i, False, scale, newCreasePattern],
            {i, 1, 2^scale}
        ];
        newCreasePattern[["polygons"]] = <|
            Join[
               Normal[creasePattern[["polygons"]]],
               newPolygons,
               createDiamondBaseInstancePolygons[2^scale, True, scale, newCreasePattern]
            ]
        |>;
        newCreasePattern
    ] *)

(* Render *)
renderPolygon[polygon_, points_] :=
    Polygon[polygon /. points]

renderCrease[crease_, type_, points_] :=
    {
        If[type == "mountain", Red, Blue],
        Tube[(crease /. points), .01]
    }

renderOrigami[origami_] :=
    Module[{points, polygons, creases},
        points = getFolded[origami];
        polygons = origami[["polygons"]];
        creases = origami[["creases"]];

        {
            Table[
                renderPolygon[polygon, points],
                {polygon, Values[polygons]}
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
        "folded" -> {},
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
