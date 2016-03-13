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
        "fractals" -> {},
        "type" -> "crease_pattern"
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
                    {"left_half_front", "left_half_forward"},
                    {"right_middle", "right_forward"},
                    {"left_middle", "left_forward"},
                    {"right_front", "right_half_front"},
                    {"right_half_front", "center_front"},
                    {"center_front", "left_half_front"},
                    {"left_half_front", "left_front"}
                },
                "valley" -> {
                    {"center_middle", "left_half_forward"},
                    {"center_middle", "right_half_forward"},
                    {"left_forward", "left_half_forward"},
                    {"right_half_forward", "right_forward"},
                    {"left_half_forward", "center_front"},
                    {"center_front", "right_half_forward"},
                    {"right_forward", "right_front"},
                    {"left_forward", "left_front"}
                }
            |>,
            "fractals" -> <|
                "origin" -> "center_middle",
                "anchors" -> <|
                    "L" -> "left_half_front",
                    "R" -> "right_half_front"
                |>,
                "scale" -> 2
            |>,
            "type" -> "crease_pattern"
        |>
    |>

creasePatternLibrary["fractal_diamond_base"] =
    Module[{fractalDiamondBase},
        fractalDiamondBase = creasePatternLibrary[["diamond_base"]];
        fractalDiamondBase[["polygons"]] = Fold[
            Delete[#1, #2]&,
            fractalDiamondBase[["polygons"]],
            {"right_back", "left_back"}
        ];
        fractalDiamondBase[["creases"]][["mountain"]] = Complement[
            Normal[fractalDiamondBase[["creases"]][["mountain"]]],
            {{"center_back", "center_middle"}}
        ];
        fractalDiamondBase[["points"]] = Complement[
            fractalDiamondBase[["points"]],
            {
                "left_back" -> {-2, 0, 0},
                "center_back" -> {0, 0, 0},
                "right_back" -> {2, 0, 0}
            }
        ];
        fractalDiamondBase
    ];

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

reframe[point_, sourceFramePoints_, destinationFramePoints_] :=
    Module[{sourceFrameVectors, destinationFrameVectors, sourceFrame,
        destinationFrame, framelessPoint},
        sourceFrameVectors = # - sourceFramePoints[[2]]& /@ sourceFramePoints;
        destinationFrameVectors = # - destinationFramePoints[[2]]& /@
            destinationFramePoints;
        {sourceFrame, destinationFrame} = Table[
            axis[points],
            {points, {sourceFrameVectors, destinationFrameVectors}},
            {axis, {calculateTangent, calculateBinormal, calculateNormal}}
        ];
        framelessPoint = recast[
            point - sourceFramePoints[[2]],
            sourceFrame
        ];
        destinationFramePoints[[2]] + (Plus@@(framelessPoint destinationFrame))
    ]

calculateBinormal[{p1_, p2_, p3_}] :=
    Normalize[Cross[calculateTangent[{p1, p2, p3}], p1 - p2]]

calculateTangent[{p1_, p2_, p3_}] :=
    Normalize[(p1 - p2) - (p3 - p2)]

calculateNormal[{p1_, p2_, p3_}] :=
    Normalize[
        Cross[calculateTangent[{p1, p2, p3}], calculateBinormal[{p1, p2, p3}]]
    ]

calculatePlanePointDistance[{point1_, point2_, point3_}, point0_] :=
    Normalize[Cross[point2 - point1, point3 - point1]].(point0 - point1)

(* Folding *)
extractPointKey[pointName_, paper_]  :=
    extractPointKey[pointName, paper[["crease_pattern"]]] /;
        paper[["type"]] === "origami"

extractPointKey[pointName_, paper_]  :=
    (pointName /. paper[["points"]]) /;
        paper[["type"]] === "crease_pattern"

extractPointKeys[pointNames_, paper_] :=
    extractPointKey[#, paper]& /@ pointNames

extractPointValue[pointName_, origami_] :=
    extractPointKey[pointName, origami] /. origami[["points"]]

extractPointValues[pointNames_, origami_] :=
    extractPointValue[#, origami]& /@ pointNames

makeOrigamiFromCreasePattern[creasePattern_] :=
    <|
        "points" -> {},
        "crease_pattern" -> creasePattern,
        "type" -> "origami"
    |>

setOrigamiToCreasePattern[origami_] :=
    Module[{points, newOrigami},
        points = Union[Values[<|origami[["crease_pattern"]][["points"]]|>]];
        newOrigami = origami;
        newOrigami["points"] = Table[
            point -> N[point], {point, points}
        ];
        newOrigami
    ]

getCreases[creaseType_, paper_] :=
    Union[Sort /@ (creasePattern[["creases"]][[creaseType]]
        /. creasePattern[["points"]])] /;
            paper[["type"]] === "crease_pattern"

getCreases[creaseType_, paper_] :=
    getCreases[paper[["crease_pattern"]]] /;
        paper[["type"]] === "origami"

updateFoldedPoints[points_, origami_] :=
    Module[{newFolded, newOrigami},
        newFolded = <|origami[["points"]]|>;
        Do[
            newFolded[Key[extractPointKey[point[[1]], origami]]] = point[[2]],
            {point, points}
        ];
        newOrigami = origami;
        newOrigami["points"] = Normal[newFolded];
        newOrigami
    ]

compositeCreasePatterns[creasePattern1_, creasePattern2_] :=
    Module[{newCreasePattern},
        newCreasePattern = creasePattern1;
        newCreasePattern[["points"]] = Join[
            creasePattern1[["points"]],
            creasePattern2[["points"]]
        ];
        newCreasePattern[["polygons"]] = Join[
            creasePattern1[["polygons"]],
            creasePattern2[["polygons"]]
        ];
        newCreasePattern[["creases"]][["mountain"]] = Join[
            creasePattern1[["creases"]][["mountain"]],
            creasePattern2[["creases"]][["mountain"]]
        ];
        newCreasePattern[["creases"]][["valley"]] = Join[
            creasePattern1[["creases"]][["valley"]],
            creasePattern2[["creases"]][["valley"]]
        ];
        newCreasePattern
    ]

fractureCreasePattern[anchorName_, creasePattern_] :=
    Module[{newCreasePattern, origin, anchorPoint, scale, prefix},
        newCreasePattern = creasePattern;
        origin = creasePattern[["fractals"]][["origin"]] /.
            creasePattern[["points"]];
        anchorPoint = creasePattern[["fractals"]][["anchors"]][[anchorName]]
            /. creasePattern[["points"]];
        scale = creasePattern[["fractals"]][["scale"]];
        newCreasePattern[["points"]] = Cases[
            newCreasePattern[["points"]],
            (name_ -> point_) :> (name -> ((point - origin)/scale + anchorPoint))
        ];
        prefix = anchorName;
        newCreasePattern[["polygons"]] = <|
            Normal[newCreasePattern[["polygons"]]] /.
                x_ /; StringQ[x] :> prefix <> x
        |>;
        newCreasePattern[["points"]] = newCreasePattern[["points"]] /.
            x_ /; StringQ[x] :> prefix <> x;
        newCreasePattern[["creases"]][["mountain"]] =
            newCreasePattern[["creases"]][["mountain"]] /.
                x_ /; StringQ[x] :> prefix <> x;
        newCreasePattern[["creases"]][["valley"]] =
            newCreasePattern[["creases"]][["valley"]] /.
                x_ /; StringQ[x] :> prefix <> x;
        newCreasePattern[["fractals"]][["anchors"]] = <|
            Cases[
                Normal[newCreasePattern[["fractals"]][["anchors"]]],
                (anchor_ -> point_) :> (anchor -> (prefix <> point))
            ]
        |>;
        newCreasePattern[["fractals"]][["origin"]] =
            prefix <> newCreasePattern[["fractals"]][["origin"]];
        newCreasePattern
    ]

calculateUp[foldedTriangle_, triangle_] :=
    Module[{up},
        up = calculateBinormal[triangle];
        If[up[[3]] < 0,
            -.001 calculateBinormal[foldedTriangle],
            .001 calculateBinormal[foldedTriangle]
        ]
    ]

calculateFoldedVectorsFromOrigin[polygonName_, origami_] :=
    Module[{points, triange, up},
        triangle = extractPointKeys[
            Take[origami[["crease_pattern"]][["polygons"]][[polygonName]], 3],
            origami
        ];
        foldedTriangle = extractPointValues[
            Take[
                origami[["crease_pattern"]][["polygons"]][[polygonName]],
                3
            ],
            origami
        ];
        center = Mean[foldedTriangle];
        up = calculateUp[
            foldedTriangle,
            triangle
        ];
        down = -up;
        {up, down}
    ]

calculateFoldedVectors[polygonName_, origami_] :=
    Module[{points, triange, up},
        triangle = extractPointKeys[
            Take[origami[["crease_pattern"]][["polygons"]][[polygonName]], 3],
            origami
        ];
        foldedTriangle = extractPointValues[
            Take[
                origami[["crease_pattern"]][["polygons"]][[polygonName]],
                3
            ],
            origami
        ];
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
    Module[{calculateVector, creases1, creases2, binormal1, binormal2, crease},
        calculateVector = Function[{polygonName},
            Module[{creases, creasePattern},
                creasePattern = origami[["crease_pattern"]];
                creases = creasePattern[["polygons"]][[polygonName]];
                calculateBinormal[
                    extractPointValues[
                        Take[creases, 3],
                        origami
                    ]
                ]
            ]
        ];
        VectorAngle[
            calculateVector[polygonName1],
            calculateVector[polygonName2]
        ]
    ]

calculateCreaseDirection[polygonName1_, polygonName2_, origami_] :=
    Module[{up1, down1, up2, down2, upDistance, downDistance, crease, creaseType,
        creasePattern, triangle1, triangle2, binormal1, binormal2, side1, side2,
        distance1, distance2},
        creasePattern = origami[["crease_pattern"]];
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

(*         crease = Intersection[
            convertPolygonToCreases[creasePattern[["polygons"]][[polygonName1]]],
            convertPolygonToCreases[creasePattern[["polygons"]][[polygonName2]]]
        ][[1]];
 *)
        (* creaseType = getCreaseType[crease, origami]; *)
        If[upDistance == downDistance,
            {up1, down1} =
                calculateFoldedVectorsFromOrigin[
                    polygonName1,
                    origami
                ];
            {up2, down2} =
                calculateFoldedVectorsFromOrigin[
                    polygonName2,
                    origami
                ];
            If[EuclideanDistance[up1, up2] < EuclideanDistance[up1, -up2],
                0,
                Pi//N
            ],
            (* triangle1 = (Take[
                creasePattern[["polygons"]][[polygonName1]],
                3
            ] /. creasePattern[["points"]]) /. origami[["points"]];
            triangle2 = (Take[
                creasePattern[["polygons"]][[polygonName2]],
                3
            ] /. creasePattern[["points"]]) /. origami[["points"]]; *)

            (* binormal1 = calculateBinormal[triangle1];
            binormal2 = calculateBinormal[triangle2];
            side1 = calculatePlanePointDistance[triangle1, binormal1 + Mean[triangle1]] > 0;
            side2 = calculatePlanePointDistance[triangle2, binormal2 + Mean[triangle2]] > 0;
            distance1 = calculatePlanePointDistance[triangle1, up2];
            distance2 = calculatePlanePointDistance[triangle2, up1]; *)
            (* If[side1,
                distance1 = -distance1
            ];
            If[side2,
                distance2 = -distance2
            ]; *)
            (* Print[{"mv", polygonName1, polygonName2, distance1, distance2, side1, side2}]; *)
            If[upDistance < downDistance,
                "valley",
                "mountain"
            ]
        ]
    ]

convertPolygonToCreases[polygon_] :=
    Sort /@ Partition[polygon, 2, 1, {1, 1}]

rotatePolygonsInCreasePattern[polygonNames_, angle_, {creaseStart_, creaseEnd_},
    origami_] :=
    Module[{foldedPoints, points, point, rotation},
        points = <||>;
        rotation = RotationMatrix[
                angle,
                creaseEnd - creaseStart
            ];
        Do[
            point = extractPointValue[pointName, origami];
            points[pointName] =
                rotation.(point - creaseStart) + creaseStart,
            {polygonName, polygonNames},
            {pointName, origami[["crease_pattern"]][["polygons"]][[polygonName]]}
        ];
        updateFoldedPoints[Normal[points], origami]
    ]

polygonsForCrease[crease_, creasePattern_] :=
    Cases[
        Normal[creasePattern[["polygons"]]],
        (polygonName_ -> (polygon_/;
            MemberQ[
                Sort[extractPointKeys[#, creasePattern]]& /@ convertPolygonToCreases[polygon],
                Sort[extractPointKeys[crease, creasePattern]]
            ])) :> polygonName
    ] /; creasePattern[["type"]] == "crease_pattern"

polygonsForCrease[crease_, origami_] :=
    polygonsForCrease[crease, origami[["crease_pattern"]]] /;
        origami[["type"]] == "origami"

getCreaseType[crease_, creasePattern_] :=
    If[MemberQ[Sort /@ (creasePattern[["creases"]][["mountain"]] /. creasePattern[["points"]]), Sort[crease /. creasePattern[["points"]]]],
        "mountain",
        If[MemberQ[Sort /@ (creasePattern[["creases"]][["valley"]] /. creasePattern[["points"]]), Sort[crease /. creasePattern[["points"]]]],
            "valley",
            Throw[crease]
        ]
    ] /; creasePattern[["type"]] == "crease_pattern"

getCreaseType[crease_, origami_] :=
    getCreaseType[crease, origami[["crease_pattern"]]] /;
        origami[["type"]] == "origami"

calculateCreaseDirectionalAngle[polygonName1_, polygonName2_, axialCrease_,
    origami_] :=
    Module[{creaseDirection, angle, a},
        creaseType = getCreaseType[axialCrease, origami];
        creaseDirection = calculateCreaseDirection[
            polygonName1,
            polygonName2,
            origami
        ];
        If[NumberQ[creaseDirection],
            creaseDirection,
            angle = calculatePolygonVectorAngle[
                polygonName1,
                polygonName2,
                origami 
            ];
(*             If[EuclideanDistance[angle, Pi] < .0000001 || EuclideanDistance[angle, -Pi] < .0000001,
                If[creaseDirection != creaseType,
                    -(Pi - angle),
                    Pi - angle
                ],
*)              If[creaseDirection == creaseType,
                    Pi - angle,
                    -(Pi - angle)
                ]
            (* ] *)
        ]
    ]

alignPolygon[polygonName1_, polygonName2_, origami_] :=
    Module[{polygon1, polygon2, points1, points2, folded, sourceFramePoints,
        destinationFramePoints, newPoints, newFolded, creasePattern},
        creasePattern = origami[["crease_pattern"]];
        polygon1 = creasePattern[["polygons"]][[polygonName1]];
        polygon2 = creasePattern[["polygons"]][[polygonName2]];
        points1 = (polygon1 /. creasePattern[["points"]]);
        points2 = (polygon2 /. creasePattern[["points"]]);
        folded = extractPointValues[polygon2, origami];
        sourceFramePoints = Take[points2, 3];
        destinationFramePoints = Take[folded, 3];
        newPoints = reframe[#, sourceFramePoints, destinationFramePoints]& /@
            points1;
        newFolded = (#[[1]] -> #[[2]])& /@ Thread[{points1, newPoints}];
        updateFoldedPoints[newFolded, origami]
    ]

selectCrease[creaseType_, creaseDirection1_, angle1_, creaseDirection2_, angle2_] :=
    1 /; creaseDirection1 == creaseType && creaseDirection2 != creaseType

selectCrease[creaseType_, creaseDirection1_, angle1_, creaseDirection2_, angle2_] :=
    2 /; creaseDirection1 != creaseType && creaseDirection2 == creaseType

selectCrease[creaseType_, creaseDirection1_, angle1_, creaseDirection2_, angle2_] :=
    If[angle1 < angle2, 1, 2] /; creaseDirection1 != creaseType && creaseDirection2 != creaseType

selectCrease[creaseType_, creaseDirection1_, angle1_, creaseDirection2_, angle2_] :=
    If[angle1 > angle2, 1, 2] /; creaseDirection1 == creaseType && creaseDirection2 == creaseType

manipulateCrease[polygonName1_, polygonName2_, polygons_, angle_, origami_] :=
    Module[{axialCrease, creaseStart, creaseEnd, origami1, origami2,
        creaseType, creaseDirection1, creaseDirection2, angle1, angle2,
        creasePattern, creaseStart2, creaseEnd2, creaseIndex, points1, points2},
        creasePattern = origami[["crease_pattern"]];
        points1 = extractPointKeys[creasePattern[["polygons"]][[polygonName1]], origami];
        points2 = extractPointKey[creasePattern[["polygons"]][[polygonName2]], origami];
(*         creases1 = convertPolygonToCreases[
            creasePattern[["polygons"]][[polygonName1]]
        ];
        creases2 = convertPolygonToCreases[
            creasePattern[["polygons"]][[polygonName2]]
        ]; *)
        creases1 = convertPolygonToCreases[points1];
        creases2 = convertPolygonToCreases[points2];
        axialCrease = Intersection[creases1, creases2][[1]];
        (* {creaseStart, creaseEnd} = extractPointValues[axialCrease, origami]; *)
        {creaseStart, creaseEnd} = axialCrease /. origami[["points"]];

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

        creaseType = getCreaseType[axialCrease /. creasePattern[["points"]], creasePattern];

        angle1 = Abs[
            calculateCreaseDirectionalAngle[
                polygonName1,
                polygonName2,
                Sort[axialCrease],
                origami1
            ]
        ];
        angle2 = Abs[
            calculateCreaseDirectionalAngle[
                polygonName1,
                polygonName2,
                Sort[axialCrease], 
                origami2
            ]
        ];
        creaseDirection1 = calculateCreaseDirection[polygonName1, polygonName2, origami1];
        creaseDirection2 = calculateCreaseDirection[polygonName1, polygonName2, origami2];

        creaseIndex = selectCrease[creaseType, creaseDirection1, angle1, creaseDirection2, angle2];
        If[angle < 0, Reverse, Identity][{origami1, origami2}][[creaseIndex]]
    ]

findPointNeighbors[pointName_, polygons_] :=
    Union[
        Flatten[
            Cases[
                polygons,
                polygon_ /; MemberQ[polygon, pointName]
            ],
            1
        ]
    ]

findReferenceNeighbors[pointName_, origami_] :=
    Module[{neighborNames, referencePoints, point, referenceDistances,
        foldedPoints},
        foldedPoints = origami[["points"]];
        point = extractPointKey[pointName, origami];
        referenceNeighborNames = Intersection[
            findPointNeighbors[
                extractPointKey[pointName, origami],
                origami[["crease_pattern"]][["polygons"]] /. origami[["crease_pattern"]][["points"]]
            ],
            findKnownPoints[origami]
        ];
        If[Length[referenceNeighborNames] >= 3,
            referencePoints = referenceNeighborNames /.
                foldedPoints;
            referenceDistances = EuclideanDistance[point, #]& /@
                referenceNeighborNames;
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
        origami[["crease_pattern"]][["points"]],
        (name_ -> point_) /; MemberQ[Keys[<|origami[["points"]]|>], point] :>
            point
    ]

findUnknownPoints[origami_] :=
    Complement[
        Values[<|origami[["crease_pattern"]][["points"]]|>],
        findKnownPoints[origami]
    ]

findCalculablePoint[origami_, polygon_] :=
    Module[{calculable},
        calculable = Cases[
            Intersection[findUnknownPoints[origami], origami[["crease_pattern"]][["polygons"]][[polygon]] /. origami[["crease_pattern"]][["points"]]],
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
            ];
        If[Length[calculable] > 0,
            RandomChoice[calculable]
        ]
    ]

canonicalizeCrease[crease_, creasePattern_] :=
    Sort[crease /. creasePattern[["points"]]]

polygonFoundQ[polygonName_, origami_] :=
    (
        Length[
            Complement[
                origami[["crease_pattern"]][["polygons"]][[polygonName]] /. origami[["crease_pattern"]][["points"]],
                findKnownPoints[origami]
            ]
        ] == 0
    )

placePolygon[polygonName_, origami_] :=
    Module[{pointLabels, foundLabels, placedPolygon, referenceLabels,
        referencePoints, frame, newPoints, recastPoints, newFolded},
        pointLabels = extractPointKeys[origami[["crease_pattern"]][["polygons"]][[polygonName]], origami];
        If[Length[Complement[pointLabels, findKnownPoints[origami]]] == 0,
            origami,
            foundLabels = Intersection[pointLabels, Keys[<|origami[["points"]]|>]];
            If[Length[foundLabels] < 3,
                origami,
                referenceLabels = Take[foundLabels, 3];
                newPoints = reframe[
                    #,
                    referenceLabels,
                    referenceLabels /. origami[["points"]]
                ]& /@ Complement[pointLabels, findKnownPoints[origami]];
                newFolded = (#[[1]] -> #[[2]])& /@ Thread[{Complement[pointLabels, findKnownPoints[origami]], newPoints}];
                updateFoldedPoints[newFolded, origami]
            ]
        ]
    ]

calculateCreaseAngleAverage[origami_] :=
    Module[{creases, polygons, polygonName1, polygonName2,
        creasePolygons, creasePattern, c, flex, flexed, samples},
        flexed = False;
        creasePattern = origami[["crease_pattern"]];
        creases = Sort[
            Union[
                extractPointKeys[creasePattern[["creases"]][["mountain"]], origami],
                extractPointKeys[creasePattern[["creases"]][["valley"]], origami]
            ]
        ];
        polygons = creasePattern[["polygons"]] /. creasePattern[["points"]];
        samples = Table[
            creasePolygons = Cases[
                Normal[polygons],
                (name_ ->
                    polygon_ /; MemberQ[
                        Sort /@
                            convertPolygonToCreases[polygon],
                        crease
                    ]) :>
                name
            ];
            If[Length[creasePolygons] == 2,
                {polygonName1, polygonName2} = creasePolygons;
                If[polygonFoundQ[polygonName1, origami] && polygonFoundQ[polygonName2, origami],
                    c = calculateCreaseDirectionalAngle[
                        polygonName1,
                        polygonName2,
                        crease,
                        origami
                    ];
                    flex = Normal[origami[["crease_pattern"]][["creases"]][["flexible"]]];
                    flex = Cases[flex, (a_ /; (Sort[extractPointKeys[a, origami]] == Sort[crease]) -> b_) :> b];
                    If[Length[flex] > 0,
                        c = c - flex[[1]];
                        {Pi - Abs[c]/2, True},
                        {c, False}
                    ],
                    Unevaluated[Sequence[]]
                ],
                Unevaluated[Sequence[]]
            ],
            {crease, creases}
        ];
        If[Or@@Table[sample[[2]], {sample, samples}],
            Re[Mean[Cases[samples, {value_, True} :> value]]],
            Re[Mean[Cases[samples, {value_, False} :> value]]]
        ]
    ]

findCloserOrigami[origami_, origami1_, origami2_] :=
    Module[{pointNames, points, points1, points2, distance1, distance2},
        pointNames = Values[<|origami1[["crease_pattern"]][["points"]]|>];
        points = extractPointValues[pointNames, origami];
        points1 = extractPointValues[pointNames, origami1];
        points2 = extractPointValues[pointNames, origami2];
        {Max[Table[EuclideanDistance[points[[i]], points1[[i]]], {i, Length[points]}]], 
            Max[Table[EuclideanDistance[points[[i]], points2[[i]]], {i, Length[points]}]]}
    ]

calculateAnyPoint[origami_, lastOrigami_, polygon_] :=
    Module[{reference, point, pointName, points, point1, point2,
        origami1, origami2, involvedPolygons, currentAverage, currentAverage1,
        currentAverage2, creasePattern, pointKey, a, b},
        creasePattern = origami[["crease_pattern"]];
        point = findCalculablePoint[origami, polygon];
        If[point =!= Null,
            {pointName, reference} = point;
            (* Print[{"pointName", pointName}]; *)
            {point1, point2} = trilaterate@@reference;
            points = <||>;
            points[pointName] = point1;
            origami1 = updateFoldedPoints[Normal[points], origami];
            Do[
                origami1 = placePolygon[polygonName, origami1],
                {polygonName, Keys[creasePattern[["polygons"]]]}
            ];
            points[pointName] = point2;
            origami2 = updateFoldedPoints[Normal[points], origami];
            Do[
                origami2 = placePolygon[polygonName, origami2],
                {polygonName, Keys[creasePattern[["polygons"]]]}
            ];

            (* Print[Graphics3D /@ renderOrigami /@ {origami1, origami2}];
            Pause[.5]; *)

(*             currentAverage = calculateCreaseAngleAverage[origami];
            currentAverage1 = calculateCreaseAngleAverage[origami1];
            currentAverage2 = calculateCreaseAngleAverage[origami2];
 *)


            (* Print[{extractPointKeys[{"Lright_forward", "Rleft_forward"}, origami], extractPointKey[pointName, origami]}]; *)
            (* If[currentAverage1 > currentAverage2,
                origami1,
                origami2
            ] *)
            {a, b} = findCloserOrigami[lastOrigami, origami1, origami2];
            If[Abs[a - b] < .01,
                currentAverage = calculateCreaseAngleAverage[origami];
                currentAverage1 = calculateCreaseAngleAverage[origami1];
                currentAverage2 = calculateCreaseAngleAverage[origami2];
                If[currentAverage1 > currentAverage2,
                    origami1,
                    origami2
                ],
                If[a < b,
                    origami1,
                    origami2
                ]
            ]

        ]
    ]

fold[origami_, previousOrigami_, solveOrder_] :=
    Module[{lastOrigami, possibleOrigami, currentOrigami, foundPolygons, lastFoundPolygons,
        angleAverage1, angleAverage2, angleAverage, found1, found2, suborigami1 ,suborigami2,
        order},
        order = solveOrder;
        currentOrigami = origami;
        While[currentOrigami =!= Null && Length[order] > 0,
            lastOrigami = currentOrigami;
            If[!polygonFoundQ[order[[1]], currentOrigami],
                currentOrigami = calculateAnyPoint[
                    currentOrigami,
                    previousOrigami,
                    order[[1]]
                ];
            ];
            order = Rest[order];
            (* Print[currentOrigami]; *)
(*             Print[Graphics3D[renderOrigami[currentOrigami]]];
            Pause[1]
            ;
 *)        ];
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

(* Render *)
renderPolygon[polygon_, points_] :=
    Polygon[polygon /. points]

renderCrease[crease_, type_, origami_] :=
    {
        If[type == "mountain", Red, Blue],
        If[And@@(
            NumericQ /@ 
                Flatten[crease /. origami[["crease_pattern"]][["points"]]]
            ),
            Tube[(crease /. origami[["crease_pattern"]][["points"]]) /. origami[["points"]]],
            Unevaluated[Sequence[]]
        ]
    }

numberOfFoundPolygons[origami_] :=
    Module[{points, polygons, creases, creasePattern},
        creasePattern = origami[["crease_pattern"]];
        polygons = creasePattern[["polygons"]];
        Sum[
            If[Length[
                    Complement[
                        polygon /. creasePattern[["points"]],
                        Keys[<|origami[["points"]]|>]
                    ]
                ] == 0,
                1,
                0
            ],
            {polygon, Values[polygons]}
        ]
    ]

renderOrigami[origami_] :=
    Module[{points, polygons, creases, creasePattern},
        creasePattern = origami[["crease_pattern"]];
        points = origami[["points"]];
        polygons = creasePattern[["polygons"]];
        creases = creasePattern[["creases"]];

        {
            Table[
                If[Length[
                        Complement[
                            polygon /. creasePattern[["points"]],
                            Keys[<|origami[["points"]]|>]
                        ]
                    ] == 0,
                    renderPolygon[polygon /. creasePattern[["points"]], origami[["points"]]],
                    {(* Opacity[.1], Black, renderPolygon[polygon, points] *)}
                    (* Unevaluated[Sequence[]] *)
                ],
                {polygon, Values[polygons]}
            ],
            Table[
                renderCrease[crease, type, origami],
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
