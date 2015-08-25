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
            newFolded[Key[point[[1]] /. origami[["crease_pattern"]][["points"]]]] = point[[2]],
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
            -calculateBinormal[foldedTriangle],
            calculateBinormal[foldedTriangle]
        ]
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
    Module[{calculateVector, creases1, creases2, binormal1, binormal2},
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
    If[MemberQ[Sort /@ origami[["creases"]][["mountain"]], Sort[crease]] ||
        MemberQ[
            Sort /@ (origami[["creases"]][["mountain"]] /.
                origami[["points"]]),
            Sort[crease]
        ],
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

alignPolygon[polygonName1_, polygonName2_, origami_] :=
    Module[{polygon1, polygon2, points1, points2, folded, sourceFramePoints,
        destinationFramePoints, newPoints, newFolded},
        polygon1 = origami[["polygons"]][[polygonName1]];
        polygon2 = origami[["polygons"]][[polygonName2]];
        points1 = (polygon1 /. origami[["points"]]);
        points2 = (polygon2 /. origami[["points"]]);
        folded = points2 /. origami[["folded"]];
        sourceFramePoints = Take[points2, 3];
        destinationFramePoints = Take[folded, 3];
        newPoints = reframe[#, sourceFramePoints, destinationFramePoints]& /@
            points1;
        newFolded = (#[[1]] -> #[[2]])& /@ Thread[{points1, newPoints}];
        updateFolded[newFolded, origami]
    ]

manipulateCrease[polygonName1_, polygonName2_, polygons_, angle_, origami_] :=
    Module[{axialCrease, creaseStart, creaseEnd, origami1, origami2,
        creaseType, creaseDirection1, creaseDirection2, angle1, angle2},
        creases1 = canonicalizeCrease[#, origami]& /@ convertPolygonToCreases[
            origami[["polygons"]][[polygonName1]]
        ];
        creases2 = canonicalizeCrease[#, origami]& /@ convertPolygonToCreases[
            origami[["polygons"]][[polygonName2]]
        ];
        axialCrease = Intersection[creases1, creases2][[1]];
        {creaseStart, creaseEnd} = axialCrease /. origami[["folded"]];
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
        (* angle2 = calculateCreaseDirectionalAngle[
            polygonName1,
            polygonName2,
            axialCrease, 
            origami2
        ]; *)

(*         Print[{angle1, angle2}];
 *)
        (* If[angle1 > angle2,
            origami1,
            origami2
        ] *)
        origami2
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
        foldedPoints = origami[["folded"]];
        point = pointName /. getFolded[origami];
        referenceNeighborNames = Intersection[
            findPointNeighbors[
                pointName /. origami[["points"]],
                Values[origami[["polygons"]]] /. origami[["points"]]
            ],
            findKnownPoints[origami] /. origami[["points"]]
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
    Module[{calculable},
        calculable = Cases[
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
            ];
        If[Length[calculable] > 0,
            RandomChoice[calculable]
        ]
    ]

canonicalizeCrease[crease_, origami_] :=
    Sort[crease /. origami[["points"]]]

polygonFoundQ[polygonName_, origami_] :=
    Length[
        Intersection[
            Keys[<|origami[["folded"]]|>],
            origami[["polygons"]][[polygonName]] /. origami[["points"]]
        ]
    ] == Length[origami[["polygons"]][[polygonName]]]

placePolygon[polygonName_, origami_] :=
    Module[{pointLabels, foundLabels, placedPolygon, referenceLabels,
        referencePoints, frame, newPoints, recastPoints, newFolded},
        pointLabels = origami[["polygons"]][[polygonName]] /.
            origami[["points"]];
        foundLabels = Intersection[pointLabels, Keys[<|origami[["folded"]]|>]];
        If[Length[foundLabels] < 3
            || Length[foundLabels] == Length[pointLabels],
            origami,
            referenceLabels = Take[foundLabels, 3];
            newPoints = reframe[
                #,
                referenceLabels,
                referenceLabels /. origami[["folded"]]
            ]& /@ pointLabels;
            newFolded = (#[[1]] -> #[[2]])& /@ Thread[{pointLabels, newPoints}];
            updateFolded[newFolded, origami]
        ]
    ]

calculateCreaseAngleAverage[origami_] :=
    Module[{creases, polygons, polygonName1, polygonName2,
        creasePolygons},
        creases = Union[
            canonicalizeCrease[#, origami]& /@
                origami[["creases"]][["mountain"]],
            canonicalizeCrease[#, origami]& /@
                origami[["creases"]][["valley"]]
        ];
        polygons = origami[["polygons"]];
        Re[
            Mean[
                Table[
                    creasePolygons = Cases[
                        Normal[polygons],
                        (name_ ->
                            polygon_ /; MemberQ[
                                canonicalizeCrease[#, origami]& /@
                                    convertPolygonToCreases[polygon],
                                crease
                            ]) :>
                        name
                    ];
                    If[And@@(
                            polygonFoundQ[#, origami]& /@
                            creasePolygons
                        ) && Length[creasePolygons] == 2,
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
    ]

calculateAnyPoint[origami_, creasePattern_] :=
    Module[{reference, point, pointName, points, point1, point2,
        origami1, origami2, involvedPolygons, currentAverage, currentAverage1,
        currentAverage2},
        point = findCalculablePoint[origami];
        If[point =!= Null,
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
            (* Do[
                Print[Intersection[Keys[<|origami1[["folded"]]|>], polygon /. origami1[["points"]]]];
                Print[Keys[<|origami1[["folded"]]|>]];
                Print[polygon /. origami1[["points"]]];
                Print["----"],
                {polygon, Values[origami1[["polygons"]]]}
            ]; *)
            involvedPolygons = Cases[
                Normal[origami1[["polygons"]]],
                (name_ -> polygon_) /; Length[Intersection[Keys[<|origami1[["folded"]]|>], polygon /. origami1[["points"]]]] != Length[polygon] &&
                    Length[Intersection[Keys[<|origami1[["folded"]]|>], polygon /. origami1[["points"]]]] > 2 :>
                    name
            ];
            Do[
                origami1 = placePolygon[polygonName, origami1],
                {polygonName, involvedPolygons}
            ];

            points[pointName] = point2;
            origami2 = updateFolded[Normal[points], origami];
            origami2 = sizeCreasePattern[
                findKnownPoints[origami2],
                origami2,
                creasePattern    
            ];
            involvedPolygons = Cases[
                Normal[origami2[["polygons"]]],
                (name_ -> polygon_) /; Length[Intersection[Keys[<|origami2[["folded"]]|>], polygon /. origami2[["points"]]]] != Length[polygon] &&
                    Length[Intersection[Keys[<|origami2[["folded"]]|>], polygon /. origami2[["points"]]]] > 2 :>
                    name
            ];
            Do[
                origami2 = placePolygon[polygonName, origami2],
                {polygonName, involvedPolygons}
            ];

            currentAverage = calculateCreaseAngleAverage[origami];
            currentAverage1 = calculateCreaseAngleAverage[origami1];
            currentAverage2 = calculateCreaseAngleAverage[origami2];

            If[currentAverage1 > currentAverage && currentAverage2 < currentAverage,
                origami1,
                If[currentAverage1 < currentAverage && currentAverage2 > currentAverage,
                    origami2,
                    If[Abs[currentAverage - currentAverage1] < Abs[currentAverage - currentAverage2],
                        origami1,
                        origami2
                    ]
                ]
            ]



            (* If[calculateCreaseAngleAverage[origami1] >
                calculateCreaseAngleAverage[origami2] || Plus@@(Abs[Im[#]]& /@ point1) > .000001 && Plus@@(Abs[Im[#]]& /@ point1) < Plus@@(Abs[Im[#]]& /@ point2),
                origami1,
                origami2
            ] *)
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
            ];
            (* Print[Graphics3D[renderOrigami[nextOrigami]]]; *)
            (* Print[Values[<|lastOrigami[["points"]]|>]];
            Print[Keys[<|lastOrigami[["folded"]]|>]]; *)
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

(* Render *)
renderPolygon[polygon_, points_] :=
    Polygon[polygon /. points]

renderCrease[crease_, type_, points_] :=
    {
        (* If[type == "mountain", Red, Blue],
        Tube[(crease /. points), .01] *)
    }

renderOrigami[origami_] :=
    Module[{points, polygons, creases},
        points = getFolded[origami];
        polygons = origami[["polygons"]];
        creases = origami[["creases"]];

        {
            Table[
                If[Length[
                        Complement[
                            polygon /. origami[["points"]],
                            Keys[<|origami[["folded"]]|>]
                        ]
                    ] == 0,
                    renderPolygon[polygon, points],
                    {(* Opacity[.1], Black, renderPolygon[polygon, points] *)}
                    (* Unevaluated[Sequence[]] *)
                ],
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
