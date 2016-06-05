BeginPackage["collapsetest`", {"collapse`"}]

report[] :=
    Module[{report},
        report = collapsetest`runCollapseTests[];
        Print[ToString[Length[report["TestsSucceeded"]]] <> " tests passed"];
        Print[Column /@ (Normal /@ report["TestsFailed"]) // TabView]
    ];

approximate[n_] :=
    n /. {x_/;NumericQ[x] :> IntegerPart[1000 N[x]]}

runCollapseTests[] := (
    TestReport[
        {
            VerificationTest[
                reframe[
                    {3, 3, 3},
                    {{3, 2, 2}, {2, 2, 2}, {2, 2, 3}},
                    {{1, 0, 0}, {0, 0, 0}, {0, 0, 1}}
                ],
                {1, 1, 1},
                TestID -> "reframe 1"
            ],
            VerificationTest[
                approximate /@ reframe[
                    {1, 0, 0},
                    {{2, 0, 0}, {0, 1, 0}, {0, 0, 1}},
                    {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}
                ],
                approximate /@ {0.866012, -0.044463, -0.398899},
                TestID -> "reframe 2"
            ],
            VerificationTest[
                approximate /@ trilaterate[{{1, 0, 0}, 1}, {{0, 1, 0}, 1.5}, {{0, 0, 1}, 1.3}],
                {
                    approximate /@ {1.1678689532204123`, 0.5428689532204122`, 0.8228689532204123`},
                    approximate /@ {0.14546438011292107`, -0.47953561988707893`, -0.19953561988707885`}
                },
                TestID -> "trilaterate"
            ],
            VerificationTest[
                calculatePointPlaneDistance[{0, 0, 1}, {{1, 0, 0}, {0, 1, 0}, {1, 1, 0}}],
                -1,
                TestID -> "calculatePointPlaneDistance1"
            ],
            VerificationTest[
                calculatePointPlaneDistance[{1, 1, 1}, {{1, 1, 0}, {0, 1, 1}, {1, 0, 1}}],
                1/Sqrt[3],
                TestID -> "calculatePointPlaneDistance2"
            ],
            VerificationTest[
                recast[{1, 1, 1}, {{1, 1, 0}, {1, 0, 1}, {0, 1, 1}}],
                {1/2, 1/2, 1/2},
                TestID -> "recast"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase["valley"]
                    ];
                    origami = setOrigamiToCreasePattern[origami]
                ],
                <|
                    "points" ->
                        {
                            {-1, 0, 0} -> {-1., 0., 0.},
                            {0, -1, 0} -> {0., -1., 0.},
                            {0, 1, 0} -> {0., 1., 0.},
                            {1, 0, 0} -> {1., 0., 0.}
                        },
                    "crease_pattern" -> generateTwoBase["valley"],
                    "type" -> "origami"
                |>,
                TestID -> "setOrigamiToCreasePattern"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase["valley"]
                    ];
                    updateFoldedPoints[
                        {("a" /. generateTwoBase["valley"][["points"]]) -> {5, 5, 5}},
                        origami
                    ]
                ],
                <|
                    "points" -> {{-1, 0, 0} -> {5, 5, 5}},
                    "crease_pattern" -> generateTwoBase["valley"],
                    "type" -> "origami"
                |>,
                TestID -> "makeOrigamiFromCreasePattern"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase["valley"]
                    ];
                    approximate[calculateFoldedVectors["p", origami]]
                ],
                approximate[{{0., -0.333333, 0.001}, {0., -0.333333, -0.001}}],
                TestID -> "calculateFoldedVectors"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase["valley"]
                    ];
                    calculatePolygonVectorAngle["p", "q", origami]
                ],
                Pi,
                TestID -> "calculatePolygonVectorAngle"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase[#]
                    ];
                    calculateCreaseDirection[
                        "p",
                        "q",
                        rotatePolygonsInCreasePattern[
                            {"p"},
                            #,
                            extractPointValues[Sort[{"a", "b"}], origami],
                            origami
                        ]
                    ]& /@ {Pi/2, -Pi/2}
                ]& /@ {"valley", "mountain"},
                {{"mountain", "valley"}, {"mountain", "valley"}},
                TestID -> "calculateCreaseDirection"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase["valley"]
                    ];
                    rotatePolygonsInCreasePattern[
                        {"p"},
                        Pi/2,
                        extractPointValues[Sort[{"a", "b"}], origami],
                        origami
                    ]
                ],
                <|
                    "points" -> {
                        {-1, 0, 0} -> {-1, 0, 0},
                        {1, 0, 0} -> {1, 0, 0},
                        {0, -1, 0} -> {0, 0, -1}
                    },
                    "crease_pattern" -> <|
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
                            "mountain" -> {},
                            "valley" -> {{"a", "b"}}
                        |>,
                        "fractals" -> {},
                        "type" -> "crease_pattern"
                    |>,
                    "type" -> "origami"
                |>,
                "TestID" -> "rotatePolygonsInCreasePattern"
            ],
            VerificationTest[
                Module[{origami, calculateAngles},
                    calculateAngles = Function[{creaseType},
                        Module[{origami},
                            origami = makeOrigamiFromCreasePattern[
                                generateTwoBase[creaseType]
                            ];
                            origami = rotatePolygonsInCreasePattern[
                                {"p"},
                                #,
                                extractPointValues[Sort[{"a", "b"}], origami],
                                origami
                            ]& /@ {Pi/2, -Pi/2};
                            calculateCreaseDirectionalAngle["p", "q", Sort[{"a", "b"}], #]& /@ origami
                        ]
                    ];
                    {
                        calculateAngles["mountain"],
                        calculateAngles["valley"]
                    }
                ],
                {{Pi/2, -Pi/2}, {-Pi/2, Pi/2}},
                TestID -> "calculateCreaseDirectionalAngle"
            ],
            VerificationTest[
                Module[{origami},
                    origami = makeOrigamiFromCreasePattern[
                        generateTwoBase["valley"]
                    ];
                    origami = rotatePolygonsInCreasePattern[
                        {"p"},
                        Pi/2,
                        extractPointValues[Sort[{"a", "b"}], origami],
                        origami
                    ];
                    alignPolygon["q", "p", origami][["points"]]
                ],
                {
                    {-1, 0, 0} -> {-1, 0, 0},
                    {1, 0, 0} -> {1, 0, 0},
                    {0, -1, 0} -> {0, 0, -1},
                    {0, 1, 0} -> {0, 0, 1}
                },
                TestID -> "alignPolygon"
            ]
        }
    ]
);

EndPackage[]
