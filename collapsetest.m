BeginPackage["collapsetest`", {"collapse`"}]

testReport = TestReport[
    {
        VerificationTest[
            reframe[
                {3, 3, 3},
                {{3, 2, 2}, {2, 2, 2}, {2, 2, 3}},
                {{1, 0, 0}, {0, 0, 0}, {0, 0, 1}}
            ],
            {1, 1, 1},
            TestID -> "reframe"
        ],
        VerificationTest[
            trilaterate[{{1, 0, 0}, 1}, {{0, 1, 0}, 1.5}, {{0, 0, 1}, 1.3}],
            {
                {1.1678689532204123`, 0.5428689532204122`, 0.8228689532204123`},
                {0.14546438011292107`, -0.47953561988707893`, -0.19953561988707885`}
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
        ]
    }
];

EndPackage[]