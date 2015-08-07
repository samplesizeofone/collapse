BeginPackage["collapsetest`", {"collapse`"}]

testReport = TestReport[
    {
        VerificationTest[
            reframe[
                {3, 3, 3},
                {{3, 2, 2}, {2, 2, 2}, {2, 2, 3}},
                {{1, 0, 0}, {0, 0, 0}, {0, 0, 1}}
            ],
            {1, 1, 1}
        ]
    }
];

EndPackage[]