BeginPackage["collapsetest`", {"collapse`"}]

reframeTest1 = VerificationTest[
	reframe[
		{3, 3, 3},
		{{3, 2, 2}, {2, 2, 2}, {2, 2, 3}},
		{{1, 0, 0}, {0, 0, 0}, {0, 0, 1}}
	],
	{1, 1, 1}
];

testReport = testReport[
	reframeTest1
];

EndPackage[]