BeginPackage["collapsedoc`", {"collapse`"}]

square1 = {
    {0, 0, 0},
    {0, 1, 0},
    {1, 1, 0},
    {1, 0, 0}
};

square2 = {
    {0, 0, 0},
    {0, 1, 0},
    {-1, 1, 0},
    {-1, 0, 0}
};

rotation1 = RotationMatrix[-Pi/5, {0, 1, 0}];
rotation2 = RotationMatrix[Pi/5, {0, 1, 0}];

square1Rotated = rotation1.#& /@ square1;
square2Rotated = rotation2.#& /@ Reverse[square2];

square1Binormal = .33 calculateBinormal[Take[square1Rotated, 3]];
square2Binormal = .33 calculateBinormal[Take[square2Rotated, 3]];

square1BinormalInverse = -square1Binormal;
square2BinormalInverse = -square2Binormal;

square1RotatedCenter = Mean[square1Rotated];
square2RotatedCenter = Mean[square2Rotated];

creaseDirectionDiagram = {
    Opacity[.5],
    Polygon[square1Rotated],
    Polygon[square2Rotated],
    Opacity[1],
    Blue,
    Arrow[{square1RotatedCenter, square1RotatedCenter + square1Binormal}],
    Arrow[{square2RotatedCenter, square2RotatedCenter + square2Binormal}],
    Tube[{square1RotatedCenter + square1Binormal, square2RotatedCenter + square2Binormal}],
    Red,
    Arrow[{square1RotatedCenter, square1RotatedCenter + square1BinormalInverse}],
    Arrow[{square2RotatedCenter, square2RotatedCenter + square2BinormalInverse}],
    Tube[{square1RotatedCenter + square1BinormalInverse, square2RotatedCenter + square2BinormalInverse}],
};

EndPackage[]
