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
