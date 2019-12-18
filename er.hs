data Point = Coord Double Double
distance (Coord x y) (Coord x' y') = sqrt ((x-x')^2 + (y-y')^2)
let p1 = Coord 2.3 7.8 in distance p1 (Coord 5.6 1.23)

//data Cercle = Param Double Point
//data Carre = Sommet Point Point
data Figure = FigP Point | FigC Point Double | FigCa Point Point
perimetre FigP = 0
perimetre FigC (Coord x y) r = 2*pi*r
perimetre FigCa p p' = 2*sqrt(2)*distance p p'


