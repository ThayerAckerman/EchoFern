module Main (main) where

-- ECHO FERN --
-- Description. --
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


-- DATA TYPES --
-- An Edge is Labeled as Plain, a Base (to match against Recurse edges), or a Recurse (to generate new polygons on)
data Edge = Plain Point Point | Base Point Point | Recurse Point Point

-- A BasePoly (bp) is a list of edges that contains exactly one non-Recurse Base
type BasePoly = [Edge]

-- A World is a composed of a BasePoly, an Integer Representing the Depth of Fractal Recursion, and an Integer Representing Color Palette
data World = World BasePoly Int Int

-- A Gradient from A Start and End Color
data Gradient = Gradient Color Color Color
-- DATA TYPES --

-- CONFIGURATION --
-- The Initial World
initialWorld = World [] 0 0

-- The List of Color Gradients
gradients = [sunset, horizon, greyScale, ice, fire, plant]
sunset = Gradient (makeColor 1 0.81 0.29 1) (makeColor 1 0 0.53 1) (makeColor 0.16 0 1 1)
horizon = Gradient (makeColor 0.87 0.67 0.25 1) (makeColor 1 0.95 0.92 1) (makeColor 0 0.67 1 1)
greyScale = Gradient (makeColor 1 1 1 1) (makeColor 0.5 0.5 0.5 1) (makeColor 0 0 0 1)
ice = Gradient (makeColor 1 1 1 1) (makeColor 0.77 0.97 1 1) (makeColor 0.34 0.39 1 1)
fire = Gradient (makeColor 1 0.79 0.32 1) (makeColor 1 0.29 0 1) (makeColor 0.28 0 0 1)
plant = Gradient (makeColor 0.79 1 0.33 1) (makeColor 0.22 0.69 0.19 1) (makeColor 0.29 0.47 0.29 1)
-- CONFIGURATION --

--

-- USER I/O AND VISUALIZATION --
-- Main I/O
main :: IO ()
main = play (InWindow "Window" (1000, 1000) (10, 10)) (makeColor 0.8 0.8 0.8 1) 1 initialWorld pictureWorld handleKeys step

-- Handle Input Events
handleKeys :: Event -> World -> World
handleKeys (EventKey (MouseButton RightButton) Down _ (x', y')) (World []       0 g) = World ([Base (0, 0) (x', y'), Recurse (0, 0) (x', y')])              0 g
handleKeys (EventKey (MouseButton RightButton) Down _ (x', y')) (World (b:e:es) 0 g) = World (Base (0, 0) (x', y') : Recurse (sndEdge e) (x', y') : e : es) 0 g
handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y'))  (World []       0 g) = World ([Base (0, 0) (x', y'), Plain (0, 0) (x', y')])                0 g
handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y'))  (World (b:e:es) 0 g) = World (Base (0, 0) (x', y') : Plain (sndEdge e) (x', y') : e : es)   0 g

handleKeys (EventKey (SpecialKey KeyBackspace) Down _ _) (World []       0 g) = World []     0 g
handleKeys (EventKey (SpecialKey KeyBackspace) Down _ _) (World [b, e]      0 g) = World []  0 g
handleKeys (EventKey (SpecialKey KeyBackspace) Down _ _) (World (b:e1:e2:es) 0 g) = World (Base (0, 0) (sndEdge e2) : e2 : es) 0 g
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) (World []       0 g) = World []     0 g
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) (World [b, e]      0 g) = World []  0 g
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) (World (b:e1:e2:es) 0 g) = World (Base (0, 0) (sndEdge e2) : e2 : es) 0 g

handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) (World bp 0 g) = World bp 1 g
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) (World bp n g) = World bp (n+1) g

handleKeys (EventKey (SpecialKey KeyBackspace) Down _ _) (World bp n g) = World bp (n-1) g
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) (World bp n g) = World bp (n-1) g

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (World bp n g) = World bp n ((g+1) `mod` length gradients)

handleKeys (EventKey (SpecialKey KeyUp) Down _ _ ) (World bp n g) = World (translateBasePoly bp 0 (-50)) n g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _ ) (World bp n g) = World (translateBasePoly bp 0 50) n g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _ ) (World bp n g) = World (translateBasePoly bp 50 0) n g
handleKeys (EventKey (SpecialKey KeyRight) Down _ _ ) (World bp n g) = World (translateBasePoly bp (-50) 0) n g
handleKeys (EventKey (Char '-') Down _ _ ) (World bp n g) = World (dilateBasePoly bp 0.75) n g
handleKeys (EventKey (Char '=') Down _ _ ) (World bp n g) = World (dilateBasePoly bp 1.25) n g

handleKeys _ es = es

-- A function to Step the World one Iteration (second)
step :: Float -> World -> World
step f w = w

-- Make a Picture from a World
pictureWorld :: World -> Picture
pictureWorld (World bp 0 _) = pictureClicking bp
pictureWorld (World bp n g) = fracture n n (gradients !! g) bp 

-- Create a Gloss Picture from a BasePoly during BasePoly Building
pictureClicking :: BasePoly -> Picture
pictureClicking [] = Color (makeColor 1 0 0 1) (Circle 2)
pictureClicking [Plain   p1 p2] = Color (makeColor 0 0 0 1) (Line [p1, p2])
pictureClicking [Base    p1 p2] = Color (makeColor 0 0 1 1) (Line [p1, p2])
pictureClicking [Recurse p1 p2] = Color (makeColor 1 0 0 1) (Line [p1, p2])
pictureClicking (e:es) = Pictures (pictureClicking [e] : [pictureClicking es])

-- Create a Gloss Picture from a BasePoly during Fractal Visualzation
picturize :: BasePoly -> Picture
picturize es = Pictures (map Polygon (triangulate (patheurize es)))

-- Create a Gloss Path from a BasePoly
patheurize :: BasePoly -> Path
patheurize [] = []
patheurize (Plain   p1 p2 : es) = p1 : patheurize es
patheurize (Base    p1 p2 : es) = p1 : p2 : patheurize es
patheurize (Recurse p1 p2 : es) = p1 : patheurize es

colorize :: Picture -> Int -> Int -> Gradient -> Picture
colorize pic n 1 (Gradient c1 c2 c3) = Color (mixColors 1 0 c3 c1) pic
colorize pic n m (Gradient c1 c2 c3)
       | n == m    = Color (mixColors 1 0 c1 c1) pic
       | m == div n 2 + 1 = Color (mixColors 1 0 c2 c3) pic
       | m < div n 2 + 1  = let mix = (fromIntegral m / fromIntegral (div n 2 + 2))
                            in Color (mixColors mix (1 - mix) c2 c3) pic
       | otherwise = let mix = ((fromIntegral m - fromIntegral (div n 2)) / fromIntegral (div n 2 + 2))
                      in Color (mixColors mix (1 - mix) c1 c2) pic
-- USER I/O AND VISUALIZATION --

--

-- LINEAR TRANSFORMATIONS OF BASEPOLYS --
-- Translation of a BasePoly
translateBasePoly :: BasePoly -> Float -> Float -> BasePoly
translateBasePoly [] xd yd = []
translateBasePoly (Plain   p1 p2 : es) xd yd = Plain   (fst p1+xd, snd p1+yd) (fst p2+xd, snd p2+yd) : translateBasePoly es xd yd
translateBasePoly (Base    p1 p2 : es) xd yd = Base    (fst p1+xd, snd p1+yd) (fst p2+xd, snd p2+yd) : translateBasePoly es xd yd
translateBasePoly (Recurse p1 p2 : es) xd yd = Recurse (fst p1+xd, snd p1+yd) (fst p2+xd, snd p2+yd) : translateBasePoly es xd yd

-- Dilation of a BasePoly
dilateBasePoly :: BasePoly -> Float -> BasePoly
dilateBasePoly [] k = []
dilateBasePoly (Plain   p1 p2 : es) k = Plain   (fst p1*k, snd p1*k) (fst p2*k, snd p2*k) : dilateBasePoly es k
dilateBasePoly (Base    p1 p2 : es) k = Base    (fst p1*k, snd p1*k) (fst p2*k, snd p2*k) : dilateBasePoly es k
dilateBasePoly (Recurse p1 p2 : es) k = Recurse (fst p1*k, snd p1*k) (fst p2*k, snd p2*k) : dilateBasePoly es k

-- Dilation About a Point of a BasePoly
dilateAboutBasePoly :: BasePoly -> Float -> Float -> Float -> BasePoly
dilateAboutBasePoly [] k xd yd = []
dilateAboutBasePoly es k xd yd = translateBasePoly (dilateBasePoly (translateBasePoly es (-xd) (-yd)) k) xd yd

-- Rotation of a BasePoly
rotateBasePoly :: BasePoly -> Float -> BasePoly
rotateBasePoly [] r = []
rotateBasePoly (Plain   p1 p2 : es) r = Plain   (fst p1*cos r - snd p1*sin r, fst p1*sin r + snd p1*cos r) (fst p2*cos r - snd p2*sin r, fst p2*sin r + snd p2*cos r) : rotateBasePoly es r
rotateBasePoly (Base    p1 p2 : es) r = Base    (fst p1*cos r - snd p1*sin r, fst p1*sin r + snd p1*cos r) (fst p2*cos r - snd p2*sin r, fst p2*sin r + snd p2*cos r) : rotateBasePoly es r
rotateBasePoly (Recurse p1 p2 : es) r = Recurse (fst p1*cos r - snd p1*sin r, fst p1*sin r + snd p1*cos r) (fst p2*cos r - snd p2*sin r, fst p2*sin r + snd p2*cos r) : rotateBasePoly es r

-- Rotation About a Point of a BasePoly
rotateAboutBasePoly :: BasePoly -> Float -> Float -> Float -> BasePoly
rotateAboutBasePoly [] r xd yd = []
rotateAboutBasePoly es r xd yd = translateBasePoly (rotateBasePoly (translateBasePoly es (-xd) (-yd)) r) xd yd
-- LINEAR TRANSFORMATIONS OF BASEPOLYS --

--

-- EDGE CALCULATIONS --
-- The First Point in One Edge
fstEdge :: Edge -> Point
fstEdge (Plain   p1 p2) = p1
fstEdge (Base    p1 p2) = p1
fstEdge (Recurse p1 p2) = p1

-- The Second Point in One Edge
sndEdge :: Edge -> Point
sndEdge (Plain   p1 p2) = p2
sndEdge (Base    p1 p2) = p2
sndEdge (Recurse p1 p2) = p2

-- The Length of One Edge
edgeLength :: Edge -> Float
edgeLength (Plain   p1 p2) = sqrt ((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)
edgeLength (Base    p1 p2) = sqrt ((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)
edgeLength (Recurse p1 p2) = sqrt ((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)

-- The Slope of One Edge
edgeSlope :: Edge -> Float
edgeSlope (Plain   p1 p2)  = (snd p2 - snd p1) / (fst p2 - fst p1)
edgeSlope (Base    p1 p2)  = (snd p2 - snd p1) / (fst p2 - fst p1)
edgeSlope (Recurse p1 p2)  = (snd p2 - snd p1) / (fst p2 - fst p1)

-- The Distance Between Two Points
distance :: Point -> Point -> Float
distance p1 p2 = sqrt ((fst p2 - fst p1)^2 + (snd p2 - snd p1)^2)

-- The Absolute Angle of a Unit Vector on the Unit Circle
unitAngle :: Edge -> Float
unitAngle e = atan2 ((snd (sndEdge e) - snd (fstEdge e)) / (edgeLength e)) ((fst (sndEdge e) - fst (fstEdge e)) / (edgeLength e))

-- The Relative Angle between Two Edges
relativeAngle :: Edge -> Edge -> Float  -- Relative Angle Between Two Edges
-- relativeAngle e1 e2 = if snd (sndEdge e1) > snd (sndEdge e2) then d else -d
-- 						where d = acos((((fst (sndEdge e1))-(fst (fstEdge e1)))*((fst (sndEdge e2))-(fst (fstEdge e2))) + ((snd (sndEdge e1))-(snd (fstEdge e1)))*((snd (sndEdge e2))-(snd (fstEdge e2))))
-- 									/ (sqrt(((fst (sndEdge e1))-(fst (fstEdge e1)))^2+((snd (sndEdge e1))-(snd (fstEdge e1)))^2)*sqrt(((fst (sndEdge e2))-(fst (fstEdge e2)))^2+((snd (sndEdge e2))-(snd (fstEdge e2)))^2)))
relativeAngle e1 e2 = unitAngle e1 - unitAngle e2
-- EDGE CALCULATIONS --

--

-- BASEPOLY WRANGLING -- 
-- Return the Base edge from a BasePoly
getBase :: BasePoly -> Edge
getBase [] = Base (0, 0) (0, 0)
getBase (Plain   p1 p2 : es) = getBase es
getBase (Base    p1 p2 : es) = Base p1 p2
getBase (Recurse p1 p2 : es) = getBase es

-- Return the Recurse edge(s) from the BasePoly
getRecurses :: BasePoly -> [Edge]
getRecurses [] = []
getRecurses (Plain   p1 p2 : es) = getRecurses es
getRecurses (Base    p1 p2 : es) = getRecurses es
getRecurses (Recurse p1 p2 : es) = Recurse p1 p2 : getRecurses es

-- Create a Child BasePoly from a BasePoly, a Base Edge, and a Recurse Edge
fracTransform :: BasePoly -> Edge -> Edge -> BasePoly
fracTransform es b@(Base b1 b2) r@(Recurse r1 r2) = translateBasePoly (rotateAboutBasePoly (dilateAboutBasePoly es (edgeLength r / edgeLength b) (fst b1) (snd b1)) (relativeAngle r b) (fst b1) (snd b1)) (fst r1 - fst b1) (snd r1 - snd b1)

-- Create a Fractal Picture from a BasePoly
fracture :: Int -> Int -> Gradient -> BasePoly -> Picture
fracture n 0 _ curPoly = Blank
fracture n m g curPoly
 = Pictures
        (colorize (picturize curPoly) n m g : map (fracture n (m-1) g) (map (fracTransform curPoly (getBase curPoly)) (getRecurses curPoly)))
-- BASEPOLY WRANGLING --

--

-- CONCAVE TRIANGULATION -- by Firas Zaidan
-- Triangulate a Polygon to a List of Triangles
triangulate :: Path -> [Path]
triangulate =
  triangulateEar 0

-- Triangulation using the Ear Clipping Method
triangulateEar :: Int -> Path -> [Path]
triangulateEar _ []  = []
triangulateEar _ [_]  = []
triangulateEar _ [_, _]  = []
triangulateEar _ [a, x, c]  = [[a, x, c]]
triangulateEar lastear (a:x:c:xs)
  | lastear > 2*size = [[a,x,c]]
  | earfound = [a, x, c] : triangulateEar 0 ([a]++[c]++xs)
  | otherwise = triangulateEar (lastear+1) ([x,c] ++ xs ++ [a])
  where earfound = convex && noPointInTriangle
        noPointInTriangle = not $ isAnyPointInTriangle [a, x, c] xs
        convex = isConvex a x c
        size = 3 + length xs

-- Is There a Point in a Triangle
isAnyPointInTriangle :: Path -> Path -> Bool
isAnyPointInTriangle triangle =
  any (pointInTriangle triangle)

-- Is a Point in a Triangle
pointInTriangle :: Path -> Point -> Bool
pointInTriangle [(ax, ay), (bx, by), (cx, cy)] (px, py)
  | b0 == 0 = False
  | otherwise = (b1 > 0) && (b2 > 0) && (b3 > 0)
  where b0 = (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)
        b1 = ((bx - px) * (cy - py) - (cx - px) * (by - py))/b0
        b2 = ((cx - px) * (ay - py) - (ax - px) * (cy - py))/b0
        b3 = 1.0 - b1 - b2
pointInTriangle _ _ = False

-- Is a Triangle Convex
isConvex :: Point -> Point -> Point -> Bool
isConvex (p1x, p1y) (px, py) (p2x, p2y)  =
  l < 0
  where
        l = (p1x - px) * (p2y - py) - (p1y - py) * (p2x - px)
-- CONCAVE TRIANGULATION --
