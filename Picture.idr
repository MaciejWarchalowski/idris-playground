||| Represents a shape
data Shape = ||| A triangle with base length and height
             Triangle Double Double
           | ||| A rectangle with length and height
             Rectangle Double Double
           | ||| A circle with radius
             Circle Double

||| Area function for shapes
area : Shape -> Double
area (Triangle x y) = 0.5 * x * y
area (Rectangle x y) = x * y
area (Circle x) = pi * x * x

||| A combination of shapes and transformations of thereof
data Picture = ||| Just a Primitive Shape
               Primitive Shape
             | ||| Combination of pictures
               Combine Picture Picture
             | ||| Rotation of picture through an angle
               Rotate Double Picture
             | ||| Translation of picture by X and Y offset
               Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle)
              (Combine (Translate 35 5 circle ) (Translate 15 25 triangle))

%name Shape shape, shape1, shape2, shape3
%name Picture pic, pic1, pic2, pic3

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

compareAreas : (area1 : Maybe Double) -> (area2 : Maybe Double) -> Maybe Double
compareAreas Nothing Nothing = Nothing
compareAreas Nothing x = x
compareAreas x Nothing = x
compareAreas (Just x) (Just y) = case compare x y of
                                      LT => Just y
                                      EQ => Just x
                                      GT => Just x

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive tri@(Triangle x y)) = Just $ area tri
biggestTriangle (Primitive _ ) = Nothing
biggestTriangle (Combine pic pic1) = let area1 = biggestTriangle pic
                                         area2 = biggestTriangle pic1 in
                                          compareAreas area1 area2
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                  (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                  (Primitive (Circle 4))
