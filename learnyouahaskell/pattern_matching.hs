-- to run, $ ghci; Prelude> :l pattern_matching.hs
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "no!"
-- recurisve pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x-1)
--note: if we dont have a catch all pattern, our programs may fail.
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) -- thanks to this type declaration we are guranteed to work only on tuples of Num
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
first :: (x, y, z) -> x
first (x, _, _) = x
second :: (x, y, z) -> y
second (_, y, _) = y
third :: (x, y, z) -> z
third (_, _, z) = z
-- pattern matching in list comprehension
-- let xs = [(1,3), (4,3), (2,4)]
-- [a + b | (a,b) <- xs]
head' :: [a] -> a
head' [] = error "silly goose, head is for non empty lists"
head' (x:_) = x
-- nice keyword, @ reference whole entity.
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
-- patterns - ways of making sure a value conforms to some form and deconstructing it. 
-- guards - way of testing whether some property of a value are true or false.
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi 
    | bmi <= 18.5 = "You're underweight. anorexic?"
    | bmi <= 25.0 = "Normal not abnormal"
    | bmi <= 30.0 = "Fatso!"
    | otherwise = "Big boii"
calcBmiTell :: (RealFloat a) => a -> a -> String
calcBmiTell weight height
    | bmi <= skinny = "You're underweight. anorexic?"
    | bmi <= normal = "Normal not abnormal"
    | bmi <= fat = "Fatso!"
    | otherwise = "Big boii"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
initials :: String -> String -> String
initials (f:fs) (l:ls) = [f] ++ "." ++ [l] ++ "."
-- let bindings let you bind to variables anywhere and are expressions themselves.
-- they are very local and dont span across guards. - can be used for pattern matching.
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea
-- all names defined in let are accessible in "in"
[let square x = x * x in (square 5, square 3, square 2)]
-- put let bindings inside list comprehension. 
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
-- return bmis of only the fat people
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
