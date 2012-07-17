import Control.Monad.Writer

data TriangleFactors = TFact {fac :: [Int], tri :: Int}

triangle :: Int -> Int
triangle 1 = 1
triangle x = x + triangle(x-1)

triangles = map triangle [1..]

factors :: Integral a => a -> [a]
factors x = lowFactors ++ hiFactors
  where 
    isFactor a = x `mod` a == 0
    limit = floor . sqrt . fromIntegral $ x
    lowFactors = filter isFactor [1..limit]
    hiFactors = filter (\a -> not (a `elem` lowFactors)) . map (div x) $ lowFactors

triangleFactors :: [TriangleFactors]
triangleFactors = map (\x -> TFact (factors x) x) $ triangles

over :: Int
over = tri . head $ dropWhile underLimit triangleFactors
  where underLimit (TFact fa tr) = length fa <= 500
