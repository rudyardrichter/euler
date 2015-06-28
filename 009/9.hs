main :: IO ()
main = print . product . head $ [ [a, b, c]
                                | a <- [1..498]
                                , b <- [1..499]
                                , c <- [1..500]
                                , b < c
                                , a < b
                                , a + b + c == 1000
                                , a * a + b * b == c * c
                                ]
