main :: IO ()
main = print . product . head $ [ [a, b, c]
                                | c <- [1..500]
                                , b <- [1..c]
                                , a <- [1..b]
                                , a + b + c == 1000
                                , a * a + b * b == c * c
                                ]
