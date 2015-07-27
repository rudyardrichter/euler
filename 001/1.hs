import Euler ((|.|), divisible)

main :: IO ()
main = print . sum . filter ((`divisible` 3) |.| (`divisible` 5)) $ [1..999]
