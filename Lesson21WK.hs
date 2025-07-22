helloPerson :: String -> String
helloPerson name = "Hello" ++ " "++ name ++ "!"

main :: IO()
main = do
    putStrLn "Hello! whats your name?"
    name <- getLine
    let statment = helloPerson name
    putStrLn statment


areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi * (size/2)^2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 > costP2
                      then p1
                      else p2
    where costP1 = costPerInch p1
          costP2 = costPerInch p2