import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Debug.Trace

data Road = Road {
    cost :: Int
  , target :: City
} deriving Eq

instance Ord Road where
  (<=) (Road cost1 _) (Road cost2 _) = cost1 <= cost2

data City = City {
    name :: String
  , roads :: [Road]
}

instance Eq City where
  (==) (City name1 _) (City name2 _) = name1 == name2


instance Show City where
    show :: City -> String
    show (City cityName roads) = cityName -- <> ": " <> intercalate ", " (map (name . target) roads)

instance Show Road where
    show (Road cost target) = "(" <> show cost <> ") " <> name target

oradeaToZerind = Road { cost = 71, target = zerind }
oradeaToSibiu = Road { cost = 151, target = sibiu }
zerindToOradea = Road { cost = 71, target = oradea }
zerindToArad = Road { cost = 75, target = arad }
aradToZerind = Road { cost = 75, target = zerind }
aradToSibiu = Road { cost = 140, target = sibiu }
aradToTimisoara = Road { cost = 118, target = timisoara }
sibiuToOradea = Road { cost = 151, target = oradea }
sibiuToArad = Road { cost = 140, target = arad }
sibiuToRimnicuVilcea = Road { cost = 80, target = rimnicuVilcea }
sibiuToFagaras = Road { cost = 99, target = fagaras }
fagarasToSibiu = Road { cost = 99, target = sibiu }
fagarasToBucharest = Road { cost = 211, target = bucharest }
timisoaraToArad = Road { cost = 118, target = arad }
timisoaraToLugoj = Road { cost = 111, target = lugoj }
rimnicuVilceaToSibiu = Road { cost = 80, target = sibiu }
rimnicuVilceaToCraiova = Road { cost = 146, target = craiova }
rimnicuVilceaToPitest = Road { cost = 97, target = pitesti }
lugojToTimisoara = Road { cost = 111, target = timisoara }
lugojToMehadia = Road { cost = 70, target = mehadia }
pitestiToRimnicuVilcea = Road { cost = 97, target = rimnicuVilcea }
pitestiToCraiova = Road { cost = 138, target = craiova }
pitestiToBucharest = Road { cost = 101, target = bucharest }
mehadiaToLugoj = Road { cost = 70, target = lugoj }
mehadiaToDrobeta = Road { cost = 75, target = drobeta }
drobetaToMehadia = Road { cost = 75, target = mehadia }
drobetaToCraiova = Road { cost = 120, target = craiova }
craiovaToDrobeta = Road { cost = 120, target = drobeta }
craiovaToRimnicuVilcea = Road { cost = 146, target = rimnicuVilcea }
craiovaToPitesti = Road { cost = 138, target = pitesti }
giurgiuToBucharest = Road { cost = 90, target = bucharest }
bucharestToFagaras = Road { cost = 211, target = fagaras }
bucharestToPitesti = Road { cost = 101, target = pitesti }
bucharestToGiurgiu = Road { cost = 90, target = giurgiu }
bucharestToUrziceni = Road { cost = 85, target = urziceni }
urziceniToBucharest = Road { cost = 85, target = bucharest }
urziceniToVaslui = Road { cost = 142, target = vaslui }
urziceniToHirsova = Road { cost = 98, target = hirsova }
hirsovaToUrziceni = Road { cost = 98, target = urziceni }
hirsovaToEforie = Road { cost = 86, target = eforie }
eforieToHirsova = Road { cost = 86, target = hirsova }
vasluiToUrziceni = Road { cost = 142, target = urziceni }
vasluiToIasi = Road { cost = 92, target = iasi }
iasiToVaslui = Road { cost = 92, target = vaslui }
iasiToNeamt = Road { cost = 87, target = neamt }
neamtToIasi = Road { cost = 87, target = iasi }

oradea = City { name = "Oradea", roads = [oradeaToZerind, oradeaToSibiu] }
zerind = City { name = "Zerind", roads = [zerindToOradea, zerindToArad] }
arad = City { name = "Arad", roads = [aradToZerind, aradToSibiu, aradToTimisoara] }
sibiu = City { name = "Sibiu", roads = [sibiuToOradea, sibiuToArad, sibiuToRimnicuVilcea, sibiuToFagaras] }
fagaras = City { name = "Fagaras", roads = [fagarasToSibiu, fagarasToBucharest] }
timisoara = City { name = "Timisoara", roads = [timisoaraToArad, timisoaraToLugoj] }
rimnicuVilcea = City { name = "Rimnicu Vilcea", roads = [rimnicuVilceaToSibiu, rimnicuVilceaToCraiova, rimnicuVilceaToPitest] }
lugoj = City { name = "Lugoj", roads = [lugojToTimisoara, lugojToMehadia] }
pitesti = City { name = "Pitest", roads = [pitestiToRimnicuVilcea, pitestiToCraiova, pitestiToBucharest] }
mehadia = City { name = "Mehadia", roads = [mehadiaToLugoj, mehadiaToDrobeta] }
drobeta = City { name = "Drobeta", roads = [drobetaToMehadia, drobetaToCraiova] }
craiova = City { name = "Craiova", roads = [craiovaToDrobeta, craiovaToRimnicuVilcea, craiovaToPitesti] }
giurgiu = City { name = "Giurgiu", roads = [giurgiuToBucharest] }
bucharest = City { name = "Bucharest", roads = [bucharestToFagaras, bucharestToPitesti, bucharestToGiurgiu, bucharestToUrziceni] }
urziceni = City { name = "Urziceni", roads = [urziceniToBucharest, urziceniToVaslui, urziceniToHirsova] }
hirsova = City { name = "Hirsova", roads = [hirsovaToUrziceni, hirsovaToEforie] }
eforie = City { name = "Eforie", roads = [eforieToHirsova] }
vaslui = City { name = "Vaslui", roads = [vasluiToUrziceni, vasluiToIasi] }
iasi = City { name = "Iasi", roads = [iasiToVaslui, iasiToNeamt] }
neamt = City { name = "Neamt", roads = [neamtToIasi] }

cities = [oradea, zerind, arad, sibiu, fagaras, timisoara, rimnicuVilcea, lugoj, pitesti, mehadia, drobeta, craiova, giurgiu, bucharest, urziceni, hirsova, eforie, vaslui, neamt]

-----

aToB = Road { cost = 10, target = cityB }
bToA = Road { cost = 10, target = cityA }
bToC = Road { cost = 20, target = cityC }
ctoB = Road { cost = 20, target = cityB }

cityA = City { name = "City A", roads = [aToB] }
cityB = City { name = "City B", roads = [bToA, bToC] }
cityC = City { name = "City C", roads = [ctoB] }

-- cities = [cityA, cityB, cityC]

citiesMap = Map.fromList (map (\city -> (name city, city)) cities)


findRoute' :: City -> City -> [City] -> Int -> ([City], Int)
findRoute' currentCity targetCity citiesAlreadyVisited totalCost
  | currentCity == targetCity = ([currentCity], totalCost)
  | null roads' = ([], 0)
  | otherwise = if targetCity `elem` restOfPath then (currentCity : restOfPath, cost choosenRoad + restOfPathCost) else
    findRoute' currentCity targetCity (currentCity : target choosenRoad : citiesAlreadyVisited) totalCost
  where
      (restOfPath, restOfPathCost) = findRoute' (target choosenRoad) targetCity (currentCity : citiesAlreadyVisited) totalCost
      choosenRoad = head roads'
      roads'
        = filter (\road -> target road `notElem` citiesAlreadyVisited) (sort (roads currentCity))


findRoute citiesMap origin target = do
    originCity <- Map.lookup origin citiesMap
    targetCity <- Map.lookup target citiesMap
    let (routes, cost) = findRoute' originCity targetCity [] 0
    if not (null routes) && last routes == targetCity then
      Just (show routes <> "; cost: " <> show cost)
    else
      Nothing

mainLoop = do
    putStrLn "Type the origin (0 to exit): "
    originName' <- getLine
    case originName' of
        "0" -> putStrLn "Bye!"
        originName -> do
            putStrLn "Type the target (0 to exit): "
            targetName' <- getLine
            case targetName' of
                "0" -> putStrLn "Bye!"
                targetName -> putStrLn "" >> putStrLn (fromMaybe "No routes available!" (findRoute citiesMap originName targetName)) >> putStrLn "" >> mainLoop


-- main = mapM print cities
main = mainLoop