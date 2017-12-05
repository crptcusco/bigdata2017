import Data.List (sortBy)
type Tag = String
type NodeData = (Tag, Double, Double, [Node])

data Node = DecisionNode NodeData | RandomNode Double NodeData deriving Show
data SortedNode = SortedNode {
    tag::Tag,
    isRandom::Bool,
    isCase::Bool,
    getValue::Double,
    chance::Double,
    children::[SortedNode]
    } deriving Show

decision :: Tag -> Double -> Double -> Maybe Node
decision tag profit loss = Just $ DecisionNode (tag, profit, loss, [])

random :: Double -> Tag -> Double -> Double -> Maybe Node
random ratio tag profit loss = Just $ RandomNode ratio (tag, profit, loss, [])

append :: [Node] -> Maybe Node -> Maybe Node
append nodeList (Just (DecisionNode (tag, profit, loss, xs))) = 
    Just $ DecisionNode (tag, profit, loss, nodeList ++ xs) 
append nodeList (Just (RandomNode ratio (tag, profit, loss, xs))) = 
    Just $ RandomNode ratio (tag, profit, loss, nodeList ++ xs) 
append nodeList Nothing = Nothing
    
toSorted :: Node -> SortedNode
toSorted node = getSorted [] node where
    sorter n1 n2 = compare (getValue n2) (getValue n1)
    sumValue acc profit loss = 
        let (totalProfits, totalLosses) = foldl1 (\(p1, l1) (p2, l2) -> (p1 + p2, l1 + l2)) acc in 
            totalProfits + profit - totalLosses - loss
    calculateValueAndType children@(x:xs)
        | isCase x == True = (True, foldl (\acc n -> acc + getValue n * chance n) 0 children)
        | otherwise = (False, getValue . head $ children)
    getSorted acc (DecisionNode (tag, profit, loss, [])) = 
        SortedNode tag False False (sumValue acc profit loss) 0 []
    getSorted acc (RandomNode ratio (tag, profit, loss, [])) = 
        SortedNode tag False True (sumValue acc profit loss) ratio []
    getSorted acc (DecisionNode (tag, profit, loss, children)) =
        let sortedChildren = sortBy sorter $ map (getSorted $ (profit, loss):acc) children
            (nodeType, value) = calculateValueAndType sortedChildren in
                SortedNode tag nodeType False value 0 sortedChildren
    getSorted acc (RandomNode ratio (tag, profit, loss, children)) =
        let sortedChildren = sortBy sorter $ map (getSorted $ (profit, loss):acc) children
            (nodeType, value) = calculateValueAndType sortedChildren in
                SortedNode tag nodeType True value ratio sortedChildren

printSorted :: Maybe Node -> Maybe String
printSorted Nothing = Nothing
printSorted (Just node) = Just . printIndent 0 $ toSorted node where
    indent i = replicate (i * 4) ' '
    printNode i isLeaf (SortedNode tag isRandom isCase value chance _) = "\n" ++ indent i ++ "|---" ++ 
        (if isRandom then "O " else if isLeaf then "" else "[] ") ++ 
        tag ++ " -- Valor: " ++ show value ++
        if isCase then " (" ++ show (chance * 100) ++ "%)" else ""
    printIndent i n@(SortedNode tag isRandom isCase value chance []) = printNode i True n
    printIndent i n@(SortedNode tag isRandom isCase value chance children) = 
        printNode i False n ++ (concat $ map (printIndent (i + 1)) children)
        

ejercicio1 :: Maybe Node
ejercicio1 = do
    let costoCelular = 122
    let valorVenta = 305
    let cantidadCelulares = 50000
    let costoTotal = cantidadCelulares * costoCelular
    ds1_1 <- random 0.75 "65% celulares buenos" (305 * cantidadCelulares * 0.65) costoTotal
    ds1_2 <- random 0.25 "55% celulares buenos" (305 * cantidadCelulares * 0.55) costoTotal
    ds2_1 <- random 0.80 "65% celulares buenos" (305 * cantidadCelulares * 0.65) costoTotal
    ds2_2 <- random 0.20 "50% celulares buenos" (305 * cantidadCelulares * 0.50) costoTotal
    ds1 <- append [ds1_1, ds1_2] $ decision "Opción 1" 0 550000
    ds2 <- append [ds2_1, ds2_2] $ decision "Opción 2" 0 734000
    append [ds1, ds2] $ decision "Inicio" 0 0
    
ejercicio4 = do
    let beneficios = 1000000
    let pérdidas = 400000
    let pAnálisisCorrecto = 0.78
    let pAnálisisIncorrecto = 0.22
    let pCIdóneoAnálisisCorrecto = 0.8
    let pCIdóneoAnálisisIncorrecto = 0.2
    let pCNoIdóneoAnálisisCorrecto = 0.2
    let pCNoIdóneoAnálisisIncorrecto = 0.8
    let pCandidatoIdóneo = (pAnálisisCorrecto * pCIdóneoAnálisisCorrecto) + (pAnálisisIncorrecto * pCIdóneoAnálisisIncorrecto)
    let pCandidatoNoIdóneo = (pAnálisisCorrecto * pCNoIdóneoAnálisisCorrecto) + (pAnálisisIncorrecto * pCNoIdóneoAnálisisIncorrecto)
    c1 <- random pAnálisisCorrecto "Análisis correcto" beneficios 0
    i1 <- random pAnálisisIncorrecto "Análisis incorrecto" 0 pérdidas
    c2 <- random (pAnálisisCorrecto * pCIdóneoAnálisisCorrecto / pCandidatoIdóneo) "Análisis correcto" beneficios 0
    i2 <- random (pAnálisisIncorrecto * pCIdóneoAnálisisIncorrecto / pCandidatoIdóneo) "Análisis incorrecto" 0 pérdidas
    c3 <- random (pAnálisisCorrecto * pCNoIdóneoAnálisisCorrecto / pCandidatoNoIdóneo) "Análisis correcto" beneficios 0
    i3 <- random (pAnálisisIncorrecto * pCNoIdóneoAnálisisIncorrecto / pCandidatoNoIdóneo) "Análisis incorrecto" 0 pérdidas
    idóneo <- append [c2, i2] $ random pCandidatoIdóneo "Candidato idóneo" 0 0
    noIdóneo <- append [c3, i3] $ random pCandidatoNoIdóneo "Candidato no idóneo" 0 0
    hacerPrueba <- append [idóneo, noIdóneo] $ decision "Hacer prueba" 0 0
    noHacerPrueba <- append [c1, i1] $ decision "No hacer prueba" 0 0
    append [hacerPrueba, noHacerPrueba] $ decision "Inicio" 0 0
    
main = do
    let ejercicios = mapM printSorted [ejercicio1, ejercicio4]
    case ejercicios of
        Just nodes -> mapM_ (\s -> putStrLn s >> putStrLn "") nodes
        Nothing -> putStrLn "No hay ejercicios que mostrar"
        
{-- Results in:
|---[] Inicio -- Valor: 2881250.0
    |---O Opción 1 -- Valor: 2881250.0
        |---65% celulares buenos -- Valor: 3262500.0 (75.0%)
        |---55% celulares buenos -- Valor: 1737500.000000001 (25.0%)
    |---O Opción 2 -- Valor: 2621000.0
        |---65% celulares buenos -- Valor: 3078500.0 (80.0%)
        |---50% celulares buenos -- Valor: 791000.0 (20.0%)
|---[] Inicio -- Valor: 692000.0000000001
    |---O Hacer prueba -- Valor: 692000.0000000001
        |---O Candidato idóneo -- Valor: 907784.4311377245 (66.80000000000001%)
            |---Análisis correcto -- Valor: 1000000.0 (93.41317365269461%)
            |---Análisis incorrecto -- Valor: -400000.0 (6.586826347305388%)
        |---O Candidato no idóneo -- Valor: 257831.3253012048 (33.20000000000001%)
            |---Análisis correcto -- Valor: 1000000.0 (46.98795180722891%)
            |---Análisis incorrecto -- Valor: -400000.0 (53.012048192771076%)
    |---O No hacer prueba -- Valor: 692000.0
        |---Análisis correcto -- Valor: 1000000.0 (78.0%)
        |---Análisis incorrecto -- Valor: -400000.0 (22.0%)
        
--}
