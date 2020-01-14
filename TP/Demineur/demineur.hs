import Data.Set (Set) -- on n’importe que Set
import qualified Data.Set as S -- puis tout mais en qualifiant
import System.Random
import Data.List
import System.IO



---------------------------------------------------
-- 1/ Structures de données de base et affichage --
---------------------------------------------------
data Cell = Covered Int Bool Bool | Uncovered Int | Selected -- Covered nb_mines_voisinage mine_sur_la_case? flag?
data Grid = Grid [[Cell]]


instance Show Cell where 
    show Selected = "x"
    show (Uncovered 0) = " "
    show (Uncovered n) = show n
    show (Covered _ _ False) = "#"
    show (Covered _ _ True) = "!" -- drapeau
    
instance Show Grid where
    show (Grid l) = unlines (map (concatMap show) l)



---------------------------------------
-- 2/ Créer la grille avec les mines --
---------------------------------------
randSet::Int -> StdGen -> StdGen -> Int -> Int -> Set (Int, Int)
randSet n genRow genCol nRow nCol = 
    let randRow = randomRs (0, nRow - 1) genRow in
    let randCol = randomRs (0, nCol - 1) genCol in
    let listSets = scanl' (flip S.insert) (S.empty) (zip randRow randCol) in 
    head $ dropWhile (\x -> S.size x < n) listSets


-- returns a Covered cell with no flag and a mine iff cellCoord is present in minesCoord
initCell::Set (Int, Int) -> (Int, Int) -> Cell
initCell minesCoord cellCoord  = 
    if S.member cellCoord minesCoord then 
        Covered 0 True False
    else
        Covered 0 False False

grid::Int -> Int -> Set (Int, Int) -> Grid
grid nRow nCol minesCoord = Grid $ map (map (initCell minesCoord)) [[(i,j) | i<-[0..(nRow-1)]] | j<-[0..(nCol-1)]]



------------------------------------------------------
-- 3/ Calculer le nombre de mines dans le voisinage --
------------------------------------------------------
mineIndic::Cell -> Int 
mineIndic (Covered _ True _) = 1
mineIndic (Covered _ False _) = 0
-- et pour les autres cas ?


mines::Grid -> [[Int]]
mines (Grid g) = map (map mineIndic) g


moveUp::[[Int]] -> [[Int]]
moveUp l = (tail l) ++ [replicate (length (head l)) 0]


moveDown::[[Int]] -> [[Int]]
moveDown [] = []
moveDown l = [replicate (length (head l)) 0] ++ (init l)


moveLeft::[[Int]] -> [[Int]]
moveLeft l = transpose $ moveUp $ transpose l


moveRight::[[Int]] -> [[Int]]
moveRight l = transpose $ moveDown $ transpose l


gridMoves::[[Int]] -> [[[Int]]]
gridMoves mines = 
    let up = moveUp mines in
    let down = moveDown mines in
    tail $ concat $ map (\x -> [x, moveLeft x, moveRight x]) [mines, up, down] -- tail pour supprimer la liste initiale (mines)


matrixSum::[[Int]] -> [[Int]] -> [[Int]]
matrixSum matA matB = zipWith (zipWith (+)) matA matB
-- main = print $ matrixSum [[1,2,3], [4,5,6]] [[1,2,3], [4,5,6]]


neighbourMap::Grid -> [[Int]]
neighbourMap g = foldl1' matrixSum (gridMoves $ mines g) -- pas besoin du cas de base


updateCell::Cell -> Int -> Cell
updateCell (Covered _ mine flag) n = Covered n mine flag
updateCell (Uncovered _) n = Uncovered n
updateCell Selected _ = Selected


updateGrid::Grid -> [[Int]] -> Grid
updateGrid (Grid g) neighb = Grid $ zipWith (zipWith updateCell) g neighb



---------------------------
-- 4/ Découvrir une case --
---------------------------
applyi::(a -> a) -> Int -> [a] -> [a]
applyi f i xs = 
    let (begin, end) = splitAt i xs in
    begin ++ (f $ head end):(tail end)
-- main = print $ applyi (\x -> 3*x) 2 [1,1,1,1,1]


applyij::(a -> a) -> Int -> Int -> [[a]] -> [[a]]
applyij f i j xss = applyi (applyi f j) i xss
-- main = print $ applyij (\x -> 3*x) 2 3 [[1,1,1,1,1], [1,1,1,1,1], [1,1,1,1,1]]


uncover::Int -> Int -> Grid -> Grid
uncover i j (Grid l) = case l!!i!!j of 
    Uncovered _ -> (Grid l)
    
    Covered 0 _ _ -> let g' = Grid $ applyij (const $ Uncovered 0) i j l in  -- on découvre la case actuelle
                     let voisins = [(i+i', j+j') | i'<-[-1,0,1], j'<-[-1,0,1], i+i'>=0, j+j'>=0, i+i'<(length l), j+j'<(length $ head l), (i+i', j+j') /= (0,0)] in  -- TODO
                     foldl' (\grid (i', j') -> uncover i' j' grid) g' voisins  -- on découvre récursivement tous les voisins
    
    Covered n _ _ -> Grid $ applyij (\(Covered n _ _)-> Uncovered n) i j l



--------------------------------------------------------------
-- 5/ Boucle principale de jeu (Read-Eval-Print Loop, REPL) --
--------------------------------------------------------------
covIndic::Cell -> Int
covIndic (Covered _ _ _) = 1
covIndic _ = 0


-- on a gagné quand le nombre de cellules couvertes = nombre de mines
won::Int -> Grid -> Bool
won n (Grid l) = (sum $ map (sum.(map covIndic)) l) == n


toggleFlag::Cell -> Cell
toggleFlag (Covered n mine flag) = Covered n mine (not flag)
toggleFlag c = c  -- ne fait rien sur les autres cellules


neighb::Grid -> (Int, Int) -> [((Int, Int), Cell)]
neighb (Grid l) (i, j) = let pos = [(i+i', j+j') | i'<-[-1,0,1], j'<-[-1,0,1], i+i'>=0, j+j'>=0, i+i'<(length l), j+j'<(length $ head l), (i+i', j+j') /= (0,0)] in
                                zip pos (map (\(i,j) -> l!!i!!j) pos)

flagIndic::Cell -> Bool
flagIndic (Covered _ _ flag) = flag
flagIndic _ = False       

maybeFindMine'::Grid -> (Int, Int) -> Maybe (Int, Int)
maybeFindMine' g@(Grid l) (i, j) = 
    if i == (length l) then maybeFindMine' g (0, j+1) -- move to the right and go back to the first row
    else if j == (length $ head l) then Nothing -- checked every cell
    else case (l!!i!!j) of -- regular cell
        (Uncovered 0) -> maybeFindMine' g (i+1, j)
        (Uncovered n) -> let covNeighb = filter (\(pos, cell) -> (covIndic cell)==1) (neighb g (i,j)) in 
                            if (length covNeighb) == n then 
                                let unflagged = filter (\(pos, cell) -> not $ flagIndic cell) covNeighb in
                                    case unflagged of
                                        [] -> maybeFindMine' g (i+1, j)
                                        (pos, cell):xs -> Just pos
                            else maybeFindMine' g (i+1, j)
        _ -> maybeFindMine' g (i+1, j)

maybeFindMine::Grid -> Maybe (Int, Int)
maybeFindMine g = maybeFindMine' g (0, 0)


loop :: Int -> Int -> Int -> Grid -> IO ()
loop i j n b@(Grid xs) -- le paramètre b se décompose en (Grid xs)
    | won n b = putStrLn "Victoire !" 
    | otherwise = do  
        -- affiche la grille avec la case i, j sélectionnée
        putStrLn $ show $ Grid $ applyij (const Selected) i j xs 
        -- lit un caractère
        c <- getChar
        case c of
            'i'       -> loop (max (i - 1) 0) j n b -- bouge le curseur vers le haut
            'k'       -> loop (min (i + 1) ((length xs) - 1)) j n b -- bouge le curseur vers le bas
            'j'       -> loop i (max (j - 1) 0) n b -- bouge le curseur vers la gauche
            'l'       -> loop i (min (j + 1) ((length (xs!!0)) - 1)) n b -- bouge le curseur vers la droite
            'f'       -> loop i j n (Grid $ applyij toggleFlag i j xs) -- pose ou enlève un drapeau sur la case i, j
            'u'       -> case (xs!!i!!j) of 
                            Covered _ True _ -> putStrLn "BOOM ! Défaite..."
                            _ -> loop i j n (uncover i j b) -- découvre la case i, j; BOUM ?
            't'       -> case (maybeFindMine b) of
                            Nothing -> loop i j n b
                            Just (i', j') -> loop i' j' n b
            otherwise -> loop i j n b -- ne fait rien                
            
main :: IO () 
main = do
    -- désactive l’attente de la touche entrée pour l’acquisition
    hSetBuffering stdin NoBuffering
    -- désactive l’écho du caractère entré sur le terminal
    hSetEcho stdin False
    -- récupère deux StdGen pour la génération aléatoire
    g <- newStdGen
    g' <- newStdGen
    -- nombre de mines, lignes, colonnes
    let nmines = 5
    let l = 7
    let c = 10
    -- créer la grille, ajouter les mines, mettre à jour les voisins
    let minesCoord = randSet nmines g g' l c
    let b' = grid l c minesCoord
    let b = updateGrid b' (neighbourMap b')
    loop 0 0 nmines b -- démarrer la REPL
