module Answer where

import GameLogic
import Lens.Micro.Platform

-- simple_moves decides which moves are valid depending on piece colour 
simple_moves :: GameState -> [Move]
simple_moves m = if (_status m == Red) then (redPawnMove(_redPieces m) m) ++ (kingMove(_redKings m) m)  else blackPawnMove(_blackPieces m) m ++ kingMove(_blackKings m) m 
-- jump_move decides which jumps are valid based on piece colour 
jump_move :: GameState -> [Move]
jump_move x = if (_status x) == Red then redPawnJump(_redPieces x) x ++ redkingJump(_redKings x)x else blackPawnJump(_blackPieces x)x ++  blackkingJump(_blackKings x) x

-- moves decides which jump/moves are valid based on piece colour 
moves :: GameState -> [Move]
moves st = simple_moves st ++ jump_move st

-- decides if piece is on the checker board 
onBoard :: (Int,Int) -> Bool
onBoard (x,y) = if  (y < 0 || y > 7) || (x < 0 || x > 7) then False else True  
              
-- This section is dedicated to piece functionality, decides the possible moves for a piece and then puts them in a list using list comprehension (ONLY IF CRITERIA ARE MET WHICH ARE AFTER THE LIST COMPRHENSION)
redPawnMove :: [Coord] -> GameState -> [Move] 
redPawnMove coords u = [[(x,y) , (a,b)] |(x,y) <- coords, (a,b) <- [(x-1,y-1) , (x+1, y-1)], isEmpty (a,b) u, onBoard (a,b)]

redPawnJump :: [Coord] -> GameState -> [Move]
redPawnJump coords u = [[(x,y) ,(a,b)] | (x,y) <- coords, (a,b) <- [(x-2,y-2), (x+2, y-2)], onBoard (a,b), isEmpty (a,b) u, redJumpTopRight u [(x,y), (a,b)] || redJumpTopLeft u [(x,y), (a,b)]]

blackPawnMove :: [Coord] -> GameState -> [Move] 
blackPawnMove coords u = [[(x,y) , (a,b)] |(x,y) <- coords,(a,b) <- [(x-1,y+1) , (x+1, y+1)],isEmpty (a,b) u, onBoard (a,b)]

blackPawnJump :: [Coord] -> GameState -> [Move]
blackPawnJump coords u = [[(x,y) ,(a,b)] | (x,y) <- coords, (a,b) <- [(x-2,y+2) , (x+2, y+2)], onBoard (a,b), isEmpty (a,b) u , blackJumpBotRight u [(x,y), (a,b)] || blackJumpBotLeft u [(x,y), (a,b)]]

kingMove :: [Coord] -> GameState -> [Move]
kingMove coords u = [[(x,y) , (a,b)] |(x,y) <- coords, (a,b) <- [(x-1,y-1) , (x+1,y-1) , (x-1,y+1) , (x+1,y+1) ], isEmpty (a,b) u, onBoard (a,b)]

redkingJump :: [Coord] -> GameState -> [Move]
redkingJump coords u = [[(x,y) , (a,b)] |(x,y) <- coords, (a,b) <- [(x-2,y-2) , (x+2,y-2) , (x-2,y+2) , (x+2,y+2) ], isEmpty (a,b) u, onBoard (a,b), redJumpBotRight u [(x,y), (a,b)] || redJumpBotLeft u [(x,y), (a,b)] || redJumpTopRight u [(x,y), (a,b)] || redJumpTopLeft u [(x,y), (a,b)]]

blackkingJump :: [Coord] -> GameState -> [Move]
blackkingJump coords u = [[(x,y) , (a,b)] |(x,y) <- coords, (a,b) <- [(x-2,y-2) , (x+2,y-2) , (x-2,y+2) , (x+2,y+2) ], isEmpty (a,b) u, onBoard (a,b), blackJumpBotRight u [(x,y), (a,b)] || blackJumpBotLeft u [(x,y), (a,b)] || blackJumpTopRight u [(x,y), (a,b)] || blackJumpTopLeft u [(x,y), (a,b)]]


-- These next 3 functions decide whether a pawn has reached the other end of the board to become a king, 7 is the end for black, 0 is end for red 
kingHasReachedHisThrone :: GameState -> Move -> Bool 
kingHasReachedHisThrone u [] = False 
kingHasReachedHisThrone u [(a,b) , (c,0)] = True 
kingHasReachedHisThrone u [(a,b) , (c,7)] = True 
kingHasReachedHisThrone u [(a,b) , (c,d)] = False

redKingJump :: GameState -> Move -> Bool 
redKingJump u [(a,b) , (c,0)] = True
redKingJump u [(a,b) , (c,d)] = False

blackKingJump :: GameState -> Move -> Bool 
blackKingJump u [(a,b) , (c,7)] = True
blackKingJump u [(a,b) , (c,d)] = False

-- checks if the space is empty or not 
isEmpty :: (Int,Int)-> GameState -> Bool       
isEmpty (a,b) u | not(elem (a,b)(_blackKings u)) && not(elem (a,b)(_redKings u)) && not(elem (a,b)(_redPieces u)) && not(elem (a,b)(_blackPieces u))  = True
                      | otherwise = False
--------- THESE NEXT LIST OF FUNCTIONS ARE USED TO DECIDE IF THERE ARE PIECES SURROUNDING A PIECE, One version (the one without detect) is called in the piece functionallity functions 
-- and dont include checks to see if moves are within jump moves as that becomes a never recursive call. 
redJumpDetectTopRight :: GameState -> Move -> Bool  
redJumpDetectTopRight u [] = False 
redJumpDetectTopRight u [(a,b), (c,d)] | (((elem (a+1, b-1)(_blackPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) && a < c && b > d) || (((elem (a+1, b-1)(_blackKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) && a < c && b > d)  = True
                                       | otherwise = False

redJumpTopRight :: GameState -> Move -> Bool  
redJumpTopRight u [] = False 
redJumpTopRight u [(a,b), (c,d)] | (((elem (a+1, b-1)(_blackPieces u))) && a < c && b > d) || (((elem (a+1, b-1)(_blackKings u))) &&  a < c && b > d)  = True
                                       | otherwise = False
                                       

redJumpDetectTopLeft :: GameState -> Move -> Bool 
redJumpDetectTopLeft u [] = False  
redJumpDetectTopLeft u [(a,b), (c,d)] | (((elem (a-1, b-1)(_blackPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a > c && b > d) || (((elem (a-1, b-1)(_blackKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a > c && b > d)  = True
                          | otherwise = False

redJumpTopLeft :: GameState -> Move -> Bool 
redJumpTopLeft u [] = False  
redJumpTopLeft u [(a,b), (c,d)] | (((elem (a-1, b-1)(_blackPieces u))) &&  a > c && b > d) || (((elem (a-1, b-1)(_blackKings u))) && a > c && b > d)  = True
                          | otherwise = False

redJumpDetectBotLeft :: GameState -> Move -> Bool 
redJumpDetectBotLeft u [] = False  
redJumpDetectBotLeft u [(a,b), (c,d)] | (((elem (a-1, b+1)(_blackPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a > c && b < d) || (((elem (a-1, b+1)(_blackKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a > c && b < d) = True 
                          | otherwise = False

redJumpBotLeft :: GameState -> Move -> Bool 
redJumpBotLeft u [] = False  
redJumpBotLeft u [(a,b), (c,d)] | (((elem (a-1, b+1)(_blackPieces u))) &&   a > c && b < d) || (((elem (a-1, b+1)(_blackKings u))) &&  a > c && b < d) = True 
                          | otherwise = False

redJumpDetectBotRight :: GameState -> Move -> Bool
redJumpDetectBotRight u [] = False   
redJumpDetectBotRight u [(a,b), (c,d)] | (((elem (a+1, b+1)(_blackPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a < c && b < d) || (((elem (a+1, b+1)(_blackKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a < c && b < d) = True 
                           | otherwise = False

redJumpBotRight :: GameState -> Move -> Bool
redJumpBotRight u [] = False   
redJumpBotRight u [(a,b), (c,d)] | (((elem (a+1, b+1)(_blackPieces u))) &&  a < c && b < d) || (((elem (a+1, b+1)(_blackKings u)))  &&  a < c && b < d) = True 
                           | otherwise = False

blackJumpDetectTopRight :: GameState -> Move -> Bool  
blackJumpDetectTopRight u [] = False 
blackJumpDetectTopRight u [(a,b), (c,d)] | (((elem (a+1, b-1)(_redPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) && a < c && b > d) || (((elem (a+1, b-1)(_redKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) && a < c && b > d)  = True
                           | otherwise = False

blackJumpDetectTopLeft :: GameState -> Move -> Bool 
blackJumpDetectTopLeft u [] = False 
blackJumpDetectTopLeft u [(a,b), (c,d)] | (((elem (a-1, b-1)(_redPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&   a > c && b > d) || (((elem (a-1, b-1)(_redKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&   a > c && b > d)  = True
                          | otherwise = False

blackJumpDetectBotLeft :: GameState -> Move -> Bool  
blackJumpDetectBotLeft u [] = False 
blackJumpDetectBotLeft u [(a,b),(c,d)] | (((elem (a-1, b+1)(_redPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a > c && b < d) || (((elem (a-1, b+1)(_redKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a > c && b < d) = True 
                          | otherwise = False

blackJumpDetectBotRight :: GameState -> Move -> Bool  
blackJumpDetectBotRight u [] = False 
blackJumpDetectBotRight u [(a,b), (c,d)] | (((elem (a+1, b+1)(_redPieces u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a < c && b < d) || (((elem (a+1, b+1)(_redKings u))) && (moveIsPiece [(a,b), (c,d)] (jump_move u)) &&  a < c && b < d) = True 
                                         | otherwise = False 

blackJumpTopRight :: GameState -> Move -> Bool  
blackJumpTopRight u [] = False 
blackJumpTopRight u [(a,b), (c,d)] | (((elem (a+1, b-1)(_redPieces u))) &&  a < c && b > d) || (((elem (a+1, b-1)(_redKings u))) &&  a < c && b > d)  = True
                           | otherwise = False

blackJumpTopLeft :: GameState -> Move -> Bool 
blackJumpTopLeft u [] = False 
blackJumpTopLeft u [(a,b), (c,d)] | (((elem (a-1, b-1)(_redPieces u))) &&    a > c && b > d) || (((elem (a-1, b-1)(_redKings u))) &&  a > c && b > d)  = True
                          | otherwise = False

blackJumpBotLeft :: GameState -> Move -> Bool  
blackJumpBotLeft u [] = False 
blackJumpBotLeft u [(a,b),(c,d)] | (((elem (a-1, b+1)(_redPieces u))) &&  a > c && b < d) || (((elem (a-1, b+1)(_redKings u))) &&   a > c && b < d) = True 
                          | otherwise = False

blackJumpBotRight :: GameState -> Move -> Bool  
blackJumpBotRight u [] = False 
blackJumpBotRight u [(a,b), (c,d)] | (((elem (a+1, b+1)(_redPieces u))) &&   a < c && b < d) || (((elem (a+1, b+1)(_redKings u))) &&   a < c && b < d) = True 
                                         | otherwise = False 

-- used to replace a piece from one checker to another 
replace :: Move -> [Coord] -> [Coord]
replace [(a,b), (c,d)] [] = []
replace [(a,b), (c,d)] (x:xs) | x == (a,b) = (c,d):xs
                              | otherwise = x:(replace [(a,b), (c,d)] xs)

                              
-- used to check if a move is in a move list 
moveIsPiece :: Move -> [Move] -> Bool
moveIsPiece x [] = False
moveIsPiece x (a:as) | x == a = True
                     |otherwise = moveIsPiece x as

-- used to remove checkers that have been destroyed 
remove :: Eq a => [a] -> [a] -> [a]
remove _ [] = []
remove (y:ys) (x:xs)
                  | y == x = remove [y] xs
                  | otherwise = x : remove [y] xs



-- used to branch out between normal moves, multiple jump moves. Also checks errors and says Nope Cant Do That                                  
applymove :: Move -> GameState -> GameState
applymove [(a,b)] u = u {_message = "Nope Cant Do That"}
applymove [] u = u {_message = "Nope Cant Do That"}
applymove (x:y:m:ns) u     | (elem (x) (_redKings u) || elem (x) (_redPieces u)) = multiJumpRed (x:y:m:ns) u u
                           | (elem (x) (_blackKings u) || elem (x) (_blackPieces u)) = multiJumpBlack (x:y:m:ns) u u
                           |otherwise =  u {_message = "Nope Cant Do That"}
applymove [(a,b), (c,d)] u | ((elem (a,b) (_redKings u)) || (elem (a,b) (_redPieces u))) && moveIsPiece [(a,b), (c,d)] (moves u) = allRedMoves [(a,b), (c,d)] u
                           | ((elem (a,b) (_blackKings u)) || (elem (a,b) (_blackPieces u))) && moveIsPiece [(a,b), (c,d)] (moves u) = allBlackMoves [(a,b), (c,d)] u
                           | (_redPieces u == [] && _redKings u == []) || (_blackPieces u  == [] && _blackKings u == []) = u{_message = "Game over"}
                           |otherwise =  u {_message = "Nope Cant Do That"}

-- all possible red moves 
allRedMoves :: Move -> GameState -> GameState 
allRedMoves [(a,b), (c,d)] u 

                           | redKingJump u [(a,b), (c,d)] && (elem (a,b) (_redPieces u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) = u {_redKings = (c,d):(_redKings u), _status = Black, _redPieces =  remove [(a,b)] (_redPieces u) ,  _message = "Black Turn"} 
                           | redKingJump u [(a,b), (c,d)] && redJumpDetectTopRight u [(a,b), (c,d)] &&  (elem (a,b) (_redPieces u)) = u {_blackPieces = remove [(a+1,b-1)] (_blackPieces u), _redKings =  (c,d) : (_redKings u),  _redPieces = remove [(a,b)](_redPieces u), _status = Black,  _message = "Black Turn"} 
                           | redKingJump u [(a,b), (c,d)] && redJumpDetectTopLeft u [(a,b), (c,d)] &&  (elem (a,b) (_redPieces u))  = u {_redKings = (c,d): (_redKings u), _blackPieces = remove [(a-1,b-1)] (_blackPieces u),_redPieces = remove [(a,b)](_redPieces u) ,_status = Black,  _message = "Black Turn"}
                           | (elem (a,b) (_redKings u)) && redJumpDetectTopRight u [(a,b), (c,d)] = u {_redKings = replace [(a,b),(c,d)] (_redKings u), _blackPieces = remove [(a+1,b-1)] (_blackPieces u), _blackKings = remove [(a+1,b-1)] (_blackKings u) ,_status = Black,  _message = "Blacks Turn"}
                           | (elem (a,b) (_redKings u)) && redJumpDetectBotRight u [(a,b), (c,d)] = u {_redKings = replace [(a,b),(c,d)] (_redKings u), _blackPieces = remove [(a+1,b+1)] (_blackPieces u), _blackKings = remove [(a+1,b+1)] (_blackKings u) ,_status = Black,  _message = "Blacks Turn"}
                           | (elem (a,b) (_redKings u)) && redJumpDetectBotLeft u [(a,b), (c,d)] = u {_redKings = replace [(a,b),(c,d)] (_redKings u), _blackPieces = remove [(a-1,b+1)] (_blackPieces u), _blackKings = remove [(a-1,b+1)] (_blackKings u) ,_status = Black,  _message = "Blacks Turn"}
                           | (elem (a,b) (_redKings u)) && redJumpDetectTopLeft u [(a,b), (c,d)] = u {_redKings = replace [(a,b),(c,d)] (_redKings u), _blackPieces = remove [(a-1,b-1)] (_blackPieces u), _blackKings = remove [(a-1,b-1)] (_blackKings u) ,_status = Black,  _message = "Blacks Turn"}
                           | redJumpDetectTopRight u [(a,b), (c,d)] &&  (elem (a,b) (_redPieces u)) = u {_redPieces = replace [(a,b),(c,d)] (_redPieces u), _blackPieces = remove [(a+1,b-1)] (_blackPieces u),_blackKings = remove [(a+1,b-1)] (_blackKings u) ,_status = Black,  _message = "Black Turn"} 
                           | redJumpDetectTopLeft u [(a,b), (c,d)] &&  (elem (a,b) (_redPieces u)) = u {_redPieces = replace [(a,b),(c,d)] (_redPieces u), _blackPieces = remove [(a-1,b-1)] (_blackPieces u), _blackKings = remove [(a-1,b-1)] (_blackKings u),_status = Black,  _message = "Black Turn"}        
                           |  (elem (a,b) (_redKings u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u))  && (jump_move u) /= [] = u {_status = Red, _message = "There is a jump" }    
                           |  (elem (a,b) (_redPieces u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) && (jump_move u) /= []  =  u{_message = "There is a jump", _status = Red}
                           | (elem (a,b) (_redKings u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) = u {_redKings = replace [(a,b),(c,d)] (_redKings u), _status = Black,  _message = "Blacks Turn"} 
                           | (elem (a,b) (_redPieces u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) = u {_redPieces = replace [(a,b),(c,d)] (_redPieces u),_redKings = replace [(a,b),(c,d)] (_redKings u) ,_status = Black,  _message = "Black Turn"} 
                           | otherwise = u{_message = "Nope Cant Do That"}

-- all possible black moves 
allBlackMoves :: Move -> GameState -> GameState 
allBlackMoves [(a,b), (c,d)] u 

                           | blackKingJump u [(a,b), (c,d)] && (elem (a,b) (_blackPieces u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) = u {_blackPieces = remove [(a,b)] (_blackPieces u) ,_blackKings = (c,d):(_blackKings u), _status = Red,  _message = "Red Turn"} 
                           | blackKingJump u [(a,b), (c,d)] && blackJumpDetectBotRight u [(a,b), (c,d)] &&  (elem (a,b) (_blackPieces u)) = u {_blackKings = (c,d): (_blackKings u), _redPieces = remove [(a+1,b+1)] (_redPieces u),_blackPieces = remove [(a,b)](_blackPieces u)  ,_status = Red,  _message = "Red Turn"}
                           | blackKingJump u [(a,b), (c,d)] && blackJumpDetectBotLeft u [(a,b), (c,d)] &&  (elem (a,b) (_blackPieces u)) = u {_blackKings = (c,d) :(_blackKings u), _redPieces = remove [(a-1,b+1)] (_redPieces u), _blackPieces = remove [(a,b)](_blackPieces u) ,_status = Red,  _message = "Red Turn"}
                           | (elem (a,b) (_blackKings u)) && blackJumpDetectTopRight u [(a,b), (c,d)] = u {_blackKings = replace [(a,b),(c,d)] (_blackKings u), _redPieces = remove [(a+1,b-1)] (_redPieces u), _redKings = remove [(a+1,b-1)] (_redKings u) ,_status = Red,  _message = "Reds Turn"}
                           | (elem (a,b) (_blackKings u)) && blackJumpDetectBotRight u [(a,b), (c,d)] = u {_blackKings = replace [(a,b),(c,d)] (_blackKings u), _redPieces = remove [(a+1,b+1)] (_redPieces u), _redKings = remove [(a+1,b+1)] (_redKings u) ,_status = Red,  _message = "Reds Turn"}
                           | (elem (a,b) (_blackKings u)) && blackJumpDetectBotLeft u [(a,b), (c,d)] = u {_blackKings = replace [(a,b),(c,d)] (_blackKings u), _redPieces = remove [(a-1,b+1)] (_redPieces u), _redKings = remove [(a-1,b+1)] (_redKings u) ,_status = Red,  _message = "Reds Turn"}
                           | (elem (a,b) (_blackKings u)) && blackJumpDetectTopLeft u [(a,b), (c,d)] = u {_blackKings = replace [(a,b),(c,d)] (_blackKings u), _redPieces = remove [(a-1,b-1)] (_redPieces u), _redKings = remove [(a-1,b-1)] (_redKings u) ,_status = Red,  _message = "Reds Turn"}
                           | blackJumpDetectBotRight u [(a,b), (c,d)] &&  (elem (a,b) (_blackPieces u))  = u {_blackPieces = replace [(a,b),(c,d)] (_blackPieces u), _redPieces = remove [(a+1,b+1)] (_redPieces u),_redKings = remove [(a+1,b+1)] (_redKings u) , _status = Red,  _message = "Red Turn"} 
                           | blackJumpDetectBotLeft u [(a,b), (c,d)] &&  (elem (a,b) (_blackPieces u))  = u {_blackPieces = replace [(a,b),(c,d)] (_blackPieces u), _redPieces = remove [(a-1,b+1)] (_redPieces u),_redKings = remove [(a-1,b+1)] (_redKings u), _status = Red,  _message = "Red Turn"}
                           | (moveIsPiece [(a,b), (c,d)] (simple_moves u)) && (jump_move u) /= [] && (elem (a,b) (_blackKings u)) = u {_message = "Jump Available", _status = Black}
                           | (moveIsPiece [(a,b), (c,d)] (simple_moves u)) && (jump_move u) /= [] && (elem (a,b) (_blackPieces u)) = u{_message = "Jump Available", _status = Black}
                           | (elem (a,b) (_blackKings u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) = u {_blackKings = replace [(a,b),(c,d)] (_blackKings u), _status = Red,  _message = "Reds Turn"} 
                           | (elem (a,b) (_blackPieces u)) && (moveIsPiece [(a,b), (c,d)] (simple_moves u)) = u {_blackPieces = replace [(a,b),(c,d)] (_blackPieces u), _blackKings = replace [(a,b),(c,d)] (_blackKings u) ,_status = Red,  _message = "Red Turn"}
                           | otherwise = u{_message = "Nope Cant Do That"}

--  multiple jumps for red, calls itself with apply move until there are only 2 elements left, and then calls apply move for the last move.
multiJumpRed :: Move -> GameState -> GameState -> GameState
multiJumpRed (x:y:[]) u q     | moveIsPiece [(x),(y)] (jump_move u) = applymove [(x),(y)] u 
                              |otherwise = q{_message = "Nope Cant Do That"}
multiJumpRed (x:y:m:ns) u q
                              | (elem (x) (_redKings u)) && (moveIsPiece [(x), (y)] (jump_move u)) = multiJumpRed (y:m:ns) (applymove [(x),(y)] u) {_status = Red} q
                              | (elem (x) (_redPieces u)) && (moveIsPiece [(x), (y)] (jump_move u)) = multiJumpRed (y:m:ns) (applymove [(x),(y)] u) {_status = Red} q
                              | otherwise = q {_message= "No move"}

-- same idea as multijumpred. 
multiJumpBlack :: Move -> GameState -> GameState -> GameState
multiJumpBlack (x:y:[]) u q   | moveIsPiece [(x),(y)] (jump_move u) = applymove [(x),(y)] u 
                              |otherwise = q{_message = "Nope Cant Do That"}
multiJumpBlack (x:y:m:ns) u q
                              | (elem (x) (_blackPieces u)) && (moveIsPiece [(x), (y)] (jump_move u)) = multiJumpBlack (y:m:ns) (applymove [(x),(y)] u) {_status = Black} q
                              | (elem (x) (_blackKings u)) && (moveIsPiece [(x), (y)] (jump_move u)) = multiJumpBlack (y:m:ns) (applymove [(x),(y)] u) {_status = Black} q
                              | otherwise = q {_message= "No move"}

                          
