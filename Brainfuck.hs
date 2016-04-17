import Data.Char (ord, chr)

data Model = Model {
  memory :: [Int],
  pointer :: Int,
  stack :: [Int],
  source :: String,
  sourceIndex :: Int
} deriving (Show)

model :: String -> Model
model source = Model {
  memory = replicate 8 0,
  pointer = 0,
  stack = [],
  source = source,
  sourceIndex = 0
}

isToken :: Char -> Bool
isToken ch =
  case ch of
    '>' -> True 
    '<' -> True 
    '+' -> True 
    '-' -> True 
    '.' -> True 
    ',' -> True 
    '[' -> True 
    ']' -> True 
    _ -> False

incSource :: Model -> Model
incSource model =
  let
    newModel = model { sourceIndex = (sourceIndex model) + 1 }
    start = sourceIndex newModel
  in
    case getAt (source model) start of
      Just ch ->
        if (start >= (length $ source model)) || (isToken ch) then
          newModel
        else
          incSource newModel

      Nothing -> newModel

incPointer :: Model -> Model
incPointer model = model {
    pointer = (pointer model) + 1
  }

decPointer :: Model -> Model
decPointer model = model {
    pointer = (pointer model) - 1
  }

incData :: Model -> Model
incData model = model {
    memory = indexedMap (\i v -> if i == pointer model then v + 1 else v) (memory model)
  }

decData :: Model -> Model
decData model = model {
    memory = indexedMap (\i v -> if i == pointer model then v - 1 else v) (memory model)
  }

printData :: Model -> IO Model
printData model =
  case getMemory model of
    Just v -> (putStrLn $ show $ chr v) >> return model
    Nothing -> return model

readData :: Model -> IO Model
readData model =
  do ch <- getChar
     return $ setMemory model ch

setMemory :: Model -> Char -> Model
setMemory model ch =
  let
    index = pointer model
  in
    model { memory = indexedMap (\i v -> if i == index then ord ch else v) (memory model) }

getMemory :: Model -> Maybe Int
getMemory model = getAt (memory model) (pointer model)

skipBlock :: Int -> Model -> Model
skipBlock counter model =
  let
    start = sourceIndex model
  in
    if (start >= (length $ source model)) || (counter == 0) then
      model
    else
      case getAt (source model) start of
        Just '[' -> skipBlock (counter+1) $ incSource model
        Just ']' -> skipBlock (counter-1) $ incSource model
        _ -> skipBlock counter $ incSource model

pushStack :: Model -> Model
pushStack model =
  case getMemory model of
    Just value ->
      if value /= 0 then
        incSource $ model { stack = [sourceIndex model] ++ (stack model) }
      else
        skipBlock 1 $ incSource model

    Nothing -> model

popStack :: Model -> Model
popStack model =
  let
    index = head $ stack model
  in
    model { sourceIndex = index, stack = drop 1 $ stack model }

execute :: Model -> IO Model
execute model =
  let
    start = sourceIndex model
  in
    case getAt (source model) start of
      Just '>' -> execute $ incSource $ incPointer model
      Just '<' -> execute $ incSource $ decPointer model
      Just '+' -> execute $ incSource $ incData model
      Just '-' -> execute $ incSource $ decData model
      Just '.' -> do m <- printData model 
                     execute $ incSource m
      Just ',' -> do m <- readData model
                     execute $ incSource m
      Just '[' -> execute $ pushStack model
      Just ']' -> execute $ popStack model
      _ -> return model

getAt :: [a] -> Int -> Maybe a
getAt list index =
  if index >= (length list) then
    Nothing
  else
    Just $ list !! index

indexedMap :: (Int -> a -> b) -> [a] -> [b]
indexedMap f l = zipWith f [0..] l

main :: IO ()
main =
  do m <- (execute $ model "++[-]>>>+")
     putStrLn $ show m

