main :: IO ()
main = interact solve

turnAndCount :: (Int, Int) -> String -> (Int, Int)
turnAndCount (cur, count) (dir:rest) =
  let dist   = read rest :: Int
      op     = if dir == 'L' then (-) else (+)
      newCur = (op cur dist) `mod` 100
      newCnt = if newCur == 0 then count + 1 else count
  in (newCur, newCnt)
turnAndCount st _ = st

solve :: String -> String
solve input =
  let (_, ans) = foldl' turnAndCount (50, 0) (lines input)
  in show ans ++ "\n"

