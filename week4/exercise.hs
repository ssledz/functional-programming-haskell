"Guards, Guards!"

:set +m

let {
  absolute x
    | x < 0 = -x
    | otherwise = x
}

absolute (-1)
absolute 1


let {
  holeScore strokes par
    | score < 0 = show (abs score) ++ " under par"
    | score == 0 = "level par"
    | otherwise = show(score) ++ " over par"
   where score = strokes-par
}

holeScore 1 1


data Pet = Dog | Cat | Fish | Parrot String

let {
  hello x = case x of
    Dog -> "woof"
    Cat -> "meeow"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name
}

hello Dog
hello (Parrot "polly")


