module Main where

import Data.List
import System.IO
import System.Console.ANSI

type Deck =
    [ String ]

main :: IO ()
main =
    do
    let deck = createDeck
    putStrLn "Hello there, what's your name?"
    name <- getLine
    putStrLn ("\nHello, " ++ name ++ ". Here are your cards:")
    print deck
    putStrLn ("Check if your deck contains card:")
    card <- getLine
    if (deckContains deck card) then
        setSGR [SetColor Foreground Vivid Green]
        putStrLn $ "Hooray! That's a valid card."
    else
        setSGR [SetColor Foreground Vivid Red]
        putStrLn "Uh oh! You've entered an invalid card."

createDeck :: Deck
createDeck =
    let
        values = ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
        suits = ["Spades", "Clubs", "Hearts", "Diamonds"]
    in
    [ value ++ " in " ++ suit | value <- values, suit <- suits]

deckContains :: Deck -> String -> Bool
deckContains deck card =
    elem card deck
