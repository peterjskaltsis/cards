module Main where

{-| A simple Haskell CLI application
    which can perform a number of
    manipulations on a deck of cards.
-}

import Data.List(isInfixOf)
import Data.List.Split
import System.IO()
import System.Console.ANSI


-- TYPES


type Deck =
    [ String ]


-- MAIN


main :: IO ()
main = do
    let deck = createDeck
    setTitle "Learn Haskell CLI"
    greetInit deck
    checkDeckContains deck
    readyToPlay
    exitApp


-- MAIN FUNCTIONS

greetInit :: Deck -> IO ()
greetInit deck = do
    setSGR [ SetConsoleIntensity BoldIntensity ]
    putStrLn "Hello there, what's your name?"
    setSGR [ Reset ]
    name <- getLine
    putStrLn $
        "\nHello, "
        ++ setSGRCode [ SetConsoleIntensity BoldIntensity ]
        ++ name
        ++ setSGRCode [Reset]
        ++ ". Here is our deck:"
    print deck

checkDeckContains :: Deck -> IO ()
checkDeckContains deck = do
    setSGR [ SetConsoleIntensity BoldIntensity ]
    putStrLn ("\nCheck if your deck contains card:")
    setSGR [ Reset ]
    card <- getLine

    if (deckContains deck card) then
        putStrLn $
            setSGRCode [SetColor Foreground Vivid Green]
            ++ "Hooray! 🎉 That's a valid card."

    else
        putStrLn $
            setSGRCode [SetColor Foreground Vivid Red]
            ++ "Uh oh! You've entered an invalid card."

    setSGR [Reset]


readyToPlay :: IO ()
readyToPlay = do
    setSGR [ SetConsoleIntensity BoldIntensity ]
    putStrLn "Are you ready to play? (y/n)"
    setSGR [ Reset ]
    ready <- getLine

    if ready == "yes" || ready == "YES" || ready == "y" || ready == "Y" then
        do
        putStrLn "\nReady! Let's go."

        setSGR [ SetConsoleIntensity BoldIntensity ]
        putStrLn "\nEnter which card number you want to use (1-7)"
        setSGR [ Reset ]
        position <- getLine

        let positionInt = read position
        if isInfixOf "Clubs" (play positionInt) then
            putStrLn $ play positionInt ++ " ♣"
        else if isInfixOf "Spades" (play positionInt) then
                putStrLn $ play positionInt ++ " ♠︎"
        else if isInfixOf "Hearts" (play positionInt) then
                putStrLn $ play positionInt ++ " ♥︎"
        else
            putStrLn $ play positionInt ++ " ♦︎"

    else if ready == "no" || ready == "NO" || ready == "n" || ready == "N" then
        putStrLn "Aw, okay maybe next time."

    else
        do
        putStrLn "Please enter yes or no (y/n/Y/N)."
        readyToPlay


exitApp :: IO ()
exitApp =
    putStrLn $ setSGRCode [Reset]  ++ "Thank you for playing 👋"


-- HELPER FUNCTIONS


deckContains :: Deck -> String -> Bool
deckContains deck card =
    elem card deck


createDeck :: Deck
createDeck =
    let
        values =
            ["A", "2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K"]
        suits =
            ["Spades", "Clubs", "Hearts", "Diamonds"]
    in
    [ value ++ " in " ++ suit | value <- values, suit <- suits]


deal :: Deck -> Int -> [Deck]
deal deck handSize =
    chunksOf handSize deck


createHand :: Int -> [Deck]
createHand handSize =
    deal createDeck handSize


playCard :: [Deck] -> Int -> String
playCard hand position =
    let
        myHand =
            head hand
    in
    if position <= length myHand  then
        myHand !! (position - 1)

    else
        "No card found at position " ++ show position


-- PLAY STARTER


play :: Int -> String
play position =
    let
        deck =
            createHand 7
    in
    playCard deck position
