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
    boldText "Hello there, what's your name?"
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
    boldText "\nCheck if your deck contains card:"
    card <- getLine

    if (deckContains deck card) then
        successText "Hooray! 🎉 That's a valid card."

    else
        errorText "Uh oh! You've entered an invalid card."

    setSGR [Reset]


readyToPlay :: IO ()
readyToPlay = do
    boldText "Are you ready to play? (y/n)"
    ready <- getLine

    if ready == "yes" || ready == "YES" || ready == "y" || ready == "Y" then
        do
        let splitDeck = createHand 7
            myHand = head splitDeck

        boldText "\nReady! Let's go."
        boldText "\nYour allocated hand:"

        -- show current hand
        print myHand
        boldText "\nEnter which card number you want to use (1-7)"
        position <- getLine

        boldText "\nSelected card:"

        let
            positionInt = read position
            result = play splitDeck positionInt

        if isInfixOf "Clubs" result then
            putStrLn $ result ++ " ♣"

        else if isInfixOf "Spades" result then
            putStrLn $ result ++ " ♠︎"

        else if isInfixOf "Hearts" result then
            putStrLn $ result ++ " ♥︎"

        else
            putStrLn $ result ++ " ♦︎"

    else if ready == "no" || ready == "NO" || ready == "n" || ready == "N" then
        putStrLn "Aw, okay maybe next time."

    else do
        putStrLn "Please enter yes or no (y/n/Y/N)."
        readyToPlay


exitApp :: IO ()
exitApp =
    putStrLn $ setSGRCode [Reset]  ++ "Thank you for playing 👋"


-- HELPER FUNCTIONS

successText :: String -> IO ()
successText text =
    putStrLn $
        setSGRCode [ SetColor Foreground Vivid Green ]
        ++ text
        ++ setSGRCode [ Reset ]


errorText :: String -> IO ()
errorText text =
    putStrLn $
        setSGRCode [ SetColor Foreground Vivid Red ]
        ++ text
        ++ setSGRCode [ Reset ]


boldText :: String -> IO ()
boldText text =
    putStrLn $
        setSGRCode [ SetConsoleIntensity BoldIntensity ]
        ++ text
        ++ setSGRCode [ Reset ]


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
        myHand = head hand
    in
    if position <= length myHand  then
        myHand !! (position - 1)

    else
        "No card found at position " ++ show position


-- PLAY STARTER


play :: [Deck] -> Int -> String
play hand position =
    playCard hand position
