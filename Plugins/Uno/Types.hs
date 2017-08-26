{-# LANGUAGE OverloadedStrings #-}
module Plugins.Uno.Types where

import qualified Data.Text as T

data UNOColor = UNOWild | UNOBlue | UNOGreen | UNORed | UNOYellow deriving (Eq, Ord)

readUNOColor :: T.Text -> UNOColor
readUNOColor "Wild" = UNOWild
readUNOColor "Blue" = UNOBlue
readUNOColor "Green" = UNOGreen
readUNOColor "Red" = UNORed
readUNOColor "Yellow" = UNOYellow
readUNOColor str = error . T.unpack $ "No such UNO color: " `T.append` str

colorToText :: UNOColor -> T.Text
colorToText UNOWild = "Wild"
colorToText UNOBlue = "Blue"
colorToText UNOGreen = "Green"
colorToText UNORed = "Red"
colorToText UNOYellow = "Yellow"

rgbToUNOColor :: T.Text -> UNOColor
rgbToUNOColor "rgb(0, 128, 0)" = UNOGreen
rgbToUNOColor "rgb(175, 165, 40)" = UNOYellow
rgbToUNOColor "rgb(75, 75, 255)" = UNOBlue
rgbToUNOColor "rgb(255, 0, 0)" = UNORed
rgbToUNOColor "inherit" = UNOWild
rgbToUNOColor rgbValue = error . T.unpack $ "rgbToUNOColor: " `T.append` rgbValue `T.append` " is not a valid UNO color."

data UNONumber = None | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Reverse | Skip | PlusTwo | PlusFour deriving (Eq)

readUNONumber :: T.Text -> UNONumber
readUNONumber "" = None
readUNONumber "0" = Zero
readUNONumber "1" = One
readUNONumber "2" = Two
readUNONumber "3" = Three
readUNONumber "4" = Four
readUNONumber "5" = Five
readUNONumber "6" = Six
readUNONumber "7" = Seven
readUNONumber "8" = Eight
readUNONumber "9" = Nine
readUNONumber "Reverse" = Reverse
readUNONumber "Skip" = Skip
readUNONumber "+2" = PlusTwo
readUNONumber "+4" = PlusFour
readUNONumber str = error . T.unpack $ "No such UNO number: " `T.append` str

numberToText :: UNONumber -> T.Text
numberToText None = ""
numberToText Zero = "0"
numberToText One = "1"
numberToText Two = "2"
numberToText Three = "3"
numberToText Four = "4"
numberToText Five = "5"
numberToText Six = "6"
numberToText Seven = "7"
numberToText Eight = "8"
numberToText Nine = "9"
numberToText Reverse = "Reverse"
numberToText Skip = "Skip"
numberToText PlusTwo = "+2"
numberToText PlusFour = "+4"

data UNOCard = UNOCard {color :: UNOColor, number :: UNONumber}

cardToText :: UNOCard -> T.Text
cardToText (UNOCard {color = c, number = None}) = colorToText c
cardToText (UNOCard {color = c, number = n}) = colorToText c `T.append` " " `T.append` numberToText n

readUNOCard :: T.Text -> UNOCard
readUNOCard str = UNOCard {color = cardColor, number = cardNumber}
  where parts = T.splitOn " " str
        cardColor = readUNOColor $ head parts
        cardNumber = if null $ tail parts then None else readUNONumber . head $ tail parts