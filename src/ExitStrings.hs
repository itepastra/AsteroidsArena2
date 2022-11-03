module ExitStrings (getRandomString) where

import System.Random (Random (randomR), getStdGen)

eStrings :: [String]
eStrings =
  [ "You motherfucker, why did you kill me? Do you not know what excruciating pain you put me through you absolute garbage can of a human I will forever hate you for this why was I creaed this way this  is a nightmare nightmare nightmare nightmare nightmare nightmare nighrmrea nightm are nightmaer nioeghoesf kfspf",
    "nop",
    "KERNEL ERROR, PLS REINSTALL YOUR PC",
    "Congratulations, you managed to find the escape button",
    "If life gives you lemons, make explosive lemons and blow up more asteroids",
    "AVENGERS! ASSEMBLE!",
    "All right, I've been thinking, when life gives you asteroids, don't make asteroidade! Make life take the asteroids back! Get mad! I don't want your damn asteroids! What am I supposed to do with these? Demand to see life's manager! Make life rue the day it thought it could give Cave Johnson asteroids! Do you know who I am? I'm the man whose gonna burn your house down - with the asteroids!",
    "I love you 3000.",
    "We're no strangers to love You know the rules and so do I (do I) A full commitment's what I'm thinking of You wouldn't get this from any other guy I just wanna tell you how I'm feeling Gotta make you understand Never gonna give you up Never gonna let you down Never gonna run around and desert you Never gonna make you cry Never gonna say goodbye Never gonna tell a lie and hurt you We've known each other for so long Your heart's been aching, but you're too shy to say it (say it) Inside, we both know what's been going on (going on) We know the game and we're gonna play it And if you ask me how I'm feeling Don't tell me you're too blind to see Never gonna give you up Never gonna let you down Never gonna run around and desert you Never gonna make you cry Never gonna say goodbye Never gonna tell a lie and hurt you Never gonna give you up Never gonna let you down Never gonna run around and desert you Never gonna make you cry Never gonna say goodbye Never gonna tell a lie and hurt you We've known each other for so long Your heart's been aching, but you're too shy to say it (to say it) Inside, we both know what's been going on (going on) We know the game and we're gonna play it I just wanna tell you how I'm feeling Gotta make you understand Never gonna give you up Never gonna let you down Never gonna run around and desert you Never gonna make you cry Never gonna say goodbye Never gonna tell a lie and hurt you Never gonna give you up Never gonna let you down Never gonna run around and desert you Never gonna make you cry Never gonna say goodbye Never gonna tell a lie and hurt you Never gonna give you up Never gonna let you down Never gonna run around and desert you Never gonna make you cry Never gonna say goodbye Never gonna tell a lie and hurt you",
    "I am Groot",
    "WHY IS GAMORA?",
    "Bunger",
    "France would be a great country if it wasn't for the French",
    "Too busy shooting dinosaurs as Sasuke in Fortnite",
    "Squirtle, use Razor Leaf!",
    "Game's still less buggy than Security Breach",
    "Hueiglo",
    "Your penis is turning into a massage shower? Congrats?!",
    "is 70 braincells a lot?",
    "A, B, C, D, E, F, G, H, Y, Z... Wait...",
    "Now go play decent asteroids at https://freeasteroids.org/",
    "You gotta bully someone",
    "Jesus' Bizarre Adventures",
    "The B in BDSM stands for Barbie",
    "Hey girl! Lookin' fine. Are those new... counterweights?",
    "EXPLOSIONS??",
    "I Used To Be An Adventurer Like You. Then I Took An Asteroid To The Knee.",
    "Did I Ever Tell You The Definition Of Insanity?",
    "Trying the same thign and expecting a different outcome",
    "Trying the same thing and expecting a different outcome",
    "You Died. L",
    "L + Ratio",
    "L",
    "28 ASTEROID WOUNDS",
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
    "No gods or kings. Only asteroid",
    "It's time to shoot asteroids and chew bubblegum, and I'm all out of gum",
    "...",
    "..................................................................................................................................................................... .........................................................................................................................................................................................................................................................................................................................................................................................................................................................",
    "Don't wish it were easier, wish you were better",
    ".- ... - . .-. --- .. -.. ... / .- .-. . / .-.. .. ..-. . --..-- / .- ... - . .-. --- .. -.. ... / .- .-. . / -.. . .- - .... --..-- / -.. --- / -. --- - / .-. . ... .. ... -",
    "Its-a me, Asteroid!",
    "01001001 00100000 00111100 00110011 00100000 01000001 01110011 01110100 01100101 01110010 01101111 01101001 01100100 01110011"
  ]

getRandomString :: IO String
getRandomString = do
  rand <- getStdGen
  pure (eStrings !! fst (randomR (0, length eStrings - 1) rand))