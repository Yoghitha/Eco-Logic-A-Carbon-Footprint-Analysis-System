{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (ToJSON, FromJSON, object, (.=), (.:))
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Status
import Data.Time
import Data.Time.Format
import System.Random
import qualified Data.ByteString.Lazy as BSL
import Network.Wai.Handler.Warp (run)

-- ============ DATA TYPES ============

data FootprintRequest = FootprintRequest
    { electricity :: Double
    , travel :: Double
    , lpg :: Double
    , diet :: Text
    } deriving (Show, Generic)

instance FromJSON FootprintRequest
instance ToJSON FootprintRequest

data FootprintResponse = FootprintResponse
    { totalCO2 :: Double
    , electricityPercent :: Int
    , travelPercent :: Int
    , lpgPercent :: Int
    , dietPercent :: Int
    , treesNeeded :: Int
    , ecoScore :: Int
    } deriving (Show, Generic)

instance ToJSON FootprintResponse

data ContactMessage = ContactMessage
    { name :: Text
    , email :: Text
    , message :: Text
    } deriving (Show, Generic)

instance FromJSON ContactMessage
instance ToJSON ContactMessage

data EcoTip = EcoTip
    { tipId :: Int
    , title :: Text
    , description :: Text
    , icon :: Text
    } deriving (Show, Generic)

instance ToJSON EcoTip

-- ============ CALCULATION FUNCTIONS ============

calculateFootprint :: Double -> Double -> Double -> Text -> (Double, Int, Int, Int, Int, Int, Int)
calculateFootprint elec travel lpg diet = 
    let elecFactor = 0.82
        travelFactor = 0.18
        lpgFactor = 3.0
        
        dietFactor = case diet of
            "Carnivore / Heavy Meat" -> 5.0
            "Omnivore" -> 3.8
            "Vegetarian" -> 2.5
            "Vegan" -> 1.8
            _ -> 3.8
        
        elecAnnual = elec * 12 * elecFactor
        travelAnnual = travel * 365 * travelFactor
        lpgAnnual = lpg * 12 * lpgFactor
        dietAnnual = dietFactor * 365 * 0.5
        
        totalKg = elecAnnual + travelAnnual + lpgAnnual + dietAnnual
        totalTons = totalKg / 1000
        
        elecPercent = round (elecAnnual / totalKg * 100)
        travelPercent = round (travelAnnual / totalKg * 100)
        lpgPercent = round (lpgAnnual / totalKg * 100)
        dietPercent = 100 - (elecPercent + travelPercent + lpgPercent)
        
        treesNeeded = round (totalKg / 22)
        rawScore = 100 - (round totalTons * 2)
        ecoScore = max 0 (min 100 rawScore)
    
    in (totalTons, elecPercent, travelPercent, lpgPercent, dietPercent, treesNeeded, ecoScore)

-- ============ DATA ============

allTips :: [EcoTip]
allTips = 
    [ EcoTip 1 "Switch to LED bulbs" "Save up to 80% electricity. Replace 5 bulbs and cut 120 kg CO2/year." "bulb"
    , EcoTip 2 "Use public transport" "Use public transport 3 days/week. Reduces emissions by 45%." "bus"
    , EcoTip 3 "Go meat-free twice a week" "Cuts diet emissions by 30%. Try local millets and greens." "food"
    , EcoTip 4 "Plant a tree" "Neem or Mango saplings offset 22 kg CO2 annually." "tree"
    , EcoTip 5 "Refuse single-use plastic" "Carry your own cloth bag. Saves 1.5 kg plastic waste per month." "bag"
    , EcoTip 6 "Unplug devices at night" "Phantom load can be 10% of your bill. Simple habit = big savings." "plug"
    ]

-- ============ HELPER FUNCTIONS ============

getCurrentDateText :: IO Text
getCurrentDateText = do
    now <- getCurrentTime
    return $ T.pack $ formatTime defaultTimeLocale "%B %Y" now

getRandomTips :: IO [EcoTip]
getRandomTips = do
    gen <- newStdGen
    let indices = take 6 (randoms gen :: [Int])
    return $ map (\i -> allTips !! (i `mod` length allTips)) indices

-- ============ MAIN ============

main :: IO ()
main = do
    putStrLn "========================================="
    putStrLn "Eco-Logic Server Starting..."
    putStrLn "========================================="
    putStrLn "Server running at: http://localhost:3000"
    putStrLn "========================================="
    putStrLn "Press Ctrl+C to stop"
    putStrLn ""
    
    scotty 3000 $ do
        
        -- API Routes
        get "/api/health" $ do
            json $ object [ "status" .= ("healthy" :: Text) ]
        
        post "/api/calculate" $ do
            req <- jsonData :: ActionM FootprintRequest
            let (total, elecPct, travelPct, lpgPct, dietPct, trees, score) = 
                    calculateFootprint 
                        (electricity req) 
                        (travel req) 
                        (lpg req) 
                        (diet req)
            json $ FootprintResponse total elecPct travelPct lpgPct dietPct trees score
        
        post "/api/contact" $ do
            msg <- jsonData :: ActionM ContactMessage
            liftIO $ putStrLn $ "Contact from: " ++ T.unpack (name msg)
            json $ object 
                [ "status" .= ("success" :: Text)
                , "message" .= ("Thank you! A tree will be planted in your name." :: Text) ]
        
        get "/api/tips" $ do
            tips <- liftIO getRandomTips
            json tips
        
        get "/api/date" $ do
            date <- liftIO getCurrentDateText
            json $ object ["date" .= date]
        
        get "/api/stats" $ do
            json $ object 
                [ "totalUsers" .= (12489 :: Int)
                , "totalCO2Saved" .= (45678.9 :: Double)
                , "treesPlanted" .= (8765 :: Int) ]
        
        -- Serve frontend.html
        get "/" $ file "frontend.html"
        
        -- Serve any other route with frontend.html
        get "/:page" $ do
            file "frontend.html"
            