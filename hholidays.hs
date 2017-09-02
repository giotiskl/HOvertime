import Data.List
import Data.Maybe
import System.IO
import Workdays
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.List.Split as Split

main :: IO () 
main = do
  withFile "./holidays.yaml" ReadMode (\handle -> do
    -- Get contents of holidays.yaml
    contents <- hGetContents handle
    -- Get today's date as :: Date {dateYear, dateMonth, dateDay}
    endDate  <- today
    -- Get total hours of work as :: Double
    -- Get starting date as :: Date {dateYear, dateMonth, dateDay}
    let contentsSplit = words contents
        values        = filter (\v -> odd (fromJust $ elemIndex v contentsSplit)) contentsSplit
        hours         = read (head values)::Double
        startDate     = dateFromString $ last values
        -- Calculate weekdays (which is the working days as long as public holidays are harvested)
        workDaysUntilNow = weekdays startDate endDate 
        -- Average time you work per day
        -- Overtime
        workTimeAvg  = hours / (fromIntegral workDaysUntilNow)
        overtime     = hours - (fromIntegral $ workDaysUntilNow * 8)
        fWorkTimeAvg = decimalToHoursAndMinutes workTimeAvg
        fOvertime    = decimalToHoursAndMinutes overtime
        -- Output the data of interest
    putStrLn $ "You work an avg. of " ++ fWorkTimeAvg ++ " per day."
    putStrLn $ "You have a total " ++ fOvertime ++ " of overtime so far."
    )

today :: IO Date -- :: Date {dateYear, dateMonth, dateDay}
today = getCurrentTime >>= return . dateFromString . showGregorian . utctDay

dateFromString :: String -> Date
dateFromString s = Date y m d
  where dateDataAsStrings = Split.splitOneOf "/-" s -- Reverse stupid american formatting of dates
        y = read (head dateDataAsStrings)::Integer
        m = read (dateDataAsStrings !! 1)::Int
        d = read (last dateDataAsStrings)::Int

decimalToHoursAndMinutes :: (Show p, RealFrac p) => p -> String
decimalToHoursAndMinutes h = (show hours) ++ " hour(s) and " ++ (show minutes) ++ " minute(s)"
  where hours   = floor h
        minutes = 
          let decimalPartString = "0." ++ (last $ Split.splitOn "." $ show h)
              decimalPart = read (decimalPartString)::Double
           in round $ decimalPart * 60
