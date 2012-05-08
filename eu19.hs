
daysInYear :: Int -> Int
daysInYear year = 365 + leap year

leap :: Int -> Int
leap year 
  | year `mod` 400 == 0 = 1
  | year `mod` 100 == 0 = 0
  | year `mod` 4 == 0   = 1
  | otherwise = 0

firstDayInYear :: Int -> Int
firstDayInYear year
  | year < 1900  = error "can't count before 1900"
  | year == 1900 = 0
  | otherwise = (daysInYear lastYear + firstDayInYear lastYear) `mod` 7
                    where lastYear = year - 1

daysInMonth :: Int -> Int -> Int
daysInMonth year month = (scanl1 (+) [31,28+leap year,31,30,31,30,31,31,30,31,30,31]) !! month

sundays = do
  year <- [1901..2000]
  month <- [0..11]
  return . sunday year $ daysInMonth year month
    where sunday year day = if (day + firstDayInYear year) `mod` 7 == 6 then 1 else 0

eu19 = sum sundays
