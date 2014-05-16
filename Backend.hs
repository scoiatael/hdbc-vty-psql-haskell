module Backend where

import DBTransaction
import Database.HDBC.Types
import Data.List
import Data.Time.LocalTime

mFromSql SqlNull = "Null"
mFromSql a = fromSql a

--- Admin
--add user
--mod user
--list users
data Role = Author | Student | Guest
roleFromString "a" = Author
roleFromString "s" = Student
roleFromString "g" = Guest
roleFromString a = error $ "No such role " ++ a

addUser :: String -> String -> Role -> DBTransaction ()
addUser lname fname role = let 
  r = case role of
     Author -> "a"
     Student -> "s"
     Guest -> "g" 
  in 
    runQuery "insert into uzytkownik (nazwisko, imie, rola) values (?,?,?);" 
      $ map toSql [ take 18 lname, take 12 fname, r]

listUsers :: DBTransaction [String]
listUsers = liftDB (convertPrettifyAddHeader ["idu", "nazwisko", "imie", "dataRej", "rola"]) $
  query "select * from uzytkownik;" []

detailsUser i = liftDB (map mFromSql .concat) $ 
  query "select imie,nazwisko from uzytkownik where idu = ?;" [toSql i]

convertPrettifyAddHeader :: [String] -> [[SqlValue]] -> [String]
convertPrettifyAddHeader head sqls = map (concat) $ 
  prettifyStringTable $ head : map (map mFromSql) sqls 

prettifyStringTable :: [[String]] -> [[String]]
prettifyStringTable t = transpose $ map prettifyStringList $ zip [0..]  $ transpose t

prettifyStringList :: (Int, [String]) -> [String]
prettifyStringList (a,l) = let 
  mlength = foldl (\a b -> let lb = length b in if lb > a then lb else a) 0 l
  in map (\s -> (if a == 0 then "" else " | ") ++ padTo mlength s) l

padTo :: Int -> String -> String
padTo l s = s ++ replicate (l - length s) ' '

modUser :: Int -> String -> String -> DBTransaction ()
modUser id lname fname = 
  runQuery "update uzytkownik set (nazwisko, imie) = (?,?) where idu = ?" 
    [toSql $ take 18 lname, toSql $ take 12 fname, toSql id]


---Teacher
--list exercises (has number of solutions and avg score)
--modify task text if status =0
--change task status
--add new task

listTeacherIDs :: DBTransaction [Int]
listTeacherIDs = liftDB (map (fromSql) . concat) $ query "select idu from uzytkownik where rola = 'a'" []

listTeacherExercises :: DBTransaction [String]
listTeacherExercises = liftDB (convertPrettifyAddHeader ["idz","status", "czasUtw", "rozwiazania", "srednia"]) $
  ( query "select idz, status, czasutw, count(*), avg(wynik) from zadanie \
            \left outer join rozwiazanie using (idz) group by idz;" [] )

incTaskStatus :: Int -> Int -> DBTransaction ()
incTaskStatus idz val = runQuery 
  "update zadanie set status = status + ? where idz = ? and status < 3 - ?;" $ map toSql [val, idz, val]

showTaskText :: Int -> DBTransaction [String]
showTaskText id = liftDB (convertPrettifyAddHeader ["tresc"]) $ 
  query "select tresc from zadanie where idz = ?;" [toSql id]

showTaskStatusText :: Int -> DBTransaction [String]
showTaskStatusText id = liftDB (map (fromSql) . concat) $ 
  query "select status, tresc from zadanie where idz = ?;" [toSql id]

setTaskText :: Int -> String -> DBTransaction ()
setTaskText id text = runQuery
  "update zadanie set tresc = ? where idz = ? and status = 0;" [toSql text, toSql id]

addNewTask :: String -> DBTransaction ()
addNewTask t = runQuery "insert into zadanie (tresc) values (?);" [toSql t]

---Student
--list tasks with status 1-2 (with number of solutions, avg score, his number of solutions, his avg score)
--see task with status 1 (with list of his solutions)
--send solution to task with status 1

listStudentIDs :: DBTransaction [Int]
listStudentIDs = liftDB (map (fromSql) . concat) $ query "select idu from uzytkownik where rola = 's'" []

listTasks :: Int -> DBTransaction [String]
listTasks id = liftDB (convertPrettifyAddHeader $ 
  words "idz status czasUtw rozwiazania srednia twoje_rozwiazania twoja_srednia") $
    query "with \
    \moje_rozwiazania as ( \
        \select idz, count(*), avg(wynik) from rozwiazanie where idu = ? group by (idz)), \
    \podsumowanie as ( \
      \select zadanie.idz, status, czasutw, count(czasZgl), avg(wynik) from zadanie \
        \left outer join rozwiazanie on (rozwiazanie.idz = zadanie.idz) where status = 1 or status = 2 \
          \group by zadanie.idz) \
   \select * from podsumowanie left outer join moje_rozwiazania using (idz);" [toSql id]

listTaskInternals :: Int -> Int -> DBTransaction [String]
listTaskInternals idu idz = liftDB (convertPrettifyAddHeader $
  words "tresc czasZgl wynik") $ 
    query "with \
    \moje_rozwiazania as ( \
        \select idz, czasZgl, wynik from rozwiazanie where idu = ? and idz = ? ) \
   \select czasZgl, wynik from moje_rozwiazania \
      \where idz = ?;" $ map toSql [idu, idz, idz]

listTasksWithStatus :: Int -> Int -> DBTransaction [Int]
listTasksWithStatus a b = liftDB (map (fromSql) . concat) $ 
  query "select idz from zadanie where status between ? and ?;" $ map toSql [a,b]

addSolution :: Int -> Int -> DBTransaction ()
addSolution idu idz = runQuery "insert into rozwiazanie (idu, idz) values (?,?);" $ map toSql [idu, idz]


---SolChecker
--list all solutions not graded
--change grade for all solutions for time interval

listSolutionsNotGraded :: DBTransaction [String]
listSolutionsNotGraded = liftDB (convertPrettifyAddHeader $ 
  words "idz idu czasZgl") $
    query "select idz,idu,czasZgl from rozwiazanie where wynik is null;" []

setSolutionGrade :: Int -> LocalTime -> LocalTime -> Int -> DBTransaction ()
setSolutionGrade idz beg end gr = runQuery "update rozwiazanie \
  \set wynik = ? where idz = ? and czasZgl >= ? and czasZgl < ? and wynik is null;" 
    [toSql gr, toSql idz, toSql beg, toSql end]

runT = runTransaction
