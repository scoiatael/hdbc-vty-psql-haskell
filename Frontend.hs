import Backend

import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty.LLInput

import qualified Data.Text as T
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.MVar
import Data.Time.LocalTime

type FrontEnd a = ReaderT (Collection, MVar [IO ()], MVar Int, MVar Int) IO a
runFrontEnd :: FrontEnd a -> IO ()
runFrontEnd act = do
  c <- newCollection
  m <- newMVar []
  i1 <- newMVar (-1)
  i2 <- newMVar (-1)
  a <- runReaderT act (c,m, i1, i2) 
  runUi c defaultContext

runInIO :: FrontEnd () -> FrontEnd (IO ())
runInIO act = do
  c <- ask
  return $ runReaderT act c 

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
thr4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

addCollection :: Show a => Widget a -> Widget FocusGroup -> FrontEnd (IO ())
addCollection w wfg = do
  c <- ask
  a <- liftIO $ addToCollection (fst4 c) w wfg
  return $ (pushToStack (snd4 c) a >> a)

takeStack :: FrontEnd (MVar [IO ()])
takeStack = do
  c <- ask
  return $ snd4 c

takeIDU :: FrontEnd (MVar Int)
takeIDU = do
  c <- ask
  return $ thr4 c

takeIDZ :: FrontEnd (MVar Int)
takeIDZ = do
  c <- ask
  return $ fth4 c

takeCollection :: FrontEnd Collection
takeCollection = do
  c <- ask
  return $ fst4 c

pushInt :: MVar Int -> Int -> IO ()
pushInt mv i = modifyMVar_ mv (\a -> return i)

takeInt mv = modifyMVar mv (\a -> return (a,a))  

pushToStack :: MVar [IO ()] -> IO () -> IO ()
pushToStack mv act = modifyMVar_ mv (return . (act:))

popFromStack :: MVar [IO ()] -> IO (Maybe (IO ()))
popFromStack mv = modifyMVar mv (return .  safeUnCon)

safeUnCon :: [a] -> ([a], Maybe a)
safeUnCon [] = ([], Nothing)
safeUnCon [_] = ([], Nothing)
safeUnCon (x:(y:xs)) = ((y:xs), Just y)

safeBack :: Maybe (IO ()) -> IO ()
safeBack Nothing = error "Quit"
safeBack (Just f) = f

backButton = do
  mv <- takeStack
  makeButton ("Back", popFromStack mv >>= safeBack)

simpleFocusGroup = do
  fg <- liftIO $ newFocusGroup
  liftIO $ fg `onKeyPressed` \_ k _ ->
    if k == KEsc then error "Quit" else return False
  return fg

simpleText t = do
  let width = length t
  let height = length $ lines t
  txt <- liftIO $ plainText $ T.pack t
  liftIO $ ( boxFixed width height =<< centered txt)

infoScreen :: String -> FrontEnd (IO ())
infoScreen t = do
  fg <- simpleFocusGroup
  b <- backButton
  txt <- simpleText t
  let cb = (centered b)
  let ct = (centered txt)
  cw <- liftIO $ ct <--> cb
  addWidgetToFG fg b
  addCollection cw fg
  

fromButtonList :: String -> FrontEnd (String -> IO (), [String]) -> FrontEnd (IO ()) 
fromButtonList t f = do
  (fs,ls) <- f
  list <- liftIO $ newTextList current_attr $ map T.pack ls
  fg <- liftIO $ newFocusGroup
  liftIO $ fg `onKeyPressed` \_ k _ ->
    if k == KEsc then error "Quit" else return False
  txt <- liftIO $ plainText $ T.pack t
  b <- backButton
  let width = maximum $ map (length) $ t:(ls)
  cw <- liftIO $ (centered =<< boxFixed width 1 =<< centered txt) 
    <--> (centered =<< bordered =<< boxFixed width 20 list) <--> (centered b)
  addWidgetToFG fg list
  addWidgetToFG fg b
  liftIO $ list `onItemActivated` (\(ActivateItemEvent _ a _) -> fs $ T.unpack a)
  addCollection cw fg
  

makeButton (str, f) = liftIO $ do
  b <- newButton $ T.pack str
  b `onButtonPressed` \_ -> f
  return $ buttonWidget b

addWidgetToFG fg w = liftIO $ addToFocusGroup fg w

listScreen :: FrontEnd (String -> IO ()) -> IO (String, [String]) -> FrontEnd (IO ())
listScreen ch act = do
  runInIO $ do
    (x,xs) <- liftIO $ act
    a <- fromButtonList x (ch >>= \f' -> return (f', xs))
    liftIO a

doNothing :: FrontEnd (String -> IO ())
doNothing = return $ (\_ -> return ())

setMVarAndGoto mv act = do
  f <- act 
  return $ \str -> let i = read (words str !! 0) :: Int in do
    pushInt mv i 
    f

splitHeader :: [String] -> IO (String, [String])
splitHeader (x:xs) = return (x, xs)
addHeader :: (a -> String) -> String -> [a] -> IO (String, [String])
addHeader f s strs = return (s, map f strs)

listTaskWithStatusScreen a b s = do
  idz <- takeIDZ
  listScreen ( setMVarAndGoto idz s)
    (runT (listTasksWithStatus a b) >>= addHeader show "Wybierz zadanie")

addText t w = liftIO $ do
  txt <- simpleText t
  (centered txt) <++> (centered w)

createEditLineAddFG fg = do
  e <- liftIO $ editWidget
  addWidgetToFG fg e
  return e

createEditMultiLineAddFG fg = do
  e <- liftIO $ multiLineEditWidget
  addWidgetToFG fg e
  return e

blank act = act >>= \f -> return $ \_ -> f

--
--- Admin
listUsersScreen = do
  idu <- takeIDU 
  listScreen (setMVarAndGoto idu modUserScreen) (runT listUsers >>= splitHeader)

addUserScreen = do
  fg <- simpleFocusGroup
  im <- createEditLineAddFG fg
  na <- createEditLineAddFG fg 
  ro <- createEditLineAddFG fg
  nxtScreen <- infoScreen "Uzytkownik zostal dodany!"
  errScreen <- infoScreen "Taka rola nie istnieje!"
  st <- takeStack
  ok <- makeButton ("Ok", 
    do
      g <- liftM T.unpack $ getEditText ro
      if not (g == "a" || g == "s" || g == "g") then
          errScreen 
        else do
          i <- liftM (T.unpack) $ getEditText im
          n <- liftM (T.unpack) $ getEditText na 
          let r = roleFromString g
          runT $ addUser n i r
          void $ popFromStack st
          nxtScreen)
  addWidgetToFG fg ok
  b <- backButton
  txt <- simpleText "Dodaj uzytkownika"
  imGr <- addText "Imie" im
  naGr <- addText "Nazwisko" na
  roGr <- addText "Rola" ro
  cw <- liftIO $ (centered txt) <--> (centered imGr) <--> (centered naGr) <--> (centered roGr) 
    <--> (centered ok) <--> (centered b)
  addWidgetToFG fg b
  addCollection cw fg
  
modUserScreen = runInIO $ do
  idu <- takeIDU
  id <- liftIO $ takeInt idu
  txt <- liftIO $ runT $ detailsUser id
  fg <- simpleFocusGroup
  im <- createEditLineAddFG fg
  liftIO $ setEditText im $ T.pack (txt !! 1)
  na <- createEditLineAddFG fg 
  liftIO $ setEditText na $ T.pack (txt !! 0)
  nxtScreen <- infoScreen "Uzytkownik zostal zmieniony!"
  st <- takeStack
  ok <- makeButton ("Ok", 
    do
      i <- liftM T.unpack $ getEditText im
      n <- liftM T.unpack $ getEditText na 
      runT $ modUser id n i 
      void $ popFromStack st
      void $ popFromStack st
      nxtScreen)
  addWidgetToFG fg ok
  b <- backButton
  txt <- simpleText "Zmien uzytkownika"
  imGr <- addText "Imie" im
  naGr <- addText "Nazwisko" na
  cw <- liftIO $ (centered txt) <--> (centered imGr) <--> (centered naGr)
    <--> (centered ok) <--> (centered b)
  addWidgetToFG fg b
  a <- addCollection cw fg
  liftIO $ a

--
---Teacher
chooseTeacherIDScreen = do
  idu <- takeIDU
  listScreen ( setMVarAndGoto idu teacherScreen) (runT listTeacherIDs >>= addHeader show "Wybierz swoj idu")
listTeacherExercisesScreen = do
  idz <- takeIDZ
  listScreen (setMVarAndGoto idz showTaskTextScreen) (runT listTeacherExercises >>= splitHeader)
showTaskTextScreen = do
  idz <- takeIDZ
  listScreen ( blank changeTaskScreen ) 
    (takeInt idz >>= ( runT . showTaskText ) >>= splitHeader)

changeTaskScreen = runInIO $ do
  idz <- takeIDZ
  id <- liftIO $ takeInt idz
  info <- liftIO $ runT $ showTaskStatusText id
  let status = read (info !! 0) :: Int
  fg <- simpleFocusGroup
  im <- liftIO $ if status == 0 then createEditMultiLineAddFG fg else multiLineEditWidget 
  liftIO $ setEditText im $ T.pack (info !! 1)
  st <- liftIO $ newMultiStateCheckbox (T.pack "Status") [(0,'0'), (1,'1'), (2,'2')]
  liftIO $ setCheckboxState st status
  addWidgetToFG fg st
  nxtScreen <- infoScreen "Zadanie zostalo zmienione!"
  errScreen <- infoScreen "Status mozna tylko zwiekszac!"
  stck <- takeStack
  ok <- makeButton ("Ok", 
    do
      t <- liftM T.unpack $ getEditText im
      ch <- getCheckboxState st
      let inc = ch - status
      if inc < 0 then errScreen else do
        runT $ setTaskText id t >> incTaskStatus id inc
        void $ popFromStack stck
        void $ popFromStack stck
        void $ popFromStack stck
        nxtScreen)
  addWidgetToFG fg ok
  b <- backButton
  txt <- simpleText "Zmien zadanie"
  stGr <- liftIO $ simpleText "Tresc" <--> (return im)
  cw <- liftIO $ (centered txt) <--> ((centered stGr) <++> (centered st))
    <--> (centered ok) <--> (centered b)
  addWidgetToFG fg b
  a <- addCollection cw fg
  liftIO $ a

addTaskScreen = do
  fg <- simpleFocusGroup
  im <- liftIO $ createEditMultiLineAddFG fg  
  nxtScreen <- infoScreen "Zadanie zostalo dodane!"
  stck <- takeStack
  ok <- makeButton ("Ok", 
    do
      t <- liftM T.unpack $ getEditText im
      runT $ addNewTask t
      void $ popFromStack stck
      nxtScreen)
  addWidgetToFG fg ok
  b <- backButton
  txt <- simpleText "Dodaj zadanie"
  stGr <- liftIO $ simpleText "Tresc" <--> (return im)
  cw <- liftIO $ (centered txt) <--> (centered stGr)
    <--> (centered ok) <--> (centered b)
  addWidgetToFG fg b
  addCollection cw fg

--
---Student
chooseStudentIDScreen = do
  idu <- takeIDU
  listScreen ( setMVarAndGoto idu studentScreen) (runT listStudentIDs >>= addHeader show "Wybierz swoj idu")

listTaskScreen = do
  idu <- takeIDU
  listScreen doNothing (takeInt idu >>= \a -> runT (listTasks a) >>= splitHeader)

listTaskInternalsScreen = runInIO $ do
  idu <- takeIDU
  idz <- takeIDZ
  a <- liftIO $ takeInt idu
  b <- liftIO $ takeInt idz
  tls <- liftIO $ runT $ listTaskInternals a b 
  let (t:ls) = tls
  list <- liftIO $ newTextList current_attr $ map T.pack ls
  fg <- simpleFocusGroup
  txtTitle <- simpleText "Tresc"
  st <- liftIO $ runT $ showTaskStatusText b
  let tresc = st !! 1
  txt <- liftIO $ simpleText tresc
  bck <- backButton
  let width = maximum $ map (length) $ tls
  tTitle <- liftIO $ simpleText t
  nxtScreen <- infoScreen "Zglosiles rozwiazanie!"
  stck <- takeStack
  ok <- makeButton ("Dodaj rozwiazanie", 
    do
      runT $ addSolution a b  
      void $ popFromStack stck
      void $ popFromStack stck
      nxtScreen)
  cw <- liftIO $ (((centered txtTitle) <--> (centered txt)) <++> 
    ((centered =<< boxFixed width 1 =<< centered tTitle) 
      <--> (centered =<< bordered =<< boxFixed width 20 list))) <--> (centered ok) <--> (centered bck)
  addWidgetToFG fg list
  addWidgetToFG fg ok
  addWidgetToFG fg bck
  a <- addCollection cw fg
  liftIO $ a

--
---Checker
listSolutionsNotGradedScreen = do
  idz <- takeIDZ
  listScreen (setMVarAndGoto idz gradeScreen) (runT listSolutionsNotGraded >>= splitHeader)

gradeScreen = runInIO $ do
  idzMVar <- takeIDZ
  idz <- liftIO $ takeInt idzMVar
  fg <- simpleFocusGroup
  od <- createEditLineAddFG fg
  dataDo <- createEditLineAddFG fg 
  gr <- createEditLineAddFG fg
  nxtScreen <- infoScreen "Oceniono zadania!"
  st <- takeStack
  ok <- makeButton ("Ok", 
    do
      o <- liftM T.unpack $ getEditText od
      d <- liftM T.unpack $ getEditText dataDo
      g <- liftM T.unpack $ getEditText gr
      runT $ setSolutionGrade idz (read o :: LocalTime) (read d :: LocalTime) (read g :: Int)
      void $ popFromStack st
      void $ popFromStack st
      nxtScreen)
  addWidgetToFG fg ok
  b <- backButton
  txt <- simpleText "Ocen zadania"
  imGr <- addText "Od" od
  naGr <- addText "Do" dataDo
  roGr <- addText "Ocena" gr
  cw <- liftIO $ (centered txt) <--> (centered imGr) <--> (centered naGr) <--> (centered roGr) 
    <--> (centered ok) <--> (centered b)
  addWidgetToFG fg b
  a <- addCollection cw fg
  liftIO a

placeholder :: FrontEnd (IO ())
placeholder = return $ return ()

adminScreen = fromButtonList "Admin" $ screenChooser [
  ( "przeglądaj listę użytkowników", listUsersScreen ),
--  ( "modyfikuj użytkownika", placeholder),
  ( "dodaj nowego użytkownika", addUserScreen) ] 

teacherScreen = fromButtonList "Nauczyciel" $ screenChooser [
  ( "przeglądaj listę zadań", listTeacherExercisesScreen),
--  ( "modyfikuj zadanie", placeholder),
  ( "dodaj nowe zadanie", addTaskScreen)] 

studentScreen = fromButtonList "Student" $ screenChooser [
  ( "przeglądaj listę zadań", listTaskScreen),
  ( "obejrzyj zadanie", listTaskWithStatusScreen 1 1 listTaskInternalsScreen)] 

checkerScreen = fromButtonList "Sprawdzaczka" $ screenChooser [
  ( "przeglądaj listę zadań", listSolutionsNotGradedScreen)
--  ,( "oceń rozwiązania", placeholder)
  ] 

screenChooser :: [(String, FrontEnd (IO ()))] -> FrontEnd (String -> IO (), [String])
screenChooser list = do
  let strs = map fst list
  fs <- sequence $  map snd list
  return $ (chooser $ zip strs fs, strs)

chooser :: [(String, IO ())] -> String -> IO ()
chooser [] s = error "No such item"
chooser ((s,f):l) s' = if s == s' then f else chooser l s'

main :: IO ()
main = runFrontEnd $ do
  w <- fromButtonList "Zaloguj sie jako" $ screenChooser
    [ ( "Admin",  adminScreen),
      ( "Nauczyciel", chooseTeacherIDScreen),
      ( "Student", chooseStudentIDScreen),
      ( "Sprawdzaczka", checkerScreen) ]
  liftIO $ w


{--
addSolutionScreen = runInIO $ do
  idu <- takeIDU
  idz <- takeIDZ
  a <- liftIO $ takeInt idu
  b <- liftIO $ takeInt idz
  fg <- simpleFocusGroup
  nxtScreen <- infoScreen "Zglosiles rozwiazanie!"
  stck <- takeStack
  ok <- makeButton ("Ok", 
    do
      runT $ addSolution a b  
      void $ popFromStack stck
      void $ popFromStack stck
      nxtScreen)
  addWidgetToFG fg ok
  b <- backButton
  txt <- simpleText "Dodaj rozwiazanie"
  cw <- liftIO $ (centered txt) <--> (centered ok) <--> (centered b)
  addWidgetToFG fg b
  a <- addCollection cw fg
  liftIO $ a
  
--}
