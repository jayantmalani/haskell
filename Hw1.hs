-- ---
-- title: Homework #1, Due Monday 1/26/15
-- ---


-- Haskell Formalities
-- -------------------

-- We declare that this is the Hw1 module and import some libraries:

module Hw1 where
import SOE
import Play
import XMLTypes
import Draw

-- Part 0: All About You
-- ---------------------

-- Tell us your name, email and student ID, by replacing the respective
-- strings below

myName  = "Jayant Malani"
myEmail = "jmalani@eng.ucsd.edu"
mySID   = "A53102766"

-- Part 1: Defining and Manipulating Shapes
-- ----------------------------------------

-- You will write all of your code in the `Hw1.hs` file, in the spaces
-- indicated. Do not alter the type annotations --- your code must
-- typecheck with these types to be accepted.

-- The following are the definitions of shapes:

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show
-- >
type Radius = Float
type Side   = Float
type Vertex = (Float, Float)

-- 1. Below, define functions `rectangle` and `rtTriangle` as suggested
--    at the end of Section 2.1 (Exercise 2.1). Each should return a Shape
--    built with the Polygon constructor.

rectangle :: Side -> Side -> Shape
rectangle a b = Polygon[(1.0,2.0),(1.0+a,2.0),(1.0,2.0+b),(1.0+a,2.0+b)]

rtTriangle :: Side -> Side -> Shape
rtTriangle a b = Polygon[(0.0,0.0),(0.0,0.0+b),(0.0+a,0.0)]

-- 2. Define a function

--   which returns the number of sides a given shape has.
--   For the purposes of this exercise, an ellipse has 42 sides,
--   and empty polygons, single points, and lines have zero sides.

sides :: Shape -> Int
sides (Ellipse a b)         = 42
sides (Rectangle a b)       = 4
sides (RtTriangle a b )     = 3
sides (Polygon x)           = if length x < 3 then 0 else length x

-- 3. Define a function

-- helper function for bigger Polyon function
eval e (a,b) c= (a*sqrt e, b*sqrt e) : c

bigger :: Shape -> Float -> Shape
bigger (Rectangle a b) e    = Rectangle (a*sqrt e) (b*sqrt e) 
bigger (Ellipse a b) e      = Ellipse (a*sqrt e) (b*sqrt e) 
bigger (RtTriangle a b) e   = RtTriangle (a*sqrt e) (b*sqrt e)
bigger (Polygon a) e        = Polygon (foldr (eval e) [] a)



--   that takes a shape `s` and expansion factor `e` and returns
--   a shape which is the same as (i.e., similar to in the geometric sense)
--   `s` but whose area is `e` times the area of `s`.

-- 4. The Towers of Hanoi is a puzzle where you are given three pegs,
--    on one of which are stacked $n$ discs in increasing order of size.
--    To solve the puzzle, you must move all the discs from the starting peg
--    to another by moving only one disc at a time and never stacking
--    a larger disc on top of a smaller one.

--    To move $n$ discs from peg $a$ to peg $b$ using peg $c$ as temporary storage:

--    1. Move $n - 1$ discs from peg $a$ to peg $c$.
--    2. Move the remaining disc from peg $a$ to peg $b$.
--    3. Move $n - 1$ discs from peg $c$ to peg $b$.

--    Write a function

hanoi :: Int -> String -> String -> String -> IO ()
hanoi 1 a b c = putStrLn("move disc from "++a++" to "++b)
hanoi n a b c = do hanoi (n-1) a c b
                   putStrLn("move disc from "++a++" to "++b)
                   hanoi (n-1) c b a
                     

--   that, given the number of discs $n$ and peg names $a$, $b$, and $c$,
--   where a is the starting peg,
--   emits the series of moves required to solve the puzzle.
--   For example, running `hanoi 2 "a" "b" "c"`

--   should emit the text

-- ~~~
-- move disc from a to c
-- move disc from a to b
-- move disc from c to b
-- ~~~

-- Part 2: Drawing Fractals
-- ------------------------

-- 1. The Sierpinski Carpet is a recursive figure with a structure similar to
--    the Sierpinski Triangle discussed in Chapter 3:

-- ![Sierpinski Carpet](/static/scarpet.png)

-- Write a function `sierpinskiCarpet` that displays this figure on the
-- screen:

sierpinskiCarpet :: IO ()
sierpinskiCarpet = runGraphics (
                       do 
                            w <- openWindow "Drawing Shapes" (600,500)
                            drawCarpet 300 250 400 5 w 
                            spaceClose w 
                       )
                      
drawCarpet:: Int -> Int -> Int-> Int->Window->IO ()
drawCarpet x y size n w = if 
                        n==0 
                        then drawInWindow w (withColor Blue (polygon [(x, y), (x+size,y), (x+size,y+size), (x,y+size), (x, y)]))
                        else let newsize = size `div` 3
                        in do
                        drawCarpet x (y+newsize) newsize (n-1) w
                        drawCarpet (x+newsize) (y+newsize) newsize (n-1) w
                        drawCarpet (x+newsize) y newsize (n-1) w 
                        drawCarpet (x+newsize) (y-newsize) newsize (n-1) w
                        drawCarpet x (y-newsize) newsize (n-1) w
                        drawCarpet (x-newsize) (y-newsize) newsize (n-1) w
                        drawCarpet (x-newsize) (y+newsize) newsize (n-1) w
                        drawCarpet (x-newsize) y newsize (n-1) w

-- Note that you either need to run your program in `SOE/src` or add this
-- path to GHC's search path via `-i/path/to/SOE/src/`.
-- Also, the organization of SOE has changed a bit, so that now you use
-- `import SOE` instead of `import SOEGraphics`.

-- 2. Write a function `myFractal` which draws a fractal pattern of your
--    own design.  Be creative!  The only constraint is that it shows some
--    pattern of recursive self-similarity.

myFractal :: IO ()
myFractal = runGraphics (
     do w <- openWindow "Drawing Shapes" (1000,1000)
        utilFractal1 400 400 150 7 w
        utilFractal2 400 400 150 7 w
        spaceClose w 
     ) 

-- helper functions
rttriangle :: Int -> Int -> Int -> Graphic 
rttriangle x y a = polygon [(x,y),(x,y+a),(x+a,y)] 

-- to generate vertex of triangle
getVertx (x,y) r = polygon [(x,y-r), (x - (mulCos30 r), y + r `div` 2),(x + (mulCos30 r), y + r `div` 2 ),(x,y-r)]
getVertxr (x,y) r = polygon [(x,y+r), (x - (mulCos30 r), y - r `div` 2),(x + (mulCos30 r), y - r `div` 2 ),(x,y+r)]

-- cos 30
mulCos30 ::Int -> Int
mulCos30 x = round ( (fromIntegral x) * 0.866 )

--first fractal
utilFractal1 :: Int -> Int -> Int -> Int -> Window -> IO ()
utilFractal1 x y size n w = if 
                           (n > 1)
                           then let newSize = size `div` 2
                           in do
                           drawInWindow w (withColor Red  (getVertxr (x,y) size))
                           drawInWindow w (withColor Green  (getVertx (x,y) (size `div` 2)))
                           utilFractal1 (x) (y - size) newSize (n-1) w
                           utilFractal1 (x + size) (y + newSize ) newSize (n-1) w
                           utilFractal1 (x - size) (y + newSize ) newSize (n-1) w
                           else
                           return ()

-- second fractal
utilFractal2 :: Int -> Int -> Int -> Int -> Window -> IO ()
utilFractal2 x y size n w = if 
                           (n > 1)
                           then let newSize = size `div` 2
                           in do
                           drawInWindow w (withColor Red  (getVertx (x,y) size))
                           drawInWindow w (withColor Green  (getVertxr (x,y) (size `div` 2)))
                           utilFractal2 (x) (y + size) newSize (n-1) w
                           utilFractal2 (x + size) (y - newSize ) newSize (n-1) w
                           utilFractal2 (x - size) (y - newSize ) newSize (n-1) w
                           else
                           return ()

-- Part 3: Recursion Etc.
-- ----------------------

-- First, a warmup. Fill in the implementations for the following functions.

-- (Your `maxList` and `minList` functions may assume that the lists
-- they are passed contain at least one element.)

-- Write a *non-recursive* function to compute the length of a list

lengthNonRecursive :: [a] -> Int
lengthNonRecursive a = foldl (\x _ -> (x+1)) 0 a

-- `doubleEach [1,20,300,4000]` should return `[2,40,600,8000]`

doubleEach :: [Int] -> [Int]
doubleEach []=[]
doubleEach (x:xs) = (2*x):doubleEach xs

-- Now write a *non-recursive* version of the above.

doubleEachNonRecursive :: [Int] -> [Int]
doubleEachNonRecursive a = map (\x -> 2*x) a

-- `pairAndOne [1,20,300]` should return `[(1,2), (20,21), (300,301)]`

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne []=[]
pairAndOne (x:xs)= (x,x+1):pairAndOne xs


-- Now write a *non-recursive* version of the above.

pairAndOneNonRecursive :: [Int] -> [(Int, Int)]
pairAndOneNonRecursive a = map (\x -> (x,x+1)) a

-- `addEachPair [(1,2), (20,21), (300,301)]` should return `[3,41,601]`

addEachPair :: [(Int, Int)] -> [Int]
addEachPair [] = []
addEachPair ((a,b):xs)= (a+b):addEachPair xs

-- Now write a *non-recursive* version of the above.

addEachPairNonRecursive :: [(Int, Int)] -> [Int]
addEachPairNonRecursive a = map (\(x,y)->x+y) a

-- `minList` should return the *smallest* value in the list. You may assume the
-- input list is *non-empty*.

ite::Bool -> t-> t -> t
ite True x _ = x
ite False _ x = x

minList :: [Int] -> Int
minList [a] = a
minList (x:xs) = if x < y then x else y
                 where y = minList xs

-- Now write a *non-recursive* version of the above.


minListNonRecursive :: [Int] -> Int
minListNonRecursive (a:b) = foldl (\x y ->ite (x>y) y x) a b

-- `maxList` should return the *largest* value in the list. You may assume the
-- input list is *non-empty*.

maxList :: [Int] -> Int
maxList [a] = a
maxList (x:xs) = if x > y then x else y
                 where y = maxList xs

-- Now write a *non-recursive* version of the above.

maxListNonRecursive :: [Int] -> Int
maxListNonRecursive (a:b) = foldl (\x y ->ite (x>y) x y) a b

-- Now, a few functions for this `Tree` type.

data Tree a = Leaf a | Branch (Tree a) (Tree a)
              deriving (Show, Eq)

-- `fringe t` should return a list of all the values occurring as a `Leaf`.
-- So: `fringe (Branch (Leaf 1) (Leaf 2))` should return `[1,2]`

fringe :: Tree a -> [a]
fringe (Leaf a)     = [a]
fringe (Branch a b) = fringe a ++ fringe b

-- `treeSize` should return the number of leaves in the tree.
-- So: `treeSize (Branch (Leaf 1) (Leaf 2))` should return `2`.

treeSize :: Tree a -> Int
treeSize (Leaf a)       = 1
treeSize (Branch a b)   = treeSize a + treeSize b

-- `treeSize` should return the height of the tree.
-- So: `height (Branch (Leaf 1) (Leaf 2))` should return `1`.

treeHeight :: Tree a -> Int
treeHeight (Leaf a)     = 0
treeHeight (Branch a b) = 1 + if x>y then x else y 
                          where 
                          x=treeHeight a 
                          y=treeHeight b
                              
                         

-- Now, a tree where the values live at the nodes not the leaf.

data InternalTree a = ILeaf | IBranch a (InternalTree a) (InternalTree a)
                      deriving (Show, Eq)

-- `takeTree n t` should cut off the tree at depth `n`.
-- So `takeTree 1 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `IBranch 1 ILeaf ILeaf`.

--z :: InternalTree Int
--z = IBranch 6 (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)) (IBranch 4 (IBranch 2 ILeaf ILeaf) ILeaf)

--urFunc = takeTreeWhile (> 1) z

takeTree :: Int -> InternalTree a -> InternalTree a
takeTree n ILeaf            = ILeaf
takeTree 0 _                = ILeaf
takeTree n (IBranch a b c)  = IBranch a (takeTree (n-1) b) (takeTree (n-1) c)  

-- `takeTreeWhile p t` should cut of the tree at the nodes that don't satisfy `p`.
-- So: `takeTreeWhile (< 3) (IBranch 1 (IBranch 2 ILeaf ILeaf) (IBranch 3 ILeaf ILeaf)))`
-- should return `(IBranch 1 (IBranch 2 ILeaf ILeaf) ILeaf)`.

takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
takeTreeWhile _ ILeaf           = ILeaf
takeTreeWhile f (IBranch a b c) = if not (f a)
                                 then ILeaf
                                 else IBranch a (takeTreeWhile f b) (takeTreeWhile f c) 

-- Write the function map in terms of foldr:

myMap :: (a -> b) -> [a] -> [b]
myMap f b = foldr (\a x -> f a:x) [] b
   
-- Part 4: Transforming XML Documents
-- ----------------------------------

-- The rest of this assignment involves transforming XML documents.
-- To keep things simple, we will not deal with the full generality of XML,
-- or with issues of parsing. Instead, we will represent XML documents as
-- instances of the following simpliﬁed type:

-- ~~~~
-- data SimpleXML =
--    PCDATA String
--  | Element ElementName [SimpleXML]
--  deriving Show

-- type ElementName = String
-- ~~~~

-- That is, a `SimpleXML` value is either a `PCDATA` ("parsed character
-- data") node containing a string or else an `Element` node containing a
-- tag and a list of sub-nodes.

-- The file `Play.hs` contains a sample XML value. To avoid getting into
-- details of parsing actual XML concrete syntax, we'll work with just
-- this one value for purposes of this assignment. The XML value in
-- `Play.hs` has the following structure (in standard XML syntax):

-- ~~~
-- <PLAY>
--   <TITLE>TITLE OF THE PLAY</TITLE>
--   <PERSONAE>
--     <PERSONA> PERSON1 </PERSONA>
--     <PERSONA> PERSON2 </PERSONA>
--     ... -- MORE PERSONAE
--     </PERSONAE>
--   <ACT>
--     <TITLE>TITLE OF FIRST ACT</TITLE>
--     <SCENE>
--       <TITLE>TITLE OF FIRST SCENE</TITLE>
--       <SPEECH>
--         <SPEAKER> PERSON1 </SPEAKER>
--         <LINE>LINE1</LINE>
--         <LINE>LINE2</LINE>
--         ... -- MORE LINES
--       </SPEECH>
--       ... -- MORE SPEECHES
--     </SCENE>
--     ... -- MORE SCENES
--   </ACT>
--   ... -- MORE ACTS
-- </PLAY>
-- ~~~

-- * `sample.html` contains a (very basic) HTML rendition of the same
--   information as `Play.hs`. You may want to have a look at it in your
--   favorite browser.  The HTML in `sample.html` has the following structure
--   (with whitespace added for readability):

-- ~~~
-- <html>
--   <body>
--     <h1>TITLE OF THE PLAY</h1>
--     <h2>Dramatis Personae</h2>
--     PERSON1<br/>
--     PERSON2<br/>
--     ...
--     <h2>TITLE OF THE FIRST ACT</h2>
--     <h3>TITLE OF THE FIRST SCENE</h3>
--     <b>PERSON1</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <b>PERSON2</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--     <h3>TITLE OF THE SECOND SCENE</h3>
--     <b>PERSON3</b><br/>
--     LINE1<br/>
--     LINE2<br/>
--     ...
--   </body>
-- </html>
-- ~~~

-- You will write a function `formatPlay` that converts an XML structure
-- representing a play to another XML structure that, when printed,
-- yields the HTML speciﬁed above (but with no whitespace except what's
-- in the textual data in the original XML).


formatPlay::SimpleXML->SimpleXML
formatPlay xml = Element "html" [Element "body" (mainconvert xml 1)]

-- helper function
mainconvert::SimpleXML->Int->[SimpleXML]
mainconvert (Element abc (t:bo)) d = (title t d)++(body bo d)

title::SimpleXML->Int->[SimpleXML]
title (Element abc [list]) d = if (d==4) then
                               [Element "b" [list]]++[Element "br" []]
                               else 
                               [Element ("h"++ show d) [list]]
                               
body::[SimpleXML]->Int->[SimpleXML]
body [] _ = []
body  (first:rest) d = if (d==1) then
                       (header first (d+1) )++ (bodyusual rest d) 
                       else if (d==4) then
                       foldr (addbrk) [] (first:rest)
                       else
                       bodyusual (first:rest) d

bodyusual::[SimpleXML]->Int->[SimpleXML]
bodyusual [] _ = []
bodyusual (first:rest) d = (mainconvert first (d+1)) ++ (bodyusual rest d)

header::SimpleXML->Int->[SimpleXML]
header (Element xyz list) d = Element ("h" ++ show d)[PCDATA "Dramatis Personae"]:foldr (addbrk) [] list

addBreak f element a = f element:(Element "br" []):a

addbrk ::SimpleXML->[SimpleXML]->[SimpleXML]
addbrk (Element abc [name]) a = name:(Element "br" [] ):a

-- The main action that we've provided below will use your function to
-- generate a ﬁle `dream.html` from the sample play. The contents of this
-- ﬁle after your program runs must be character-for-character identical
-- to `sample.html`.

mainXML = do writeFile "dream.html" $ xml2string $ formatPlay play
             testResults "dream.html" "sample.html"
-- >
firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds)
     | c==d = firstDiff cs ds
     | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)
-- >
testResults :: String -> String -> IO ()
testResults file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> do
      putStr "Success!\n"
    Just (cs,ds) -> do
      putStr "Results differ: '"
      putStr (take 40 cs)
      putStr "' vs '"
      putStr (take 40 ds)
      putStr "'\n"

-- Important: The purpose of this assignment is not just to "get the job
-- done" --- i.e., to produce the right HTML. A more important goal is to
-- think about what is a good way to do this job, and jobs like it. To
-- this end, your solution should be organized into two parts:

-- 1. a collection of generic functions for transforming XML structures
--    that have nothing to do with plays, plus

-- 2. a short piece of code (a single deﬁnition or a collection of short
--    deﬁnitions) that uses the generic functions to do the particular
--    job of transforming a play into HTML.

-- Obviously, there are many ways to do the ﬁrst part. The main challenge
-- of the assignment is to ﬁnd a clean design that matches the needs of
-- the second part.

-- You will be graded not only on correctness (producing the required
-- output), but also on the elegance of your solution and the clarity and
-- readability of your code and documentation.  Style counts.  It is
-- strongly recommended that you rewrite this part of the assignment a
-- couple of times: get something working, then step back and see if
-- there is anything you can abstract out or generalize, rewrite it, then
-- leave it alone for a few hours or overnight and rewrite it again. Try
-- to use some of the higher-order programming techniques we've been
-- discussing in class.

-- Submission Instructions
-- -----------------------

-- * If working with a partner, you should both submit your assignments
--   individually.

-- * Make sure your `Hw1.hs` is accepted by GHCi without errors or warnings.

-- * Attach your `hw1.hs` file in an email to `cse230@goto.ucsd.edu` with the
--   subject "HW1" (minus the quotes). *This address is unmonitored!*

-- Credits
-- -------

-- This homework is essentially Homeworks 1 & 2 from
-- <a href="http://www.cis.upenn.edu/~bcpierce/courses/552-2008/index.html">UPenn's CIS 552</a>.
