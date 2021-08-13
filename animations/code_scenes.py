# General means of taking text from a file and doing certain colored highlighting of it.

# Structure
# Map to each line. Have a set of tuples (color, start, end) that should cover
# everything that isn't the default color.

# Let's start with a paragraph of Python code.

from manim import *

def fadeAll(scene, views):
    scene.play(AnimationGroup(*[FadeOut(v) for v in views]))

str1 = """def quicksort(arr):
    if len(arr) == 0:
        return []
    x = arr[0]
    smaller_sorted = quicksort(filter(lambda i: i <= x, arr[1:]))
    bigger_sorted = quicksort(filter(lambda i: i > x, arr[1:]))
    return smaller_sorted + [x] + bigger_sorted"""

str2 = ("quicksort :: (Ord a) => [a] -> [a]\n"
        "quicksort [] = []\n"
        "quicksort (x : xs) = smallerSorted ++ [x] ++ biggerSorted\n"
        "  where\n"
        "    smallerSorted = quicksort [a | a <- xs, a <= x]\n"
        "    biggerSorted = quicksort [a | a <- xs, a > x]\n"
        )

str3 = ("def swap(arr, i, j):\n"
        "  temp = arr[i]\n"
        "  arr[i] = arr[j]\n"
        "  arr[j] = temp\n"
        )

str4 = ("def partition(arr, start, end):\n"
        "  pivotElement = arr[start]\n"
        "  pivotIndex = start + 1\n"
        "  for i in range(start + 1, end):\n"
        "    if arr[i] <= pivotElement:\n"
        "      swap(arr, i, pivotIndex)\n"
        "      pivotIndex += 1\n"
        "  swap(arr, start, pivotIndex - 1)\n"
        "  return pivotIndex - 1\n"
        )

str5 = ("def quicksortHelper(arr, start, end):\n"
        "  if start + 1 >= end:\n"
        "    return\n"
        "  pivotIndex = partition(arr, start, end)\n"
        "  quicksortHelper(arr, start, pivotIndex)\n"
        "  quicksortHelper(arr, pivotIndex + 1, end)\n"
        )

str6 = ("def quicksort(arr):\n"
        "  quicksortHelper(arr, 0, len(arr))\n"
        )

def makeHighlightedCode(text, highlights):
    code = Code(code=text, insert_line_no=False, language="text", indentation_chars="  ")
    # code.stretch_to_fit_height(4)
    # code.stretch_to_fit_width(10)
    for (color, line_no, bounds) in highlights:
        if bounds is None:
            bounds = (0, len(code.code.chars[line_no]) - 1)
        startChar, endChar = bounds
        for c in range(startChar, endChar + 1):
            code.code.chars[line_no][c].set_fill(color)
    return code

def makeFullyHighlightedCode(text, color):
    code = Code(code=text, insert_line_no=False, language="text", indentation_chars="  ")
    # code.stretch_to_fit_height(4)
    # code.stretch_to_fit_width(10)
    code.code.set_fill(color)
    return code

# Is it easier for me to make separate objects and transform them?
# Or is it easier to transform a single code object?
class PythonBasic(Scene):
    def addHighlights(self, code_object, highlights):
        anims = []
        for (color, line_no, bounds) in highlights:
            if bounds is None:
                bounds = (0, len(code_object.code.chars[line_no]) - 1)
            startChar, endChar = bounds
            for c in range(startChar, endChar + 1):
                anims.append(ApplyMethod(code_object.code.chars[line_no][c].set_fill, color))
        self.play(AnimationGroup(*anims))

    def clearColor(self, codeFields):
        anims = []
        for f in codeFields:
            anims.append(ApplyMethod(f.code.set_fill, WHITE))
        self.play(AnimationGroup(*anims))

    def construct(self):
        p0 = makeHighlightedCode(str1, [])
        self.add(p0)
        self.wait(2.0)
        self.addHighlights(p0, [(RED, 4, (19, 27)), (RED, 5, (18, 26))])
        self.wait(2.0)
        self.addHighlights(p0, [(BLUE, 1, None), (BLUE, 2, None)])
        self.wait(2.0)
        self.addHighlights(p0, [(GREEN, 4, (2, 15)), (GREEN, 5, (2, 14)), (GREEN, 6, (9, 44))])
        self.wait(2.0)

        anim1 = ApplyMethod(p0.scale, 0.65)
        self.play(anim1, run_time=1.0)
        self.wait(1.0)

        p2 = makeHighlightedCode(str2, [])
        self.play(FadeIn(p2))
        self.wait(2.0)
        self.addHighlights(p2, [(BLUE, 1, None)])
        self.wait(2.0)
        self.addHighlights(p2, [(RED, 4, (18, 26)), (RED, 5, (17, 25))])
        # self.addHighlights(p2, [(RED, 3, (21, 29)), (RED, 4, (20, 28))])
        self.wait(2.0)
        # self.addHighlights(p2, [(GREEN, 3, (5, 18)), (GREEN, 4, (5, 17))])
        # self.addHighlights(p2, [(GREEN, 3, (5, 18)), (GREEN, 4, (5, 17))])
        self.addHighlights(p2, [(GREEN, 4, (2, 15)), (GREEN, 5, (2, 14)), (GREEN, 2, (21, 56))])
        # self.wait(2.0)

        anim1 = ApplyMethod(p2.scale, 0.65)
        self.play(anim1, run_time=1.0)
        self.wait(1.0)

        anim2 = ApplyMethod(p0.shift, LEFT * 3.55)
        anim3 = ApplyMethod(p2.shift, RIGHT * 3.55)
        self.play(AnimationGroup(anim2, anim3))
        self.wait(1.0)
        self.clearColor([p0, p2])

        self.addHighlights(p0, [(GREEN, 6, (9, 44))])
        self.addHighlights(p2, [(GREEN, 2, (21, 56))])
        self.wait(2.0)

        # Scale 1.5, Move Closer (Trial and Error)
        # Make like new just having the last green part highlighted.
        fadeAll(self, [p0, p2])
        self.wait(1.0)

class PythonInPlace(Scene):
    def construct(self):
        p0 = makeFullyHighlightedCode(str3, BLUE)
        self.play(FadeIn(p0))
        self.wait(2.0)

        anim1 = ApplyMethod(p0.scale, 0.7)
        anim2 = ApplyMethod(p0.shift, LEFT * 3.55 + UP * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(1.0)

        p1 = makeFullyHighlightedCode(str4, GREEN)
        self.play(FadeIn(p1))
        self.wait(2.0)

        anim1 = ApplyMethod(p1.scale, 0.7)
        anim2 = ApplyMethod(p1.shift, LEFT * 3.55 + DOWN * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(1.0)

        p2 = makeFullyHighlightedCode(str5, YELLOW)
        self.play(FadeIn(p2))
        self.wait(2.0)

        anim1 = ApplyMethod(p2.scale, 0.7)
        anim2 = ApplyMethod(p2.shift, RIGHT * 3.55 + UP * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(1.0)

        p3 = makeFullyHighlightedCode(str6, RED)
        self.play(FadeIn(p3))
        self.wait(2.0)

        anim1 = ApplyMethod(p3.scale, 0.7)
        anim2 = ApplyMethod(p3.shift, RIGHT * 3.55 + DOWN * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(2.0)
        fadeAll(self, [p0, p1, p2, p3])

class HaskellDS(Scene):
    def construct(self):
        str1 = "-- Data.Map\ninsert :: key -> value -> Map key value -> Map key value"
        str2 = "-- Data.Array\n(//) :: Array index elem -> [(index, elem)] -> Array index elem"
        p0 = makeHighlightedCode(str1, [(GRAY, 0, (0, 10)), (BLUE, 1, (0, 5)), (GREEN, 1, (26, 38)), (GREEN, 1, (43, 55))])
        p1 = makeHighlightedCode(str2, [(GRAY, 0, (0, 12)), (BLUE, 1, (0, 3)), (GREEN, 1, (8, 23)), (RED, 1, (47, 62))])
        self.play(FadeIn(p0))
        self.wait(2.0)
        self.play(FadeOut(p0))
        self.play(FadeIn(p1))
        self.wait(2.0)
        self.play(FadeOut(p1))
        self.wait(1.0)

marray_string = ("(MArray array elem m, Ix index) =>\n\n"
                 "readArray :: array index elem -> index -> m elem\n\n"
                 "writeArray :: array index elem -> index -> elem -> m ()\n\n"
                 "getBounds :: array index elem -> m (index, index)\n\n"
                 "getElems :: array index elem -> m [elem]\n\n"
                 "newListArray :: (index, index) -> [elem] -> m (array index elem)"
                )

class MArrayDisplay(Scene):
    def construct(self):
        str1 = "class (Monad m) => MArray array elem monad"
        p0 = makeHighlightedCode(str1, [(GREEN, 0, (26, 30)), (BLUE, 0, (32, 35)), (YELLOW, 0, (37, 41))])
        p1 = makeHighlightedCode(marray_string,
                [ (GREEN, 0, (8, 12)), (BLUE, 0, (14, 17)), (YELLOW, 0, (19, 19)), (RED, 0, (25, 29)),
                  # readArray
                  (GREEN, 2, (13, 17)), (RED, 2, (19, 23)), (BLUE, 2, (25, 28)), (RED, 2, (33, 37)), (YELLOW, 2, (42, 42)), (BLUE, 2, (44, 47)),
                  # writeArray
                  (GREEN, 4, (14, 18)), (RED, 4, (20, 24)), (BLUE, 4, (26, 29)), (RED, 4, (34, 38)), (BLUE, 4, (43, 46)), (YELLOW, 4, (51, 51)),
                  # getBounds
                  (GREEN, 6, (13, 17)), (RED, 6, (19, 23)), (BLUE, 6, (25, 28)), (YELLOW, 6, (33, 33)), (RED, 6, (36, 40)), (RED, 6, (43, 47)),
                  # getElems
                  (GREEN, 8, (12, 16)), (RED, 8, (18, 22)), (BLUE, 8, (24, 27)), (YELLOW, 8, (32, 32)), (BLUE, 8, (35, 38)),
                  # newListArray
                  (RED, 10, (17, 21)), (RED, 10, (24, 28)), (BLUE, 10, (35, 38)), (YELLOW, 10, (44, 44)), (GREEN, 10, (47, 51)), (RED, 10, (53, 57)), (BLUE, 10, (59, 62))
                ])
        self.play(FadeIn(p0))
        self.wait(2.0)
        self.play(FadeOut(p0))
        self.play(FadeIn(p1))
        self.wait(2.0)
        self.play(FadeOut(p1))
        self.wait(2.0)

# G5,11 R13,17 B19,22
ioArr1 = ("data IOArray index elem")

ioArr2 = ("data IOArray index elem\n\n"
          "instance MArray IOArray elem IO\n\n"
          "readArray :: IOArray index elem -> index -> IO elem\n\n" # +2, IO is 2
          "writeArray :: IOArray index elem -> index -> elem -> IO ()\n\n" # +2, IO is 2
          "getBounds :: IOArray index elem -> IO (index, index)\n\n" #+2, IO, +3
          "getElems :: IOArray index elem -> IO [elem]\n\n" #+2, IO, +3
          "newListArray :: (index, index) -> [elem] -> IO (IOArray index elem)" #IO, +3
          )

class SquareWithText(VGroup):
    def __init__(self, color, text):
        super().__init__()
        self.square = Square(side_length=1.0, color=color)
        self.square.set_stroke(color, opacity=1.0)
        self.text = Text(text, color=color)
        self.add(self.square, self.text)
        self.text.move_to(self.square.get_center())

    def setColor(self, color):
        return [ApplyMethod(self.square.set_stroke, color), ApplyMethod(self.text.set_fill, color)]

class IOArray(Scene):
    def construct(self):
        hl1 = [
            (GREEN, 0, (5, 11)), (RED, 0, (13, 17)), (BLUE, 0, (19, 22))
        ]
        p1 = makeHighlightedCode(ioArr1, hl1)

        hl2 = [
            (GREEN, 0, (5, 11)), (RED, 0, (13, 17)), (BLUE, 0, (19, 22)),
            (GREEN, 2, (16, 22)), (BLUE, 2, (24, 27)), (YELLOW, 2, (29, 30)),
            # readArray
            (GREEN, 4, (13, 19)), (RED, 4, (21, 25)), (BLUE, 4, (27, 30)), (RED, 4, (35, 39)), (YELLOW, 4, (44, 45)), (BLUE, 4, (47, 50)),
            # writeArray
            (GREEN, 6, (14, 20)), (RED, 6, (22, 26)), (BLUE, 6, (28, 31)), (RED, 6, (36, 40)), (BLUE, 6, (45, 48)), (YELLOW, 6, (53, 54)),
            # getBounds
            (GREEN, 8, (13, 19)), (RED, 8, (21, 25)), (BLUE, 8, (27, 30)), (YELLOW, 8, (35, 36)), (RED, 8, (39, 43)), (RED, 8, (46, 50)),
            # getElems
            (GREEN, 10, (12, 18)), (RED, 10, (20, 24)), (BLUE, 10, (26, 29)), (YELLOW, 10, (34, 35)), (BLUE, 10, (38, 41)),
            # newListArray
            (RED, 12, (17, 21)), (RED, 12, (24, 28)), (BLUE, 12, (35, 38)), (YELLOW, 12, (44, 45)), (GREEN, 12, (48, 54)), (RED, 12, (56, 60)), (BLUE, 12, (62, 65))
        ]
        p2 = makeHighlightedCode(ioArr2, hl2)

        self.play(FadeIn(p1))
        self.wait(2.0)
        self.play(Transform(p1, p2))
        self.wait(2.0)
        self.play(FadeOut(p1))
        # self.play(FadeOut(p2))

        cppStr = ("const int* ptr\n\nint* const ptr")
        p3 = makeHighlightedCode(cppStr, [])
        p3.shift(LEFT * 5)
        p4 = makeHighlightedCode(cppStr,[(YELLOW, 0, (0, 13))])
        p4.shift(LEFT * 5)
        p5 = makeHighlightedCode(cppStr,[(YELLOW, 2, (0, 13))])
        p5.shift(LEFT * 5)
        self.play(FadeIn(p3))
        self.wait(2.0)

        ptrObject = Code(code="ptr", insert_line_no=False, language="text")
        box1 = SquareWithText(WHITE, "2")
        box1.shift(RIGHT*3)
        box3 = SquareWithText(GREEN, "5")
        box3.shift(RIGHT*3)
        arrow1 = Arrow(start=RIGHT*0.5, end=RIGHT*2.5)
        arrow3 = Arrow(start=RIGHT*0.5, end=RIGHT*2.5)
        arrow4 = Arrow(start=RIGHT*0.5, end=RIGHT*2.5, color=RED)
        anims = AnimationGroup(FadeIn(ptrObject), FadeIn(box1), FadeIn(arrow1))
        self.play(anims)
        self.wait(2.0)

        anims = AnimationGroup(*([Transform(p3, p4)] + box1.setColor(RED)))
        self.play(anims)
        self.wait(2.0)

        box2 = SquareWithText(RED, "5")
        box2.shift(RIGHT*3 + DOWN * 2)
        arrow2 = Arrow(start=RIGHT*0.5, end=(RIGHT*2.5 + DOWN * 2), color=GREEN)
        anims = AnimationGroup(FadeIn(box2), Transform(arrow1, arrow2))
        self.play(anims)
        self.wait(2.0)

        anims = AnimationGroup(*([Transform(p4, p5), Transform(arrow2, arrow3), FadeOut(arrow1), FadeOut(box2)] + box1.setColor(WHITE)))
        self.play(anims)
        self.wait(2.0)

        self.play(Transform(arrow3, arrow4))
        self.wait(2.0)
        self.play(AnimationGroup(*(box1.setColor(GREEN))))
        self.wait(2.0)
        self.play(Transform(box1, box3))
        self.wait(2.0)

        # Replace Code on Side with arr :: IOArray, ptr with arr
        # Series of white boxes with numbers, arrow pointing to them
        # Two turn green, change numbers.
        p6 = makeHighlightedCode("ioArr :: IOArray Int Int", [])
        p6.shift(LEFT * 5)
        ptrObject2 = Code(code="ioArr", insert_line_no=False, language="text")
        arrow4 = Arrow(start=DOWN*0.25, end=DOWN*2.5, color=RED)
        arrayBox1 = SquareWithText(WHITE, "1")
        arrayBox1.shift(DOWN * 3 + LEFT * 1.5)
        arrayBox2 = SquareWithText(WHITE, "2")
        arrayBox2.shift(DOWN * 3 + LEFT * 0.5)
        arrayBox3 = SquareWithText(WHITE, "3")
        arrayBox3.shift(DOWN * 3 + RIGHT * 0.5)
        arrayBox4 = SquareWithText(WHITE, "4")
        arrayBox4.shift(DOWN * 3 + RIGHT * 1.5)
        anims = [
            FadeOut(box1),
            FadeOut(arrow2),
            FadeOut(arrow3),
            Transform(p5, p6),
            FadeOut(p4),
            FadeOut(p3),
            Transform(ptrObject, ptrObject2),
            FadeIn(arrow4),
            FadeIn(arrayBox1),
            FadeIn(arrayBox2),
            FadeIn(arrayBox3),
            FadeIn(arrayBox4),
        ]
        self.play(AnimationGroup(*anims))
        self.wait(2.0)

        arrayBox5 = SquareWithText(GREEN, "5")
        arrayBox5.shift(DOWN * 3 + LEFT * 0.5)
        arrayBox6 = SquareWithText(GREEN, "6")
        arrayBox6.shift(DOWN * 3 + RIGHT * 0.5)
        anims = AnimationGroup(Transform(arrayBox2, arrayBox5), Transform(arrayBox3, arrayBox6))
        self.play(anims)
        self.wait(2.0)

        fadeAll(self, [arrayBox1, arrayBox4, arrayBox3, arrayBox2, arrow4, ptrObject, p5])
        self.wait(2.0)

hsStr1 = ("swap :: IOArray Int Int -> Int -> Int -> IO ()\n"
          "swap arr i j = do\n"
          "  elem1 <- readArray arr i\n"
          "  elem2 <- readArray arr j\n"
          "  writeArray arr i elem2\n"
          "  writeArray arr j elem1\n"
         )

hsStr2 = ("partitionLoop :: IOArray Int Int -> Int -> Int -> StateT Int IO ()\n"
          "partitionLoop arr pivotElement i = do\n"
          "  pivotIndex <- get\n"
          "  currentElement <- lift $ readArray arr i\n"
          "  when (currentElement <= pivotElement) $ do\n"
          "  lift $ swap arr i pivotIndex\n"
          "  put (pivotIndex + 1)\n\n"
          "partition :: IOArray Int Int -> Int -> Int -> IO Int\n"
          "partition arr start end = do\n"
          "  pivotElement <- readArray arr start\n"
          "  let pivotIndex_0 = start + 1\n"
          "  finalPivotIndex <- execStateT\n"
          "    (mapM (partitionLoop arr pivotElement) [(start + 1)..(end - 1)])\n"
          "    pivotIndex_0\n"
          "  swap arr start (finalPivotIndex - 1)\n"
          "  return $ finalPivotIndex - 1\n"
         )

hsStr3 = ("quicksortHelper :: Int -> Int -> IOArray Int Int -> IO ()\n"
          "quicksortHelper start end arr = when (start + 1 < end) $ do\n"
          "  pivotIndex <- partition arr start end\n"
          "  quicksortHelper start pivotIndex arr\n"
          "  quicksortHelper (pivotIndex + 1) end arr\n"
         )

hsStr4 = ("quicksort :: IOArray Int Int -> IO ()\n"
          "quicksort arr = do\n"
          "  (minIndex, maxIndex) <- getBounds arr\n"
          "  quicksortHelper minIndex (maxIndex + 1) arr\n"
         )

class HaskellInPlace(Scene):
    def construct(self):
        p0 = makeFullyHighlightedCode(hsStr1, BLUE)
        self.play(FadeIn(p0))
        self.wait(2.0)

        anim1 = ApplyMethod(p0.scale, 0.7)
        anim2 = ApplyMethod(p0.shift, LEFT * 3.55 + UP * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(1.0)

        p1 = makeFullyHighlightedCode(hsStr2, GREEN)
        self.play(FadeIn(p1))
        self.wait(2.0)

        anim1 = ApplyMethod(p1.scale, 0.6)
        anim2 = ApplyMethod(p1.shift, LEFT * 3.45 + DOWN * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(1.0)

        p2 = makeFullyHighlightedCode(hsStr3, YELLOW)
        self.play(FadeIn(p2))
        self.wait(2.0)

        anim1 = ApplyMethod(p2.scale, 0.7)
        anim2 = ApplyMethod(p2.shift, RIGHT * 3.45 + UP * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(1.0)

        p3 = makeFullyHighlightedCode(hsStr4, RED)
        self.play(FadeIn(p3))
        self.wait(2.0)

        anim1 = ApplyMethod(p3.scale, 0.7)
        anim2 = ApplyMethod(p3.shift, RIGHT * 3.55 + DOWN * 2)
        self.play(anim1)
        self.play(anim2)
        self.wait(2.0)

        fadeAll(self, [p0, p1, p2, p3])

typeSigs1 = ("swap :: IOArray Int Int -> Int -> Int -> IO ()\n\n" 
             "partitionLoop :: IOArray Int Int -> Int -> Int -> StateT Int IO ()\n\n" 
             "partition :: IOArray Int Int -> Int -> Int -> IO Int\n\n" 
             "quicksortHelper :: Int -> Int -> IOArray Int Int -> IO ()\n\n" 
             "quicksort :: IOArray Int Int -> IO ()\n"
            )

typeSigs2 = ("swap :: (Num i, Ix i, MArray a e m) => a i e -> i -> i -> m ()\n\n" 
             "partitionLoop :: (Ord e, Num i, Ix i, MArray a e m) => a i e -> e -> i -> StateT i m ()\n\n"
             "partition :: (Ord e, Enum i, Num i, Ix i, MArray a e m) => a i e -> i -> i -> m i\n\n"
             "quicksortHelper :: (Ord e, Enum i, Num i, Ix i, MArray a e m) => i -> i -> a i e -> m ()\n\n"
             "quicksort :: (Ord e, Enum i, Num i, Ix i, MArray a e m) => a i e -> m ()\n"
            )

typeSigs3 = ("swap :: (Num index, Ix index, MArray array elem monad)\n"
             "  => array index elem -> index -> index -> monad ()\n\n"
             "partitionLoop :: (Ord elem, Num index, Ix index, MArray array elem monad)\n"
             "  => array index elem -> elem -> index -> StateT index monad ()\n\n"
             "partition :: (Ord elem, Enum index, Num index, Ix index, MArray array elem monad)\n"
             "  => array index elem -> index -> index -> monad index\n\n"
             "quicksortHelper :: (Ord elem, Enum index, Num index, Ix index, MArray array elem monad)\n"
             "  => index -> index -> array index elem -> monad ()\n\n"
             "quicksort :: (Ord elem, Enum index, Num index, Ix index, MArray array elem monad)\n"
             "  => array index elem -> monad ()\n"
             )

# Array = Green
# Index = Red
# Elem = Blue
# Monad = Yellow
class MArrayTypeSigs(Scene):
    def construct(self):
        # G9,15 R17,19 B21,23 R28,30 R35,37 Y42,43
        #G17,23 R25,27 B29,31 R36,38 B43,45, R57,59 Y61,63
        #G13,19 R21,23 B25,27 R32,34, R39,41 Y46,47, R49,51
        #R19,21 R26,28 G34,40 R42,44 B46,48 Y54,55
        #G13,19 R21,23 B25,27 Y33,34
        hl1 = [
            (GREEN, 0, (8, 14)), (RED, 0, (16, 18)), (BLUE, 0, (20, 22)), (RED, 0, (27, 29)), (RED, 0, (34, 36)), (YELLOW, 0, (41, 42)),
            (GREEN, 2, (17, 23)), (RED, 2, (25, 27)), (BLUE, 2, (29, 31)), (RED, 2, (36, 38)), (BLUE, 2, (43, 45)), (RED, 2, (57, 59)), (YELLOW, 2, (61, 63)),
            (GREEN, 4, (13, 19)), (RED, 4, (21, 23)), (BLUE, 4, (25, 27)), (RED, 4, (32, 34)), (RED, 4, (39, 41)), (YELLOW, 4, (46, 47)), (RED, 4, (49, 51)),
            (RED, 6, (19, 21)), (RED, 6, (26, 28)), (GREEN, 6, (33, 39)), (RED, 6, (41, 43)), (BLUE, 6, (45, 47)), (YELLOW, 6, (52, 53)),
            (GREEN, 8, (13, 19)), (RED, 8, (21, 23)), (BLUE, 8, (25, 27)), (YELLOW, 8, (32, 33))
        ]
        p0 = makeHighlightedCode(typeSigs1, hl1)
        self.play(FadeIn(p0))
        self.wait(2.0)

        #R13,13 R19,19, G29,29 R31,31 Y33,33 G39,39 R41,41 B43,43 R48,48 R53,53
        # B22 R29 R35 G45 B47 Y49 G55 R57 B59 B64 R69 R81 Y83
        # B18 R26 R33 R39 G49 B51 Y53 G59 R61 B63 R68 R73 Y78 R80
        # B24 R32 R39 R45 G55 B57 Y59 R65 R70 G75 R77 B79 Y84
        # B19 R26 R34 R40 G49 B51 Y53 G59 R61 B63 Y68
        hl2 = [
            (RED, 0, (13, 13)), (RED, 0, (19, 19)), (GREEN, 0, (29, 29)), (RED, 0, (31, 31)), (YELLOW, 0, (33, 33)), (GREEN, 0, (39, 39)), (RED, 0, (41, 41)), (BLUE, 0, (43, 43)), (RED, 0, (48, 48)), (RED, 0, (53, 53)), (YELLOW, 0, (58, 58)),
            (BLUE, 2, (22, 22)), (RED, 2, (29, 29)), (RED, 2, (35, 35)), (GREEN, 2, (45, 45)), (BLUE, 2, (47, 47)), (YELLOW, 2, (49, 49)), (GREEN, 2, (55, 55)), (RED, 2, (57, 57)), (BLUE, 2, (59, 59)), (BLUE, 2, (64, 64)), (RED, 2, (69, 69)), (RED, 2, (81, 81)), (YELLOW, 2, (83, 83)),
            (BLUE, 4, (18, 18)), (RED, 4, (26, 26)), (RED, 4, (33, 33)), (RED, 4, (39, 39)), (GREEN, 4, (49, 49)), (BLUE, 4, (51, 51)), (YELLOW, 4, (53, 53)), (GREEN, 4, (59, 59)), (RED, 4, (61, 61)), (BLUE, 4, (63, 63)), (RED, 4, (68, 68)), (RED, 4, (73, 73)), (YELLOW, 4, (78, 78)), (RED, 4, (80, 80)),
            (BLUE, 6, (24, 24)), (RED, 6, (32, 32)), (RED, 6, (39, 39)), (RED, 6, (45, 45)), (GREEN, 6, (55, 55)), (BLUE, 6, (57, 57)), (YELLOW, 6, (59, 59)), (RED, 6, (65, 65)), (RED, 6, (70, 70)), (GREEN, 6, (75, 75)), (RED, 6, (77, 77)), (BLUE, 6, (79, 79)), (YELLOW, 6, (84, 84)),
            (BLUE, 8, (18, 18)), (RED, 8, (26, 26)), (RED, 8, (33, 33)), (RED, 8, (39, 39)), (GREEN, 8, (49, 49)), (BLUE, 8, (51, 51)), (YELLOW, 8, (53, 53)), (GREEN, 8, (59, 59)), (RED, 8, (61, 61)), (BLUE, 8, (63, 63)), (YELLOW, 8, (68, 68)),
        ]
        p1 = makeHighlightedCode(typeSigs2, hl2)
        p1.scale(0.95)
        self.play(Transform(p0, p1))
        self.wait(2.0)

        # R13,17 R23,27, G37,41 B43,46 Y48,52
        # G4,8 R10,14 B16,19 R24,28, R33,37, Y42,46
        # B22,25 R32,36 R42,46, G56,60 B62,65 Y67,71
        # G4,8 R10,14 B16,19 B24,27 R32,36 R48,52 Y54,58
        # B18,21 R29,33 R40,44 R50,54, G64,68 B70,73 Y75,79
        # G4,8 R10,14 B16,19 R24,28 R33,37 Y42,46 R48,52
        # B24,27 R35,39 R46,50, R56,60 G70,74 B76,79 Y81,85
        # R4,8 R13,17 G22,26 R28,32 B34,37 Y42,46
        # B18,21 R29,33 R40,44 R50,54 G64,68 B70,73 Y75,79
        # G4,8 R10,14 B16,19 Y24,28
        hl3 = [
            (RED, 0, (13, 17)), (RED, 0, (23, 27)), (GREEN, 0, (37, 41)), (BLUE, 0, (43, 46)), (YELLOW, 0, (48, 52)),
            (GREEN, 1, (4, 8)), (RED, 1, (10, 14)), (BLUE, 1, (16, 19)), (RED, 1, (24, 28)), (RED, 1, (33,37)), (YELLOW, 1, (42, 46)),
            (BLUE, 3, (22, 25)), (RED, 3, (32, 36)), (RED, 3, (42, 46)), (GREEN, 3, (56, 60)), (BLUE, 3, (62, 65)), (YELLOW, 3, (67, 71)),
            (GREEN, 4, (4, 8)), (RED, 4, (10, 14)), (BLUE, 4, (16, 19)), (BLUE, 4, (24, 27)), (RED, 4, (32, 36)), (RED, 4, (48, 52)), (YELLOW, 4, (54, 58)),
            (BLUE, 6, (18, 21)), (RED, 6, (29, 33)), (RED, 6, (40, 44)), (RED, 6, (50, 54)), (GREEN, 6, (64, 68)), (BLUE, 6, (70, 73)), (YELLOW, 6, (75, 79)),
            (GREEN, 7, (4, 8)), (RED, 7, (10, 14)), (BLUE, 7, (16, 19)), (RED, 7, (24, 28)), (RED, 7, (33, 37)), (YELLOW, 7, (42, 46)), (RED, 7, (48, 52)),
            (BLUE, 9, (24, 27)), (RED, 9, (35, 39)), (RED, 9, (46, 50)), (RED, 9, (56, 60)), (GREEN, 9, (70, 74)), (BLUE, 9, (76, 79)), (YELLOW, 9, (81, 85)),
            (RED, 10, (4, 8)), (RED, 10, (13, 17)), (GREEN, 10, (22, 26)), (RED, 10, (28, 32)), (BLUE, 10, (34, 37)), (YELLOW, 10, (42, 46)),
            (BLUE, 12, (18, 21)), (RED, 12, (29, 33)), (RED, 12, (40, 44)), (RED, 12, (50, 54)), (GREEN, 12, (64, 68)), (BLUE, 12, (70, 73)), (YELLOW, 12, (75, 79)),
            (GREEN, 13, (4, 8)), (RED, 13, (10, 14)), (BLUE, 13, (16, 19)), (YELLOW, 13, (24, 28))
        ]
        p2 = makeHighlightedCode(typeSigs3, hl3)
        p2.scale(0.92)
        self.play(Transform(p1, p2))
        self.wait(2.0)
        self.play(FadeOut(p0))
        self.play(FadeOut(p1))
        self.wait(1.0)


stStr1 = "data STArray st index elem"

stStr2 = ("instance MArray (STArray st) elem (ST st)\n\n"
          "thaw :: Array index elem -> ST st (STArray st index elem)"
         )

stStr3 = ("quicksort :: STArray st index elem -> ST st ()"
         )

stStr4 = ("quicksortPure :: (Ord elem, Enum index, Num index, Ix index)\n"
          "  => Array index elem -> Array index elem\n"
          "quicksortPure input = runSTArray $ do\n"
          "  arr <- thaw input\n"
          "  quicksort arr\n"
          "  return arr\n"
          )

class STArray(Scene):
    def construct(self):
        # G5,11 O13,14 R16,20 B22,25
        h1 = [
            (GREEN, 0, (5, 11)), (ORANGE, 0, (13, 14)), (RED, 0, (16, 20)), (BLUE, 0, (22, 25))
        ]
        p1 = makeHighlightedCode(stStr1, h1)
        p1.shift(UP * 2)

        # G17,23 O25,26 B29,33 Y35,36 O38,39
        # G8,12 R14,18 B20,23, Y28,29 O31,32 G35,41 O43,44 R46,50 B52,55
        h2 = [
            (GREEN, 0, (17, 23)), (ORANGE, 0, (25, 26)), (BLUE, 0, (29, 33)), (YELLOW, 0, (35, 36)), (ORANGE, 0, (38, 39)),
            (GREEN, 2, (8, 12)), (RED, 2, (14, 18)), (BLUE, 2, (20, 23)), (YELLOW, 2, (28, 29)), (ORANGE, 2, (31, 32)), (GREEN, 2, (35, 41)),
            (ORANGE, 2, (43, 44)), (RED, 2, (46, 50)), (BLUE, 2, (52, 55))
        ]
        p2 = makeHighlightedCode(stStr2, h2)

        # G13,19 O21,22 R24,28 B30,33 Y38,39 O41,42
        h3 = [
            (GREEN, 0, (13, 19)), (ORANGE, 0, (21, 22)), (RED, 0, (24, 28)), (BLUE, 0, (30, 33)), (YELLOW, 0, (38, 39)), (ORANGE, 0, (41, 42)),
        ]
        p3 = makeHighlightedCode(stStr3, h3)
        p3.shift(DOWN * 2)

        # B22,25 R33,37 R44,48 R54,58
        # G4,8 R10,14 B16,19 G24,29 R31,35 B37,40
        # O22,31
        #
        # P1,9
        #
        h4 = [
            (BLUE, 0, (22, 25)), (RED, 0, (33, 37)), (RED, 0, (44, 48)), (RED, 0, (54, 58)),
            (GREEN, 1, (4, 8)), (RED, 1, (10, 14)), (BLUE, 1, (16, 19)), (GREEN, 1, (24, 28)), (RED, 1, (30, 34)), (BLUE, 1, (36, 39)),
            (ORANGE, 2, (22, 31)),
            (ORANGE, 3, (8, 11)),
            (PURPLE, 4, (1, 9)),
        ]
        p4 = makeHighlightedCode(stStr4, h4)

        self.play(FadeIn(p1))
        self.wait(2.0)
        self.play(FadeIn(p2))
        self.wait(2.0)
        self.play(FadeIn(p3))
        self.wait(2.0)

        anims = AnimationGroup(FadeOut(p1), FadeOut(p2), FadeOut(p3))
        self.play(anims)
        self.play(FadeIn(p4))
        self.wait(2.0)
        self.play(FadeOut(p4))
        self.wait(1.0)

pyRand = ("def partition(arr, start, end):\n"
          "  indexForPivotElement = random.randint(start, end - 1)\n"
          "  swap(arr, start, indexForPivotElement)\n"
          "  pivotElement = arr[start]\n"
          "  pivotIndex = start + 1\n"
          "  for i in range(start + 1, end):\n"
          "    if arr[i] <= pivotElement:\n"
          "       swap(arr, i, pivotIndex)\n"
          "       pivotIndex += 1\n"
          "  swap(arr, start, pivotIndex - 1)\n"
          "  return pivotIndex - 1\n"
          )

hsRand1Early = ("partition :: IOArray Int Int -> Int -> Int -> IO Int\n"
                "partition arr start end = do\n"
                "  pivotElement <- readArray arr start\n"
                "  let pivotIndex_0 = start + 1\n"
                "  finalPivotIndex <- execStateT\n"
                "    (mapM (partitionLoop arr pivotElement) [(start + 1)..(end - 1)])\n"
                "    pivotIndex_0\n"
                "  swap arr start (finalPivotIndex - 1)\n"
                "  return $ finalPivotIndex - 1"
                )

hsRand1 = ( "partition :: IOArray Int Int -> Int -> Int -> StdGen -> IO (Int, StdGen)\n"
            "partition arr start end gen1 = do\n"
            "  let (indexForPivotElement, gen2) = randomR (start, end - 1) gen1\n"
            "  swap arr start indexForPivotElement\n"
            "  pivotElement <- readArray arr start\n"
            "  let pivotIndex_0 = start + 1\n"
            "  finalPivotIndex <- execStateT\n"
            "    (mapM (partitionLoop arr pivotElement) [(start + 1)..(end - 1)])\n"
            "    pivotIndex_0\n"
            "  swap arr start (finalPivotIndex - 1)\n"
            "  return $ (finalPivotIndex - 1, gen2)"
           )

hsRand2 = ("quicksortHelper :: Int -> Int -> IOArray Int Int -> StdGen -> IO StdGen\n"
           "quicksortHelper start end arr gen1 = if start + 1 >= end then return gen1\n"
           "  else do\n"
           "    (pivotIndex, gen2) <- partition arr start end gen1\n"
           "    gen3 <- quicksortHelper start pivotIndex arr gen2\n"
           "    quicksortHelper (pivotIndex + 1) end arr gen3"
           )

hsRand3 = ("quicksort :: => IOArray Int Int -> StdGen -> IO ()\n"
           "quicksort arr gen = do\n"
           "  (minIndex, maxIndex) <- getBounds arr\n"
           "  void $ quicksortHelper minIndex (maxIndex + 1) arr gen"
           )

class Random(Scene):
    def construct(self):
        light_green = "#03fc0b"
        p1 = makeHighlightedCode(str4, [])
        pyHighlights = [(light_green, 1, (1, 53)), (light_green, 2, (1, 38))]
        p2 = makeHighlightedCode(pyRand, pyHighlights)
        self.play(FadeIn(p1))
        self.wait(2.0)
        self.play(Transform(p1, p2))
        self.wait(2.0)

        self.play(FadeOut(p1))
        # self.play(FadeOut(p2))

        p3 = makeHighlightedCode(hsRand1Early, [])
        p3.scale(0.7)
        p3.shift(UP * 2)
        # 46,51 65,70
        # 24,27
        # 1,64
        # 1,35
        # (10) 32,35
        p3_ = makeHighlightedCode(hsRand1, [
            (light_green, 0, (46, 51)), (light_green, 0, (65, 70)),
            (light_green, 1, (24, 27)),
            (light_green, 2, (1, 64)),
            (light_green, 3, (1, 35)),
            (light_green, 10, (32, 35)),
        ])
        p3_.scale(0.7)
        p3_.shift(UP * 2)

        # 52,57 65,70
        # 31,34 70,73
        #
        # 14,17 47,50
        # 1,4 46,49
        # 42,45
        p4 = makeHighlightedCode(hsStr3, [])
        p4_ = makeHighlightedCode(hsRand2, [
            (light_green, 0, (52, 57)), (light_green, 0, (65, 70)),
            (light_green, 1, (30, 33)), (light_green, 1, (69, 72)),
            (light_green, 3, (15, 18)), (light_green, 3, (48, 51)),
            (light_green, 4, (2, 5)), (light_green, 4, (47, 50)),
            (light_green, 5, (43, 46)),
        ])
        p4.scale(0.7)
        p4.shift(DOWN)
        p4_.scale(0.7)
        p4_.shift(DOWN)

        p5 = makeHighlightedCode(hsStr4, [])
        p5.scale(0.7)
        p5.shift(DOWN * 3)
        # G78,86
        # G14,16
        # G34,36
        p5_ = makeHighlightedCode(hsRand3, [(light_green, 0, (35, 40)), (light_green, 1, (14, 16)), (light_green, 3, (52, 54))])
        p5_.scale(0.7)
        p5_.shift(DOWN * 3)

        anims = AnimationGroup(FadeIn(p3), FadeIn(p4), FadeIn(p5))
        self.play(anims)
        self.wait(2.0)

        anims = AnimationGroup(Transform(p3, p3_), Transform(p4, p4_), Transform(p5, p5_))
        self.play(anims)
        self.wait(2.0)

        fadeAll(self, [p3, p4, p5])
        self.wait(1.0)

