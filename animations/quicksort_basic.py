from manim import *

class CircleWithText(VGroup):
    def __init__(self, color, radius, text):
        super().__init__()
        self.circle = Circle(radius=radius, color=color)
        self.circle.set_fill(color, opacity=1.0)
        self.text = Text(text)
        self.add(self.circle, self.text)
        self.text.move_to(self.circle.get_center())

def mkPivotArrow(color):
    t = Triangle(color=color)
    t.set_fill(color, opacity=1.0)
    t.rotate(PI)
    t.scale(0.5)
    return t

class QuicksortBasic(Scene):
    SEPARATION = 1.25
    def xForIndex(self, i):
        return (i - 5) * self.SEPARATION

    def basePosForIndex(self, i):
        return [self.xForIndex(i), 0.0, 0.0]

    def lowPosForIndex(self, i):
        return [self.xForIndex(i), -self.SEPARATION, 0.0]

    def highPosForIndex(self, i):
        return [self.xForIndex(i), self.SEPARATION, 0.0]

    def moveCirclesDown(self, moves, run_time=1.0):
        animations = []
        for (ball_label, index) in moves:
            animations.append(ApplyMethod(self.circles[ball_label - 1].move_to, self.lowPosForIndex(index)))
        self.play(AnimationGroup(*animations), run_time=run_time)

    def moveAllCirclesUp(self, run_time=1.0, excepts=()):
        animations = []
        for (i, c) in enumerate(self.circles):
            if i not in excepts:
                animations.append(ApplyMethod(c.shift, UP * self.SEPARATION))
        self.play(AnimationGroup(*animations), run_time=run_time)

    def addGreenPivots(self, idxs, run_time=0.25):
        animations = []
        for i in idxs:
            p = mkPivotArrow(GREEN)
            p.move_to(self.highPosForIndex(i))
            animations.append(FadeIn(p))
            self.green_pivots.append(p)
        self.play(AnimationGroup(*animations), run_time=run_time)

    def removeGreenPivots(self, run_time=0.25):
        animations = []
        for p in self.green_pivots:
            animations.append(FadeOut(p))
        self.play(AnimationGroup(*animations), run_time=run_time)
        self.green_pivots = []

    def addYellowPivots(self, idxs, run_time=0.25):
        for p in self.pivots:
            self.remove(p)
        self.pivots = []
        animations = []
        for i in idxs:
            p = mkPivotArrow(YELLOW)
            p.move_to(self.highPosForIndex(i))
            animations.append(FadeIn(p))
            self.pivots.append(p)
        self.play(AnimationGroup(*animations), run_time=run_time)

    def removeYellowPivots(self, run_time=0.25):
        animations = []
        for p in self.pivots:
            animations.append(FadeOut(p))
        self.play(AnimationGroup(*animations), run_time=run_time)
        self.pivots = []

    def construct(self):
        colors = ["#fc0303", "#fc034e", "#fc0373", "#fc0390", "#fc03b6", "#fc03d7", "#fc03fc", "#c603fc", "#9003fc", "#6203fc", "#030ffc"]
        locs = [self.basePosForIndex(i) for i in range(0, 12)]
        starting_locations = [locs[2], locs[8], locs[9], locs[4], locs[3], locs[0], locs[5], locs[1], locs[10], locs[7], locs[6]]
        self.circles = []
        self.pivots = []
        self.green_pivots = []

        # Display circles
        for (i, (color, loc)) in enumerate(zip(colors, starting_locations)):
            c = CircleWithText(radius=0.5, color=color, text=str(i + 1))
            c.move_to(loc)
            self.circles.append(c)
            self.add(c)
        self.wait(1.0)

        # Arrow over first pivot
        self.addYellowPivots([0])
        self.wait(1.0)

        # Move left balls
        left_moves = [(1, 0), (5, 1), (4, 2), (2, 3), (3, 4)]
        self.moveCirclesDown(left_moves)
        self.wait(1.0)
        # Move right balls
        right_moves = [(8, 6), (7, 7), (11, 8), (10, 9), (9, 10)]
        self.moveCirclesDown(right_moves)
        self.wait(1.0)
        # Move pivot
        pivot_move = [(6, 5)]
        self.removeYellowPivots()
        self.moveCirclesDown(pivot_move)
        self.wait(1.0)

        self.moveAllCirclesUp()
        self.wait(1.0)
        self.addGreenPivots([5])
        self.wait(1.0)

        self.addYellowPivots([0, 6])
        self.wait(1.0)

        moves = [(5, 1), (4, 2), (2, 3), (3, 4), (7, 6), (11, 8), (10, 9), (9, 10)]
        self.moveCirclesDown(moves)
        self.wait(1.0)

        pivot_moves = [(1, 0), (8, 7)]
        self.removeYellowPivots()
        self.moveCirclesDown(pivot_moves)
        self.wait(1.0)
        self.moveAllCirclesUp(excepts={5})
        self.addGreenPivots([0, 7])
        self.wait(1.0)

        self.addYellowPivots([1, 6, 8])
        moves = [(4, 1), (2, 2), (3, 3), (10, 8), (9, 9)]
        self.moveCirclesDown(moves)
        self.wait(1.0)

        pivot_moves = [(5, 4), (7, 6), (11, 10)]
        self.removeYellowPivots()
        self.moveCirclesDown(pivot_moves)
        self.wait(1.0)
        self.moveAllCirclesUp(excepts={0, 5, 7})
        self.addGreenPivots([4, 6, 10])
        self.wait(1.0)

        self.addYellowPivots([1, 8])
        moves = [(2, 1), (3, 2), (9, 8)]
        self.moveCirclesDown(moves)
        self.wait(1.0)

        pivot_moves = [(4, 3), (10, 9)]
        self.removeYellowPivots()
        self.moveCirclesDown(pivot_moves)
        self.wait(1.0)
        self.moveAllCirclesUp(excepts={0, 4, 5, 6, 7, 10})
        self.addGreenPivots([3, 9])
        self.wait(1.0)

        self.addYellowPivots([1, 8])
        moves = [(3, 2)]
        self.moveCirclesDown(moves)
        self.wait(1.0)

        pivot_moves = [(2, 1), (9, 8)]
        self.removeYellowPivots()
        self.moveCirclesDown(pivot_moves)
        self.wait(1.0)
        self.moveAllCirclesUp(excepts={0, 3, 4, 5, 6, 7, 9, 10})
        self.addGreenPivots([1, 8])
        self.wait(1.0)

        self.addYellowPivots([2])
        self.wait(1.0)
        self.removeYellowPivots()
        self.moveCirclesDown([(3, 2)])
        self.wait(1.0)
        self.moveAllCirclesUp(excepts={0, 1, 3, 4, 5, 6, 7, 8, 9, 10})
        self.addGreenPivots([2])
        self.wait(1.0)
        self.removeGreenPivots()
        self.wait(1.0)
