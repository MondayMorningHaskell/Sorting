from manim import *

class CircleWithText(VGroup):
    def __init__(self, color, radius, text):
        super().__init__()
        self.circle = Circle(radius=radius, color=color)
        self.circle.set_fill(color, opacity=1.0)
        self.text = Text(text)
        self.add(self.circle, self.text)
        self.text.move_to(self.circle.get_center())

def mkPivotArrow(color, scale=0.5, extra_rotate=0.0):
    t = Triangle(color=color)
    t.set_fill(color, opacity=1.0)
    t.rotate(PI + extra_rotate)
    t.scale(scale)
    return t

class QuicksortInPlace(Scene):

    SEPARATION = 1.25

    def xForIndex(self, i):
        return (i - 5) * self.SEPARATION

    def basePosForIndex(self, i):
        return [self.xForIndex(i), 0.0, 0.0]

    def highPosForIndex(self, i):
        return [self.xForIndex(i), self.SEPARATION, 0.0]

    def doubleHighPosForIndex(self, i):
        return [self.xForIndex(i), 2 * self.SEPARATION, 0.0]

    def lowPosForIndex(self, i):
        return [self.xForIndex(i), -self.SEPARATION, 0.0]

    def addPivot(self, color, loc, run_time=0.25, extra_rotate=0.0):
        p = mkPivotArrow(color, scale=0.35, extra_rotate=extra_rotate)
        p.move_to(loc)
        return p

    def addGreenPivot(self, idx, run_time=0.25):
        p = self.addPivot(GREEN, self.doubleHighPosForIndex(idx), run_time=run_time)
        self.play(FadeIn(p), run_time=run_time)
        self.green_pivots.append(p)

    def removeGreenPivots(self, run_time=0.25):
        animations = []
        for p in self.green_pivots:
            animations.append(FadeOut(p))
        self.play(AnimationGroup(*animations), run_time=run_time)
        self.green_pivots = []

    def addYellowPivot(self, idx, run_time=0.25):
        p = self.addPivot(YELLOW, self.doubleHighPosForIndex(idx), run_time=run_time)
        self.play(FadeIn(p), run_time=run_time)
        self.yellow_pivot = p

    def addOrangePivot(self, idx, run_time=0.25):
        p = self.addPivot(ORANGE, self.doubleHighPosForIndex(idx), run_time=run_time)
        self.orange_pivot = p
        return p

    def addWhitePivot(self, idx, run_time=0.25):
        p = self.addPivot(WHITE, self.lowPosForIndex(idx), run_time=run_time, extra_rotate=PI)
        self.white_pivot = p
        return p

    def addOWPivots(self, orange_index, white_index, run_time=0.25):
        op = self.addOrangePivot(orange_index, run_time)
        wp = self.addWhitePivot(white_index, run_time)
        self.play(AnimationGroup(FadeIn(op), FadeIn(wp)), run_time=run_time)

    def boundForIndex(self, idx):
        xPos = self.xForIndex(idx) - 0.5 * self.SEPARATION
        start = [xPos, self.SEPARATION, 0.0]
        end = [xPos, -self.SEPARATION, 0.0]
        return Line(start, end, color=RED)

    def addBounds(self, start, end, run_time=0.25):
        b1 = self.boundForIndex(start)
        b2 = self.boundForIndex(end)
        self.play(AnimationGroup(FadeIn(b1), FadeIn(b2)), run_time=run_time)
        return (b1, b2)

    def removeBounds(self, bound1, bound2, run_time=0.25):
        self.play(AnimationGroup(FadeOut(bound1), FadeOut(bound2)), run_time=run_time)

    def moveOrangePivot(self, idx, run_time=0.25):
        if self.orange_pivot:
            self.play(ApplyMethod(self.orange_pivot.move_to, self.doubleHighPosForIndex(idx)), run_time=run_time)

    def moveWhitePivot(self, idx, run_time=0.25):
        if self.white_pivot:
            self.play(ApplyMethod(self.white_pivot.move_to, self.lowPosForIndex(idx)), run_time=run_time)

    def removePivots(self, run_time=0.25):
        animations = []
        if self.white_pivot:
            animations.append(FadeOut(self.white_pivot))
            self.white_pivot = None
        if self.orange_pivot:
            animations.append(FadeOut(self.orange_pivot))
            self.orange_pivot = None
        if self.yellow_pivot:
            animations.append(FadeOut(self.yellow_pivot))
            self.yellow_pivot = None
        self.play(AnimationGroup(*animations), run_time=run_time)

    def swapCircles(self, tuple1, tuple2, run_time=1.0):
        i1, dstIndex1 = tuple1
        circle1 = self.circles[i1 - 1]
        up1 = ApplyMethod(circle1.shift, self.SEPARATION * UP)
        over1 = ApplyMethod(circle1.move_to, self.highPosForIndex(dstIndex1))
        down1 = ApplyMethod(circle1.shift, self.SEPARATION * DOWN)
        i2, dstIndex2 = tuple2
        circle2 = self.circles[i2 - 1]
        up2 = ApplyMethod(circle2.shift, self.SEPARATION * UP)
        over2 = ApplyMethod(circle2.move_to, self.highPosForIndex(dstIndex2))
        down2 = ApplyMethod(circle2.shift, self.SEPARATION * DOWN)

        rt_each = run_time / 3.0
        self.play(AnimationGroup(up1, up2), run_time=rt_each)
        self.play(AnimationGroup(over1, over2), run_time=rt_each)
        self.play(AnimationGroup(down1, down2), run_time=rt_each)

    def run_iteration(self, start, end, moves, endPivot, finalMove):
        self.addYellowPivot(start)
        wp = start + 1
        op = start + 1
        self.addOWPivots(op, wp)
        self.wait(1.0)

        for mv in moves:
            if mv is not None:
                op += 1
                self.swapCircles(mv[0], mv[1])
                self.moveOrangePivot(op)
                self.wait(1.0)
            wp += 1
            if wp < end:
                self.moveWhitePivot(wp)
                self.wait(1.0)

        self.swapCircles(*finalMove)
        self.wait(1.0)
        self.removePivots()
        self.addGreenPivot(endPivot)

    def iteration1(self):
        moves = [
            None,
            ((1,1), (8, 2)),
            ((5,2), (8, 3)),
            ((4,3), (8, 4)),
            None,
            None,
            None,
            ((2,4), (8, 8)),
            ((3,5), (7, 9)),
            None,
        ]
        self.run_iteration(0, 11, moves, 5, ((6, 5), (3, 0)))

    def iteration2(self):
        moves = [
            ((1, 1), (1, 1)),
            None,
            None,
            ((2, 2), (5, 4))
        ]
        self.run_iteration(0, 5, moves, 2, ((3, 2), (2, 0)))

    def iteration3(self):
        moves = [
            ((1, 1), (1, 1)),
        ]
        self.run_iteration(0, 2, moves, 0, ((1, 0), (2, 1)))

    def iteration4(self):
        self.addGreenPivot(1)

    def iteration5(self):
        moves = [
            None,
        ]
        self.run_iteration(3, 5, moves, 3, ((4, 3), (5, 4)))

    def iteration6(self):
        self.addGreenPivot(4)

    def iteration7(self):
        moves = [
            ((10, 7), (10, 7)),
            ((8, 8), (8, 8)),
            ((7, 9), (7, 9)),
            ((9, 10), (9, 10))
        ]
        self.run_iteration(6, 11, moves, 10, ((11, 10), (9, 6)))

    def iteration8(self):
        moves = [
            None,
            ((8, 7), (10, 8)),
            ((7, 8), (10, 9)),
        ]
        self.run_iteration(6, 10, moves, 8, ((9, 8), (7, 6)))

    def iteration9(self):
        moves = [
            None,
        ]
        self.run_iteration(6, 8, moves, 6, ((7, 6), (8, 7)))

    def iteration10(self):
        self.addGreenPivot(7)

    def iteration11(self):
        self.addGreenPivot(9)

    def construct(self):
        colors = ["#fc0303", "#fc034e", "#fc0373", "#fc0390", "#fc03b6", "#fc03d7", "#fc03fc", "#c603fc", "#9003fc", "#6203fc", "#030ffc"]
        locs = [self.basePosForIndex(i) for i in range(0, 12)]
        starting_locations = [locs[2], locs[8], locs[9], locs[4], locs[3], locs[0], locs[5], locs[1], locs[10], locs[7], locs[6]]
        self.circles = []
        self.yellow_pivot = None
        self.orange_pivot = None
        self.white_pivot = None
        self.green_pivots = []

        # Display circles
        for (i, (color, loc)) in enumerate(zip(colors, starting_locations)):
            c = CircleWithText(radius=0.5, color=color, text=str(i + 1))
            c.move_to(loc)
            self.circles.append(c)
            self.add(c)
        self.wait(1.0)

        self.iteration1()
        b1, b2 = self.addBounds(0, 11)
        self.wait(2.0)
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(0, 5)
        self.iteration2()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(0, 2)
        self.iteration3()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(1, 2)
        self.iteration4()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(3, 5)
        self.iteration5()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(4, 5)
        self.iteration6()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(6, 11)
        self.iteration7()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(6, 10)
        self.iteration8()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(6, 8)
        self.iteration9()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(7, 8)
        self.iteration10()
        self.removeBounds(b1, b2)

        b1, b2 = self.addBounds(9, 10)
        self.iteration11()
        self.removeBounds(b1, b2)
        self.wait(2.0)
        self.removeGreenPivots()
