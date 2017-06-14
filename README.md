<img src="https://github.com/kivy-garden/garden.moretransitions/blob/master/screenshot.png" align="right" width="256" />



garden.moretransitions
======================

More transitions for the Kivy screen manager

Usage:

Import the transitions and use them with the `ScreenManager` class.

    from kivy.garden.moretransitions import PixelTransition,RippleTransition,
                                            BlurTransition,RVBTransition

    screenManager = ScreenManager(transition=PixelTransition())

or

    screenManager.transition = RVBTransition(duration=2.0)

