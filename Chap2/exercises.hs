module UsePictures where
import Pictures

black :: Picture
black = invertColour white

ex2 :: Picture
ex2 = above (sideBySide white black) (sideBySide black white )

ex3 :: Picture
ex3 = above (sideBySide horse (invertColour horse)) (sideBySide (invertColour horse) horse)