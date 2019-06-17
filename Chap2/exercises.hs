module UsePictures where
import Pictures


ex2 :: Picture
ex2 = above (beside white black) (beside black white)

ex3 :: Picture
ex3 = above (beside horse (invertColour horse))
            (beside (invertColour horse) horse)
