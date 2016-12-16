

Import images as pngs with the command fromPng and a file path

After an image is imported, you have two opions:
	1. Apply image functions to it
	2. Get an animation from it if it is a sprite sheet

If a regular png image is imported, any of the image functions can be applied to it, and custom functions can also be used. Just make sure that the end type is MyImage. Function composition allows for the composition of many of these functions.
For a sprite sheet you need to call splitSpriteSheet with the image and the number of sprites as well as the number of pixels between each sprite. A list of images is then imported. Mapping image map functions will work, as will applying the animation functions or using an animation map (which takes a function from MyImage to MyImage).

In order to export the image(s) to png or gif after applying functions to them, all one has to do is use the function toPng or toGif.
toPng takes in a file path along with the modified image itself and writes the file. Because pngs are imported as io files, there is a convenience function ioToPng.
toGif takes in a file path and an animation (a list of images) as well as a frame delay in 1/100th of a second and writes to the computer.

To preview an image or animation, use the functions exImg, exAni, exBoth, and exAniTwo. While drawing images tends to work fine, animations are highly variable in performance.
exImg takes in an io image and a function from MyImage to MyImage and draws using openGL.
exAni takes in an io image and a function from MyImage tp [MyImage] and draws using openGL. Note that the animation speed is highly dependent on the number of pixels used.
exBoth and exAniTwo are both convenience functions that draw both an image and an animation using the same loaded image or draws two animations using the same loaded image respectively. Likewise, they both take two input functions as well.

If you want to create your own function, the main helper functions would be imageMap, imageMapXY, imageConditionMap, animationMap, and splitSpriteSheet