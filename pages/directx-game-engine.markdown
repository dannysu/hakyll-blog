---
title: DirectX Game Engine
---
Three of my classmates and I were on our co-op work term in different cities,
but we still decided to have some fun trying to write a DirectX game engine
together. We held weekly meeting on Sundays using webcam and chat to get things
done. We used perforce as our source control software and in the end of the
project we were able to make a skeleton character walk and avoid obstacles by
clicking somewhere on the screen.

The exact replication of our project tracking page is show below:

### Goals

#### Project Goals (by March 13, 2004):

Game should be able to receive input via clicking on screen. Hero will walk
according to translated map location and camera following our Hero.

#### Revised Project Goals (by March 20, 2004):

Have an AnimatedMesh moving towards a house (after mouse clicking on screen)...
GameCamera following the Hero and perform collision detection to allow Hero to
stop right in front of the house.

#### Project Goals (by March 27, 2004):

Integrate bounding sphere collision detection with quadtree detection. Hero can
attack other units, other units will react when hit. Game Menus will be ready,
be able to switch between game states. Should have Lighting and more models to
work with.

#### Project Goals (by end first week of April):

Enemy units will be able to automatically initiate attack at the Hero. Have
weather conditions. Have Level Editor. Units can go around things to reach
destination.

#### Project Goals (end of term):

A person walking in a terrain hitting people; with menus; simple game logic;

* * *

### Current Status

Game Engine has the following capabilities:
	
  * Terrain rendering
  * Animated mesh
  * Animated Skybox and water
  * Collision Detection with character moving around obstacles
  * Camera Control
  * Game Logic Interface
  * Scene-Model-Texture Management
  * Pick Ray - character movement controlled by mouse clicks

### Team Member

Chien-Wen (Danny) Su
Johnny Yip
Brian Keng
Felix Lo

### Design


Main Components: Game Logic, Game Engine, Win32
![](http://www.dannysu.com/wp-content/uploads/2011/03/structure4.jpg)


#### Game Logic Modules


**Pink: High Priority, Blue: Medium Priority**








**Module Name**


**Description**


**Status**


**Developers**






**Map**


load from file, contain game logic map, pass pointer to terrain of vertices


under development


Danny






**Menu**


display menu with items, etc


under development


Brian






**Load Game File**


load map data, load character data, load misc data... strictly file i/o.


not started








**Characters**


health, type, state, location and movement on the given map


under development


Danny






**Inanimate objects**


building, tree, boat. etc...


under development


Danny






**Game camera**


higher level camera for game


under development


Felix






**Input handling**


reads input and does game logic (GameInput)


under development


Brian






**AI**


later


not started


Felix




Major Components for Game Engine:


#### Graphics Modules










**Module Name**


**Description**


**Status**


**Developers**






**Camera**


controls the scene camera (multiple cameras?)


under development


Felix






**Mesh**


Manage all loaded mesh


good for now


Johnny






**Model Manager**


Manage all loaded models, and use the structure for culling


under development


Johnny






**Pick**


transforms a 2D input (mouse) into which polygon it corresponds to in 3D world


under development


Brian






**Animation**


Similar to 3DMeshFrame except this is for animation
Classes involved: 3DAnimMeshFrame, 3DMeshHierarchy


under development








**3DModel**






	
  * Base class for all Models

	
  * Classes involved: 3DMeshFrame





pending review


Johnny






**Terrain**


takes map data, and build terrain mesh, handles the textures


good for now


Danny






**Lighting**


vertex lighting, light mapping


not started








**2D drawing**


draw a 2d image on to screen (bitmaps), text, basic shapes(?)


good for now


Brian






**Scene Manager**


Manage all drawing operation to optimize render state changes


not started








**D3D Module**


Interface to D3D Device


good for now








**D3DEnum**


enumeration used in the d3d application, such as vertex, config etc.


not started








**Derived Models**


derived models from 3DModel such as character, inanimate object


suspended








**Particles**


particle


not started








**demo**


show off and test graphics modules


on going


Everyone




![](http://www.dannysu.com/wp-content/uploads/2011/03/graphics.jpg)


#### Game Modules










**Module Name**


**Description**


**Status**


**Developers**






**Game Interface**


Game logic will never touch any class except for this one


inital phase








#### Other Modules










**Module Name**


**Description**


**Status**


**Developers**






**Collision Detection**


collision detection


under development


Danny






**Input**


mouse and keyboard input


good for now








**Sound**


work on later


not started








**Timer**


global timer to synchronize all components in game engine


good for now








**Physics**


work on later


not started








### Screenshots


![](http://www.dannysu.com/wp-content/uploads/2011/03/sky.jpg)

![](http://www.dannysu.com/wp-content/uploads/2011/03/landscape.jpg)


