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

&nbsp;  

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
	
- Terrain rendering
- Animated mesh
- Animated Skybox and water
- Collision Detection with character moving around obstacles
- Camera Control
- Game Logic Interface
- Scene-Model-Texture Management
- Pick Ray - character movement controlled by mouse clicks

### Team Member

Chien-Wen (Danny) Su  
Johnny Yip  
Brian Keng  
Felix Lo

### Design

Main Components: Game Logic, Game Engine, Win32

![](/images/structure4.jpg)

#### Game Logic Modules

<p><strong><span style="color: #ff8866;">Pink: High Priority</span>,&nbsp;<span style="color: #44aaee;">Blue: Medium Priority</span></strong></p>
<table border="1">
<tbody>
<tr>
<td><strong>Module Name</strong></td>
<td><strong>Description</strong></td>
<td><strong>Status</strong></td>
<td><strong>Developers</strong></td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Map</strong></td>
<td>load from file, contain game logic map, pass pointer to terrain of vertices</td>
<td>under development</td>
<td>Danny</td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Menu</strong></td>
<td>display menu with items, etc</td>
<td>under development</td>
<td>Brian</td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Load Game File</strong></td>
<td>load map data, load character data, load misc data&#8230; strictly file i/o.</td>
<td>not started</td>
<td></td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Characters</strong></td>
<td>health, type, state, location and movement on the given map</td>
<td>under development</td>
<td>Danny</td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Inanimate objects</strong></td>
<td>building, tree, boat. etc&#8230;</td>
<td>under development</td>
<td>Danny</td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Game camera</strong></td>
<td>higher level camera for game</td>
<td>under development</td>
<td>Felix</td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Input handling</strong></td>
<td>reads input and does game logic (GameInput)</td>
<td>under development</td>
<td>Brian</td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>AI</strong></td>
<td>later</td>
<td>not started</td>
<td>Felix</td>
</tr>
</tbody>
</table>

Major Components for Game Engine:

#### Graphics Modules

<table border="1">
<tbody>
<tr>
<td><strong>Module Name</strong></td>
<td><strong>Description</strong></td>
<td><strong>Status</strong></td>
<td><strong>Developers</strong></td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Camera</strong></td>
<td>controls the scene camera (multiple cameras?)</td>
<td>under development</td>
<td>Felix</td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Mesh</strong></td>
<td>Manage all loaded mesh</td>
<td>good for now</td>
<td>Johnny</td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Model Manager</strong></td>
<td>Manage all loaded models, and use the structure for culling</td>
<td>under development</td>
<td>Johnny</td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Pick</strong></td>
<td>transforms a 2D input (mouse) into which polygon it corresponds to in 3D world</td>
<td>under development</td>
<td>Brian</td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Animation</strong></td>
<td>Similar to 3DMeshFrame except this is for animation<br />
Classes involved: 3DAnimMeshFrame, 3DMeshHierarchy</td>
<td>under development</td>
<td></td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>3DModel</strong></td>
<td>
<ul>
<li>Base class for all Models</li>
<li>Classes involved: 3DMeshFrame</li>
</ul>
</td>
<td>pending review</td>
<td>Johnny</td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Terrain</strong></td>
<td>takes map data, and build terrain mesh, handles the textures</td>
<td>good for now</td>
<td>Danny</td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Lighting</strong></td>
<td>vertex lighting, light mapping</td>
<td>not started</td>
<td></td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>2D drawing</strong></td>
<td>draw a 2d image on to screen (bitmaps), text, basic shapes(?)</td>
<td>good for now</td>
<td>Brian</td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Scene Manager</strong></td>
<td>Manage all drawing operation to optimize render state changes</td>
<td>not started</td>
<td></td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>D3D Module</strong></td>
<td>Interface to D3D Device</td>
<td>good for now</td>
<td></td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>D3DEnum</strong></td>
<td>enumeration used in the d3d application, such as vertex, config etc.</td>
<td>not started</td>
<td></td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Derived Models</strong></td>
<td>derived models from 3DModel such as character, inanimate object</td>
<td>suspended</td>
<td></td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Particles</strong></td>
<td>particle</td>
<td>not started</td>
<td></td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>demo</strong></td>
<td>show off and test graphics modules</td>
<td>on going</td>
<td>Everyone</td>
</tr>
</tbody>
</table>

![](/images/graphics.jpg)

#### Game Modules

<table border="1">
<tbody>
<tr>
<td><strong>Module Name</strong></td>
<td><strong>Description</strong></td>
<td><strong>Status</strong></td>
<td><strong>Developers</strong></td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Game Interface</strong></td>
<td>Game logic will never touch any class except for this one</td>
<td>inital phase</td>
<td></td>
</tr>
</tbody>
</table>

#### Other Modules

<table border="1">
<tbody>
<tr>
<td><strong>Module Name</strong></td>
<td><strong>Description</strong></td>
<td><strong>Status</strong></td>
<td><strong>Developers</strong></td>
</tr>
<tr>
<td style="background-color: #ff8866;" valign="top"><strong>Collision Detection</strong></td>
<td>collision detection</td>
<td>under development</td>
<td>Danny</td>
</tr>
<tr>
<td style="background-color: #44aaee;" valign="top"><strong>Input</strong></td>
<td>mouse and keyboard input</td>
<td>good for now</td>
<td></td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Sound</strong></td>
<td>work on later</td>
<td>not started</td>
<td></td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Timer</strong></td>
<td>global timer to synchronize all components in game engine</td>
<td>good for now</td>
<td></td>
</tr>
<tr>
<td style="background-color: #efefef;" valign="top"><strong>Physics</strong></td>
<td>work on later</td>
<td>not started</td>
<td></td>
</tr>
</tbody>
</table>

### Screenshots

![](/images/sky.jpg)

![](/images/landscape.jpg)

