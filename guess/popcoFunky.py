# popco.py
# Marshall Abrams, Feb 2012
# Script containing useful routines for viewing POPCO graphs in GUESS.
# Most of this code is based on various bits of sample code that came with GUESS.
# (This partly explains the stylistic inconsistencies, along with my
# unfamiliarity with Python.)
#
# Causes edges between proposition nodes to have default behavior 
# of displaying the map node which is the source of the link weight
# when they are moused over.
# The relationship between the edge and the map node is
# represent by a convex hull.  It goes away when the
# mouse moves on.
# Also creates menu items to turn on all such "weightsource hulls", and to turn them off.
# Assumes that the Lisp routines that create GraphML files for GUESS
# define a weightsource field on edges, which contains a string representing
# the name of the corresponding map node in GUESS iff one exists for that edge.
# [Note: Will also cause any other convex hulls to disappear when
# the mouse leaves any edge.]
# Also: menu items to cause mouseover of nodes to change color of connected edges and 
# of nodes at other ends.

import java
import javax.swing
import java.awt
import com

def noop():
	pass

# Yep--global variables.  Don't yet know the Python Way to do this.
prevMouseEnterNode = 0
prevMouseLeaveNode = 0

def restoreGraphEvents():
	graphevents.mouseEnterNode = prevMouseEnterNode
	graphevents.mouseLeaveNode = prevMouseLeaveNode
	vf.defaultNodeHighlights(true)
	vf.defaultNodeZooming(true)

# Cause mouseover of nodes to change color of connected edges and of nodes at other ends
class showNeighbors(java.lang.Object):

	# This arry will store old node/edge colors so we can restore them:
	_prevNodeColors = {}
	inOperation = 0

	# store previous mouseover behavior (does this work?)
	# I also put this in init.
	prevMouseEnterNode = graphevents.mouseEnterNode
	prevMouseLeaveNode = graphevents.mouseLeaveNode
	
	def __init__(self):
		# store old listeners (also above):
		prevMouseEnterNode = graphevents.mouseEnterNode
		prevMouseLeaveNode = graphevents.mouseLeaveNode
		# add the new listeners:
		graphevents.mouseEnterNode = self.mouseEnter
		graphevents.mouseLeaveNode = self.mouseLeave
		# remove default behaviors:
		vf.defaultNodeHighlights(false)
		vf.defaultNodeZooming(false)

	def do(self):
		# store old listeners (also above):
		prevMouseEnterNode = graphevents.mouseEnterNode
		prevMouseLeaveNode = graphevents.mouseLeaveNode
		# add the new listeners:
		graphevents.mouseEnterNode = self.mouseEnter
		graphevents.mouseLeaveNode = self.mouseLeave
		# remove default behaviors:
		vf.defaultNodeHighlights(false)
		vf.defaultNodeZooming(false)

	def undo(self):
		# put back original listeners:
		graphevents.mouseEnterNode = self.prevMouseEnterNode
		graphevents.mouseLeaveNode = self.prevMouseLeaveNode
		# put back default behaviors:
		vf.defaultNodeHighlights(true)
		vf.defaultNodeZooming(true)

	def toggle(self):
		if self.inOperation == 0:
			self()
			self.inOperation = 1
		else:
			self.undo()
			self.inOperation = 0

	def mouseEnter(self, _node):
		StatusBar.setStatus(str(_node))

		self._prevNodeColors[_node] = _node.color
		_node.color = white
		_node.labelvisible = true

		for _n in _node.getNeighbors():
			# _node doesn't seem to be its own neighbor,
			# but I'm leaving the following if-test in for now,
			# just in case--it was in the original sample code. -MA
			if (_n != _node):
				self._prevNodeColors[_n] = _n.color
				_n.color = white
				_n.labelvisible = true

		for _e in _node.getOutEdges():
			self._prevNodeColors[_e] = _e.color
			_e.color = white

	def mouseLeave(self, _node):
		# put back all the original colors
		for _elem in self._prevNodeColors.keys():
			_elem.color = self._prevNodeColors[_elem]
			_elem.labelvisible = false
		self._prevNodeColors.clear();

# showWeightSource
# Show the weight source map node for a particular edge e, if such a node exists.
# Notes:
# node1 and node2 are edge fields defined by GUESS, which can be specified
# by the source and target attributes in GraphML.
# weightsource is defined in a GraphML file produced by POPCO routines.
# If non-empty, weightsource contains a string with the name of a map node
# in the syntax that we're using in GUESS. [POPCO's # yada-blah=this-that is illegal.]
def showWeightSource(e):
	if e.weightsource != "":
        	createConvexHull([e.node1, e.node2, eval(e.weightsource)], lightgray)

def removeAnyHull():
        for h in getConvexHulls(): removeConvexHull(h)

def showEdgeLabel(e):
	e.labelvisible = true

def hideEdgeLabel(e):
	e.labelvisible = false

def showEdgeInfo(e):
	showWeightSource(e)
	showEdgeLabel(e)
	(eval(e.weightsource)).labelvisible = true
	#(eval(e.weightsource)).labelcolor = white

def hideEdgeInfo(e):
	removeAnyHull()    # this is overkill, but I don't know yet sure how to hide a single hull
	hideEdgeLabel(e)
	(eval(e.weightsource)).labelvisible = false

# showWeightSources
# Show the weight source map nodes for all edge that have them.
# [Duplicates code in showWeightSource, but I haven't yet figured out how to pass colors as args.]
def showWeightSources():
	# display all weightsource hulls:
	for e in (weightsource != ""):
		createConvexHull([e.node1, e.node2, eval(e.weightsource)], randomColor(120))
	# prevent mouseovers from turning them on/off:
	graphevents.mouseEnterEdge = showEdgeLabel
	graphevents.mouseLeaveEdge = hideEdgeLabel

def hideWeightSources():
	# remove all weightsource hulls:
	removeAnyHull()
	# make it possible for mouseovers to turn on/off individual weightsource hulls:
	graphevents.mouseLeaveEdge = hideEdgeInfo
	graphevents.mouseEnterEdge = showEdgeInfo

def toggleWeightSources():
	if toggleWeightSources.visible == 0:
		showWeightSources()
		toggleWeightSources.visible = 1
	else:
		hideWeightSources()
		toggleWeightSources.visible = 0
toggleWeightSources.visible = 0


def showPropnLabels():
	for n in concerns1 == "":    # this field is always empty in proposition nodes
		n.labelvisible = 1
	center()

def hidePropnLabels():
	for n in concerns1 == "":    # this field should always be non-empty in map nodes
		n.labelvisible = 0
	center()

def togglePropnLabels():
	if togglePropnLabels.visible == 0:
		showPropnLabels()
		togglePropnLabels.visible = 1
	else:
		hidePropnLabels()
		togglePropnLabels.visible = 0
togglePropnLabels.visible = 0

def showMapLabels():
	for n in concerns1<>"":
		n.labelvisible = 1
	center()

def hideMapLabels():
	for n in concerns1<>"":
		n.labelvisible = 0
	center()

def toggleMapLabels():
	if toggleMapLabels.visible == 0:
		showMapLabels()
		toggleMapLabels.visible = 1
	else:
		hideMapLabels()
		toggleMapLabels.visible = 0
toggleMapLabels.visible = 0

# create a menu to show/hide weight sources:
def makePopcoMenu(): 
	popcoMenu = JMenu("POPCO")

	#showPropnLabelsItem = JMenuItem("Show propn node labels")
	#popcoMenu.add(showPropnLabelsItem)
	#showPropnLabelsItem.actionPerformed = lambda event : showPropnLabels()
	#hidePropnLabelsItem = JMenuItem("Hide propn node labels")
	#popcoMenu.add(hidePropnLabelsItem)
	#hidePropnLabelsItem.actionPerformed = lambda event : hidePropnLabels()
	togglePropnLabelsItem = JMenuItem("Show/hide propn node labels")
	popcoMenu.add(togglePropnLabelsItem)
	togglePropnLabelsItem.actionPerformed = lambda event : togglePropnLabels()

	#showMapLabelsItem = JMenuItem("Show map node labels")
	#popcoMenu.add(showMapLabelsItem)
	#showMapLabelsItem.actionPerformed = lambda event : showMapLabels()
	#hideMapLabelsItem = JMenuItem("Hide map node labels")
	#popcoMenu.add(hideMapLabelsItem)
	#hideMapLabelsItem.actionPerformed = lambda event : hideMapLabels()
	toggleMapLabelsItem = JMenuItem("Show/hide map node labels")
	popcoMenu.add(toggleMapLabelsItem)
	toggleMapLabelsItem.actionPerformed = lambda event : toggleMapLabels()

	#showWeightSourcesItem = JMenuItem("Show weight sources")
	#popcoMenu.add(showWeightSourcesItem)
	#showWeightSourcesItem.actionPerformed = lambda event : showWeightSources()
	#hideWeightSourcesItem = JMenuItem("Hide weight sources")
	#popcoMenu.add(hideWeightSourcesItem)
	#hideWeightSourcesItem.actionPerformed = lambda event : hideWeightSources()
	toggleWeightSourcesItem = JMenuItem("Show/hide weight sources")
	popcoMenu.add(toggleWeightSourcesItem)
	toggleWeightSourcesItem.actionPerformed = lambda event : toggleWeightSources()

	showNeighborsItem = JMenuItem("Show neighbors on rollover")
	popcoMenu.add(showNeighborsItem)
	showNeighborsItem.actionPerformed = lambda event : showNeighbors()

	altShowNeighborsItem = JMenuItem("Show neighbors on rollover(2)")
	popcoMenu.add(altShowNeighborsItem)
	altShowNeighborsItem.actionPerformed = lambda event : showNeighbors.__init__()

	dontShowNeighborsItem = JMenuItem("Stop showing neighbors")
	popcoMenu.add(dontShowNeighborsItem)
	dontShowNeighborsItem.actionPerformed = lambda event : showNeighbors().undo()

	toggleShowNeighborsItem = JMenuItem("Start/stop showing neighbors")
	popcoMenu.add(toggleShowNeighborsItem)
	toggleShowNeighborsItem.actionPerformed = lambda event : showNeighbors.toggle()

	ui.getGMenuBar().add(popcoMenu)

def miscConfig():
        #setDisplayBackground("40,40,40") # set bg color
        setDisplayBackground("70,70,70") # set bg color
        InfoWindow.create() # Make the Information Window on the left show up by default:


class WeightSourceConfig(com.hp.hpl.guess.ui.DockableAdapter):

	def __init__(self):
		# Make the weightsource hull for an edge display on mouseover:
		graphevents.mouseEnterEdge = showEdgeInfo
		# Make leaving an edge turn off all weightsource hulls (usually only one):
		graphevents.mouseLeaveEdge = hideEdgeInfo

	# define the title in the window
	def getTitle(self):
		return("weight sources")

# run class (instantiate it) to add mouseover behavior
WeightSourceConfig()

# add menu:
makePopcoMenu()

miscConfig()
