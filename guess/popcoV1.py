# popco.py
# Marshall Abrams, Jan 2012
# Script containing useful routines for viewing POPCO graphs in GUESS.
#
# Causes edges between proposition nodes to have default behavior 
# of displaying the map node which is the source of the link weight
# when they are moused over.
# The relationship between the edge and the map node is
# represent by a convex hull.  It goes away when the
# mouse moves on.
# Also creates buttons to turn on all such "weightsource hulls", and to turn them off.
# Assumes that the Lisp routines that create GraphML files for GUESS
# define a weightsource field on edges, which contains a string representing
# the name of the corresponding map node in GUESS iff one exists for that edge.
# [Note: Will also cause any other convex hulls to disappear when
# the mouse leaves any edge.]

import java
import javax.swing
import com

def noop():
	pass

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
        	createConvexHull([e.node1, e.node2, eval(e.weightsource)], darkgray)

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



class WeightSourceConfig(com.hp.hpl.guess.ui.DockableAdapter):

	def __init__(self):
		# store what leaving an edge usually does:
		#defaultMouseLeaveEdgeFn = graphevents.mouseLeaveEdge
		#defaultMouseEnterEdgeFn = graphevents.mouseEnterEdge

		showWeightSourceButton = JButton("show weight sources")
		showWeightSourceButton.actionPerformed = lambda event: showWeightSources()
		self.add(showWeightSourceButton)

		hideWeightSourceButton = JButton("hide weight sources")
		hideWeightSourceButton.actionPerformed = lambda event: hideWeightSources()
		self.add(hideWeightSourceButton)

		# add the toolbar to the main UI window
		ui.dock(self)

		# Make the weightsource hull for an edge display on mouseover:
		graphevents.mouseEnterEdge = showEdgeInfo

		# Make leaving an edge turn off all weightsource hulls (usually only one):
		graphevents.mouseLeaveEdge = hideEdgeInfo

	# define the title in the window
	def getTitle(self):
		return("weight sources")

# run it (instantiate it)
WeightSourceConfig()
