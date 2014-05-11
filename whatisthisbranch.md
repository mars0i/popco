What is this branch?
=====

This is MASTER in the github repository.  
It was formerly SEPROADBACK in the assembla.com repository.
See below for notes on the history.

branch SEPROAD is like SEPLOOPS in that it has the replacement
settle-nets call tree, but will incorporate the call tree
from master, i.e. the old one using the old run-hyp-net,
so that I can incrementally move from the latter to the
former and figure out at what point I'm starting to get
slight changes in activations.  i.e. the old and the new
should produce identical output, but they don't.
The reason this is needed is that the alternate functions
like run-hyp-net in seploops are those from SEPSETTLE,
which was an incremental modification of MASTER.

Branch SEPROADBACK contains a series of mods starting from seproad,
with the goal of replacing master.  It encountered problems in
early Dec 2012.

(Branch SEPROADBACK2 was a branch to allow playing around
with things in seproadback to make its behavior more
like the assembla master in some ways.)

-MA 11/22/2012
