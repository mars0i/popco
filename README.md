POPCO (population coherence)
=======

Copyright (c) 2012, 2013 by Marshall Abrams

This work is currently free for academic use.  Distribution and use for
other purposes allowed only with permission from the author.  The
overall conception and much of the code for POPCO are by Marshall
Abrams, but many parts of POPCO are by Paul Thagard and his
collaborators on COHERE, with extensive modifications by Marshall
Abrams.  Kristen Hammack also made significant contributions to POPCO.  See
the acknowledgements section of the article listed below for a list of
others whose help was very important to the project.

POPCO is a framework for agent-based simulations in which agents’
communication of their simulated beliefs depends how those beliefs do or
do not fit into analogies.  For the motivation of this project, an
illustration of its use, and the primary documentation of how the
software works, see the open-access article listed below.

POPCO is written mainly in Common Lisp, but there are some ancillary
programs in NetLogo and R.  I'm currently exploring the suitability of
rewriting POPCO in Clojure (see [popco-x](https://github.com/mars0i/popco-x)).  This should result in
cleaner, simpler code--not because of Clojure, but because I'm
rewriting POPCO from scratch now that I understand it well.  However,
implementing POPCO in Clojure will make it easier to work with external
Java libraries and programs.

I'm more than happy to answer questions or work with others who want
to use POPCO. See http://members.logical.net/~marshall for more info.

-------

#### How to use POPCO

To use POPCO, load start.lisp in an implementation of Common Lisp.
Then load one or more files that define persons and their
propositions, and set parameters.  Then run the command '(popco)'.

utils/popco and utils/popco.fast are *nix shell scripts that
illustrate how to run POPCO using Steel Bank Common Lisp.

Examples of files defining persons, etc. can be found in the crime and
sanday directories.  See for example crime/crime3example.lisp, which
also loads crime/crime3.lisp, which in turn loads
crime/crime3propns.lisp.

If `*do-report-propns-to-csv*` is non-`nil`, a CSV file with data on all
proposition activation values in each person at each tick will be
generated.  This can be read by functions defined in R/df2ra.R.  If
`*do-report-to-netlogo*` is non-`nil`, a file that can be read by
netlogo/popco.nlogo will be generated.  See doc/GuessTips.txt for info
on on how to generate files that can be loaded into the GUESS network
visualization package.  By default, POPCO generates a file
data/RUN*.lisp, which stores a command to set the same random seed.
If you load this after start.lisp but before loading the files that
define persons, etc., and before running `(popco)`, you can replay
the session that generated the RUN*.lisp file.

(I don't consider the rest self-explanatory, but the preceding is a
start.  If you want to experiment with POPCO, I suggest contacting me.
I'll be very happy to help you get started.)

I've run POPCO successfully in Steel Bank Common Lisp (SBCL), Armed
Bear Common Lisp (ABCL), Clozure Common Lisp (CCL), Embeddable Common
Lisp (ECL), LispWorks, and CLISP.  POPCO runs fastest in SBCL, in my
experience. (There is a problem that can occur with CLISP and (at
least) the free version of LispWorks.  If you set
`*do-report-to-netlogo*` to `t` in order to generate a data file to be
read by popco.nlogo, it's possible for the code that generates the
NetLogo data file to construct a function call with an extremely long
argument list.  CLISP and LispWorks have a limit on the length of
argument lists, and therefore can generate errors in this situation.)

#### How to read POPCO's source code

I suggest starting with popco.lisp.  A good starting point for reading
is the function `run-population-once`.  You'll also want to reference
variables.lisp along the way.  Note that start.lisp loads all of the
lisp files that make up POPCO.

-------

#### Article on POPCO:

#### Marshall Abrams, ["A moderate role for cognitive models in agent-based modeling of cultural change"](http://www.casmodeling.com/content/1/1/16), *Complex Adaptive Systems Modeling* 2013, 1(16):1-33.

#### Abstract:

##### Purpose

Agent-based models are typically “simple-agent” models, in which agents
behave according to simple rules, or “complex-agent” models which
incorporate complex models of cognitive processes. I argue that there is
also an important role for agent-based computer models in which agents
incorporate cognitive models of moderate complexity. In particular, I
argue that such models have the potential to bring insights from the
humanistic study of culture into population-level modeling of cultural
change.

##### Methods

I motivate my proposal in part by describing an agent-based modeling
framework, POPCO, in which agents’ communication of their simulated
beliefs depends on a model of analogy processing implemented by
artificial neural networks within each agent. I use POPCO to model a
hypothesis about causal relations between cultural patterns proposed by
Peggy Sanday.

##### Results

In model 1, empirical patterns like those reported by Sanday emerge from
the influence of analogies on agents’ communication with each other.
Model 2 extends model 1 by allowing the components of a new analogy to
diffuse through the population for reasons unrelated to later effects of
the analogy. This illustrates a process by which novel cultural features
might arise.

##### Conclusions

The inclusion of relatively simple cognitive models in agents allows
modeling population-level effects of inferential and cultural coherence
relations, including symbolic cultural relationships. I argue that such
models of moderate complexity can illuminate various causal
relationships involving cultural patterns and cognitive processes.

Keywords: Simulation; Culture; Cognition; Analogy; Metaphor;
Hermeneutics
