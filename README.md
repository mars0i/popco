popco
=======

Copyright (c) 2012, 2013 by Marshall Abrams
This work is free for academic use.  Distribution and use for other
purposes allowed only with permission from the author.
Many parts of POPCO in other files are by Paul Thagard and his
collaborators on COHERE, with extensive modifications in many areas.
Kristen Hammack made significant contributions to POPCO.

POPCO is a framework for agent-based simulations in which agents’
communication of their simulated beliefs depends how those beliefs do or
do not fit into analogies.  For the motivation of this project, an
illustration of its use, and the primary documentation of how the
software works see the open-access article listed below.

POPCO is written mainly in Common Lisp, but there are some ancillary
programs in NetLogo and R.  I'm currently exploring the suitability of
rewriting POPCO in Clojure (see project popco-x).  This should result in
cleaner, simpler code--not because of Clojure, really, but because I'm
rewriting POPCO from scratch now that I understand it well.  However,
implementing POPCO in Clojure will make it easier to work with external
Java libraries.

I'm more than happy to answer questions or work with others who want
to use POPCO. See http://members.logical.net/~marshall for more info.

-------

#### Marshall Abrams, "A moderate role for cognitive models in agent-based modeling of cultural change", *Complex Adaptive Systems Modeling* 2013, 1(16):1-33.

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
