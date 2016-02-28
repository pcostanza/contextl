# ContextL
ContextL is a CLOS extension for Context-oriented Programming (COP), and was the first language extension that explicitly supports COP when it was originally introduced in 2005.

ContextL is also provided by [Quicklisp](https://www.quicklisp.org/).

Currently, there is no documentation available, but you can find some test cases in the distribution and an overview of ContextL's features in [an overview paper](http://www.p-cos.net/documents/contextl-soa.pdf "Context-oriented Programming in ContextL"). See also this [general overview article about COP](http://www.jot.fm/issues/issue_2008_03/article4/ "Context-oriented Programming") which also contains some ContextL examples.

ContextL depends on [Closer to MOP](https://github.com/pcostanza/closer-mop "Closer to MOP").

New in version 1.0.0:
* New version number based on semantic versioning.
* Since version 0.61, support for Allegro Common Lisp 8.2 & 9.0, ABCL, and LispWorks 6.1 has been added.
* An option has been added to disable special slots, which may improve performance if they are not needed.
* Thread safety has been improved.

New in version 0.61:
* Added support for LispWorks 6.0.

Highlights of version 0.6:
* Added support for first-class dynamic environments and DYNAMIC-WIND. Due to popular demand, these features can be used independently of the rest of ContextL, by way of using a separate system definition. (In that case, no CLOS MOP is used, so this should run in any ANSI-compliant Common Lisp implementation.)
* Added support for Embeddable Common Lisp.
* Resurrected support for Macintosh Common Lisp (now RMCL).
* Improved extensibility of ContextL metaclasses.
* Removed unnecessary redefinition warnings for layered classes in Allegro and LispWorks.
* Removed dependencies on portable-threads and trivial-garbage.
* Improved use of synchronization features for multithreaded CL implementations.
* Switched to a uniform model for optional features.
* Simplified and improved conditionalizations for Clozure Common Lisp and LispWorks, and removed mentions of OpenMCL (which was just the old name for Clozure Common Lisp).
* Lots of small little bug fixes and improvements here and there.
* Lots of special thanks to the following people who provided useful patches and comments: Willem Broekema, Theam Yong Chew, Alexander Gravilov, Attila Lendvai, and Tobias Rittweiler. Extra special thanks to Duane Rettig (of Franz Inc.) and Martin Simmons (of LispWorks Ld.) for helping with Allegro-specific and LispWorks-specific issues.

Highlights of version 0.51:
* Ensure-layered-method now accepts function designators instead of just function names, due to Drew Crampsie.

Highlights of version 0.5:
* ContextL is now thread-safe.
* Added :in as an alternative for :in-layer in the various define-layered-xyz macros.
* Added new functions active-layers and (setf current-layer-context).
* Added a garbage collector for layer caches, such that redefinition of layers or certain methods in the ContextL MOP have an effect.
* Simplified mapping of layer-related names to internal names, which should also make things easier to read when debugging ContextL programs.

Highlights of version 0.4:
* The deflayer macro doesn't take a :layer-class option anymore, but instead a :metaclass option. This reflects that layers are in fact represented as CLOS classes.
* Added the function current-layer-context. This captures the set of currently active layers, which can later be reinstalled with funcall-with-layer-context and apply-with-layer-context.
* Added several readers for ContextL's metaclasses.

Highlights of version 0.31:
* Added two versions of the figure editor example from the JMLC'06 paper to the test suite.

Highlights of version 0.3:
* Added metacircular layer activation through ACTIVATE-LAYER-USING-CLASS and DEACTIVATE-LAYER-USING-CLASS.
* Added WITH-SPECIAL-INITARGS and WITH-SPECIAL-INITARGS\* macros for rebinding special slots based on their initargs.
* WITH-ACTIVE-LAYERS and WITH-ACTIVE-LAYERS\* now process initargs for layer-specific special slots such that they can be rebound at the same time when the respective layer is activated.
* Added CALL-NEXT-LAYERED-METHOD for more convenient super calls in layered methods.
* Added singleton classes. Layers are singletons and internally represented as singleton classes, but the notion of a singleton class is useful in itself, so ContextL provides this as a separate feature.

This project was partially funded by the Institute for the Promotion of Innovation through Science and Technology in Flanders (IWT-Vlanderen) from 2005-2008.
