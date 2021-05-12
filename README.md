# templateModeller

This is a simple set of functions that allows for the programmatic construction and running of models via composition of model template functions. Additionally, there is support for the programmatic insertion of results into Excel .xlsx template files. An example is included. 

Practically speaking, this allows you to produce and tweak a comparatively large number of model/sub-cohort permutations relative to the effort required, and then run your modelling R script and have formatted Excel tables containing your result be produced automagically. 

This enables the decoupling of model specification constructs, fitting the model(s), and result presentation/formatting, hopefully decreasing the total amount of effort required during the analytic lifecycle.
