Genetic algorithm to build planetary systems
--------------------------------------------

**Objective:** create a planet system of size _n_ with entities having 
initial velocities of at least _v_ and not colliding for at least _t_ 
steps of gravity simulation. Entities are prohibited from escaping the 
observable universe (window in our case).

**Mutation set:** (update-mass), (update-velocity), (update-position)

**Terminal set:** (.Entity)

**Fitness:** minimize number of simulation steps without any collisions,
maximizes number of entities after _t_ steps

**Parameters:** 500+ population size; 50% crossover, 10% reproduction, 40% mutation

**Termination:** planet system that survives _t_ simulation steps.


