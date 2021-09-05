Machinations.io implementation in Haskell with some extra abilities

Supports: Sources, Pools, Drains, Gates, Converters, Traders, Delays, Queues and Resource edges
State edges with triggers (! and *), label modifiers, interval modifiers, and resource test are supported.

Register are missing. Modifiers related to probabilities are missing. Probabilities on state edges that leave gates are triggers, not yet implemented.

Generated resources and killed resources are accurate, other stats are not yet tested

Extensions to machinations: 
1. Resources have unique UUIDs
2. Nodes can be triggered by external events (arbitrary tags). Store a list of
   comma-separated tags in the node label like "MyNode;tag1,tag2". The resulting
   node will be triggered automatically whenever that external event is sent.
3. (TODO) Resources edges can contain constraints, such as only allowing
   resources that participated in a collision. These are also stored as
   separated by semicolons, like ">3;type(other)==\"bullet\"", which only allows
   resources that collided with a bullet to go through this edge (in addition to
   its other constraints).


Compile with: stack build

Run tests: stack test 

Run the server: stack exec machinations-server

With the server up the API is described at: http://localhost:8000/swagger-ui/
