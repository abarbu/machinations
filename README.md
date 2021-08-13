Machinations.io implementation in Haskell with some extra abilities

Supports: Sources, Pools, Drains, Gates, Converters, Traders, Delays, Queues and Resource edges
State edges with triggers (! and *), label modifiers, interval modifiers, and resource test are supported.

Register are missing. Modifiers related to probabilities are missing. Probabilities on state edges that leave gates are triggers, not yet implemented.

Generated resources and killed resources are accurate, other stats are not yet tested




Compile with: stack build

Run tests: stack test 

Run the server: stack exec machinations-server

With the server up the API is described at: http://localhost:8000/swagger-ui/
