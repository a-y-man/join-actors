# Project Plan

## Steps

1. **Quote matching, Constraint CaseDef** : In this part, the aim is to have pattern matching move from reflection to quotes, and restrict the guards to function applications

2. **Implement Simple Algorithm** : The objective is to have a simple algorithm that allows to match the messages/events in the queue against the internal representation of the patterns

3. **Code Generation** : The macro should produce a well-formed function expression, intented to use in an Actor

4. **Implement Complex Algorithm** : Here a more complex matching algorithm, such as Rete, would be implemented, involving a lot more comptime logic and structure, such as trees

5. **Possible Extensions** : [Optional] This allocates  time for any extension (for example, message ordering, wildcards) that adds value to the project

6. **Create Samples** : During this step use cases must be written from typical applications, and compare the macro-based approach to ScalaJoins and Akka

7. **Produce results** : Use the a benchmark framework (possibly scalameter) to gather the data, and generate  figures for the report

8. **Write paper** : Can start as soon as the simple  algorithm is implemented

## Diagram


