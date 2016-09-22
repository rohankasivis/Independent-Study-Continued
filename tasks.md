# Task list

## Fix current distributed aggregation protocol implementation

- create separate methods for processing each message
- only use matching (`match { ... }`) where appropriate, e.g., on `Option[T]` and on messages
- make sure the Scala code matches the abstract protocol definition closely
- let each actor have its own setup code for sending the messages `SendAggregate` and `Broadcast` to itself

## Generalize aggregation operation

Currently, aggregation is done for integers (`Int`) using the plus (`+`) and minus (`-`) operations. This should be generalized to any type `T` which has: 
- a binary operator (`op`), generalizing plus
- a unary inverse operator (`inv`), generalizing minus
- an identity element (`id`), generalizing 1

For example, arithmetic modulo 15 with the multiplication operation.

For the protocol to work properly, programmers must check that the following conditions hold:

- `op` is commutative
- `op` is associative
- for all `t : T`, `op(t, 1) = t` and `op(1, t) = t`
- for all `t : T`, `op(t, inv(t)) = op(inv(t), t) = 1`

These conditions can be checked by test cases.

## Use ScalaTest for testing

- current tests are ad-hoc and require a lot of setup code
- change current tests to use [ScalaTest](http://www.scalatest.org)

## Make it easy to define actor configurations for testing

- come up with an easy syntax to define connections between actors when setting up an aggregation scenario
- for example, there might be a good Scala library for defining graphs that could be used

## Simplify protocol

- the current version of the protocol can be simplified by merging the `sent` and `received` maps
- change the implementation to use this simpler version and make sure all tests pass

## Implement another aggregation protocol

## Compare protocol performance
