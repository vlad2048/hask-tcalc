# Overview

As a learning exercise, I've written this little tool to automatically find an expression that fits a target type given some definitions
Here's how it works:

### Inputs
```hs
f :: a -> (b -> r) -> r
m :: (a -> r) -> r
```
### Question
```hs
-- how do we write an expression of this type using the terms f and m ?
? :: (b -> r) -> r
```
### Output
```
ghci> main
\t₀ -> m \t₁ -> f t₁ t₀
```

I've thought up of the algorithm myself, it's surely very naive. Here's my description of it I wrote initially:

&nbsp;

# Algorithm

## Example
Implement the bind operation for the Continuation monad

```hs
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

(>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
```

&nbsp;

## Question
```hs
(>>=) m f = x1
m  :: (a -> r) -> r
f  :: a -> (b -> r) -> r
x1 :: (b -> r) -> r
```
How to we implement **x1** in function of **m** and **f** ?

&nbsp;

## Algorithm

To resolve a variable (ie: **x1**) we try in this order:

### 1) **from ctx**
Is **x1** already in the current context ?
```hs
x1 from ctx -> YES / NO
```

### 2) **from funapp**
Can **x1** be obtained by function application from the current context ?
For this to work, the context needs to have functions that return a value of **x1**'s type. We then need to construct the arguments needed to return the right type. So we create new variables and recurse on those
```hs
x1 from funapp
    candidate   "x1 = f x2 x3"      x2 :: a        x3 :: b -> r
    x2 from ctx -> YES / NO
    (...)
```
  
### 3) **from lambda**
If **x1** is a function type, can we implement it as a lambda ?
create a new variable for the result (ie: **x2)** and recurse on it
```hs
-- if x1 :: (b -> r) -> r
x1 from lambda    "x1 = \l -> x2"   x2 :: r     l :: b -> r
    x2 from ctx -> YES / NO
(...)
```

### Notes
- step 1) can be considered a special case of step 2)

&nbsp;

## Application
This is the algorithm finding how to implement the bind operation for the continuation monad:
```hs
-- Problem:
-- How do we implement (>>=) ?
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
(>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b

-- ctx contains:
--      f  :: a -> (b -> r) -> r
--      m  :: (a -> r) -> r
-- and we're looking for:
--      x1 :: (b -> r) -> r         = \l -> m (\g -> f g l)

x1 from ctx -> NO
x1 from funapp
    ┌ candidate    "f x2"            x2 :: a
    │ x2 from ctx    -> NO
    │ x2 from funapp -> NO
    └ => NO
x1 from lambda    "x1 = \l -> x2"            x2 :: r        l :: b -> r
    x2 from ctx -> NO
    x2 from funapp
        ┌ candidate    "x2 = f x3 x4"        x3 :: a        x4 :: b -> r
        │ x3 from ctx    -> NO
        │ x3 from funapp -> NO
        └ => NO
        ┌ candidate    "x2 = m x3"            x3 :: a -> r
        │ x3 from ctx    -> NO
        │ x3 from funapp -> NO
        │ x3 from lambda    "x3 = \g -> x4"        x4 :: r        g :: a
        │     x4 from ctx -> NO
        │     x4 from funapp
        │         ┌ candidate    "x4 = f x5 x6"        x5 :: a        x6 :: b -> r
        │         │
        │         │ -- ctx contains:
        │         │ --      f  :: a -> (b -> r) -> r
        │         │ --      m  :: (a -> r) -> r
        │         │ --      l :: b -> r
        │         │ --      g :: a
        │         │ -- and we're looking for:
        │         │ --      x5 :: a
        │         │ --      x6 :: b -> r
        │         │ -- we can then find the solution using:
        │         │ --      x4 = f x5 x6
        │         │ --      x3 = \g -> x4
        │         │ --      x2 = m x3
        │         │ --      x1 = \l -> x2
        │         │
        │         │ x5 from ctx -> YES    x5 = g
        │         │ x6 from ctx -> YES    x6 = l
        │         └ => YES
        └ => YES

===>
                    
x4 = f g l
x3 = \g -> f g l
x2 = m (\g -> f g l)
x1 = \l -> m (\g -> f g l)
```

### Result:
```hs
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

(>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
(>>=) m f = \l -> m (\g -> f g l)
```
