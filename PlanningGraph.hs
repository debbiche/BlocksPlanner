module PlanningGraph where

data Fact = Fact Name Bool
data Name = String
data Action = Action Name [Precondition] [Result]
data Precondition = Precondition Fact Bool
data Effect = Effect Fact Bool
data Result = Result [Effect]
data Mutex = Mutex Fact Fact

