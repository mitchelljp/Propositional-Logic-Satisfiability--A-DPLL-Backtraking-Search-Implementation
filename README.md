# DPLL Implementation
Implemented the DPLL (Davis–Putnam–Logemann–Loveland) algorithm for Informatics 2D Reasoning an Agents, an AI based course in Edinburgh, Scotland. DPLL is a backtracking search algorithms that determines whether or not a propositional logic formula in conjunctive normal form is satisfiable (if it evaluates to true in any given situation). 

My code, and all methods I wrote are located in Inf2d.hs. Main.hs checks the progress of the program at various steps and ensures accuracy of my work (this was supplied to us by instructors). 

 It takes as input a 
-- list of clauses in CNF, a list of symbols for the domain, and model. 
-- It returns true if there is a model which satises the propositional sentence. 

Tha algorithm was written in Haskell. In general, the project was broken down into four categories:

1. Implement some general functions
2. Implement the simplification functions
3. Write the DPLL procedure
4. Create heuristics for DPLL. 