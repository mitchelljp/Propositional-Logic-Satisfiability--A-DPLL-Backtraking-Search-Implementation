-- Inf2D Assignment 1

module Inf2d where
import Data.List
import Debug.Trace
import Data.Ord
import Data.Maybe
import Data.Time
import Control.Monad
import Control.Exception

-- Type synonyms for the data structures

-- Symbols are strings (a negative sign as the first character represents a negated symbol)
type Symbol = String  
-- Clause = a disjuntion of symbols
type Clause = [Symbol] 
-- Sentence = Statements. This is a list of a list of symbols
type Sentence = [[Symbol]] 
-- Models are represented as a list of (Symbol,Boolean) tuples
type Model = [(Symbol, Bool)]
-- The knowledge base is represented as a list of statements
type KB = [Sentence]


-----------------------------------------------
-- STUDENT MATRICULATION NUMBER: 
-----------------------------------------------
studentId::String
studentId = "s1582805"

--------------------------------------------------
-- ASSIGNMENT TASKS
-- Refer to assignment sheet for details of tasks
--------------------------------------------------

----------TASK 1: REPRESENTATION (2 marks)----------------------------------------------------------
wumpusFact::Sentence
wumpusFact = [["-B11","P11","P22","P31"],["B11","-P11"],["B11","-P22"],["B11","-P31"]]

----------TASK 2: GENERAL HELPER FUNCTIONS (10 marks)-----------------------------------------------

-- Finds the assigned literal to a symbol from a given model
lookupAssignment :: Symbol -> Model -> Maybe Bool
lookupAssignment symbol model = if null (filter ((==symbol).fst) model) --checks if the symbol exists somewhere in the list of models
then Nothing
else if filter ((==symbol).fst) model == [(symbol, True)] --checks what the boolean associated with symbol is, then returns Just True or Just False
then Just True
else Just False

-- Negate a symbol
negateSymbol :: Symbol -> Symbol
negateSymbol symbol = '-' : symbol --adds '-'

-- For a given symbol, this function checks if it is negated(i.e., has a negation sign).
isNegated :: Symbol -> Bool
isNegated symbol = if head symbol == '-'
then True
else False

-- This function takes a symbol and returns an Symbol without any negation sign if the original 
-- symbol had one.
getUnsignedSymbol :: Symbol -> Symbol
getUnsignedSymbol symbol = if isNegated symbol -- checks if it's already negated
then drop 1 symbol -- removes negation
else symbol

-- Gets a list of all symbols in for all given sentences

getSymbols :: [Sentence] -> [Symbol]
getSymbols stmts = nub (map getUnsignedSymbol ((concat(concat stmts)))) --concats the sentence to reduce into simply just symbols. If there are multiple symbols, it uses nub and removes the multiples.


----------TASK 3: TRUTH TABLE ENUMERATION AND ENTAILMENT (40 marks)---------------------------------


-- Function takes as input a list of symbols, and returns a list of models (all possible assignment 
-- of True or False to the symbols.)

generateModels :: [Symbol] -> [Model]
generateModels symbols = mapM (\symb -> [(symb,True), (symb,False)]) symbols --mapM is equivalent to sequence mapping. Maps True and False to each element in Symbols and creates the final list of all combinations.

-- This function evaluates the truth value of a propositional sentence using the symbols 
-- assignments in the model.

--helper functions:
--clauseChecker :: Clause -> Model -> Bool
--clauseChecker clause model = do
--b -> map lookupAssignment clause

pLogicEvaluate :: Sentence -> Model -> Bool
pLogicEvaluate stmt model = and[or[if isNegated symbol then not (fromJust(lookupAssignment (getUnsignedSymbol symbol) model)) else (fromJust(lookupAssignment (getUnsignedSymbol symbol) model)) |symbol<-individualSymbols ] |individualSymbols<-stmt]

-- splits up the stmt into clauses, then splits up into symbols. checks the assignment of symbols or reverses the assignment if the symbol is negated in the clause, or's them together, then and's them together in the end to give final result.

-- This function checks the truth value of list of a propositional sentence using the symbols 
-- assignments in the model. It returns true only when all sentences in the list are true.

plTrue :: [Sentence]-> Model -> Bool 
plTrue sentences model = if and[pLogicEvaluate stmt model |stmt<-sentences] -- and's together multiple evaluated statements.
then True
else False


--ttCheckAllHelper is built to be able to take a model and is essentially a haskell representation of the pseudoCode for ttCheckAll. 
ttCheckAllHelper :: [Sentence] -> Sentence -> [Symbol] -> Model -> [Model]
ttCheckAllHelper kb query symbols model = 
    if symbols == []
    then if plTrue kb model --checks truth of kb against model
        then if plTrue [query] model then [model] else [] --returns list of models if the query is true, otherwise []
        else [model] 
    else let (first:rest)=symbols in -- recursive format to call ttCheckAllHelper with newly inserted models. 
        ((ttCheckAllHelper kb query rest (insert (first, True) model)) ++ (ttCheckAllHelper kb query rest (insert (first, False) model)))


-- This function takes as input a knowledgebase (i.e. a list of propositional sentences), 
-- a query (i.e. a propositional sentence), and a list of symbols.
-- IT recursively enumerates the models of the domain using its symbols to check if there -- is a model that satisfies the knowledge base and the query. It returns a list of all such models.
ttCheckAll :: [Sentence] -> Sentence -> [Symbol] -> [Model]
ttCheckAll kb query symbols = ttCheckAllHelper kb query symbols []

-- This function determines if a model satisfes both the knowledge base and the query, returning
-- true or false.
ttEntails :: [Sentence] -> Sentence -> Bool
ttEntails kb query = let models = ttCheckAll kb query symbols in --generates list of models
                     if models == generateModels symbols then True else False --if they are the same, true
                     where symbols = nub((getSymbols kb) ++ (getSymbols [query])) --conditions for where


-- This function determines if a model satisfes both the knowledge base and the query. 
-- It returns a list of all models for which the knowledge base entails the query.

ttEntailsModels :: [Sentence] -> Sentence -> [Model]
ttEntailsModels kb query =  if ttEntails kb query 
                                then ttCheckAll kb query symbols
                            else []
                            where symbols = ((getSymbols kb) ++ (getSymbols [query]))



----------TASK 4: DPLL (43 marks)-------------------------------------------------------------------

-- The early termination function checks if a sentence is true or false even with a 
-- partially completed model.
 
earlyTerminate :: Sentence -> Model -> Bool
earlyTerminate sentence model = let (first:rest)=sentence in --recursive setup
    if or[if isNegated symbol then not (fromJust(lookupAssignment (getUnsignedSymbol symbol) model)) else (fromJust(lookupAssignment(getUnsignedSymbol symbol) model)) | symbol <- first , lookupAssignment (getUnsignedSymbol symbol) model /= Nothing ] --or's together clauses and decides whether or not it can be earlyTerminated based on symbols in model
    then if length rest == 0 --if it's gotten through every clauses then true
        then True 
        else and([True,earlyTerminate rest model]) --continues recursion 
    else False
   

-- This function is a helper function for findPureSymbol and finds the opposite of the symbol passed to it. If you pass it "a", "-a" is returned, and the converse. 
symbolFlipper :: Symbol -> Symbol
symbolFlipper symbol = if head symbol == '-' then getUnsignedSymbol symbol else negateSymbol symbol

-- This function finds pure symbol, i.e, a symbol that always appears with the same "sign" in all 
-- clauses.
-- It takes a list of symbols, a list of clauses and a model as inputs. 
-- It returns Just a tuple of a symbol and the truth value to assign to that
-- symbol. If no pure symbol is found, it should return Nothing

findPureSymbol :: [Symbol] -> [Clause] -> Model -> Maybe (Symbol, Bool)
findPureSymbol symbols clauses model = 

    if pureSymbolsList clauses model /= [] 
    then Just (if isNegated(pureSymbol clauses model) then (symbolFlipper(pureSymbol clauses model), False) else (pureSymbol clauses model, True)) --determines how to assign boolean to pureSymbol. 
    else Nothing

    where
    pureSymbol clauses model = head(pureSymbolsList clauses model) --grabs the first pureSymbol in the list. 
    pureSymbolsList clauses model = pureSymbolsInClauseFinder (nub(concat [clause | clause <- (removeTrueClauses clauses model)])) model --list of all pure symbols after removing true clauses and removing duplicates.
    pureSymbolsInClauseFinder clause model = [symbol | symbol <- clause, not(elem (symbolFlipper symbol) clause), not(lookupAssignment symbol model /= Nothing)] --finds teh pureSymbols in an individual clause
    removeTrueClauses clauses model = [clause |clause <-clauses, not(clauseTruth clause model)] --remmoves clause from list of clauses if it's true (aka can be ignored)
    clauseTruth clause model = or[if symb /= symbol then not val else val | (symb,val) <- model, symbol <- clause, getUnsignedSymbol(symb) == getUnsignedSymbol(symbol)] --determines if an individual clause is true or not


-- This function finds a unit clause from a given list of clauses and a model of assignments.
-- It returns Just a tuple of a symbol and the truth value to assign to that symbol. If no unit  
-- clause is found, it should return Nothing.

findUnitClause :: [Clause] -> Model -> Maybe (Symbol, Bool)

findUnitClause clauses model = 
    if newClauses clauses model /= [] 
    then Just (if isNegated(firstClause clauses model) then (symbolFlipper(firstClause clauses model), False) else (firstClause clauses model, True)) --determines how to assign boolean to unitClause
    else Nothing
    where
    firstClause clauses model = head(concat(concat([newClauses clauses model])))--concat's and grabs the first unit clause. 
    newClauses clauses model = nub(concat([singleClauseFinder (map (clauseTransformer (model)) clauses)] ++ [singleClauseFinder clauses])) --adds both lists together.
    clauseTransformer model clause= [symbol |symbol<- clause, not(lookupAssignment (getUnsignedSymbol(symbol)) model /= Nothing)]--determines if a clause of a longer size can be converted to a unit clause. 
    singleClauseFinder clauses = [clause |clause<-clauses, length(clause)==1] --finds clauses of length 1. 



   
-- This function check the satisfability of a sentence in propositional logic. It takes as input a 
-- list of clauses in CNF, a list of symbols for the domain, and model. 
-- It returns true if there is a model which satises the propositional sentence. 
-- Otherwise it returns false.
dpll :: [Clause] -> [Symbol] -> Bool
dpll clauses symbols = dpllHelper clauses symbols []

--helper function for dpll
dpllHelper :: [Clause] -> [Symbol] -> Model -> Bool
dpllHelper clauses symbols model = 
    if earlyTerminate clauses model -- if earlyTerminate is true
        then pLogicEvaluate clauses model --evaluate the statement. 
    else if findPureSymbol symbols clauses model /= Nothing --time for some hueristics. 
        then dpllHelper clauses (delete (fst (fromJust(findPureSymbol symbols clauses model))) symbols) (insert (fromJust(findPureSymbol symbols clauses model)) model) --call to dpll with new parameters
    else if findUnitClause clauses model /= Nothing
        then dpllHelper clauses (delete (fst (fromJust(findUnitClause clauses model))) symbols) (insert (fromJust(findUnitClause clauses model)) model) --call to dpll with edited parameters again. 
    else let (p:rest)=symbols in --recursive part. 
        or[dpllHelper clauses rest (insert (p,True) model), dpllHelper clauses rest (insert (p,False) model)] --or's together the two dpll's with new models, one with P and True, one with False. 

-- This function serves as the entry point to the dpll function. It takes a list clauses in CNF as 
-- input and returns true or false. 
-- It uses the dpll function above to determine the satisability of its input sentence.
dpllSatisfiable :: [Clause] -> Bool
dpllSatisfiable clauses = dpll clauses (getSymbols [clauses]) --calls to dpll, uses symbols in the clauses. 

----------TASK 5: EVALUATION (5 marks)--------------------------------------------------------------
-- EVALUATION
-- a knowledge base (i.e. a sentence in propositional 
-- logic), and a query sentence. Both items should have their clauses in CNF representation 
-- and should be assigned to the following respectively:
evalKB :: [Sentence]
evalKB = [[["A","B"],["B","-C"],["-A","C"]],[["-A","-B"],["C"]]] --just a sentence

evalQuery :: Sentence
evalQuery = [["A","B"],["C"]] --query sentence


-- RUNTIMES
-- Enter the average runtimes of the ttEntails and dpllSatisable functions respectively
runtimeTtEntails :: Double
runtimeTtEntails = 0.000833233

runtimeDpll :: Double
runtimeDpll = 0.00033375

--runtimes are averaged over 6 trials. 

--used for timing purposes of two algorithms DPLL and TtEntailss

main=do
    start1<-getCurrentTime
    evaluate ((ttEntails evalKB evalQuery)) 
    end1<-getCurrentTime

    start2<-getCurrentTime
    evaluate ((dpllSatisfiable [(concat(evalKB,(map symbolFlipper (getSymbols([evalQuery])))))])) --had to call dpll this way for it to work. 
    end2<-getCurrentTime

    print (diffUTCTime end1 start1)
    print (diffUTCTime end2 start2)