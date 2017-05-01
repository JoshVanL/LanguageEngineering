import WhileParser
test1_stm = parse "\
\a := a + 1; \
\source := source + 0; \
\begin \
    \begin \
        \var b := 234 + b; \
        \var z := 7; \
        \proc p is ( \
            \d := 1234; \
            \f := z \
        \); \
        \proc pp is ( \
            \call p \
        \); \
        \( \
            \b := b + 2; \
            \begin \
                \var b := 100; \
                \var z := 3; \
                \proc p is ( \
                    \d := 9876; \
                    \f := z \
                \); \
                \call pp \
            \end \
        \) \
    \end; \
    \begin \
        \proc recursive is ( \
            \if (source <= 100) then ( \
                \source := source + 1; \
                \call recursive \
            \) else skip \
        \); \
        \call recursive \
    \end \
\end"
 
--runs the functions with initial state created from the first list,
--testStaticScope tests if all var values in the second list match with the produced state's value
main = do
  print (testDynamicScope [("a", 999)] [("a", 1000), ("source", 101), ("b", 0), ("y", 0), ("d", 9876), ("f", 3)] test1_stm)
  print (testMixedScope [("a", 999)] [("a", 1000), ("source", 101), ("b", 0), ("y", 0), ("d", 1234), ("f", 3)] test1_stm)
  --print (testStaticScope [("a", 999)] [("a", 1000), ("source", 101), ("b", 0), ("y", 0), ("d", 1234), ("f", 7)] test1_stm)
  print (runDynamic [("a", 999), ("source", 0), ("b", 0), ("y", 0), ("d", 0), ("f", 0)] test1_stm)
  print (runMixed [("a", 999), ("source", 0), ("b", 0), ("y", 0), ("d", 0), ("f", 0)] test1_stm)
  --print (runStatic [("a", 999), ("source", 0), ("b", 0), ("y", 0), ("d", 0), ("f", 0)] test1_stm)
 
--first list is the initial state, all var values in the second list are compared against the produced state from running the function
testDynamicScope :: [(Var, Z)] -> [(Var, Z)] -> Stm -> Bool
testDynamicScope inputList outputList stm = foldr (\x y -> x && y) True matches
    where result_state = s_dynamic stm (createState inputList)
          matches = map (\(var_name, var_value) -> (var_value == (result_state var_name))) outputList
 
--first list is the initial state, all var values in the second list are compared against the produced state from running the function
testMixedScope :: [(Var, Z)] -> [(Var, Z)] -> Stm -> Bool
testMixedScope inputList outputList stm = foldr (\x y -> x && y) True matches
    where result_state = s_mixed stm (createState inputList)
          matches = map (\(var_name, var_value) -> (var_value == (result_state var_name))) outputList
 
----first list is the initial state, all var values in the second list are compared against the produced state from running the function
--testStaticScope :: [(Var, Z)] -> [(Var, Z)] -> Stm -> Bool
--testStaticScope inputList outputList stm = foldr (\x y -> x && y) True matches
--    where result_state = s_static stm (createState inputList)
--          matches = map (\(var_name, var_value) -> (var_value == (result_state var_name))) outputList
 
runDynamic :: [(Var, Z)] -> Stm -> [(Var, Z)]
runDynamic vars stm = extractState (s_dynamic stm (createState vars)) (fst (unzip vars))
 
runMixed :: [(Var, Z)] -> Stm -> [(Var, Z)]
runMixed vars stm = extractState (s_mixed stm (createState vars)) (fst (unzip vars))
 
--runStatic :: [(Var, Z)] -> Stm -> [(Var, Z)]
--runStatic vars stm = extractState (s_static stm (createState vars)) (fst (unzip vars))
 
--createState :: [(Var, Z)] -> State
--createState vars x = case elemIndex x (fst (unzip vars)) of
--    Just index -> snd (vars !! index)
--    Nothing -> 0
 
extractState :: State -> [Var] -> [(Var, Z)]
extractState state var_names = map (\var_name -> (var_name, state var_name)) var_names
