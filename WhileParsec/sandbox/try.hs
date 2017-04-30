import WhileParser

-- Utils --
test_state :: State
test_state _ = 0

test_state_x_7 :: State
test_state_x_7 "x" = 7
test_state_x_7 _ = 0

test_state_x_14 :: State
test_state_x_14 "x" = 14
test_state_x_14 _ = 0

-- static: y=5; dynamic: y=6; mixed: y=10
scope_stm = parse " \
\ begin var x:= 0; var y:=0; \
  \ proc p is x:=x*2; \
  \ proc q is call p; \
  \ begin var x := 5; \
    \ proc p is x := x+1; \
    \ call q;\
    \ y := x \
  \ end \
\ end"

-- x=11
recursive_stm = parse "\
\begin var x:=1; \
  \proc fac1 is begin \
    \if x<=10 then \
      \x:=x+1; \
      \call fac1 \
    \else \
      \skip \
  \end; \
  \call fac1 \
\end"


-- Even varible should be 1 if x is even else 0
-- Note load x via a state
mutal_recursion_stm = parse "\
\begin var even:=0; \
  \proc even is begin \
    \if x=0 then \
        \even:=1 \
    \else \
        \x:=x-1; \
        \call odd \
  \end; \
  \proc odd is begin \
    \if x=0 then \
        \even:=0 \
    \else \
        \x:=x-1; \
        \call even \
    \end; \
    \call even \
\end"

test_dynamic_scope = (s_dynamic scope_stm test_state) "y" == 6
test_dynamic_recusion = (s_dynamic recursive_stm test_state) "x" == 11
test_dynamic_mutal_recusion = ((s_dynamic mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_dynamic mutal_recursion_stm test_state_x_14) "even" == 1)

test_mixed_scope = (s_mixed scope_stm test_state) "y" == 10
test_mixed_recusion = (s_mixed recursive_stm test_state) "x" == 11
test_mixed_mutal_recusion = ((s_mixed mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_mixed mutal_recursion_stm test_state_x_14) "even" == 1)

--test_static_scope = (s_static scope_stm test_state) "y" == 5
--test_static_recusion = (s_static recursive_stm test_state) "x" == 11
--test_static_mutal_recusion = ((s_static mutal_recursion_stm test_state_x_7) "even" == 0) && ((s_static mutal_recursion_stm test_state_x_14) "even" == 1)


main = do
  print test_dynamic_scope
  print test_dynamic_recusion
  print test_dynamic_mutal_recusion
  print test_mixed_scope
  print test_mixed_recusion
  print test_mixed_mutal_recusion
  --print test_static_scope
  --print test_static_recusion
  --print test_static_mutal_recusion
